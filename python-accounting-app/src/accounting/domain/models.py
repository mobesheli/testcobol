"""
Domain models based on FRD data dictionary (docs/Frd.md Table: Input/Output Specifications).

Maps COBOL PIC formats to Python types:
- PIC 9: int
- PIC 9(6)V99: int (stored as cents to avoid floating point issues)
- PIC X(6): str
"""

from datetime import datetime
from enum import Enum
from typing import Annotated

from pydantic import BaseModel, Field, field_validator


class OperationType(str, Enum):
    """
    Operation codes from FRD line 24.
    
    COBOL: PASSED-OPERATION PIC X(6)
    Values: 'TOTAL ', 'CREDIT', 'DEBIT '
    """

    TOTAL = "TOTAL"
    CREDIT = "CREDIT"
    DEBIT = "DEBIT"


class MenuChoice(int, Enum):
    """
    User menu choices from FRD line 22.
    
    COBOL: USER-CHOICE PIC 9
    Range: 1-4
    Maps to: BR-01 Menu Navigation
    """

    VIEW_BALANCE = 1
    CREDIT_ACCOUNT = 2
    DEBIT_ACCOUNT = 3
    EXIT = 4


# Type aliases for money amounts (stored as cents)
# COBOL: PIC 9(6)V99 → 0.01 to 999,999.99 → 1 to 99,999,999 cents
AmountCents = Annotated[
    int,
    Field(ge=1, le=99_999_999, description="Amount in cents (1 cent to $999,999.99)")
]

BalanceCents = Annotated[
    int,
    Field(ge=0, le=99_999_999, description="Balance in cents (0 to $999,999.99)")
]


class BalanceResponse(BaseModel):
    """
    Response for balance inquiries.
    
    Maps to: BR-03 Balance Inquiry, FRD line 25
    COBOL: FINAL-BALANCE PIC 9(6)V99
    """

    balance_cents: BalanceCents
    balance_display: str = Field(description="Formatted balance like $1,000.00")
    
    @classmethod
    def from_cents(cls, cents: int) -> "BalanceResponse":
        """Create from cents value."""
        dollars = cents / 100
        return cls(
            balance_cents=cents,
            balance_display=f"${dollars:,.2f}"
        )


class CreditRequest(BaseModel):
    """
    Credit transaction request.
    
    Maps to: BR-04 Credit Processing, VR-03, VR-06
    COBOL: AMOUNT PIC 9(6)V99 from FRD line 23
    Reference: docs/Frd.md line 31-34 (VR-03, VR-06)
    """

    amount_cents: AmountCents

    @field_validator("amount_cents")
    @classmethod
    def validate_positive(cls, v: int) -> int:
        """
        Implements VR-06: Reject negative or zero amounts.
        Reference: docs/Frd.md line 34
        """
        if v <= 0:
            raise ValueError("Amount must be positive (VR-06)")
        return v


class DebitRequest(BaseModel):
    """
    Debit transaction request.
    
    Maps to: BR-05 Debit Processing, VR-02, VR-03, VR-06
    COBOL: AMOUNT PIC 9(6)V99 from FRD line 23
    Reference: docs/Frd.md line 30, 31, 34
    """

    amount_cents: AmountCents

    @field_validator("amount_cents")
    @classmethod
    def validate_positive(cls, v: int) -> int:
        """
        Implements VR-06: Reject negative or zero amounts.
        Reference: docs/Frd.md line 34
        """
        if v <= 0:
            raise ValueError("Amount must be positive (VR-06)")
        return v


class TransactionResponse(BaseModel):
    """
    Response for credit/debit transactions.
    
    Maps to: BR-04, BR-05 transaction confirmations
    COBOL: FINAL-BALANCE PIC 9(6)V99
    """

    success: bool
    message: str
    new_balance_cents: BalanceCents
    new_balance_display: str
    transaction_type: OperationType
    amount_cents: int
    timestamp: datetime = Field(default_factory=datetime.utcnow)

    @classmethod
    def success_response(
        cls,
        operation: OperationType,
        amount_cents: int,
        new_balance_cents: int
    ) -> "TransactionResponse":
        """Create a successful transaction response."""
        dollars = new_balance_cents / 100
        return cls(
            success=True,
            message=f"Amount {'credited' if operation == OperationType.CREDIT else 'debited'}. New balance: ${dollars:,.2f}",
            new_balance_cents=new_balance_cents,
            new_balance_display=f"${dollars:,.2f}",
            transaction_type=operation,
            amount_cents=amount_cents
        )

    @classmethod
    def insufficient_funds_response(
        cls,
        amount_cents: int,
        current_balance_cents: int
    ) -> "TransactionResponse":
        """
        Create insufficient funds error response.
        
        Implements BR-05: Debit Processing, VR-02
        Reference: docs/Frd.md line 30, docs/Brd.md line 19
        """
        dollars = current_balance_cents / 100
        return cls(
            success=False,
            message="Insufficient funds for this debit.",
            new_balance_cents=current_balance_cents,
            new_balance_display=f"${dollars:,.2f}",
            transaction_type=OperationType.DEBIT,
            amount_cents=amount_cents
        )


class Account(BaseModel):
    """
    Account aggregate root.
    
    Maps to: DataProgram storage (STORAGE-BALANCE)
    COBOL: STORAGE-BALANCE PIC 9(6)V99 VALUE 1000.00 (data.cob:6)
    Reference: docs/Brd.md line 12, docs/Frd.md line 6
    """

    id: int
    balance_cents: BalanceCents
    created_at: datetime = Field(default_factory=datetime.utcnow)
    updated_at: datetime = Field(default_factory=datetime.utcnow)

    class Config:
        from_attributes = True

