"""
API endpoints for account operations.

Maps COBOL operations to REST endpoints:
- TOTAL (view balance) → GET /balance
- CREDIT (add funds) → POST /credit
- DEBIT (withdraw funds) → POST /debit
"""

from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session

from accounting.business_rules.business_rules import (
    br_03_balance_inquiry,
    br_04_credit_processing,
    br_05_debit_processing,
)
from accounting.domain.models import (
    BalanceResponse,
    CreditRequest,
    DebitRequest,
    OperationType,
    TransactionResponse,
)
from accounting.infrastructure.database import get_db
from accounting.infrastructure.repository import AccountRepository

router = APIRouter(prefix="/api/v1/account", tags=["account"])


@router.get("/balance", response_model=BalanceResponse)
async def view_balance(db: Session = Depends(get_db)) -> BalanceResponse:
    """
    View current account balance.
    
    Implements: BR-03 Balance Inquiry
    Maps to: COBOL menu option 1, operations.cob:16-18
    Reference: docs/Brd.md line 17, docs/Frd.md line 14
    
    COBOL equivalent:
    ```cobol
    IF OPERATION-TYPE = 'TOTAL '
        CALL 'DataProgram' USING 'READ', FINAL-BALANCE
        DISPLAY "Current balance: " FINAL-BALANCE
    ```
    
    Returns:
        BalanceResponse with current balance
    """
    repo = AccountRepository(db)
    
    # BR-06: Read balance (data persistence)
    balance_cents = repo.read_balance()
    
    # BR-03: Format and return balance
    balance_data = br_03_balance_inquiry(balance_cents)
    
    return BalanceResponse(
        balance_cents=balance_data["balance_cents"],
        balance_display=balance_data["balance_display"]
    )


@router.post("/credit", response_model=TransactionResponse)
async def credit_account(
    request: CreditRequest,
    db: Session = Depends(get_db)
) -> TransactionResponse:
    """
    Credit account (add funds).
    
    Implements: BR-04 Credit Processing
    Maps to: COBOL menu option 2, operations.cob:20-26
    Reference: docs/Brd.md line 18, docs/Frd.md line 15
    
    COBOL equivalent:
    ```cobol
    IF OPERATION-TYPE = 'CREDIT'
        DISPLAY "Enter credit amount: "
        ACCEPT AMOUNT
        CALL 'DataProgram' USING 'READ', FINAL-BALANCE
        ADD AMOUNT TO FINAL-BALANCE
        CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
        DISPLAY "Amount credited. New balance: " FINAL-BALANCE
    ```
    
    Business Rules Applied:
    - VR-06: Positive amount validation
    - VR-03: Amount format validation  
    - VR-05: Immediate persistence
    
    Args:
        request: Credit request with amount_cents
        db: Database session
        
    Returns:
        TransactionResponse with result
        
    Raises:
        HTTPException: If validation fails
    """
    repo = AccountRepository(db)
    
    # BR-06: Read current balance
    current_balance_cents = repo.read_balance()
    
    # BR-04: Process credit (includes VR-03, VR-05, VR-06)
    success, new_balance_cents, message = br_04_credit_processing(
        current_balance_cents,
        request.amount_cents
    )
    
    if not success:
        # Record failed transaction
        repo.record_transaction(
            account_id=1,
            transaction_type=OperationType.CREDIT.value,
            amount_cents=request.amount_cents,
            balance_before_cents=current_balance_cents,
            balance_after_cents=current_balance_cents,
            success=False,
            error_message=message
        )
        raise HTTPException(status_code=400, detail=message)
    
    # BR-06: Write new balance (VR-05: immediate persistence)
    repo.write_balance(new_balance_cents)
    
    # Record successful transaction (modernization enhancement)
    repo.record_transaction(
        account_id=1,
        transaction_type=OperationType.CREDIT.value,
        amount_cents=request.amount_cents,
        balance_before_cents=current_balance_cents,
        balance_after_cents=new_balance_cents,
        success=True
    )
    
    return TransactionResponse.success_response(
        operation=OperationType.CREDIT,
        amount_cents=request.amount_cents,
        new_balance_cents=new_balance_cents
    )


@router.post("/debit", response_model=TransactionResponse)
async def debit_account(
    request: DebitRequest,
    db: Session = Depends(get_db)
) -> TransactionResponse:
    """
    Debit account (withdraw funds).
    
    Implements: BR-05 Debit Processing
    Maps to: COBOL menu option 3, operations.cob:28-38
    Reference: docs/Brd.md line 19, docs/Frd.md line 16
    
    COBOL equivalent:
    ```cobol
    IF OPERATION-TYPE = 'DEBIT '
        DISPLAY "Enter debit amount: "
        ACCEPT AMOUNT
        CALL 'DataProgram' USING 'READ', FINAL-BALANCE
        IF FINAL-BALANCE >= AMOUNT
            SUBTRACT AMOUNT FROM FINAL-BALANCE
            CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
            DISPLAY "Amount debited. New balance: " FINAL-BALANCE
        ELSE
            DISPLAY "Insufficient funds for this debit."
        END-IF
    ```
    
    Business Rules Applied:
    - VR-06: Positive amount validation
    - VR-03: Amount format validation
    - VR-02: Sufficient funds check
    - VR-05: Immediate persistence
    
    Args:
        request: Debit request with amount_cents
        db: Database session
        
    Returns:
        TransactionResponse with result (may indicate insufficient funds)
    """
    repo = AccountRepository(db)
    
    # BR-06: Read current balance
    current_balance_cents = repo.read_balance()
    
    # BR-05: Process debit (includes VR-02, VR-03, VR-05, VR-06)
    success, new_balance_cents, message = br_05_debit_processing(
        current_balance_cents,
        request.amount_cents
    )
    
    if not success:
        # Record failed transaction (with reason)
        repo.record_transaction(
            account_id=1,
            transaction_type=OperationType.DEBIT.value,
            amount_cents=request.amount_cents,
            balance_before_cents=current_balance_cents,
            balance_after_cents=current_balance_cents,
            success=False,
            error_message=message
        )
        
        # BR-08: Return insufficient funds response (not an HTTP error, per COBOL behavior)
        return TransactionResponse.insufficient_funds_response(
            amount_cents=request.amount_cents,
            current_balance_cents=current_balance_cents
        )
    
    # BR-06: Write new balance (VR-05: immediate persistence)
    repo.write_balance(new_balance_cents)
    
    # Record successful transaction
    repo.record_transaction(
        account_id=1,
        transaction_type=OperationType.DEBIT.value,
        amount_cents=request.amount_cents,
        balance_before_cents=current_balance_cents,
        balance_after_cents=new_balance_cents,
        success=True
    )
    
    return TransactionResponse.success_response(
        operation=OperationType.DEBIT,
        amount_cents=request.amount_cents,
        new_balance_cents=new_balance_cents
    )


@router.get("/transactions")
async def get_transaction_history(
    limit: int = 100,
    db: Session = Depends(get_db)
) -> dict:
    """
    Get transaction history.
    
    Enhancement: Addresses BRD Pain Point - "No transaction history"
    Reference: docs/Brd.md line 39, docs/Brd.md line 42
    
    This is a modernization feature not present in the COBOL implementation.
    
    Args:
        limit: Maximum number of transactions to return
        db: Database session
        
    Returns:
        List of transaction records
    """
    from accounting.infrastructure.database import TransactionModel
    
    transactions = (
        db.query(TransactionModel)
        .order_by(TransactionModel.created_at.desc())
        .limit(limit)
        .all()
    )
    
    return {
        "transactions": [
            {
                "id": t.id,
                "type": t.transaction_type,
                "amount_cents": t.amount_cents,
                "amount_display": f"${t.amount_cents / 100:.2f}",
                "balance_before_cents": t.balance_before_cents,
                "balance_after_cents": t.balance_after_cents,
                "success": t.success,
                "error_message": t.error_message,
                "timestamp": t.created_at.isoformat()
            }
            for t in transactions
        ]
    }

