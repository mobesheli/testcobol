"""
Repository pattern for data access.

Maps to: DataProgram module (data.cob)
Reference: docs/Frd.md line 6 - Balance Persistence Module
"""

from typing import Optional

from sqlalchemy.orm import Session

from accounting.domain.models import Account
from accounting.infrastructure.config import settings
from accounting.infrastructure.database import AccountModel, TransactionModel


class AccountRepository:
    """
    Repository for account data access.
    
    Implements: BR-06 Data Persistence
    Maps to: COBOL DataProgram (data.cob)
    Reference: docs/Brd.md line 20, docs/Frd.md line 6
    """
    
    def __init__(self, db: Session):
        """
        Initialize repository.
        
        Args:
            db: SQLAlchemy database session
        """
        self.db = db
    
    def get_or_create_default_account(self) -> Account:
        """
        Get the default account or create it with initial balance.
        
        Maps to: COBOL STORAGE-BALANCE PIC 9(6)V99 VALUE 1000.00 (data.cob:6)
        Reference: docs/Brd.md line 20 - "starting balance initializes to 1,000.00"
        
        Returns:
            Account domain model
        """
        # Get or create account with ID 1 (single account system per BRD)
        account_model = self.db.query(AccountModel).filter(AccountModel.id == 1).first()
        
        if not account_model:
            # Create with initial balance from settings (default 100,000 cents = $1,000.00)
            account_model = AccountModel(
                id=1,
                balance_cents=settings.initial_balance_cents
            )
            self.db.add(account_model)
            self.db.commit()
            self.db.refresh(account_model)
        
        return Account.model_validate(account_model)
    
    def read_balance(self, account_id: int = 1) -> int:
        """
        Read current balance.
        
        Implements: BR-06 with operation='READ'
        Maps to: COBOL IF OPERATION-TYPE = 'READ' (data.cob:16-17)
        Reference: docs/Frd.md line 6
        
        Args:
            account_id: Account ID (default 1 for single account system)
            
        Returns:
            Balance in cents
        """
        account = self.get_or_create_default_account()
        return account.balance_cents
    
    def write_balance(self, new_balance_cents: int, account_id: int = 1) -> None:
        """
        Write new balance.
        
        Implements: BR-06 with operation='WRITE', VR-05 Immediate Persistence
        Maps to: COBOL IF OPERATION-TYPE = 'WRITE' (data.cob:19-20)
        Reference: docs/Frd.md line 6, line 33
        
        Args:
            new_balance_cents: New balance in cents
            account_id: Account ID (default 1 for single account system)
        """
        account_model = self.db.query(AccountModel).filter(AccountModel.id == account_id).first()
        
        if account_model:
            account_model.balance_cents = new_balance_cents
            self.db.commit()
    
    def record_transaction(
        self,
        account_id: int,
        transaction_type: str,
        amount_cents: int,
        balance_before_cents: int,
        balance_after_cents: int,
        success: bool,
        error_message: Optional[str] = None
    ) -> None:
        """
        Record transaction history.
        
        Enhancement: Implements audit trail (BRD Modernization Objective).
        Reference: docs/Brd.md line 42 - "durable, auditable persistence"
        Reference: docs/Brd.md line 39 - Pain point: "No transaction history"
        
        Args:
            account_id: Account ID
            transaction_type: 'CREDIT' or 'DEBIT'
            amount_cents: Transaction amount in cents
            balance_before_cents: Balance before transaction
            balance_after_cents: Balance after transaction
            success: Whether transaction succeeded
            error_message: Error message if failed
        """
        transaction = TransactionModel(
            account_id=account_id,
            transaction_type=transaction_type,
            amount_cents=amount_cents,
            balance_before_cents=balance_before_cents,
            balance_after_cents=balance_after_cents,
            success=success,
            error_message=error_message
        )
        self.db.add(transaction)
        self.db.commit()

