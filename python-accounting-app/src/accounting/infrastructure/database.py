"""
Database models using SQLAlchemy 2.0.

Maps COBOL data structures to relational model:
- STORAGE-BALANCE → accounts table
- Transaction history (enhancement over COBOL for modernization)
"""

from datetime import datetime
from typing import Optional

from sqlalchemy import String, create_engine
from sqlalchemy.orm import DeclarativeBase, Mapped, Session, mapped_column, sessionmaker

from accounting.infrastructure.config import settings


class Base(DeclarativeBase):
    """Base class for all SQLAlchemy models."""
    pass


class AccountModel(Base):
    """
    Account database model.
    
    Maps to: COBOL STORAGE-BALANCE (data.cob:6)
    Reference: docs/Brd.md line 12, docs/Frd.md line 6
    
    Enhancement: Adds timestamp tracking for modernization (not in COBOL).
    """
    
    __tablename__ = "accounts"
    
    id: Mapped[int] = mapped_column(primary_key=True, autoincrement=True)
    balance_cents: Mapped[int] = mapped_column(nullable=False, default=100_000)
    created_at: Mapped[datetime] = mapped_column(nullable=False, default=datetime.utcnow)
    updated_at: Mapped[datetime] = mapped_column(
        nullable=False,
        default=datetime.utcnow,
        onupdate=datetime.utcnow
    )
    
    def __repr__(self) -> str:
        """String representation."""
        return f"<Account(id={self.id}, balance_cents={self.balance_cents})>"


class TransactionModel(Base):
    """
    Transaction history model.
    
    Enhancement over COBOL: Adds audit trail and transaction history.
    Reference: docs/Brd.md line 39 (Pain Point: "No transaction history")
    Reference: docs/Brd.md line 42 (Modernization: "durable, auditable persistence")
    
    This addresses BRD Pain Point about lack of transaction history.
    """
    
    __tablename__ = "transactions"
    
    id: Mapped[int] = mapped_column(primary_key=True, autoincrement=True)
    account_id: Mapped[int] = mapped_column(nullable=False)
    transaction_type: Mapped[str] = mapped_column(String(10), nullable=False)  # CREDIT or DEBIT
    amount_cents: Mapped[int] = mapped_column(nullable=False)
    balance_before_cents: Mapped[int] = mapped_column(nullable=False)
    balance_after_cents: Mapped[int] = mapped_column(nullable=False)
    success: Mapped[bool] = mapped_column(nullable=False, default=True)
    error_message: Mapped[Optional[str]] = mapped_column(String(200), nullable=True)
    created_at: Mapped[datetime] = mapped_column(nullable=False, default=datetime.utcnow)
    
    def __repr__(self) -> str:
        """String representation."""
        return f"<Transaction(id={self.id}, type={self.transaction_type}, amount={self.amount_cents})>"


# Database engine and session factory
engine = create_engine(
    settings.database_url,
    echo=settings.log_level == "DEBUG",
    connect_args={"check_same_thread": False} if settings.is_sqlite else {}
)

SessionLocal = sessionmaker(
    autocommit=False,
    autoflush=False,
    bind=engine
)


def create_tables() -> None:
    """Create all database tables."""
    Base.metadata.create_all(bind=engine)


def get_db() -> Session:
    """
    Get database session for dependency injection.
    
    Yields:
        Database session
    """
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()

