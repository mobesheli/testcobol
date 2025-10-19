# Architecture Documentation

## Overview

This Python application is a faithful modernization of the COBOL Account Management System, implementing all business rules and validation logic as pure functions while providing a modern REST API interface.

## Design Principles

1. **BRD/FRD Fidelity** - All business logic traces back to documented requirements
2. **Pure Functions** - Business rules implemented as testable pure functions
3. **Repository Pattern** - Clean separation of business and data access logic
4. **DTO Pattern** - Pydantic models for request/response validation
5. **Type Safety** - Full type hints for better IDE support and error prevention

## Architecture Layers

### 1. API Layer (`src/accounting/api/`)

**Responsibility:** HTTP interface and request/response handling

**Files:**
- `app.py` - FastAPI application setup, CORS, lifespan management
- `endpoints.py` - REST endpoint definitions
- `main.py` - Application entry point

**Maps to COBOL:** MainProgram (main.cob) - User interaction module

**Key Principles:**
- Thin layer - only handles HTTP concerns
- Delegates business logic to business_rules layer
- Uses dependency injection for database sessions

### 2. Domain Layer (`src/accounting/domain/`)

**Responsibility:** Data Transfer Objects (DTOs) and domain models

**Files:**
- `models.py` - Pydantic models based on FRD data dictionary

**Maps to COBOL:** Data declarations (WORKING-STORAGE SECTION, LINKAGE SECTION)

**Key Models:**
- `BalanceResponse` - Balance inquiry result
- `CreditRequest` / `DebitRequest` - Transaction requests
- `TransactionResponse` - Transaction results
- `Account` - Account aggregate root

**Type Mapping:**
```python
# COBOL PIC 9 → Python int
# COBOL PIC 9(6)V99 → Python int (stored as cents)
# COBOL PIC X(6) → Python str
```

### 3. Business Rules Layer (`src/accounting/business_rules/`)

**Responsibility:** Pure functions implementing business and validation logic

**Files:**
- `business_rules.py` - BR-01 through BR-09
- `validation_rules.py` - VR-01 through VR-06

**Maps to COBOL:** OperationsProgram (operations.cob) - Transaction processing logic

**Function Naming Convention:**
```python
def br_XX_descriptive_name(...) -> ...:
    """
    BR-XX: Business rule description
    
    Reference: docs/Brd.md line XX
    COBOL: operations.cob:XX-XX
    """
```

**Key Characteristics:**
- ✅ Pure functions (no side effects)
- ✅ Named with BR-XX or VR-XX prefix
- ✅ Docstrings reference BRD/FRD line numbers
- ✅ Unit testable in isolation

### 4. Infrastructure Layer (`src/accounting/infrastructure/`)

**Responsibility:** Database, configuration, and external concerns

**Files:**
- `config.py` - Application settings (Pydantic Settings)
- `database.py` - SQLAlchemy models and session management
- `repository.py` - Repository pattern for data access

**Maps to COBOL:** DataProgram (data.cob) - Data persistence module

**Repository Pattern:**
```python
class AccountRepository:
    def read_balance(self) -> int:
        """BR-06: Read operation"""
        
    def write_balance(self, new_balance: int) -> None:
        """BR-06: Write operation, VR-05: Immediate persistence"""
```

## Data Flow

### Balance Inquiry Flow

```
GET /api/v1/account/balance
    ↓
endpoints.view_balance()
    ↓
AccountRepository.read_balance()  [BR-06 READ]
    ↓
br_03_balance_inquiry()  [BR-03]
    ↓
BalanceResponse.from_cents()
    ↓
JSON Response
```

**COBOL Equivalent:**
```cobol
IF OPERATION-TYPE = 'TOTAL '
    CALL 'DataProgram' USING 'READ', FINAL-BALANCE
    DISPLAY "Current balance: " FINAL-BALANCE
```

### Credit Transaction Flow

```
POST /api/v1/account/credit
    ↓
endpoints.credit_account(CreditRequest)
    ↓
AccountRepository.read_balance()  [BR-06 READ]
    ↓
br_04_credit_processing()  [BR-04]
    ├── vr_06_validate_positive_amount()  [VR-06]
    ├── vr_03_validate_amount_format()  [VR-03]
    └── vr_05_validate_immediate_persistence_preconditions()  [VR-05]
    ↓
AccountRepository.write_balance()  [BR-06 WRITE]
AccountRepository.record_transaction()  [Modernization]
    ↓
TransactionResponse.success_response()
    ↓
JSON Response
```

**COBOL Equivalent:**
```cobol
IF OPERATION-TYPE = 'CREDIT'
    ACCEPT AMOUNT
    CALL 'DataProgram' USING 'READ', FINAL-BALANCE
    ADD AMOUNT TO FINAL-BALANCE
    CALL 'DataProgram' USING 'WRITE', FINAL-BALANCE
    DISPLAY "Amount credited. New balance: " FINAL-BALANCE
```

### Debit Transaction Flow

```
POST /api/v1/account/debit
    ↓
endpoints.debit_account(DebitRequest)
    ↓
AccountRepository.read_balance()  [BR-06 READ]
    ↓
br_05_debit_processing()  [BR-05]
    ├── vr_06_validate_positive_amount()  [VR-06]
    ├── vr_03_validate_amount_format()  [VR-03]
    ├── vr_02_validate_debit_sufficiency()  [VR-02]
    └── vr_05_validate_immediate_persistence_preconditions()  [VR-05]
    ↓
IF success:
    AccountRepository.write_balance()  [BR-06 WRITE]
    AccountRepository.record_transaction()
    → TransactionResponse.success_response()
ELSE:
    AccountRepository.record_transaction(success=False)
    → TransactionResponse.insufficient_funds_response()  [BR-08]
    ↓
JSON Response
```

**COBOL Equivalent:**
```cobol
IF OPERATION-TYPE = 'DEBIT '
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

## Business Rules Traceability

### Validation Rules (VR-01 through VR-06)

| Rule ID | Function | FRD Reference | COBOL Location |
|---------|----------|---------------|----------------|
| VR-01 | `vr_01_validate_menu_selection` | Frd.md:29 | main.cob:22-33 |
| VR-02 | `vr_02_validate_debit_sufficiency` | Frd.md:30 | operations.cob:32 |
| VR-03 | `vr_03_validate_amount_format` | Frd.md:31 | PIC 9(6)V99 |
| VR-04 | `vr_04_validate_session_continuity` | Frd.md:32 | main.cob:11 |
| VR-05 | `vr_05_validate_immediate_persistence_preconditions` | Frd.md:33 | operations.cob:23-25 |
| VR-06 | `vr_06_validate_positive_amount` | Frd.md:34 | (Enhancement) |

### Business Rules (BR-01 through BR-09)

| Rule ID | Function | BRD Reference | COBOL Location |
|---------|----------|---------------|----------------|
| BR-01 | `br_01_menu_navigation` | Brd.md:15 | main.cob:11-34 |
| BR-02 | `br_02_input_validation_menu` | Brd.md:16 | main.cob:22-32 |
| BR-03 | `br_03_balance_inquiry` | Brd.md:17 | operations.cob:16-18 |
| BR-04 | `br_04_credit_processing` | Brd.md:18 | operations.cob:20-26 |
| BR-05 | `br_05_debit_processing` | Brd.md:19 | operations.cob:28-38 |
| BR-06 | `br_06_data_persistence` | Brd.md:20 | data.cob:16-20 |
| BR-07 | `br_07_precision_handling` | Brd.md:21 | PIC 9(6)V99 |
| BR-08 | `br_08_exception_handling_*` | Brd.md:24-25 | main.cob:32, operations.cob:37 |
| BR-09 | `br_09_amount_boundaries` | Brd.md:22 | PIC 9(6)V99 |

## Testing Strategy

### Unit Tests
- Test each VR-XX and BR-XX function in isolation
- No database or HTTP dependencies
- Fast execution (<1s total)

### Parity Tests
- Golden-master tests ensuring Python matches COBOL behavior
- Based on TESTPLAN.md test cases
- Uses real HTTP client and database
- Validates same inputs produce same outputs

### Integration Tests
- End-to-end API tests
- Full stack: HTTP → Business Logic → Database
- Transaction scenarios

## Database Schema

### accounts table
```sql
CREATE TABLE accounts (
    id INTEGER PRIMARY KEY,
    balance_cents INTEGER NOT NULL,  -- PIC 9(6)V99 as cents
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL
);
```

Maps to: `STORAGE-BALANCE PIC 9(6)V99` (data.cob:6)

### transactions table (Modernization Enhancement)
```sql
CREATE TABLE transactions (
    id INTEGER PRIMARY KEY,
    account_id INTEGER NOT NULL,
    transaction_type VARCHAR(10) NOT NULL,  -- 'CREDIT' or 'DEBIT'
    amount_cents INTEGER NOT NULL,
    balance_before_cents INTEGER NOT NULL,
    balance_after_cents INTEGER NOT NULL,
    success BOOLEAN NOT NULL,
    error_message VARCHAR(200),
    created_at TIMESTAMP NOT NULL
);
```

Addresses BRD Pain Point: "No transaction history" (Brd.md:39)

## Money Handling

### Why Cents, Not Dollars?

**Problem:** Floating-point arithmetic is imprecise
```python
0.1 + 0.2 == 0.3  # False in Python!
```

**Solution:** Store money as integer cents
```python
# $1.00 → 100 cents
# $999,999.99 → 99,999,999 cents
balance_cents: int = 100_000  # $1,000.00
```

**COBOL Mapping:**
```cobol
COBOL: PIC 9(6)V99      →  Python: int (cents)
       Range: 0.00-999,999.99  →  0-99,999,999
```

## Configuration

### Environment Variables
```python
class Settings(BaseSettings):
    database_url: str = "sqlite:///./accounting.db"
    app_env: str = "development"
    log_level: str = "INFO"
    initial_balance_cents: int = 100_000  # $1,000.00 (COBOL default)
```

### Configuration Sources (Priority Order)
1. Environment variables
2. `.env` file
3. Default values in Settings class

## Modernization vs. COBOL

### Preserved from COBOL ✅
- All business rules (BR-01 through BR-09)
- All validation rules (VR-01 through VR-06)
- Initial balance: $1,000.00
- Insufficient funds error handling
- Immediate persistence semantics
- Two decimal place precision
- Maximum balance: $999,999.99

### Enhancements (Modernization) 🚀
- RESTful API instead of console interface
- Transaction history / audit trail
- Persistent storage (database)
- Structured error responses
- Type safety with Pydantic
- Comprehensive test suite
- API documentation (Swagger)

### Not Implemented (Out of Scope) ⚠️
- Authentication (BRD limitation: "No authentication")
- Multiple accounts (BRD limitation: "Single account design")
- Interest calculations (BRD: "No interest")
- External integrations (COBOL: standalone system)

## Deployment

### Development
```bash
# SQLite database
DATABASE_URL=sqlite:///./accounting.db
uvicorn accounting.api.main:app --reload
```

### Production
```bash
# PostgreSQL database
DATABASE_URL=postgresql://user:pass@host/db
uvicorn accounting.api.main:app --host 0.0.0.0 --port 8000 --workers 4
```

### Docker (Future)
```dockerfile
FROM python:3.11-slim
WORKDIR /app
COPY requirements.txt .
RUN pip install -r requirements.txt
COPY src/ ./src/
CMD ["uvicorn", "accounting.api.main:app", "--host", "0.0.0.0"]
```

## Future Enhancements

Based on BRD Modernization Objectives (Brd.md:41-46):

1. **Multi-account support** - Extend repository for multiple accounts
2. **Authentication** - Add JWT/OAuth2 for user identification
3. **Role-based access** - Add authorization layer
4. **Interest accrual** - New business rule for interest calculation
5. **Alerts/notifications** - Event-driven notifications
6. **Compliance checks** - Additional validation rules

## References

- **BRD:** `docs/Brd.md` - Business requirements and pain points
- **FRD:** `docs/Frd.md` - Functional specifications and traceability
- **Rules:** `docs/rules/` - Detailed business rules analysis
- **Tests:** `docs/TESTPLAN.md` - Test cases from COBOL system
- **COBOL Source:** `../main.cob`, `../operations.cob`, `../data.cob`

