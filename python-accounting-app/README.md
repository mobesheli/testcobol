# Python Account Management System

Modern Python implementation of the COBOL Account Management System using FastAPI, Pydantic v2, SQLAlchemy 2.0, and pytest.

## 📋 Overview

This application is a faithful modernization of the legacy COBOL accounting system, implementing all business rules and validation logic as specified in the BRD (Business Requirements Document) and FRD (Functional Requirements Document).

**Based on:**
- `docs/Brd.md` - Business Requirements
- `docs/Frd.md` - Functional Requirements  
- `docs/rules/` - Detailed business rules analysis
- `docs/TESTPLAN.md` - Test cases

## 🎯 Features

### Core Operations (From COBOL)
- **Balance Inquiry** - View current account balance (BR-03)
- **Credit Account** - Add funds to account (BR-04)
- **Debit Account** - Withdraw funds from account (BR-05)

### Modernization Enhancements
- **RESTful API** - Web-based interface instead of console
- **Transaction History** - Audit trail of all transactions
- **Persistent Storage** - SQLite (dev) / PostgreSQL (prod)
- **Input Validation** - Comprehensive VR-01 through VR-06
- **API Documentation** - Interactive Swagger UI

## 🏗️ Architecture

```
src/accounting/
├── api/                    # FastAPI application and endpoints
│   ├── app.py             # Application setup
│   ├── endpoints.py       # REST API endpoints
│   └── main.py            # Entry point
├── business_rules/        # Pure functions implementing BR-XX and VR-XX
│   ├── business_rules.py  # BR-01 through BR-09
│   └── validation_rules.py # VR-01 through VR-06
├── domain/                # Pydantic models (DTOs)
│   └── models.py          # Data models from FRD data dictionary
└── infrastructure/        # Database and repositories
    ├── config.py          # Application settings
    ├── database.py        # SQLAlchemy models
    └── repository.py      # Repository pattern for data access
```

## 🚀 Quick Start

### Prerequisites

- Python 3.11+
- Poetry (recommended) or pip

### Installation

1. **Clone the repository**
```bash
cd cobol-accounting-system/python-accounting-app
```

2. **Install dependencies**

Using Poetry (recommended):
```bash
poetry install
```

Using pip:
```bash
pip install -r requirements.txt
```

3. **Configure environment**
```bash
cp env.example .env
# Edit .env if needed (default SQLite works out of the box)
```

4. **Run the application**

Using Poetry:
```bash
poetry run uvicorn accounting.api.main:app --reload
```

Using Python directly:
```bash
uvicorn accounting.api.main:app --reload
```

5. **Access the API**
- API: http://localhost:8000
- Interactive Docs: http://localhost:8000/docs
- Alternative Docs: http://localhost:8000/redoc

## 📚 API Endpoints

### Balance Inquiry
```bash
GET /api/v1/account/balance
```
**COBOL Equivalent:** Menu option 1, `OPERATION-TYPE = 'TOTAL '`

**Example:**
```bash
curl http://localhost:8000/api/v1/account/balance
```

**Response:**
```json
{
  "balance_cents": 100000,
  "balance_display": "$1,000.00"
}
```

### Credit Account
```bash
POST /api/v1/account/credit
Content-Type: application/json

{
  "amount_cents": 50000
}
```
**COBOL Equivalent:** Menu option 2, `OPERATION-TYPE = 'CREDIT'`

**Example:**
```bash
curl -X POST http://localhost:8000/api/v1/account/credit \
  -H "Content-Type: application/json" \
  -d '{"amount_cents": 50000}'
```

**Response:**
```json
{
  "success": true,
  "message": "Amount credited. New balance: $1,500.00",
  "new_balance_cents": 150000,
  "new_balance_display": "$1,500.00",
  "transaction_type": "CREDIT",
  "amount_cents": 50000,
  "timestamp": "2025-10-17T12:00:00Z"
}
```

### Debit Account
```bash
POST /api/v1/account/debit
Content-Type: application/json

{
  "amount_cents": 30000
}
```
**COBOL Equivalent:** Menu option 3, `OPERATION-TYPE = 'DEBIT '`

**Example:**
```bash
curl -X POST http://localhost:8000/api/v1/account/debit \
  -H "Content-Type: application/json" \
  -d '{"amount_cents": 30000}'
```

**Response (Success):**
```json
{
  "success": true,
  "message": "Amount debited. New balance: $700.00",
  "new_balance_cents": 70000,
  "new_balance_display": "$700.00",
  "transaction_type": "DEBIT",
  "amount_cents": 30000,
  "timestamp": "2025-10-17T12:01:00Z"
}
```

**Response (Insufficient Funds):**
```json
{
  "success": false,
  "message": "Insufficient funds for this debit.",
  "new_balance_cents": 70000,
  "new_balance_display": "$700.00",
  "transaction_type": "DEBIT",
  "amount_cents": 100000,
  "timestamp": "2025-10-17T12:02:00Z"
}
```

### Transaction History (Enhancement)
```bash
GET /api/v1/account/transactions?limit=100
```
**Note:** This is a modernization enhancement not present in COBOL.

## 🧪 Testing

### Run All Tests
```bash
poetry run pytest
```

### Run with Coverage
```bash
poetry run pytest --cov=src/accounting --cov-report=html
```

### Run Specific Test Suites

**Unit Tests (Business Rules):**
```bash
poetry run pytest tests/unit/
```

**Parity Tests (COBOL Behavior Matching):**
```bash
poetry run pytest tests/parity/
```

### Test Coverage

The test suite includes:
- ✅ **Unit tests** for all VR-01 through VR-06 validation rules
- ✅ **Unit tests** for all BR-01 through BR-09 business rules
- ✅ **Parity tests** matching COBOL behavior from TESTPLAN.md
- ✅ **Integration tests** for API endpoints

## 📊 Business Rules Implementation

### Validation Rules (VR-01 through VR-06)

| Rule | Description | Reference |
|------|-------------|-----------|
| VR-01 | Menu selection (1-4 only) | Frd.md:29 |
| VR-02 | Debit sufficiency check | Frd.md:30 |
| VR-03 | Amount format (0.01-999,999.99) | Frd.md:31 |
| VR-04 | Session continuity | Frd.md:32 |
| VR-05 | Immediate persistence | Frd.md:33 |
| VR-06 | Positive amount validation | Frd.md:34 |

### Business Rules (BR-01 through BR-09)

| Rule | Description | Reference |
|------|-------------|-----------|
| BR-01 | Menu navigation loop | Brd.md:15 |
| BR-02 | Input validation | Brd.md:16 |
| BR-03 | Balance inquiry | Brd.md:17 |
| BR-04 | Credit processing | Brd.md:18 |
| BR-05 | Debit processing | Brd.md:19 |
| BR-06 | Data persistence | Brd.md:20 |
| BR-07 | Precision handling | Brd.md:21 |
| BR-08 | Exception handling | Brd.md:24-25 |
| BR-09 | Amount boundaries | Brd.md:22 |

## 🗄️ Database

### SQLite (Development)
Default configuration uses SQLite for easy setup:
```
DATABASE_URL=sqlite:///./accounting.db
```

### PostgreSQL (Production)
For production, configure PostgreSQL:
```
DATABASE_URL=postgresql://user:password@localhost/accounting
```

### Schema

**accounts table:**
```sql
CREATE TABLE accounts (
    id INTEGER PRIMARY KEY,
    balance_cents INTEGER NOT NULL,
    created_at TIMESTAMP NOT NULL,
    updated_at TIMESTAMP NOT NULL
);
```

**transactions table:**
```sql
CREATE TABLE transactions (
    id INTEGER PRIMARY KEY,
    account_id INTEGER NOT NULL,
    transaction_type VARCHAR(10) NOT NULL,
    amount_cents INTEGER NOT NULL,
    balance_before_cents INTEGER NOT NULL,
    balance_after_cents INTEGER NOT NULL,
    success BOOLEAN NOT NULL,
    error_message VARCHAR(200),
    created_at TIMESTAMP NOT NULL
);
```

## 🔧 Configuration

Configuration is managed through environment variables (see `env.example`):

| Variable | Description | Default |
|----------|-------------|---------|
| DATABASE_URL | Database connection string | sqlite:///./accounting.db |
| APP_ENV | Environment (development/production) | development |
| LOG_LEVEL | Logging level | INFO |
| INITIAL_BALANCE_CENTS | Initial account balance | 100000 ($1,000.00) |

## 📖 COBOL Mapping

### Data Types
| COBOL | Python | Storage |
|-------|--------|---------|
| PIC 9 | int | Integer |
| PIC 9(6)V99 | int | Cents (avoid float) |
| PIC X(6) | str | String |

### Operations Mapping
| COBOL Operation | Python Endpoint | Function |
|-----------------|-----------------|----------|
| EVALUATE USER-CHOICE | Menu routing | br_01_menu_navigation |
| IF OPERATION-TYPE = 'TOTAL ' | GET /balance | br_03_balance_inquiry |
| IF OPERATION-TYPE = 'CREDIT' | POST /credit | br_04_credit_processing |
| IF OPERATION-TYPE = 'DEBIT ' | POST /debit | br_05_debit_processing |

### COBOL Programs → Python Modules
| COBOL Program | Python Module | Responsibility |
|---------------|---------------|----------------|
| MainProgram (main.cob) | api/app.py | User interface / routing |
| OperationsProgram (operations.cob) | business_rules/*.py | Business logic |
| DataProgram (data.cob) | infrastructure/repository.py | Data persistence |

## 🚧 Modernization Enhancements

This implementation addresses pain points identified in the BRD:

1. **✅ Persistent Storage** - Database replaces in-memory storage
2. **✅ Transaction History** - Audit trail for all operations
3. **✅ Web API** - RESTful interface for modern integration
4. **✅ Validation Layer** - Comprehensive input validation
5. **✅ Error Handling** - Structured error responses

## 🔐 Security Considerations

**Current Implementation:**
- ❌ No authentication (matches COBOL behavior)
- ❌ Single account system (matches COBOL)
- ✅ Input validation (VR-01 through VR-06)
- ✅ SQL injection protection (SQLAlchemy ORM)

**For Production:**
- Add authentication (JWT, OAuth2)
- Add authorization / role-based access
- Implement rate limiting
- Add HTTPS/TLS
- Configure CORS appropriately

## 📄 License

This is a demonstration/educational project implementing the COBOL specification from the parent repository.

## 🤝 Contributing

This project strictly follows the BRD and FRD specifications. Any changes must:
1. Reference specific BR-XX or VR-XX rules
2. Include docstring references to docs/
3. Add corresponding tests
4. Maintain parity with COBOL behavior

## 📞 Support

For questions about:
- **Business rules**: See `docs/Brd.md` and `docs/Frd.md`
- **COBOL mapping**: See `docs/rules/`
- **Test cases**: See `docs/TESTPLAN.md`
- **API usage**: Visit http://localhost:8000/docs

