"""
FastAPI application.

Maps COBOL console interface to REST API:
- COBOL menu options → API endpoints
- DISPLAY statements → JSON responses
- ACCEPT statements → Request bodies
"""

from contextlib import asynccontextmanager
from typing import AsyncGenerator

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from accounting.infrastructure.database import create_tables


@asynccontextmanager
async def lifespan(app: FastAPI) -> AsyncGenerator[None, None]:
    """
    Application lifespan context manager.
    
    Creates database tables on startup.
    """
    # Startup
    create_tables()
    yield
    # Shutdown (cleanup if needed)


app = FastAPI(
    title="Account Management System API",
    description="""
    Modern Python implementation of COBOL Account Management System.
    
    Based on Business Requirements Document (BRD) and Functional Requirements Document (FRD)
    in docs/ directory.
    
    Implements:
    - Business Rules BR-01 through BR-09
    - Validation Rules VR-01 through VR-06
    
    This system provides:
    - Balance inquiry (view current balance)
    - Credit transactions (add funds)
    - Debit transactions (withdraw funds)
    - Transaction history (enhancement over COBOL)
    
    Reference: docs/Brd.md, docs/Frd.md
    """,
    version="1.0.0",
    lifespan=lifespan
)

# CORS middleware (for web frontend integration)
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],  # Configure appropriately for production
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


@app.get("/", tags=["system"])
async def root() -> dict[str, str]:
    """
    Root endpoint - system information.
    
    Maps to: COBOL main menu display (main.cob:12-18)
    Reference: docs/Brd.md line 7
    """
    return {
        "message": "Account Management System API",
        "version": "1.0.0",
        "documentation": "/docs",
        "endpoints": {
            "balance": "GET /api/v1/account/balance",
            "credit": "POST /api/v1/account/credit",
            "debit": "POST /api/v1/account/debit",
            "history": "GET /api/v1/account/transactions"
        }
    }


@app.get("/health", tags=["system"])
async def health() -> dict[str, str]:
    """Health check endpoint."""
    return {"status": "healthy"}

