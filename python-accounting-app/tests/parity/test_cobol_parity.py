"""
Golden-master parity tests ensuring Python implementation matches COBOL behavior.

These tests validate that the Python implementation produces identical results
to the COBOL system as documented in docs/TESTPLAN.md.

Reference: docs/TESTPLAN.md lines 1-90
"""

import pytest
from fastapi.testclient import TestClient


class TestBalanceInquiryParity:
    """
    Test Case 1.1: View Current Balance (TESTPLAN.md lines 7-15).
    
    COBOL behavior: Display current balance unchanged.
    """
    
    def test_view_initial_balance(self, client: TestClient):
        """Test viewing initial balance of $1,000.00."""
        response = client.get("/api/v1/account/balance")
        
        assert response.status_code == 200
        data = response.json()
        
        # Initial balance: $1,000.00 (COBOL default from data.cob:6)
        assert data["balance_cents"] == 100_000
        assert data["balance_display"] == "$1,000.00"
    
    def test_view_balance_idempotent(self, client: TestClient):
        """Test that viewing balance multiple times doesn't change it."""
        # View balance 3 times
        for _ in range(3):
            response = client.get("/api/v1/account/balance")
            assert response.status_code == 200
            data = response.json()
            assert data["balance_cents"] == 100_000


class TestCreditAccountParity:
    """
    Test Cases 2.1-2.2: Credit Account (TESTPLAN.md lines 17-39).
    
    COBOL behavior: Add credit amount to balance, display new balance.
    """
    
    def test_credit_valid_amount(self, client: TestClient):
        """
        Test Case 2.1: Credit Account with Valid Amount.
        
        COBOL: Enter 100.00, new balance should be 1100.00
        """
        # Credit $100.00 (10,000 cents)
        response = client.post("/api/v1/account/credit", json={"amount_cents": 10_000})
        
        assert response.status_code == 200
        data = response.json()
        
        assert data["success"] is True
        assert data["new_balance_cents"] == 110_000
        assert "$1,100.00" in data["message"]
        
        # Verify balance was updated
        balance_response = client.get("/api/v1/account/balance")
        assert balance_response.json()["balance_cents"] == 110_000
    
    def test_credit_zero_amount(self, client: TestClient):
        """
        Test Case 2.2: Credit Account with Zero Amount.
        
        COBOL: Balance should remain unchanged.
        Note: COBOL accepts but doesn't validate; Python implements VR-06.
        """
        # Attempt to credit $0.00
        response = client.post("/api/v1/account/credit", json={"amount_cents": 0})
        
        # Python enhancement: explicit validation (VR-06)
        assert response.status_code == 400
        
        # Verify balance unchanged
        balance_response = client.get("/api/v1/account/balance")
        assert balance_response.json()["balance_cents"] == 100_000
    
    def test_credit_large_amount(self, client: TestClient):
        """Test crediting large amount (COBOL: $200.00)."""
        response = client.post("/api/v1/account/credit", json={"amount_cents": 20_000})
        
        assert response.status_code == 200
        data = response.json()
        
        assert data["new_balance_cents"] == 120_000
        assert "$1,200.00" in data["message"]


class TestDebitAccountParity:
    """
    Test Cases 3.1-3.3: Debit Account (TESTPLAN.md lines 41-74).
    
    COBOL behavior: Subtract debit if sufficient funds, else reject.
    """
    
    def test_debit_valid_amount(self, client: TestClient):
        """
        Test Case 3.1: Debit Account with Valid Amount.
        
        COBOL: Enter 50.00, new balance should be 950.00
        """
        # Debit $50.00 (5,000 cents)
        response = client.post("/api/v1/account/debit", json={"amount_cents": 5_000})
        
        assert response.status_code == 200
        data = response.json()
        
        assert data["success"] is True
        assert data["new_balance_cents"] == 95_000
        assert "$950.00" in data["message"]
        
        # Verify balance was updated
        balance_response = client.get("/api/v1/account/balance")
        assert balance_response.json()["balance_cents"] == 95_000
    
    def test_debit_exceeds_balance(self, client: TestClient):
        """
        Test Case 3.2: Debit Amount Greater Than Balance.
        
        COBOL: Display "Insufficient funds", balance unchanged.
        """
        # Attempt to debit $2,000.00 (200,000 cents) with balance of $1,000.00
        response = client.post("/api/v1/account/debit", json={"amount_cents": 200_000})
        
        assert response.status_code == 200  # Not an error in COBOL, just message
        data = response.json()
        
        assert data["success"] is False
        assert "Insufficient funds" in data["message"]
        assert data["new_balance_cents"] == 100_000  # Unchanged
        
        # Verify balance unchanged
        balance_response = client.get("/api/v1/account/balance")
        assert balance_response.json()["balance_cents"] == 100_000
    
    def test_debit_zero_amount(self, client: TestClient):
        """
        Test Case 3.3: Debit Account with Zero Amount.
        
        COBOL: Balance should remain unchanged.
        Note: Python implements explicit VR-06 validation.
        """
        # Attempt to debit $0.00
        response = client.post("/api/v1/account/debit", json={"amount_cents": 0})
        
        # Python enhancement: explicit validation
        assert response.status_code in (200, 400)  # Handled as insufficient funds or validation
        
        # Verify balance unchanged
        balance_response = client.get("/api/v1/account/balance")
        assert balance_response.json()["balance_cents"] == 100_000
    
    def test_debit_exact_balance(self, client: TestClient):
        """Test debiting exact balance amount."""
        # Debit exact balance ($1,000.00)
        response = client.post("/api/v1/account/debit", json={"amount_cents": 100_000})
        
        assert response.status_code == 200
        data = response.json()
        
        assert data["success"] is True
        assert data["new_balance_cents"] == 0
        
        # Verify zero balance
        balance_response = client.get("/api/v1/account/balance")
        assert balance_response.json()["balance_cents"] == 0


class TestCompleteScenarioParity:
    """
    Complete scenario test matching COBOL user interaction flow.
    
    Simulates: View → Credit → View → Debit → View sequence.
    """
    
    def test_complete_transaction_flow(self, client: TestClient):
        """Test complete transaction flow matching COBOL example."""
        # Step 1: View initial balance ($1,000.00)
        response = client.get("/api/v1/account/balance")
        assert response.json()["balance_cents"] == 100_000
        
        # Step 2: Credit $200.00 (README example from COBOL docs)
        response = client.post("/api/v1/account/credit", json={"amount_cents": 20_000})
        assert response.status_code == 200
        assert response.json()["new_balance_cents"] == 120_000
        
        # Step 3: View balance after credit ($1,200.00)
        response = client.get("/api/v1/account/balance")
        assert response.json()["balance_cents"] == 120_000
        
        # Step 4: Debit $300.00 (README example from COBOL docs)
        response = client.post("/api/v1/account/debit", json={"amount_cents": 30_000})
        assert response.status_code == 200
        assert response.json()["new_balance_cents"] == 90_000
        
        # Step 5: View final balance ($900.00)
        response = client.get("/api/v1/account/balance")
        assert response.json()["balance_cents"] == 90_000
        assert response.json()["balance_display"] == "$900.00"


class TestDataPersistenceParity:
    """
    Test data persistence behavior matches COBOL.
    
    COBOL: Immediate persistence within session, lost on exit.
    Python: Persistent across requests within database session.
    """
    
    def test_immediate_persistence(self, client: TestClient):
        """Test that changes persist immediately (VR-05)."""
        # Credit $50
        client.post("/api/v1/account/credit", json={"amount_cents": 5_000})
        
        # Immediately view balance - should reflect change
        response = client.get("/api/v1/account/balance")
        assert response.json()["balance_cents"] == 105_000
        
        # Debit $30
        client.post("/api/v1/account/debit", json={"amount_cents": 3_000})
        
        # Immediately view balance - should reflect both changes
        response = client.get("/api/v1/account/balance")
        assert response.json()["balance_cents"] == 102_000
    
    def test_failed_transaction_no_persistence(self, client: TestClient):
        """Test that failed transactions don't change balance."""
        initial_response = client.get("/api/v1/account/balance")
        initial_balance = initial_response.json()["balance_cents"]
        
        # Attempt invalid debit
        client.post("/api/v1/account/debit", json={"amount_cents": 500_000})
        
        # Balance should be unchanged
        final_response = client.get("/api/v1/account/balance")
        assert final_response.json()["balance_cents"] == initial_balance


class TestPrecisionParity:
    """
    Test precision handling matches COBOL PIC 9(6)V99 (BR-07).
    
    COBOL: Two decimal places, max 999,999.99
    """
    
    def test_cent_precision(self, client: TestClient):
        """Test handling of cent-level precision."""
        # Credit 1 cent
        response = client.post("/api/v1/account/credit", json={"amount_cents": 1})
        assert response.status_code == 200
        assert response.json()["new_balance_cents"] == 100_001
        
        # Debit 1 cent
        response = client.post("/api/v1/account/debit", json={"amount_cents": 1})
        assert response.status_code == 200
        assert response.json()["new_balance_cents"] == 100_000
    
    def test_maximum_amount_boundary(self, client: TestClient):
        """Test maximum amount boundary (999,999.99)."""
        # Attempt to credit amount that would exceed maximum
        response = client.post("/api/v1/account/credit", json={"amount_cents": 100_000_000})
        assert response.status_code == 400  # Validation error
        
        # Balance should be unchanged
        balance_response = client.get("/api/v1/account/balance")
        assert balance_response.json()["balance_cents"] == 100_000

