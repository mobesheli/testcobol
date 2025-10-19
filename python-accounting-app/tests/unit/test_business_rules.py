"""
Unit tests for Business Rules (BR-01 through BR-09).

Tests each business rule as specified in docs/Brd.md lines 14-22.
"""

import pytest

from accounting.business_rules.business_rules import (
    br_01_menu_navigation,
    br_02_input_validation_menu,
    br_03_balance_inquiry,
    br_04_credit_processing,
    br_05_debit_processing,
    br_06_data_persistence,
    br_07_precision_handling,
    br_08_exception_handling_insufficient_funds,
    br_08_exception_handling_invalid_selection,
    br_09_amount_boundaries,
)


class TestBR01MenuNavigation:
    """Test BR-01: Menu navigation loop control."""
    
    def test_continue_yes(self):
        """Test menu continues with YES flag."""
        assert br_01_menu_navigation("YES") is True
    
    def test_continue_no(self):
        """Test menu exits with NO flag."""
        assert br_01_menu_navigation("NO") is False


class TestBR02InputValidation:
    """Test BR-02: Menu input validation."""
    
    def test_valid_menu_choices(self):
        """Test valid menu choices."""
        for choice in [1, 2, 3, 4]:
            valid, msg = br_02_input_validation_menu(choice)
            assert valid is True
    
    def test_invalid_menu_choices(self):
        """Test invalid menu choices."""
        for choice in [0, 5, -1, 99]:
            valid, msg = br_02_input_validation_menu(choice)
            assert valid is False
            assert "Invalid choice" in msg


class TestBR03BalanceInquiry:
    """Test BR-03: Balance inquiry without mutation."""
    
    def test_balance_inquiry(self):
        """Test balance inquiry returns formatted balance."""
        result = br_03_balance_inquiry(100_000)  # $1,000.00
        
        assert result["balance_cents"] == 100_000
        assert result["balance_display"] == "$1,000.00"
    
    def test_balance_inquiry_zero(self):
        """Test balance inquiry with zero balance."""
        result = br_03_balance_inquiry(0)
        
        assert result["balance_cents"] == 0
        assert result["balance_display"] == "$0.00"
    
    def test_balance_inquiry_max(self):
        """Test balance inquiry with maximum balance."""
        result = br_03_balance_inquiry(99_999_999)  # $999,999.99
        
        assert result["balance_cents"] == 99_999_999
        assert result["balance_display"] == "$999,999.99"


class TestBR04CreditProcessing:
    """Test BR-04: Credit processing."""
    
    def test_valid_credit(self):
        """Test valid credit transaction."""
        success, new_balance, msg = br_04_credit_processing(
            current_balance_cents=100_000,
            credit_amount_cents=50_000
        )
        
        assert success is True
        assert new_balance == 150_000
        assert "credited" in msg
        assert "$1,500.00" in msg
    
    def test_credit_zero_amount(self):
        """Test credit with zero amount."""
        success, new_balance, msg = br_04_credit_processing(
            current_balance_cents=100_000,
            credit_amount_cents=0
        )
        
        assert success is False
        assert new_balance == 100_000  # Unchanged
    
    def test_credit_negative_amount(self):
        """Test credit with negative amount."""
        success, new_balance, msg = br_04_credit_processing(
            current_balance_cents=100_000,
            credit_amount_cents=-1000
        )
        
        assert success is False
        assert new_balance == 100_000  # Unchanged


class TestBR05DebitProcessing:
    """Test BR-05: Debit processing with sufficiency check."""
    
    def test_valid_debit(self):
        """Test valid debit transaction."""
        success, new_balance, msg = br_05_debit_processing(
            current_balance_cents=100_000,
            debit_amount_cents=30_000
        )
        
        assert success is True
        assert new_balance == 70_000
        assert "debited" in msg
        assert "$700.00" in msg
    
    def test_debit_exact_balance(self):
        """Test debit of exact balance."""
        success, new_balance, msg = br_05_debit_processing(
            current_balance_cents=100_000,
            debit_amount_cents=100_000
        )
        
        assert success is True
        assert new_balance == 0
    
    def test_debit_insufficient_funds(self):
        """Test debit with insufficient funds."""
        success, new_balance, msg = br_05_debit_processing(
            current_balance_cents=100_000,
            debit_amount_cents=150_000
        )
        
        assert success is False
        assert new_balance == 100_000  # Unchanged
        assert "Insufficient funds" in msg
    
    def test_debit_zero_amount(self):
        """Test debit with zero amount."""
        success, new_balance, msg = br_05_debit_processing(
            current_balance_cents=100_000,
            debit_amount_cents=0
        )
        
        assert success is False
        assert new_balance == 100_000  # Unchanged


class TestBR06DataPersistence:
    """Test BR-06: Data persistence conceptual function."""
    
    def test_read_operation(self):
        """Test READ operation returns balance."""
        balance = br_06_data_persistence(100_000, "READ")
        assert balance == 100_000
    
    def test_write_operation(self):
        """Test WRITE operation."""
        balance = br_06_data_persistence(150_000, "WRITE")
        assert balance == 150_000


class TestBR07PrecisionHandling:
    """Test BR-07: Precision handling."""
    
    def test_valid_precision(self):
        """Test valid precision amounts."""
        assert br_07_precision_handling(100) is True  # $1.00
        assert br_07_precision_handling(99_999_999) is True  # Max
    
    def test_invalid_precision(self):
        """Test invalid precision amounts."""
        assert br_07_precision_handling(0) is False
        assert br_07_precision_handling(100_000_000) is False


class TestBR08ExceptionHandling:
    """Test BR-08: Exception handling messages."""
    
    def test_invalid_selection_message(self):
        """Test invalid selection error message."""
        msg = br_08_exception_handling_invalid_selection()
        assert msg == "Invalid choice, please select 1-4."
    
    def test_insufficient_funds_message(self):
        """Test insufficient funds error message."""
        msg = br_08_exception_handling_insufficient_funds()
        assert msg == "Insufficient funds for this debit."


class TestBR09AmountBoundaries:
    """Test BR-09: Amount boundaries validation."""
    
    def test_valid_boundaries(self):
        """Test valid amount boundaries."""
        valid, msg = br_09_amount_boundaries(1)  # Min
        assert valid is True
        
        valid, msg = br_09_amount_boundaries(99_999_999)  # Max
        assert valid is True
    
    def test_below_minimum(self):
        """Test amount below minimum boundary."""
        valid, msg = br_09_amount_boundaries(0)
        assert valid is False
    
    def test_above_maximum(self):
        """Test amount above maximum boundary."""
        valid, msg = br_09_amount_boundaries(100_000_000)
        assert valid is False

