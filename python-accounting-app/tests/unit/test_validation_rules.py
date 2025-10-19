"""
Unit tests for Validation Rules (VR-01 through VR-06).

Tests each validation rule as specified in docs/Frd.md lines 28-34.
"""

import pytest

from accounting.business_rules.validation_rules import (
    vr_01_validate_menu_selection,
    vr_02_validate_debit_sufficiency,
    vr_03_validate_amount_format,
    vr_04_validate_session_continuity,
    vr_05_validate_immediate_persistence_preconditions,
    vr_06_validate_positive_amount,
)


class TestVR01MenuSelection:
    """Test VR-01: Menu selection validation."""
    
    def test_valid_choices(self):
        """Test valid menu choices 1-4."""
        for choice in [1, 2, 3, 4]:
            valid, msg = vr_01_validate_menu_selection(choice)
            assert valid is True
            assert msg == ""
    
    def test_invalid_choices(self):
        """Test invalid menu choices."""
        for choice in [0, 5, 99, -1]:
            valid, msg = vr_01_validate_menu_selection(choice)
            assert valid is False
            assert msg == "Invalid choice, please select 1-4."


class TestVR02DebitSufficiency:
    """Test VR-02: Debit sufficiency check."""
    
    def test_sufficient_funds(self):
        """Test debit with sufficient funds."""
        valid, msg = vr_02_validate_debit_sufficiency(
            amount_cents=50_000,
            balance_cents=100_000
        )
        assert valid is True
        assert msg == ""
    
    def test_exact_balance(self):
        """Test debit of exact balance."""
        valid, msg = vr_02_validate_debit_sufficiency(
            amount_cents=100_000,
            balance_cents=100_000
        )
        assert valid is True
        assert msg == ""
    
    def test_insufficient_funds(self):
        """Test debit exceeding balance."""
        valid, msg = vr_02_validate_debit_sufficiency(
            amount_cents=150_000,
            balance_cents=100_000
        )
        assert valid is False
        assert msg == "Insufficient funds for this debit."


class TestVR03AmountFormat:
    """Test VR-03: Amount format validation."""
    
    def test_valid_amounts(self):
        """Test valid amount ranges."""
        valid_amounts = [
            1,  # Minimum (0.01)
            100,  # 1.00
            100_000,  # 1,000.00
            99_999_999  # Maximum (999,999.99)
        ]
        for amount in valid_amounts:
            valid, msg = vr_03_validate_amount_format(amount)
            assert valid is True, f"Amount {amount} should be valid"
            assert msg == ""
    
    def test_below_minimum(self):
        """Test amount below minimum."""
        valid, msg = vr_03_validate_amount_format(0)
        assert valid is False
        assert "at least 0.01" in msg
    
    def test_above_maximum(self):
        """Test amount above maximum."""
        valid, msg = vr_03_validate_amount_format(100_000_000)
        assert valid is False
        assert "cannot exceed" in msg
    
    def test_non_integer(self):
        """Test non-integer amount."""
        valid, msg = vr_03_validate_amount_format("not a number")  # type: ignore
        assert valid is False
        assert "numeric" in msg


class TestVR04SessionContinuity:
    """Test VR-04: Session continuity."""
    
    def test_continue_yes(self):
        """Test session continues with YES flag."""
        assert vr_04_validate_session_continuity("YES") is True
    
    def test_continue_no(self):
        """Test session ends with NO flag."""
        assert vr_04_validate_session_continuity("NO") is False


class TestVR05ImmediatePersistence:
    """Test VR-05: Immediate persistence preconditions."""
    
    def test_valid_credit(self):
        """Test valid credit operation."""
        valid, msg = vr_05_validate_immediate_persistence_preconditions(
            amount_cents=10_000,
            balance_cents=100_000,
            is_debit=False
        )
        assert valid is True
        assert msg == ""
    
    def test_valid_debit(self):
        """Test valid debit operation."""
        valid, msg = vr_05_validate_immediate_persistence_preconditions(
            amount_cents=50_000,
            balance_cents=100_000,
            is_debit=True
        )
        assert valid is True
        assert msg == ""
    
    def test_debit_insufficient_funds(self):
        """Test debit with insufficient funds."""
        valid, msg = vr_05_validate_immediate_persistence_preconditions(
            amount_cents=150_000,
            balance_cents=100_000,
            is_debit=True
        )
        assert valid is False
        assert "Insufficient funds" in msg
    
    def test_credit_overflow(self):
        """Test credit causing overflow."""
        valid, msg = vr_05_validate_immediate_persistence_preconditions(
            amount_cents=10_000_000,
            balance_cents=95_000_000,
            is_debit=False
        )
        assert valid is False
        assert "exceed maximum balance" in msg


class TestVR06PositiveAmount:
    """Test VR-06: Positive amount validation."""
    
    def test_positive_amount(self):
        """Test positive amounts."""
        for amount in [1, 100, 50_000, 99_999_999]:
            valid, msg = vr_06_validate_positive_amount(amount)
            assert valid is True
            assert msg == ""
    
    def test_zero_amount(self):
        """Test zero amount rejection."""
        valid, msg = vr_06_validate_positive_amount(0)
        assert valid is False
        assert "positive and non-zero" in msg
    
    def test_negative_amount(self):
        """Test negative amount rejection."""
        valid, msg = vr_06_validate_positive_amount(-100)
        assert valid is False
        assert "positive and non-zero" in msg

