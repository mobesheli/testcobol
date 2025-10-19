"""
Validation Rules (VR-01 through VR-06) as pure functions.

Reference: docs/Frd.md lines 28-34
All validation rules from the Functional Requirements Document.
"""

from typing import Literal


def vr_01_validate_menu_selection(choice: int) -> tuple[bool, str]:
    """
    VR-01 Menu Selection: Accept only numeric entries 1-4.
    
    Reference: docs/Frd.md line 29
    COBOL: main.cob:22-33 EVALUATE statement
    
    Args:
        choice: User menu selection
        
    Returns:
        Tuple of (is_valid, error_message)
    """
    if choice not in (1, 2, 3, 4):
        return False, "Invalid choice, please select 1-4."
    return True, ""


def vr_02_validate_debit_sufficiency(
    amount_cents: int,
    balance_cents: int
) -> tuple[bool, str]:
    """
    VR-02 Debit Sufficiency: Reject debit requests when AMOUNT exceeds FINAL-BALANCE.
    
    Reference: docs/Frd.md line 30
    COBOL: operations.cob:32 IF FINAL-BALANCE >= AMOUNT
    Maps to: BR-05 Debit Processing
    
    Args:
        amount_cents: Debit amount in cents
        balance_cents: Current balance in cents
        
    Returns:
        Tuple of (is_valid, error_message)
    """
    if balance_cents < amount_cents:
        return False, "Insufficient funds for this debit."
    return True, ""


def vr_03_validate_amount_format(amount_cents: int) -> tuple[bool, str]:
    """
    VR-03 Amount Format: Enforce numeric input in range 0.01-999,999.99.
    
    Reference: docs/Frd.md line 31
    COBOL: PIC 9(6)V99 → 1 to 99,999,999 cents
    Maps to: BR-07 Precision Handling
    
    Args:
        amount_cents: Amount in cents
        
    Returns:
        Tuple of (is_valid, error_message)
    """
    if not isinstance(amount_cents, int):
        return False, "Amount must be a numeric value."
    
    if amount_cents < 1:
        return False, "Amount must be at least 0.01 (1 cent)."
    
    if amount_cents > 99_999_999:
        return False, "Amount cannot exceed 999,999.99."
    
    return True, ""


def vr_04_validate_session_continuity(continue_flag: Literal["YES", "NO"]) -> bool:
    """
    VR-04 Session Continuity: Maintain loop execution while CONTINUE-FLAG equals YES.
    
    Reference: docs/Frd.md line 32
    COBOL: main.cob:11 PERFORM UNTIL CONTINUE-FLAG = 'NO'
    Maps to: BR-01 Menu Navigation
    
    Args:
        continue_flag: Session continuation flag
        
    Returns:
        True if session should continue, False otherwise
    """
    return continue_flag == "YES"


def vr_05_validate_immediate_persistence_preconditions(
    amount_cents: int,
    balance_cents: int,
    is_debit: bool
) -> tuple[bool, str]:
    """
    VR-05 Immediate Persistence: Validate before applying balance changes.
    
    Reference: docs/Frd.md line 33
    Maps to: BR-06 Data Persistence
    
    Ensures:
    - Amount is valid (VR-03)
    - For debits: sufficient funds exist (VR-02)
    - Computation won't cause overflow
    
    Args:
        amount_cents: Transaction amount in cents
        balance_cents: Current balance in cents
        is_debit: True if debit operation, False if credit
        
    Returns:
        Tuple of (is_valid, error_message)
    """
    # Validate amount format
    valid, msg = vr_03_validate_amount_format(amount_cents)
    if not valid:
        return False, msg
    
    # For debits, check sufficiency
    if is_debit:
        valid, msg = vr_02_validate_debit_sufficiency(amount_cents, balance_cents)
        if not valid:
            return False, msg
        
        # Check underflow
        if balance_cents - amount_cents < 0:
            return False, "Operation would result in negative balance."
    else:
        # For credits, check overflow
        if balance_cents + amount_cents > 99_999_999:
            return False, "Operation would exceed maximum balance of 999,999.99."
    
    return True, ""


def vr_06_validate_positive_amount(amount_cents: int) -> tuple[bool, str]:
    """
    VR-06 Positive Amount: Reject negative or zero amounts for transactions.
    
    Reference: docs/Frd.md line 34
    Maps to: BR-09 Amount Boundaries
    
    Args:
        amount_cents: Amount in cents
        
    Returns:
        Tuple of (is_valid, error_message)
    """
    if amount_cents <= 0:
        return False, "Amount must be positive and non-zero."
    return True, ""

