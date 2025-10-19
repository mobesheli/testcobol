"""
Business Rules (BR-01 through BR-09) as pure functions.

Reference: docs/Brd.md lines 14-22, docs/Frd.md traceability matrix lines 46-56
All business rules from the Business Requirements Document.
"""

from typing import Literal

from accounting.business_rules.validation_rules import (
    vr_02_validate_debit_sufficiency,
    vr_03_validate_amount_format,
    vr_05_validate_immediate_persistence_preconditions,
    vr_06_validate_positive_amount,
)


def br_01_menu_navigation(continue_flag: Literal["YES", "NO"]) -> bool:
    """
    BR-01 Menu Navigation: Main loop remains active until user selects exit.
    
    Reference: docs/Brd.md line 15, docs/Frd.md line 46
    COBOL: main.cob:11-34 PERFORM UNTIL CONTINUE-FLAG = 'NO'
    Implements: User Interaction Module - Session Control Loop
    
    Args:
        continue_flag: Session continuation flag ('YES' or 'NO')
        
    Returns:
        True if menu should continue, False if exit requested
    """
    return continue_flag == "YES"


def br_02_input_validation_menu(choice: int) -> tuple[bool, str]:
    """
    BR-02 Input Validation: Only numeric selections 1-4 are valid.
    
    Reference: docs/Brd.md line 16, docs/Frd.md line 48
    COBOL: main.cob:22-32 EVALUATE USER-CHOICE WHEN OTHER
    Implements: VR-01 Menu Selection
    
    Args:
        choice: User menu selection
        
    Returns:
        Tuple of (is_valid, error_message)
    """
    if choice not in (1, 2, 3, 4):
        return False, "Invalid choice, please select 1-4."
    return True, ""


def br_03_balance_inquiry(balance_cents: int) -> dict[str, int | str]:
    """
    BR-03 Balance Inquiry: Retrieve and echo current balance without mutation.
    
    Reference: docs/Brd.md line 17, docs/Frd.md line 49
    COBOL: operations.cob:16-18 IF OPERATION-TYPE = 'TOTAL '
    Implements: Transaction Processing Module - Balance Inquiry Flow
    
    Args:
        balance_cents: Current balance in cents
        
    Returns:
        Dict with balance_cents and formatted balance_display
    """
    dollars = balance_cents / 100
    return {
        "balance_cents": balance_cents,
        "balance_display": f"${dollars:,.2f}"
    }


def br_04_credit_processing(
    current_balance_cents: int,
    credit_amount_cents: int
) -> tuple[bool, int, str]:
    """
    BR-04 Credit Processing: Accept positive amount, add to balance, persist immediately.
    
    Reference: docs/Brd.md line 18, docs/Frd.md line 50
    COBOL: operations.cob:20-26 IF OPERATION-TYPE = 'CREDIT'
    Implements: Transaction Processing Module - Credit Flow & VR-05
    
    Enforces:
    - VR-06: Positive amounts only
    - VR-03: Amount format validation
    - VR-05: Immediate persistence preconditions
    
    Args:
        current_balance_cents: Current balance in cents
        credit_amount_cents: Credit amount in cents
        
    Returns:
        Tuple of (success, new_balance_cents, message)
    """
    # VR-06: Validate positive amount
    valid, msg = vr_06_validate_positive_amount(credit_amount_cents)
    if not valid:
        return False, current_balance_cents, msg
    
    # VR-03: Validate amount format
    valid, msg = vr_03_validate_amount_format(credit_amount_cents)
    if not valid:
        return False, current_balance_cents, msg
    
    # VR-05: Validate persistence preconditions
    valid, msg = vr_05_validate_immediate_persistence_preconditions(
        credit_amount_cents, current_balance_cents, is_debit=False
    )
    if not valid:
        return False, current_balance_cents, msg
    
    # Perform credit operation (COBOL: ADD AMOUNT TO FINAL-BALANCE)
    new_balance_cents = current_balance_cents + credit_amount_cents
    dollars = new_balance_cents / 100
    message = f"Amount credited. New balance: ${dollars:,.2f}"
    
    return True, new_balance_cents, message


def br_05_debit_processing(
    current_balance_cents: int,
    debit_amount_cents: int
) -> tuple[bool, int, str]:
    """
    BR-05 Debit Processing: Require sufficient funds, deny if exceeds balance.
    
    Reference: docs/Brd.md line 19, docs/Frd.md line 51
    COBOL: operations.cob:28-38 IF OPERATION-TYPE = 'DEBIT '
    Implements: Transaction Processing Module - Debit Flow & VR-02
    
    Enforces:
    - VR-06: Positive amounts only
    - VR-03: Amount format validation
    - VR-02: Sufficient funds check
    - VR-05: Immediate persistence preconditions
    
    Args:
        current_balance_cents: Current balance in cents
        debit_amount_cents: Debit amount in cents
        
    Returns:
        Tuple of (success, new_balance_cents, message)
    """
    # VR-06: Validate positive amount
    valid, msg = vr_06_validate_positive_amount(debit_amount_cents)
    if not valid:
        return False, current_balance_cents, msg
    
    # VR-03: Validate amount format
    valid, msg = vr_03_validate_amount_format(debit_amount_cents)
    if not valid:
        return False, current_balance_cents, msg
    
    # VR-02: Check sufficient funds (COBOL: IF FINAL-BALANCE >= AMOUNT)
    valid, msg = vr_02_validate_debit_sufficiency(debit_amount_cents, current_balance_cents)
    if not valid:
        # Return unchanged balance with insufficient funds message
        return False, current_balance_cents, msg
    
    # VR-05: Validate persistence preconditions
    valid, msg = vr_05_validate_immediate_persistence_preconditions(
        debit_amount_cents, current_balance_cents, is_debit=True
    )
    if not valid:
        return False, current_balance_cents, msg
    
    # Perform debit operation (COBOL: SUBTRACT AMOUNT FROM FINAL-BALANCE)
    new_balance_cents = current_balance_cents - debit_amount_cents
    dollars = new_balance_cents / 100
    message = f"Amount debited. New balance: ${dollars:,.2f}"
    
    return True, new_balance_cents, message


def br_06_data_persistence(
    balance_cents: int,
    operation: Literal["READ", "WRITE"]
) -> int:
    """
    BR-06 Data Persistence: Immediate write-back model.
    
    Reference: docs/Brd.md line 20, docs/Frd.md line 52
    COBOL: data.cob:16-20 IF OPERATION-TYPE = 'READ'/'WRITE'
    Implements: Balance Persistence Module & VR-05
    
    Note: This is a conceptual function. Actual persistence handled by repository layer.
    
    Args:
        balance_cents: Balance value in cents
        operation: 'READ' or 'WRITE'
        
    Returns:
        Balance value (for READ operations)
    """
    # In actual implementation, this is handled by the repository
    # This function documents the business rule
    return balance_cents


def br_07_precision_handling(amount_cents: int) -> bool:
    """
    BR-07 Precision Handling: Enforce two decimal places and maximum of 999,999.99.
    
    Reference: docs/Brd.md line 21, docs/Frd.md line 53
    COBOL: PIC 9(6)V99 data type enforcement
    Implements: VR-03 & Data Specifications
    
    Args:
        amount_cents: Amount in cents
        
    Returns:
        True if amount meets precision requirements
    """
    valid, _ = vr_03_validate_amount_format(amount_cents)
    return valid


def br_08_exception_handling_invalid_selection() -> str:
    """
    BR-08 Exceptions: Invalid menu selections intercepted with message.
    
    Reference: docs/Brd.md line 24, docs/Frd.md line 54
    COBOL: main.cob:31-32 WHEN OTHER
    Implements: VR-01, VR-02 & Console Presentation Layer
    
    Returns:
        Standard error message for invalid selections
    """
    return "Invalid choice, please select 1-4."


def br_08_exception_handling_insufficient_funds() -> str:
    """
    BR-08 Exceptions: Insufficient funds message for rejected debits.
    
    Reference: docs/Brd.md line 25, docs/Frd.md line 54
    COBOL: operations.cob:37 DISPLAY "Insufficient funds for this debit."
    Implements: VR-02 & Console Presentation Layer
    
    Returns:
        Standard error message for insufficient funds
    """
    return "Insufficient funds for this debit."


def br_09_amount_boundaries(amount_cents: int) -> tuple[bool, str]:
    """
    BR-09 Amount Boundaries: Validate positive amounts within valid range.
    
    Reference: docs/Brd.md line 22, docs/Frd.md line 56
    COBOL: PIC 9(6)V99 accepting 0.01-999,999.99
    Implements: VR-03, VR-06
    
    Args:
        amount_cents: Amount in cents
        
    Returns:
        Tuple of (is_valid, error_message)
    """
    # Check positive (VR-06)
    valid, msg = vr_06_validate_positive_amount(amount_cents)
    if not valid:
        return False, msg
    
    # Check format and range (VR-03)
    valid, msg = vr_03_validate_amount_format(amount_cents)
    if not valid:
        return False, msg
    
    return True, ""

