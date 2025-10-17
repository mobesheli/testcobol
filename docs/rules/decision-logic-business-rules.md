# Decision Logic and Business Rules

## Decision Logic Structure

### Main Program Decision Logic (EVALUATE Statement)
```cobol
EVALUATE USER-CHOICE
    WHEN 1 → Call OperationsProgram with 'TOTAL '
    WHEN 2 → Call OperationsProgram with 'CREDIT'
    WHEN 3 → Call OperationsProgram with 'DEBIT '
    WHEN 4 → Set CONTINUE-FLAG to 'NO' (Exit)
    WHEN OTHER → Display "Invalid choice, please select 1-4."
END-EVALUATE
```

### Operations Program Decision Logic (IF Statements)
```cobol
IF OPERATION-TYPE = 'TOTAL '
    → Read balance and display it

ELSE IF OPERATION-TYPE = 'CREDIT'
    → Accept amount, add to balance, save, display confirmation

ELSE IF OPERATION-TYPE = 'DEBIT '
    → Accept amount, check sufficient funds, subtract if valid, save, display result
```

### Debit Transaction Decision Logic (Nested IF)
```cobol
IF FINAL-BALANCE >= AMOUNT
    → Allow debit, subtract amount, save new balance
ELSE
    → Display "Insufficient funds for this debit."
END-IF
```

### Data Program Decision Logic
```cobol
IF OPERATION-TYPE = 'READ'
    → Return current balance
ELSE IF OPERATION-TYPE = 'WRITE'
    → Update stored balance
END-IF
```

## Business Rules Summary

### 1. Menu Navigation Rules
- **Rule**: System displays menu continuously until user chooses to exit
- **Logic**: Loop continues while CONTINUE-FLAG = 'YES'
- **Exit Condition**: User selects option 4

### 2. Input Validation Rules
- **Rule**: Only valid menu choices (1-4) are accepted
- **Logic**: EVALUATE statement with WHEN OTHER clause
- **Error Handling**: Display "Invalid choice" message and continue loop

### 3. Balance Inquiry Rules
- **Rule**: Users can view current balance at any time
- **Logic**: Read balance from storage and display
- **No Restrictions**: No authentication or limits

### 4. Credit Transaction Rules
- **Rule**: Users can add any amount to their account
- **Logic**: Accept amount, add to current balance, save result
- **No Limits**: No maximum credit amount restrictions
- **Confirmation**: Display new balance after credit

### 5. Debit Transaction Rules
- **Rule**: Users can only withdraw if sufficient funds exist
- **Logic**: Check if balance >= withdrawal amount
- **Sufficient Funds**: Allow withdrawal, update balance, confirm
- **Insufficient Funds**: Reject transaction, display error message
- **No Overdraft**: No negative balance allowed

### 6. Data Persistence Rules
- **Rule**: All balance changes are immediately saved
- **Logic**: Write operation updates storage after each transaction
- **Scope**: Changes persist only during program execution
- **Limitation**: Data lost when program terminates

### 7. Amount Format Rules
- **Rule**: All amounts must be numeric with 2 decimal places
- **Format**: PIC 9(6)V99 (supports up to 999,999.99)
- **Validation**: COBOL data type enforcement
- **Precision**: Currency amounts to the cent

### 8. System Initialization Rules
- **Rule**: Account starts with $1,000.00 balance
- **Logic**: Hardcoded initial value in STORAGE-BALANCE
- **No Configuration**: Fixed starting amount
- **Reset**: Balance resets to $1,000.00 each program start
