# System Inputs and Outputs

## Inputs

### User Inputs (Screen/Console)
1. **Menu Choice** (`USER-CHOICE`)
   - Format: Single digit (1-4)
   - Purpose: Selects operation to perform
   - Values: 1=View Balance, 2=Credit, 3=Debit, 4=Exit

2. **Transaction Amount** (`AMOUNT`)
   - Format: Numeric with 2 decimal places (PIC 9(6)V99)
   - Purpose: Specifies amount for credit/debit operations
   - Range: 0.01 to 999,999.99

### Program Inputs (Internal)
1. **Operation Type** (`PASSED-OPERATION`)
   - Format: 6-character string
   - Values: 'TOTAL ', 'CREDIT', 'DEBIT '
   - Purpose: Communicates operation type between programs

## Outputs

### Screen Outputs (Console Display)
1. **Menu Display**
   - Shows available operations (1-4)
   - Displays system title and separators

2. **Balance Information**
   - Current balance display
   - New balance after transactions

3. **Transaction Confirmations**
   - Credit confirmation with new balance
   - Debit confirmation with new balance
   - Insufficient funds error message

4. **System Messages**
   - Invalid choice error message
   - Exit confirmation message

### Data Outputs (Internal)
1. **Updated Balance** (`STORAGE-BALANCE`)
   - Format: Numeric with 2 decimal places
   - Purpose: Stores current account balance
   - Initial Value: 1000.00

## Data Storage

### In-Memory Storage
- **Account Balance**: Stored in `STORAGE-BALANCE` variable
- **Persistence**: Data is lost when program terminates
- **Initial Value**: $1,000.00 (hardcoded)

### No External Files
- No database connections
- No file I/O operations
- No persistent storage mechanism
