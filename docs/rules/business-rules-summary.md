# Business Rules Summary

## Core Business Rules

### Account Management Rules
1. **Single Account System**: The system manages exactly one account
2. **Initial Balance**: Every account starts with $1,000.00
3. **Balance Display**: Users can view their balance at any time
4. **No Account Limits**: No maximum balance restrictions

### Transaction Rules
1. **Credit Transactions**:
   - Users can add any amount to their account
   - No maximum credit limit
   - All credits are immediately processed
   - Confirmation shows new balance

2. **Debit Transactions**:
   - Users can only withdraw if sufficient funds exist
   - No overdraft facility (no negative balances)
   - Insufficient funds transactions are rejected
   - Confirmation shows new balance or error message

### System Operation Rules
1. **Menu-Driven Interface**: All operations accessed through numbered menu
2. **Continuous Operation**: System runs until user explicitly exits
3. **Input Validation**: Only valid menu choices (1-4) accepted
4. **Error Handling**: Invalid inputs display error message and continue

### Data Management Rules
1. **In-Memory Storage**: Balance stored in program memory only
2. **Immediate Persistence**: All changes saved immediately
3. **Session-Based**: Data lost when program terminates
4. **No External Storage**: No database or file connections

### Amount Format Rules
1. **Currency Precision**: All amounts handled to 2 decimal places
2. **Numeric Format**: Supports amounts up to $999,999.99
3. **Type Safety**: COBOL data types enforce numeric validation

## Business Process Flow

```
Start → Display Menu → Get User Choice → Process Choice → Update Display → Repeat
                                                      ↓
                                                 Exit Selected
                                                      ↓
                                                   End Program
```

## Key Business Characteristics

- **Simplicity**: Basic account operations only
- **Real-time**: All transactions processed immediately
- **User-Friendly**: Clear menu interface with confirmations
- **Safe**: Prevents overdrafts and invalid transactions
- **Temporary**: No persistent data storage
- **Single-User**: Designed for one user session at a time

## Limitations

1. **No Data Persistence**: Account balance resets on each program run
2. **Single Account**: Cannot manage multiple accounts
3. **No Authentication**: No user login or security
4. **No Transaction History**: No record of past transactions
5. **No Interest**: No interest calculations or accruals
6. **No External Integration**: No connection to banking systems
