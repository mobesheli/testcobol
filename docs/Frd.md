# Simple Account Management Functional Requirements Document

## Functional Scope and Modules
- **User Interaction Module (`MainProgram`, `main.cob`)**: Presents menu options, captures console inputs, dispatches user selections to downstream modules, and controls session lifecycle through the `CONTINUE-FLAG`.
- **Transaction Processing Module (`OperationsProgram`, `operations.cob`)**: Interprets the requested operation (`TOTAL `, `CREDIT`, `DEBIT `), orchestrates read-update-write cycles, performs debit sufficiency checks, and returns user-facing messages.
- **Balance Persistence Module (`DataProgram`, `data.cob`)**: Maintains the session balance in-memory via `STORAGE-BALANCE`, provides read access to current value, and commits updates received from the transaction processor.
- **Console Presentation Layer**: Formats and displays menus, informational messages, confirmations, and exception notices in real time to ensure user comprehension.
- **Session Control**: Enforces continuous looping until an exit command is received and guarantees cleanup messaging on termination.

## Data Flow Diagrams (Text Description)
- **Level 0 – Context View**: The Account Holder interacts with the User Interaction Module through the console. The module invokes the Transaction Processing Module, which in turn exchanges balance data with the Balance Persistence Module. Responses flow back to the Account Holder via the console.
- **Level 1 – Functional Decomposition**:
  - *Menu Selection Flow*: User input (`USER-CHOICE`) enters the User Interaction Module, which evaluates the choice and forwards an operation code to the Transaction Processing Module.
  - *Balance Inquiry Flow*: Operation code `TOTAL ` triggers the Transaction Processing Module to request a read from the Balance Persistence Module, receive `FINAL-BALANCE`, and display the value back to the user.
  - *Credit Flow*: Operation code `CREDIT` causes the Transaction Processing Module to prompt for `AMOUNT`, read current balance, compute a new total, write the updated balance, and display confirmation.
  - *Debit Flow*: Operation code `DEBIT ` prompts for `AMOUNT`, retrieves the balance, validates sufficiency, either writes the reduced amount and confirms or returns an insufficient funds message without altering storage.
  - *Exit Flow*: Selection `4` instructs the User Interaction Module to update `CONTINUE-FLAG` to `NO`, emit the exit acknowledgement, and close the loop.

## Input/Output Specifications
| Interface Element | Source | Data Type / Format | Allowed Range | Destination | Description |
| --- | --- | --- | --- | --- | --- |
| `USER-CHOICE` | Account Holder (console) | `PIC 9` | 1–4 | User Interaction Module | Determines which operation is executed. |
| `AMOUNT` | Account Holder (console) | `PIC 9(6)V99` | 0.01–999,999.99 | Transaction Processing Module | Monetary value supplied for credit or debit transactions. |
| `PASSED-OPERATION` | User Interaction Module | `PIC X(6)` | `'TOTAL '`, `'CREDIT'`, `'DEBIT '` | Transaction Processing Module | Operation code handed off for execution. |
| `FINAL-BALANCE` | Balance Persistence Module | `PIC 9(6)V99` | 0.00–999,999.99 | Transaction Processing Module / Console | Provides current balance to inform calculations and output. |
| Console Messages | Transaction Processing Module & User Interaction Module | Alphanumeric text | N/A | Account Holder | Menu, confirmation, error, and exit messaging. |
| Error Messages | Transaction Processing Module | Alphanumeric text | N/A | Console | Error-specific messages (invalid input, insufficient funds). |

## Validation Rules
- **VR-01 Menu Selection**: Accept only numeric entries 1–4; invalid entries trigger an error message and the menu redisplays.
- **VR-02 Debit Sufficiency**: Reject debit requests when `AMOUNT` exceeds `FINAL-BALANCE`; display "Insufficient funds" and leave balance unchanged.
- **VR-03 Amount Format**: Enforce numeric input with two decimal places in range 0.01–999,999.99; reject non-numeric, negative, zero, or out-of-range values and prompt for re-entry.
- **VR-04 Session Continuity**: Maintain loop execution while `CONTINUE-FLAG` equals `YES`, ensuring uninterrupted access until exit is confirmed.
- **VR-05 Immediate Persistence**: Apply balance changes only after successful computation and validation, then write to storage before returning control.
- **VR-06 Positive Amount**: Reject negative or zero amounts for credit and debit transactions; display error message and re-prompt for valid input.

## Non-Functional Requirements
- **Availability**: The console application must remain responsive throughout a session; restart time after termination should not exceed one minute.
- **Usability**: Menu text and prompts must be concise and unambiguous to support non-technical users; confirmation messages must include the resulting balance.
- **Performance**: Balance inquiries and transaction confirmations must display within one second of user input, accounting for in-memory operations.
- **Security**: Although legacy design lacks authentication, modernization efforts must plan for user identification, access control, and audit trails before deployment in production environments.
- **Maintainability**: Modular separation between interaction, transaction, and data storage components must be preserved to facilitate future enhancements and testing.
- **Reliability**: Data integrity within a session must be guaranteed; no transaction should partially execute or leave the balance in an inconsistent state.

## Traceability Matrix
| Business Rule (BRD) | Functional Requirement / Module | Notes |
| --- | --- | --- |
| BR-01 Menu Navigation | User Interaction Module – Session Control Loop | Implements continuous menu display until exit selection. |
| BR-02 Input Validation | Validation Rules VR-01 & VR-03 | Ensures only valid menu and amount inputs proceed. |
| BR-03 Balance Inquiry | Transaction Processing Module – Balance Inquiry Flow | Executes read-only retrieval and console display. |
| BR-04 Credit Processing | Transaction Processing Module – Credit Flow & VR-05 | Handles amount capture, balance update, and persistence. |
| BR-05 Debit Processing | Transaction Processing Module – Debit Flow & VR-02 | Enforces sufficiency check and conditionally updates storage. |
| BR-06 Data Persistence | Balance Persistence Module & VR-05 | Maintains in-memory balance and immediate write-back semantics. |
| BR-07 Precision Handling | Validation Rule VR-03 & Data Specifications | Applies currency precision through field definitions. |
| BR-08 Exceptions | Validation Rules VR-01, VR-02 & Console Presentation Layer | Communicates invalid selection and insufficient funds cases. |
| BR-09 Amount Boundaries | Validation Rules VR-03, VR-06 | Enforces positive amounts within valid range (0.01–999,999.99). |
