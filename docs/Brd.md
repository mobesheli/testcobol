# Simple Account Management Business Requirements Document

## Purpose
Deliver a concise business definition of the legacy COBOL account management application so modernization teams can preserve core functionality while addressing limitations in usability, resilience, and scalability.

## Current Process
- **Process Overview**: A looping menu in `main.cob:12` presents balance inquiry, credit, debit, and exit options. User selections are routed to `operations.cob` which orchestrates business rules, while `data.cob` maintains an in-memory balance store.
- **Execution Flow**: The main program evaluates `USER-CHOICE` via an `EVALUATE` block, invoking the operations module for each transaction type and terminating only when option 4 sets `CONTINUE-FLAG` to `NO`.
- **Program Interactions**: `OperationsProgram` delegates reads and writes to `DataProgram` using verb-based linkage parameters (`'READ'`, `'WRITE'`). Balance inquiries surface immediately to the console; credits and debits trigger read-update-write sequences before returning control to the main loop.
- **Inputs**: Menu choices (`PIC 9`, values 1–4) and monetary amounts (`PIC 9(6)V99`, accepting 0.01–999,999.99) are captured through console `ACCEPT` statements.
- **Outputs**: Menu screens, balance confirmations, debit/credit acknowledgements, and error messages (invalid selection, insufficient funds, exit confirmation) are displayed on-screen in real time.
- **File Layouts & Data Structures**: The system persists a single session balance in `STORAGE-BALANCE PIC 9(6)V99` (`data.cob:6`). Operational context is passed via fixed-length flags (`PASSED-OPERATION PIC X(6)`), while control flags such as `CONTINUE-FLAG PIC X(3)` (`main.cob:7`) and working variables like `FINAL-BALANCE PIC 9(6)V99` (`operations.cob:8`) govern loop continuation and transaction processing.

## Business Rules
- **Menu Navigation**: The main loop remains active until the user selects exit; any other choice re-displays the menu (`main.cob:15-35`).
- **Input Validation**: Only numeric selections 1–4 are considered valid; other inputs generate a corrective prompt without breaking the session.
- **Balance Inquiry**: Selecting option 1 retrieves and echoes the current balance without mutation, guaranteeing real-time visibility.
- **Credit Processing**: Credits accept any positive amount, add it to the stored balance, persist the result immediately, and confirm the new total.
- **Debit Processing**: Debits require sufficient funds; attempts exceeding the current balance are denied and the balance remains unchanged.
- **Data Persistence**: `DataProgram` honors an immediate write-back model to in-memory storage only; the starting balance initializes to 1,000.00 every execution.
- **Precision Handling**: All monetary fields enforce two decimal places and a maximum of 999,999.99, preventing malformed currency entries.
- **Amount Boundaries**: Transaction amounts must be positive and within valid range; the system accepts values from 0.01 to 999,999.99, with behavior for zero or negative entries determined by the implementation.

## Exceptions
- Invalid menu selections are intercepted and resolved with a message before re-presenting options.
- Debit requests that would drive the balance below zero produce an "Insufficient funds" message and do not update storage.
- Exiting the application clears all session data because no external persistence layer exists.

## Stakeholders
- **Account Holders**: Rely on accurate, immediate balance updates during interactive sessions.
- **Operations Support**: Maintains program execution environments and handles restart scenarios.
- **Finance Controls**: Requires assurance that debits never overdraw and totals remain accurate within a session.
- **Modernization Team**: Responsible for translating legacy logic into contemporary platforms without losing business integrity.

## Pain Points
- Session-only storage resets balances at each launch, making the system unusable for real banking scenarios.
- Single-account design prevents managing multiple customers or products.
- Lack of authentication or audit trails exposes the process to misuse and compliance gaps.
- Console-based interaction limits integration with digital channels or automated workflows.
- No transaction history impedes reconciliation, reporting, and dispute resolution.

## Modernization Objectives
- Implement durable, auditable persistence for balances and transaction history.
- Support multi-account structures with user authentication and role-based access.
- Provide APIs and/or graphical interfaces to integrate with modern banking channels.
- Introduce validation and error-handling layers that accommodate broader input sources while retaining core rules.
- Enable extensibility for future services (interest accrual, alerts, compliance checks) without modifying foundational business rules.
