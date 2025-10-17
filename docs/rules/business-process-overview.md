# Business Process Overview

## What This COBOL Program Does

This COBOL program implements a **Simple Account Management System** - essentially a basic banking application that allows users to manage a single account balance through a menu-driven interface.

## Business Process Description

The system provides a user-friendly interface for account holders to:
1. Check their current account balance
2. Add money to their account (credit transactions)
3. Withdraw money from their account (debit transactions)
4. Exit the system when done

The program operates as a continuous loop, presenting a menu to the user until they choose to exit.

## System Architecture

The program is built using a **modular design** with three separate COBOL programs:

1. **MainProgram** - User interface and menu handling
2. **OperationsProgram** - Business logic for account operations
3. **DataProgram** - Data storage and retrieval

This separation follows good software engineering practices by separating concerns:
- User interface logic
- Business rules and calculations
- Data persistence

## Key Business Characteristics

- **Single Account System**: Manages only one account at a time
- **In-Memory Storage**: Account balance is stored in program memory (not persistent)
- **Interactive Menu**: User-driven operations through console interface
- **Real-time Processing**: All transactions are processed immediately
- **Basic Validation**: Includes insufficient funds checking for withdrawals
