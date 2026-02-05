# 🏦 COBOL Banking System

A fully functional banking system built with COBOL, demonstrating classic mainframe programming techniques and modern best practices for enterprise applications.

![COBOL](https://img.shields.io/badge/COBOL-GnuCOBOL%203.1.2-blue)
![License](https://img.shields.io/badge/license-MIT-green)
![Platform](https://img.shields.io/badge/platform-Windows%20%7C%20Linux-lightgrey)

## 📋 Table of Contents

- [Overview](#overview)
- [Features](#features)
- [System Architecture](#system-architecture)
- [Installation](#installation)
- [Usage](#usage)
- [Project Structure](#project-structure)
- [Technical Details](#technical-details)
- [Sample Operations](#sample-operations)
- [Error Handling](#error-handling)
- [Contributing](#contributing)
- [License](#license)

---

## 🎯 Overview

This project is a comprehensive banking system implementation in COBOL that showcases:

- **Account Management** - Create and query customer accounts
- **Transaction Processing** - Deposits, withdrawals, and transfers
- **Batch Processing** - Process multiple transactions from files
- **Data Integrity** - Balance validation and error handling
- **Transaction Logging** - Complete audit trail of all operations
- **File Organization** - Indexed files for efficient data access

Perfect for learning COBOL programming, understanding legacy banking systems, or demonstrating enterprise application development concepts.

---

## ✨ Features

### Core Banking Operations

- ✅ **Account Creation** - Set up new customer accounts with unique IDs
- ✅ **Account Inquiry** - Retrieve account information and balance
- ✅ **Deposits** - Add funds to any active account
- ✅ **Withdrawals** - Remove funds with insufficient balance protection
- ✅ **Transfers** - Move money between accounts atomically
- ✅ **Transaction History** - Automatic logging of all operations

### Data Management

- ✅ **Indexed File Access** - Fast account lookups using ISAM
- ✅ **Sequential Transaction Log** - Append-only audit trail
- ✅ **Batch Processing** - Process bulk transactions from text files
- ✅ **File Status Checking** - Comprehensive error handling

### Business Logic

- ✅ **Negative Balance Detection** - Prevent overdrafts
- ✅ **Account Status Tracking** - Active/Closed/Frozen states
- ✅ **Duplicate Account Prevention** - Unique key constraints
- ✅ **Timestamp Generation** - Date/time stamping for transactions

---

## 🏗️ System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     USER INTERFACES                         │
├─────────────────────┬───────────────────────────────────────┤
│  Interactive Menu   │      Batch Processor                  │
│   (MAIN.cbl)        │      (BATCHRUN.cbl)                   │
└──────────┬──────────┴─────────────┬─────────────────────────┘
           │                        │
           ▼                        ▼
┌─────────────────────────────────────────────────────────────┐
│                   BUSINESS LOGIC LAYER                      │
├──────────────────────────┬──────────────────────────────────┤
│  Account Management      │  Transaction Processing          │
│  (ACCTMGT.cbl)          │  (TXNPROC.cbl)                   │
│  - Create Account       │  - Deposit                        │
│  - Inquiry              │  - Withdraw                       │
│                         │  - Transfer                       │
└──────────┬───────────────┴────────────┬─────────────────────┘
           │                            │
           ▼                            ▼
┌─────────────────────────────────────────────────────────────┐
│                      DATA LAYER                             │
├──────────────────────┬──────────────────────────────────────┤
│  Indexed Files       │  Sequential Files                    │
│  - accounts.dat      │  - trans.dat (transaction log)       │
│  (ISAM for lookups)  │  - batch_input.txt (input queue)     │
└──────────────────────┴──────────────────────────────────────┘
```

### Module Responsibilities

| Module | Purpose | File Type |
|--------|---------|-----------|
| **MAIN.cbl** | Interactive user interface with menu-driven operations | Executable |
| **ACCTMGT.cbl** | Account creation and inquiry operations | Callable module |
| **TXNPROC.cbl** | Transaction processing (D/W/T) with logging | Callable module |
| **BATCHRUN.cbl** | Batch transaction processor with reporting | Executable |
| **SETUP.cbl** | Database initialization utility | Executable |

---

## 🚀 Installation

### Prerequisites

- **GnuCOBOL 3.1.2+** (Open Source COBOL compiler)
- **Linux/WSL** environment (Ubuntu, Debian, etc.)
- **Basic shell access**

### For Windows Users (WSL)

1. **Enable WSL and install Ubuntu:**
   ```powershell
   wsl --install
   ```

2. **Update package manager:**
   ```bash
   sudo apt-get update
   ```

3. **Install GnuCOBOL:**
   ```bash
   sudo apt-get install -y gnucobol
   ```

4. **Verify installation:**
   ```bash
   cobc --version
   # Expected: cobc (GnuCOBOL) 3.1.2
   ```

### For Linux Users

```bash
# Debian/Ubuntu
sudo apt-get update
sudo apt-get install -y gnucobol

# Fedora/RHEL
sudo dnf install gnucobol

# Arch Linux
sudo pacman -S gnucobol
```

### Clone and Build

```bash
# Clone the repository
git clone https://github.com/yourusername/cobol-banking-system.git
cd cobol-banking-system

# Make build script executable
chmod +x build_wsl.sh

# Compile all programs
./build_wsl.sh
```

**Expected Output:**
```
Compiling Banking System in WSL...
Compiling MAIN...
Compiling BATCHRUN...
Compilation Successful!
Run ./BIN/main to start.
```

---

## 💻 Usage

### Interactive Mode

Launch the menu-driven interface:

```bash
./BIN/main
```

**Main Menu:**
```
========================================
      COBOL BANKING SYSTEM v1.0
========================================
1. Create Account
2. Account Inquiry
3. Deposit
4. Withdraw
5. Transfer
6. Exit
Enter Selection:
```

#### Example: Creating an Account

```
Enter Selection: 1
Create New Account
Enter ID (5 digits): 10001
Enter Name (30 chars): John Doe
Account Created Successfully!
```

#### Example: Making a Deposit

```
Enter Selection: 3
Account ID: 10001
Amount: 1000.00
Transaction Successful!
```

### Batch Processing Mode

Process multiple transactions from a file:

```bash
./BIN/batchrun
```

**Input File Format** (`DATA/batch_input.txt`):
```
D1000100000000001000000
W1000100000000000500000
T1000100001000020300000
```

**Output:**
```
Starting Batch Processing...
Line 1: Success
Line 2: Success
Line 3: Success
Batch Processing Complete.
Total Processed: 3
Successful:      3
Failed:          0
```

---

## 📁 Project Structure

```
cobol-banking-system/
│
├── SOURCE/                      # COBOL source files
│   ├── MAIN.cbl                # Interactive menu system (159 lines)
│   ├── ACCTMGT.cbl             # Account management (108 lines)
│   ├── TXNPROC.cbl             # Transaction processor (174 lines)
│   ├── BATCHRUN.cbl            # Batch runner (75 lines)
│   └── SETUP.cbl               # Database setup utility
│
├── COPY/                        # Copybook definitions
│   ├── ACCOUNT.cpy             # Account record structure
│   ├── TRANS.cpy               # Transaction record structure
│   └── ERROR.cpy               # Error code definitions
│
├── DATA/                        # Data files (created at runtime)
│   ├── accounts.dat            # Indexed account database
│   ├── trans.dat               # Transaction log (sequential)
│   ├── batch_input.txt         # Sample batch input
│   └── batch_input_insufficient.txt  # Test case for errors
│
├── BIN/                         # Compiled executables
│   ├── main                    # Interactive banking system
│   ├── batchrun                # Batch processor
│   └── setup                   # Setup utility
│
├── build_wsl.sh                 # Build script for WSL/Linux
├── build.bat                    # Build script for Windows
├── run_main.bat                 # Windows launcher
├── run_batch.bat                # Windows batch launcher
├── QUICKSTART.md                # Quick reference guide
└── README.md                    # This file
```

---

## 🔧 Technical Details

### Data Structures

#### Account Record (72 bytes)

```cobol
01  ACCOUNT-RECORD.
    05  ACCT-ID             PIC 9(05).        *> 5-digit account ID
    05  ACCT-NAME           PIC X(30).        *> Customer name
    05  ACCT-BALANCE        PIC S9(09)V99 COMP-3.  *> Packed decimal
    05  ACCT-STATUS         PIC X(01).        *> A/C/F (Active/Closed/Frozen)
        88 ACCT-ACTIVE      VALUE 'A'.
        88 ACCT-CLOSED      VALUE 'C'.
        88 ACCT-FROZEN      VALUE 'F'.
    05  ACCT-OPEN-DATE      PIC 9(08).        *> YYYYMMDD
    05  FILLER              PIC X(20).        *> Reserved
```

#### Transaction Record (36 bytes)

```cobol
01  TRANS-RECORD.
    05  TRANS-ID            PIC 9(08).        *> Unique transaction ID
    05  TRANS-ACCT-ID       PIC 9(05).        *> Source account
    05  TRANS-TYPE          PIC X(01).        *> D/W/T
        88 TRANS-DEPOSIT    VALUE 'D'.
        88 TRANS-WITHDRAW   VALUE 'W'.
        88 TRANS-TRANSFER   VALUE 'T'.
    05  TRANS-AMOUNT        PIC S9(09)V99.    *> Transaction amount
    05  TRANS-DATE          PIC 9(08).        *> YYYYMMDD
    05  TRANS-TIME          PIC 9(06).        *> HHMMSS
```

### File Organization

| File | Type | Access | Key | Purpose |
|------|------|--------|-----|---------|
| `accounts.dat` | INDEXED | DYNAMIC | ACCT-ID | Fast account lookups |
| `trans.dat` | LINE SEQUENTIAL | SEQUENTIAL | N/A | Append-only transaction log |
| `batch_input.txt` | LINE SEQUENTIAL | SEQUENTIAL | N/A | Batch input queue |

### Batch Input Format

Each line represents a transaction:

```
[Type][Account][Target][Amount]
  1      5        5       11
```

**Examples:**
```
D1000100000000001000000  → Deposit $10,000.00 to account 10001
W1000200000000000500000  → Withdraw $5,000.00 from account 10002
T1000300001000040300000  → Transfer $3,000.00 from 10003 to 10004
```

**Format Breakdown:**
- **Type** (1 char): `D` = Deposit, `W` = Withdraw, `T` = Transfer
- **Account** (5 digits): Source account ID
- **Target** (5 digits): Target account (used only for transfers, 00000 otherwise)
- **Amount** (11 digits): Amount in cents (9 digits + 2 decimal places implied)

---

## 📊 Sample Operations

### Creating Multiple Accounts

```bash
# Using the interactive system
./BIN/main

# Select option 1 (Create Account) for each:
# Account 10001: John Doe
# Account 10002: Jane Smith
# Account 10003: ABC Corporation
```

### Running a Complete Batch Job

**Create `DATA/batch_input.txt`:**
```
D1000100000000005000000
D1000200000000003000000
D1000300000000010000000
T1000100001000020100000
W1000200000000000050000
```

**Run:**
```bash
./BIN/batchrun
```

**Expected Results:**
- Account 10001: $50,000.00 deposited → $40,000.00 after transfer out → Final: $40,000.00
- Account 10002: $30,000.00 deposited → $10,000.00 from transfer → $500.00 withdrawn → Final: $39,500.00
- Account 10003: $100,000.00 deposited → Final: $100,000.00

---

## ⚠️ Error Handling

### Result Codes

| Code | Meaning | Triggered By |
|------|---------|--------------|
| `00` | Success | Any successful operation |
| `01` | Duplicate Key | Creating account with existing ID |
| `02` | Not Found | Account doesn't exist |
| `10` | Insufficient Funds | Withdrawal/transfer exceeds balance |
| `99` | System Error | File I/O errors, invalid operations |

### Example Error Scenarios

#### Insufficient Funds

**Input:**
```
W1000100000000009999999  (Withdraw $99,999.99)
```

**Output:**
```
Line 1: Failed (Code 10)
```

#### Account Not Found

**Input:**
```
D9999900000000001000000  (Deposit to non-existent account)
```

**Output:**
```
Line 1: Failed (Code 02)
```

---

## 🏆 Educational Value

This project demonstrates:

### COBOL Concepts
- ✅ **File I/O** - Sequential, indexed, and line sequential file handling
- ✅ **CALL statements** - Inter-program communication
- ✅ **Copybooks** - Code reusability and standardization
- ✅ **LINKAGE SECTION** - Parameter passing between programs
- ✅ **88 level conditions** - Readable boolean conditions
- ✅ **COMP-3 (Packed Decimal)** - Efficient numeric storage

### Software Engineering
- ✅ **Modular design** - Separation of concerns
- ✅ **Error handling** - Comprehensive validation
- ✅ **Data integrity** - Balance validation, duplicate detection
- ✅ **Batch processing** - Enterprise-style bulk operations
- ✅ **Audit logging** - Transaction history tracking

### Business Logic
- ✅ **Double-entry bookkeeping** - Transfer operations
- ✅ **Account lifecycle** - Status management (Active/Closed/Frozen)
- ✅ **Overdraft prevention** - Balance validation
- ✅ **Idempotency** - Safe retry of operations

---

## 🔄 Rebuilding

After modifying COBOL sources:

```bash
# WSL/Linux
./build_wsl.sh

# Windows
build.bat
```

**Build Process:**
1. Creates `BIN/` directory if missing
2. Compiles MAIN with linked modules
3. Compiles BATCHRUN with dependencies
4. Reports success or errors

---

## 🤝 Contributing

Contributions are welcome! Here are some ideas:

- [ ] Add interest calculation module
- [ ] Implement account closing functionality
- [ ] Add multi-currency support
- [ ] Create transaction reversal mechanism
- [ ] Build reporting module (statements, summaries)
- [ ] Add authentication/authorization
- [ ] Implement database backup/restore
- [ ] Create RESTful API wrapper

**To contribute:**
1. Fork the repository
2. Create a feature branch (`git checkout -b feature/NewFeature`)
3. Commit your changes (`git commit -m 'Add NewFeature'`)
4. Push to the branch (`git push origin feature/NewFeature`)
5. Open a Pull Request

---

## 📜 License

This project is licensed under the MIT License - see the LICENSE file for details.

---

## 🙏 Acknowledgments

- **GnuCOBOL Team** - For the excellent open-source COBOL compiler
- **Classic Banking Systems** - Inspiration for this implementation
- **COBOL Community** - For keeping the language alive and relevant

---

## 📚 Additional Resources

- [GnuCOBOL Documentation](https://gnucobol.sourceforge.io/)
- [COBOL Programming Guide](https://www.ibm.com/docs/en/cobol-zos)
- [Mainframe Best Practices](https://www.ibm.com/mainframe)
