# Quick Start Guide

## Running the COBOL Banking System

### 1. Interactive Mode (Menu-driven)
```bash
wsl ./BIN/main
```

You'll see a menu with 6 options:
1. **Create Account** - Set up new customer accounts
2. **Account Inquiry** - Check account balance and details  
3. **Deposit** - Add money to an account
4. **Withdraw** - Take money out (with balance validation)
5. **Transfer** - Move money between accounts
6. **Exit** - Close the program

### 2. Batch Processing Mode
```bash
wsl ./BIN/batchrun
```

Processes transactions from `DATA/batch_input.txt` and shows results.

### 3. Demo Script (Quick Test)
```bash
wsl ./demo.sh
```

Runs a demonstration with sample transactions to verify the system works.

---

## Rebuilding the System

If you make changes to the COBOL source code:

```bash
wsl ./build_wsl.sh
```

This will recompile all programs and create fresh executables in the `BIN/` directory.

---

## Sample Batch Input Format

Edit `DATA/batch_input.txt` to add transactions:

```
D1000100000000000500000  (Deposit $5,000.00 to account 10001)
W1000100000000000100000  (Withdraw $1,000.00 from 10001)
T1000200001000300050000  (Transfer $500.00 from 10002 to 10003)
```

Format: `[Type][FromAccount][ToAccount][Amount]`
- Type: D=Deposit, W=Withdraw, T=Transfer
- Account IDs: 5 digits
- Amount: 9 digits + 2 decimal places (no decimal point)

---

## System Status: ✅ VERIFIED & OPERATIONAL

All components compile successfully and run without errors in WSL using GnuCOBOL 3.1.2.
