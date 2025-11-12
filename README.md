# GnuCOBOL Sample: Sales Data Processing and Master Update

This project is a sample COBOL program for processing sales data, performing validation, and updating a master file. It demonstrates the use of subroutines, copybooks, and indexed files.

## 1. Program Overview

*   **SALES_PROC (Main Program)**: Reads daily sales data, validates it, and calls a subroutine to update the inventory.
*   **INV_UPDATE (Subroutine)**: Updates the stock quantity in the item master file.
*   **CREATE_MASTER**: A utility program to create the initial indexed master file from a sequential file.

## 2. Directory Structure

```
.
├── data/                 # Data files
│   ├── DAILY_SALES.DAT     # Input: Daily sales data
│   ├── initial_master.dat  # Input: Initial master data (sequential)
│   ├── MASTER_ITEM.IDX     # Generated: Indexed item master
│   ├── SALES_HISTORY.DAT   # Output: Valid sales records
│   └── ERROR_SALES.LST     # Output: Invalid sales records
├── src/                  # Source code
│   ├── SALES_PROC.cbl      # Main program
│   ├── INV_UPDATE.cbl      # Subroutine
│   ├── CREATE_MASTER.cbl   # Master creation utility
│   └── copybook/           # Copybooks (shared data definitions)
│       ├── FILEDEF.CPY
│       ├── ITEMREC.CPY
│       └── SALESREC.CPY
└── README.md
```

## 3. Execution Steps

### Step 1: Compile the Master File Creation Utility

This command compiles `CREATE_MASTER.cbl` and creates an executable file `src/CREATE_MASTER`.

```bash
cobc -x -o src/CREATE_MASTER src/CREATE_MASTER.cbl -I src/copybook
```

### Step 2: Create the Indexed Master File

Run the compiled utility to create `MASTER_ITEM.IDX` from `initial_master.dat`.

```bash
src/CREATE_MASTER
```

### Step 3: Compile the Main Program and Subroutine

This command compiles both `SALES_PROC.cbl` and `INV_UPDATE.cbl` together and creates a single executable `src/SALES_PROC`.

```bash
cobc -x -o src/SALES_PROC src/SALES_PROC.cbl src/INV_UPDATE.cbl -I src/copybook
```

### Step 4: Run the Main Program

Execute the main program to process the sales data.

```bash
src/SALES_PROC
```

After execution, you can check the output files `SALES_HISTORY.DAT` and `ERROR_SALES.LST` in the `data` directory to verify the results.
