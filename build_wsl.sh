#!/bin/bash
set -e

echo "Compiling Banking System in WSL..."
mkdir -p BIN

# Convert line endings if necessary (Windows -> Unix)
# dos2unix SOURCE/*.cbl COPY/*.cpy 2>/dev/null || true

echo "Compiling MAIN..."
cobc -x -free -I COPY SOURCE/MAIN.cbl SOURCE/ACCTMGT.cbl SOURCE/TXNPROC.cbl -o BIN/main

echo "Compiling BATCHRUN..."
cobc -x -free -I COPY SOURCE/BATCHRUN.cbl SOURCE/TXNPROC.cbl SOURCE/ACCTMGT.cbl -o BIN/batchrun

echo "Compilation Successful!"
echo "Run ./BIN/main to start."
