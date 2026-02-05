@echo off
echo Compiling Banking System...

if not exist BIN mkdir BIN

echo Compiling MAIN...
cobc -x -I COPY SOURCE\MAIN.cbl SOURCE\ACCTMGT.cbl SOURCE\TXNPROC.cbl -o BIN\MAIN.exe
if errorlevel 1 (
    echo Compilation of MAIN failed!
    pause
    exit /b 1
)

echo Compiling BATCHRUN...
cobc -x -I COPY SOURCE\BATCHRUN.cbl SOURCE\TXNPROC.cbl SOURCE\ACCTMGT.cbl -o BIN\BATCHRUN.exe
if errorlevel 1 (
    echo Compilation of BATCHRUN failed!
    pause
    exit /b 1
)

echo Compilation Successful!
pause
