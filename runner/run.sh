#!/bin/bash

# Enable strict error handling
set -euo pipefail

# Define base directory relative to the script location
BASE_DIR="$(cd "$(dirname "$0")/.." && pwd)"

# Define variables
SRC_DIR="$BASE_DIR/src"
BNF_DIR="$BASE_DIR/examples/bnf"
EXPR_DIR="$BASE_DIR/examples/expr"
JIKESPG_DIR="$BASE_DIR/examples/jikespg"
FORMULACHESS_DIR="$BASE_DIR/examples/formulachess"
JAVA_DIR="$BASE_DIR/examples/java"

# Function to display error messages in red
display_error() {
    echo -e "\033[0;31m$1\033[0m" >&2
}

# Trap errors to ensure cleanup happens even if a command fails
trap 'display_error "A command failed. Cleaning up..."; cleanup' ERR

# Cleanup function
cleanup() {
    rm -f "$BNF_DIR/jikespg"
    rm -f "$EXPR_DIR/jikespg"
    rm -f "$JIKESPG_DIR/jikespg"
    rm -f "$FORMULACHESS_DIR/jikespg"
    rm -f "$JAVA_DIR/jikespg"
    cd "$SRC_DIR"
    make clean > /dev/null || echo "Failed to clean up build artifacts in $SRC_DIR"
    echo "Cleanup completed."
}

# Step 1: Navigate to the source directory and build jikespg
cd "$SRC_DIR"
make clean > /dev/null
make jikespg > /dev/null

# Step 2: Copy jikespg to the BNF directory and run it
cp "$SRC_DIR/jikespg" "$BNF_DIR/"
cd "$BNF_DIR"
./jikespg bnf.g

# Step 3: Copy jikespg to the EXPR directory and run it
cp "$SRC_DIR/jikespg" "$EXPR_DIR/"
cd "$EXPR_DIR"
./jikespg expr.g

# Step 4: Copy jikespg to the JIKESPG directory and run it
cp "$SRC_DIR/jikespg" "$JIKESPG_DIR/"
cd "$JIKESPG_DIR"
./jikespg jikespg.g

# Step 5: Copy jikespg to the FORMULACHESS directory and run it
# cp "$SRC_DIR/jikespg" "$FORMULACHESS_DIR/"
# cd "$FORMULACHESS_DIR"
# echo "Running './jikespg jikespg.g' in $FORMULACHESS_DIR"
# ./jikespg pgn.g

# Step 6: Copy jikespg to the JAVA directory and run it
# cp "$SRC_DIR/jikespg" "$JAVA_DIR/"
# cd "$JAVA_DIR"
# echo "Running './jikespg jikespg.g' in $JAVA_DIR"
# ./jikespg java.g

# Step 6: Cleanup - Remove jikespg from the example directories and clean the source directory
cleanup

echo "All commands completed successfully."
