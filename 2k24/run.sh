#!/bin/bash

# Exit the script on any error
set -e

# Define file names
MODULE_FILE="util.f95"
OUTPUT_EXEC="main"

# Prompt the user to input a number
read -p "Enter the day: " NUMBER

# Format the number with leading zero if necessary
MAIN_FILE=$(printf "%02d.f95" "$NUMBER")

# Check if the module file exists
if [ ! -f "$MODULE_FILE" ]; then
    echo "Error: Module file '$MODULE_FILE' not found!"
    exit 1
fi

# Check if the main program file exists
if [ ! -f "$MAIN_FILE" ]; then
    echo "Error: Main program file '$MAIN_FILE' not found!"
    exit 1
fi

# Clean up previous builds
echo "Cleaning up old builds..."
rm -f *.o *.mod "$OUTPUT_EXEC"

# Compile the module
echo "Compiling the module..."
gfortran -c "$MODULE_FILE"

# Compile the main program
echo "Compiling the main program ($MAIN_FILE)..."
gfortran -c "$MAIN_FILE"

# Link and create the executable
echo "Linking and creating the executable..."
gfortran -o "$OUTPUT_EXEC" "$MODULE_FILE" "$MAIN_FILE"

# Run the executable
echo "Running the program..."
./"$OUTPUT_EXEC"

# Finish
echo "Execution completed."
