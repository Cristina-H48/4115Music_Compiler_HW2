#!/bin/bash

# Installation and execution script for the lexer

# Check if OCaml is installed
if ! command -v ocamlc &> /dev/null
then
    echo "OCaml compiler could not be found. Please install OCaml."
    exit 1
fi

echo "OCaml compiler found."

# Clean previous builds
echo "Cleaning previous builds..."
rm -f lexer *.cmo *.cmi

# Compile the code
echo "Compiling the lexer..."
ocamlc -c tokens.ml
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c ast_printer.ml
ocamlc -o main.exe tokens.cmo lexer.cmo parser.cmo ast_printer.cmo main.ml

if [ $? -ne 0 ]; then
    echo "Compilation failed."
    exit 1
fi

echo "Compilation successful."

# Run the lexer with input file provided as argument
if [ $# -eq 0 ]; then
    echo "No input file provided. Usage: ./install_and_run.sh <input_file>"
    exit 1
fi

echo "Running the lexer on input file: $1"
./main.exe "$1"

