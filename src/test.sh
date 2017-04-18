#! /bin/bash

rm -rf opl.exe
make prolog_interpreter
rm -rf *.cmi
rm -rf *.cmo
rm -rf lexer.ml
rm -rf parser.mli
rm -rf parser.ml
echo Done Compiling
 
