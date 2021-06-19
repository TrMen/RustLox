# RustLox
An interpreter based on a simple Bytecode VM for the language Lox, specified in the book "Crafting Interpreters"

This is an implementation of the second half of the above-mentioned book. Compared to my other repo [CppLox](https://github.com/TrMen/CppLox), this is still in its early stages. 
CppLox implements a treewalker interpreter, which does not compile to any instruction set. 

This repo compiles to a very simple bytecode, which is then executed by a stack-based VM. The interpreted language is the same (modulo some extensions changes I made to CppLox).
