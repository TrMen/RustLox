# RustLox
An interpreter based on a simple Bytecode VM for the language Lox, specified in the book "Crafting Interpreters".

This is an implementation of the second half of the above-mentioned book. Compared to my other repo [CppLox](https://github.com/TrMen/CppLox), this is still in its early stages.

The basic language works, including expressions, local and global variable usage and assignment. 
Unimplemented so far are: functions and closures, classes and inheritance, garbage collection

Unlike CppLox, this repo compiles to a very simple bytecode, which is then executed by a stack-based VM. The interpreted language is the same (modulo some extensions changes I made to CppLox).
