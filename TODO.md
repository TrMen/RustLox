- Run in valgrind to detect memory leaks
- Optimize
- Run an integration test suite
- Implement optimization for constant strings borrowing the src code
  - `String` becomes `enum (String, &'a str)`
  - If we wanted to make the code compile to a binary, it could easily borrow the constant section then
- Make parsing functions more composable, e.g. Option<...> over so many bool return values everywhere
- Error recovery probably doesn't quite work. E.g. when I define a variable but run out of constants to put the name
  - The variable name is probably still defined afterwards, since I only synchronize after the end of a statement
  - And the name declaration happens before that
- Line information for runtime errors is off by one
 ```
 let a = 2;

{
    let b = 3;
    print c;
    print b;
    print a;
}
print b;
print 2; //  This program only reports undefined global b, not the undefined local c;
```
- Refactor parser and compiler to be easier to place responsibilities.
- Implement functions


