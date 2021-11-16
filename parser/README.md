# Liva specs
[![codecov](https://codecov.io/gh/HallerPatrick/live_parser/branch/master/graph/badge.svg?token=B4T9MOAJAT)](https://codecov.io/gh/HallerPatrick/live_parser)

This module contains a parser, which is able to parse the liva programming language.

For now this means we generate a intermediate representation, generated with the help of `nom`,
 so this is kind of a mix between a lexer and parser.

Lua directly emits the opcodes to the VM while parsing. We dont do this cause we like the
decoupling of the parser from the VM. When perfomance become critical/present, this may change.


## What to parse

* [x] Comments
* [x] Literals:
    * [x] Num
    * [x] String
    * [x] Bool
    * [x] Array
    * [x] Map
    * [x] Nil
    * [x] Variable Names
* [x] Expressions:
* [x] Statements:
    * [x] For loop
    * [x] While loop
    * [x] If(/Else) // TODO: ELSE
    * [x] Declarations:
        * [x] Classes
        * [x] Functions
        * [x] Assignment
    * [x] Return
    * [x] Import 
* [x] Keep track of line number and line span
* [ ] Type annotations
* [ ] Annonymus functios?


## Crashing snippets

* TODO: Create automated running of the parser for a folder full of source files, they should all running        and return a valid and complete liva ast

```Lua
print(":")
pritn("!")
```
