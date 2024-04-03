# LucidVM
Lucid is a virtual machine for C-like languages intended to run on a variety of embedded platforms. It has grown out of two other interpreted language I wrote in other repositories: m8rscript and Clover. I wanted to make a runtime that could run on Arduino hardware (e.g., ATMega128 based), ESP8266/ESP32, and a bare-metal Raspberry Pi. I also want to run it on a Mac to allow code to be simulated. It is feature rich, but expensive features can be optionally excluded to allow the runtime to work on small platforms like Arduino.

## Execution Engine

The Lucid interpreter is a stack based virtual machine. Opcodes are one byte followed by zero or more bytes of operands. Lucid is class based. All code is inside classes. A Lucid executable file has an identifying header and then a list of all the classes which gives the size and code of each class. To run the executable, the runtime instantiates the "main" class and calls its constructor.

## Classes

To instantiate a class you specify the address of the prototype. The prototype indicates the amount of memory needed to hold the class instance. The memory is allocated on the stack for classes instantiated directly, or in the heap for classes instantiated with new. Each member variable is accessed by giving the address of the instance on the stack or in the heap and the offset
of the variable in that instance.

To access a constant in a class prototype you specify the address of the constant in that prototype.

## Addressing

Values are pushed as one of the supported datatypes. There is no type checking done at runtime. It's expected that the right type is determined at compile time. Class prototype constants are pushed on the stack by specifying the address of the constant in the executable, determined at compile time. When a function is entered the bp is set to the start of the passed params. These are followed by local variables needed by the function. Params and locals are both addresses in the same space, using the bp as the base.

When a address is specified, either in the PushRef, Push or Pop opcodes it is a single integer value. Negative values are reserved for constants in class prototypes. The address of the constant is determined by negating the address value. Non-negative values are for variables on the stack, relative to the bp.

> ### TODO:
> 
> * What about upvalues?
> * What about addresses in the heap

## Limits

There are various size limits in the execution engine, determined by the size of addresses in the opcodes. With a 16 bit operand the system supports 32k constants and 32k locals

> ### TODO:
> 
> This seems like a poor allocation of the address spaces. It would limit the stack size to 32k. Maybe allow a 32 bit address and have different opcodes which take 1, 2 or 4 bytes to allow for more compact small programs.

## Upvalues

Lucid supports closures: function object which contain runtime state. A closure can capture variables in its parent function. These are represented by Upvalues which is defined in Lua. An Upvalue can be open meaning the closure is executing while its parent function is still active. This allows the closure to change the upvalue and have that change reflected in the parent when the closure finishes. Or it can be closed meaning the parent function has returned and the closure stores a static version of the variable.

A function prototype has a list of all upvalues it contains. Each entry has a stack index, relative to the stack frame containing the upvalue and a frame number, relative to the enclosing function (direct parent is zero, its parent is one, etc.). When a closure is created from its function prototype, an UpValue object is created for each upvalue
