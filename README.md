# Clover
Clover - A tiny C-like language for embedded systems

## Overview

Clover is a strongly typed, C-like language designed to run in a small microcontroller environment. It is a standalone package with a separate compiler and runtime. There is a Mac project to run the compiler, which takes Clover source an turns it into interpreted bytecodes which are executed by the Runtime. The runtime works on Mac so Clover programs can be debugged. It also runs on Arduino and ESP microcontrollers, and maybe others in the future.

The Interpreter is an templated class which is specialized with the size of memory allocated to it. Since embedded systems tend to have a small amount of RAM, this allows memory to be given to the Interpreter at compile time to avoid memory allocation. In fact, Clover does not currently have a memory allocation API. All memory is allocated on the stack. The top level of every Clover program is a struct which is instantiated on the stack before execution begins. This is a big limitation but it allows Clover to run on the base Arduino which has 2KB of RAM.

When instantiating the Interpreter, you can pass a list of NativeModules. These are subclasses of NativeModule which add functionality to the interpreter in the form of native function calls. Parameters are passed to a native function on the stack, so when the NativeModule call virtual method is called, a pointer to the Interpreter is passed. This allows you to read stack values, set errors that the interpreter can return, etc.

## History

Clover grew out my need to upload code to an Arduino (Nano in this case) without having to connect to it via USB. It was for a Project where I was daisy chaining a series of Neopixel LED lights on top of fence posts. Each ring of pixels is connected to an Arduino Nano and do effects like flicker, strobe and a rainbow effect. I didn't want to have disassemble all 9 light posts, plug into usb and upload new Arduino code just to add or change an effect. So I created Clover, a simple C-like language. You compile the source code, producing bytecodees which are interpreted by the Clover runtime. SoftSerial is used to talk to the Arduinos from a Raspberry Pi running Node-Red. The RPi is connected to the SoftSerial input of the first Nano, and its output is connected to the SoftSerial input of the next, etc. From a web page you can upload new interpreted code and send commands to select the effect, color, rate, etc. All without ever touching the Nanos.

The bytecode has to be very compact because it has to be uploaded to the EEPROM of the Arduino and the Nano only has 1KB of EEPROM. But this is enough to run multiple lighting effects and to communicate between Arduinos.

But I realized that Clover was more general purpose than that. So I increased its capabilities by adding support for 32 bit integers and floats. You can create a runtime with or without these fetures. Compile the engine without either and you get a compact runtime and byte code that can fit in the 1KB of EEPROM of the Arduino. Compile with these features and you can run more powerful programs on systems like the ESP32. In the future I plan on added even more features to give Clover more object oriented features.

## Language Features

### Constants

The 'const' element defines a constant value that can be an integer, float or array of integers or floats. Scalar values are encoded into bytecodes and arrays are stored and accessed from the program memory space. Another type of constant is strings. These are also stored in program memory as part of the PUSHS bytecode and are limited to 254 characters in length.

### Structs

The 'struct' element allows the definition of a structure of named int and float values. A struct can contain member variables, functions and a constructor. Every struct declares the amount of memory needed to contain its local variables. At the top level in every Clover program is a struct which contains a main() function. To start a program the top level struct is instantiated with the number of bytes needed by its locals allocated on the stack. Then its constructor is called with args passed to it available from various methods in the 'core' module. Then the main function is called.

### Variables

Variables are introduced in the struct by naming its type followed by an identifier and optionally initializers. Variables can be int (signed or unsigned, 8, 16 or 32 bits), float, pointer, struct or array.

### Functions

The 'function' element introduces a function. The function name can be preceded by an optional type which is its return type. It can have 0 or more formal parameters, which can have the type int, float or pointer to int, float or struct. Every function is considered vararg. It more arguments are passed than expected they are made available from methods in the 'core' module. If fewer arguments are passed they will be set to 0.

## Runtime

The runtime is a stack oriented virtual machine. There are opcodes for pushing and popping, function calls keep the return pc and base pointer on the stack, and all operations are performed on the top one or two stack elements. In addition to ints and floats, the stack can also contain a pointer. To store the result of an operation you first PushRef to push a pointer to where you want the result, then Push the two operands, do the operation which leaves the result on the stack, then PopDeref to store the result at the pushed address.

### Strong Typing
The runtime is strongly typed. Every value on the stack is an int, float or pointer. The operation performed assuming the value is of the correct type. There is no runtime type checking. For instance, there are AddInt and AddFloat operations, which assume the two operands are both int or float. It's up to the compiler to keep track of the types and perform type conversion or generate type clash errors.

### Pointers

There are a number of operations that work with pointers. PushRef pushes a pointer to a value in memory (ROM or RAM). PushDeref loads the value at the address on top of stack and pushes it. PopDeref stores the value on the top of stack at the address in TOS-1. Offset adds the value in the opcode to the address on the top of stack. It's used to access structure members. Index takes the top of stack as an array index, multiplies it by the element size value in the opcode and the adds that to the address on the stack. It's used to index into arrays.

### Flow Control

Clover has an if/then/else construct. The If opcode looks at the value on TOS and if zero it jumps the number of bytes in the sz field of the opcode. If that jump takes you to an Else opcode then that clause is executed if the if was skipped, or skips that clause if the if clause was executed.

Clover also contains looping statements. These are represented in the runtime by the Jump and Loop opcodes. The former jumps forward and the latter jumps backward. Currently Clover has a 'for' which increments a variable until it equals a limit value. Tests are generated and Jump and Loop are used to implement the iteration. There are also 'while' and 'loop' statements using the same opcodes. And finally 'break' and 'continue' statements can be used in any looping clause to just out of or to the end of the clause. All of these are built on top of Jump and Loop.

Switch statements work differently in Clover than in other C-like languages. Rather than ending a sequence of statements in a case block with a break statement, each case executes only one statement and then exits the switch. To include more than one statement a block is used. This avoids the common mistake of forgetting to include the break and makes the contents of a case more clear. 

### Function calls

Function arguments and local variables are kept on the stack. The bp pointer defines the start of the stack frame for the current function. The first n words of the frame are passed arguments, followed by local variable storage, followed by the return pc and previous bp value. The sp points past the all this and is used to push values onto the stack. The SetFrame opcode is used to establish the number of arguments and local variables. This adjusts the sp and bp for the current function and makes room for local variables. After SetFrame the return pc and previous bp (pushed by the Call opcode) are on top of the stack. The Return opcode pops these, adjusts sp, restores bp jumps to the return pc address.

## Native Modules

Developers can add functionality to the runtime by subclassing NativeModule and implementing the pure virtual functions. Each module has a compile side, which has a table of all functions, their id and the number and type of arguments they expect. There is also an interpreter side which decides if the module implements a given id, how many arguments that function has and implements the actual call. The compile side can be omitted on Arduino with an ifdef to save space. Clover has a NativeCore module which has general purpose methods for converting types, generating random numbers, etc.

The NativeCall opcode is the same as Call, in that it pushes pc and bp, but the target is an id of a native function (installed as a NativeModule). The call() virtual method of the NativeModule is called to execute the added functionality. There are 256 ids possible. Each module has 16 possible ids, from 0x?0 to 0x?f. So there are 16 modules possible. The first two modules (0x0? and 0x1?) are reserved for core functions. There is no attempt to manage the module ids. If you add more than one you need to make sure their ids don't clash.

An executable compiled with a given set of NativeModules much be executed by an Interpreter with those same NativeModules or unexpected results will occur.

## Limitations

The Clover virtual machine is very small and designed to run on AVR processors with 32K of ROM, 2K of RAM, and 1K of EEPROM. So many of the limits are based on memory size. Also Arly, the Clover machine language is designed to be very compact. So it adds it's own limits. All memory in Clover is addressed as a 4 byte value, either integer, float or pointer. The machine code itself is made up of single byte entries, so jumps in the code are on byte boundaries as well. When describing limits I will describe what would have to be done to expand these limits.

### Variables and constants

Clover can address constants in EEPROM, as well as global and local variables in RAM. The machine code represents these as a single byte. A value between 0 and 0x7f is a constant value in EEPROM, starting at 0. This allows for 128 values, so constants must be in the first 512 bytes of EEPROM. In reality constant memory takes up as much space as needed and the rest of EEPROM is used for code. At compile time if more than 128 words of constants are created the compile fails.

A value from 0x80 to 0xbf is in global memory, so the maximum global size is 64 words or 256 bytes. The address is (value - 0x80). The global size is determined at compile time and allocated at runtime. A value from 0xc0 to 0xff is a local variable. The address is (value - 0xc0). Locals are allocated on the stack with the bp register as the base. At function entry, enough space is reserved for locals and at function exit it is returned to the stack.

More flexibility would be possible if the ranges of the 3 value type were not fixed. You might have a compilation that needs more globals than constants, so you might want the start of globals to be before 0x80. Since we know the size required by globals at runtime, the base of each value type could be set for each compilations unit. This is a possible future change, It's also possible to allow 256 values of each type if opcodes were added to handle each value types. Alternatively the opcodes taking a value id (PushRef, Push and Pop) could be made into extended opcodes, which have 4 extra data bits in the lower 4 bits of the opcode. This would allow an id of 12 bits, which would be distributed to the 3 value types, for a total access of 4K words or 16K bytes.

### Jumps

The Loop, Jump, If and Else opcodes take a sz byte, which allows for a jump of 256 bytes (backward in the case of Loop and forward for the others). So it's possible for a jump to be too big. This is detected at compile time and the compilation fails when it happens. There are two possibilities to resolve this. First, these opcodes could be made extended, which would allow for a 4K jump distance. Or a jump instruction with a 2 byte sz could be created and used when a long jump is required. It would have a signed sz, so it could take the place of Jump and Loop directly. There would also be a long If and Else with a 2 byte sz value. This would allow for a jump of 32KB.

### Structs and Arrays

The Offset opcode has a 4 bit offset value. It's used with the dot notation, allowing access into structs. Each entry in a struct is a one word value (structs with struct or array entries are not allowed). So each struct is limited to 16 values. The Index opcode also has a 4 bit size value. It's used to access elements of an array. This value is the size of each entry in the array, so it's multiplied by the array index. That means each array entry can be a maxumum of 16 words in size. An array of floats of Ints has an entry size of 1, so the only time this happens is for arrays of structs. Since a struct is already limited to 16 entries, this limit matches that. Array indexes are integers, so theoretically they could be 64K words in length.

A long Index and Offset opcode could be added to extend this to 12 bit entries, allowing 4K word structs. This could allow for structs containing fixed size arrays, as long as the array size doesn't make the struct more than 4K words. This is currently no syntax to do this, so that would have to be added.

### Functions

Functions must start with a SetFrame opcode. This has a 4 bit param value and an 8 bit local value. Formal parameters can only be int, float or pointer, so functions are limited to 16 params. Local variables can be structs or arrays as well and there can be a total of 256 words of local variables. 16 formal params is not a big restriction. But 256 words of locals restricts the size of local arrays and the number of possible structs. So this limit could be solved with a long SetFrame form, taking a 2 byte local size, allowing for 64K words of local variables.

The target for a function call is an absolute byte address in RAM. The Call instruction is extended and takes a subsequent byte allowing for an address 4KB in size. A long Call could be created which would allow for a 64KB address range.

### Native Functions

Clover also supports native functions. They are kept in modules added to the runtime to add function calls for specific applications. There is a Core set included with Clover which does things like converting float to int, generating random numbers, etc. Modules can be written by subclassing the NativeModule base class. These are passed to the compiler and interpreter to add functionality. ifdefs are used to compile only the runtime parts for Arduino to reduce space. The functions in each module have a byte id. Each module can have up to 16 functions, so each module has a prefix of 0x?0. The first two prefixes (0x00 and 0x10) are reserved for the Core module. The prefix for other modules must be managed by the developer to avoid conflicts.

A long version of CallNative could be created with a 2 byte function id to extend the number of functions to 64K.

## Future Work

- [ ] Add block scope support.
- [ ] Add closures and upvalues.
- [ ] Add heap allocation for some profiles

## Upvalues

Future support for closures: function object which contain runtime state. A closure can capture variables in its parent function. These are represented by Upvalues which is defined in Lua. An Upvalue can be open meaning the closure is executing while its parent function is still active. This allows the closure to change the upvalue and have that change reflected in the parent when the closure finishes. Or it can be closed meaning the parent function has returned and the closure stores a static version of the variable.

A function prototype has a list of all upvalues it contains. Each entry has a stack index, relative to the stack frame containing the upvalue and a frame number, relative to the enclosing function (direct parent is zero, its parent is one, etc.). When a closure is created from its function prototype, an UpValue object is created for each upvalue

## BNF for Clover

 program:
    { import } struct ;

import:
    'import' <id> [ 'as' <id> ] ;
    
struct:
    'struct' <id> '{' { structEntry ';' } '}' ;
    
structEntry:
    struct | varStatement | function | ctor | enum  ;

varStatement:
    [ 'const' ] type [ '*' ] var ';' ;

var:
    <id> [ '[' <integer> ']' ] [ '=' initializer ] ;
    
initializer:
    arithmeticExpression | '{' [ initializer { ',' initializer } ] '}'

function:
    'function' [ <type> ] <id> '(' formalParameterList ')' compoundStatement ;

ctor:
    <id> '(' ')' compoundStatement ;

enum:
    'enum' <id> '{' [enumEntry ] { ',' enumEntry } '}' ;

enumEntry:
    <id> [ '=' <integer> ] ;

// <id> is a struct or enum name
type:
      'float'
    | 'hfloat'
    | 'int8_t'
    | 'uint8_t'
    | 'int16_t'
    | 'uint16_t'
    | 'int32_t'
    | 'uint32_t'
    | <id>
    ;
    
statement:
      compoundStatement
    | ifStatement
    | switchStatement
    | forStatement
    | whileStatement
    | loopStatement
    | returnStatement
    | jumpStatement
    | varStatement
    | expressionStatement
    ;
  
compoundStatement:
    '{' { statement } '}' ;

ifStatement:
    'if' '(' arithmeticExpression ')' statement ['else' statement ] ;

switchStatement:
    'switch' '(' arithmeticExpression ')' '{' { caseClause } '}' ;

caseClause:
    ('case' value | 'default) ':' statement ;

forStatement:
    'for' '(' [ [ <type> ] identifier '=' arithmeticExpression ] ';'
            [ arithmeticExpression ] ';' [ arithmeticExpression ] ')' statement ;
    
whileStatement:
    'while' '(' arithmeticExpression ')' statement ;

loopStatement:
    'loop' statement ;

returnStatement:
      'return' [ arithmeticExpression ] ';' ;
      
jumpStatement:
      'break' ';'
    | 'continue' ';'
    ;

expressionStatement:
    [ arithmeticExpression ] ';' ;
    
arithmeticExpression:
      conditionalExpression
    | unaryExpression operator arithmeticExpression

conditionalExpression:
      unaryExpression
    | conditionalExpression '?' expression : expression
    ;
    
unaryExpression:
      postfixExpression
    | '-' unaryExpression
    | '~' unaryExpression
    | '!' unaryExpression
    | '++' unaryExpression
    | '--' unaryExpression
    | '&' unaryExpression
    ;

postfixExpression:
      primaryExpression
    | postfixExpression '(' argumentList ')'
    | postfixExpression '[' arithmeticExpression ']'
    | postfixExpression '.' identifier
    | postfixExpression '++'
    | postfixExpression '--'
    ;

primaryExpression:
      '(' arithmeticExpression ')'
    | <id>
    | <float>
    | <integer>
    ;
    
formalParameterList:
      (* empty *)
    | type ['*'] identifier { ',' type identifier }
    ;

argumentList:
        (* empty *)
      | arithmeticExpression { ',' arithmeticExpression }
      ;

value:
    
operator: (* operator   precedence   association *)
               '='     (*   1          Right    *)
    |          '+='    (*   1          Right    *)
    |          '-='    (*   1          Right    *)
    |          '*='    (*   1          Right    *)
    |          '/='    (*   1          Right    *)
    |          '%='    (*   1          Right    *)
    |          '&='    (*   1          Right    *)
    |          '|='    (*   1          Right    *)
    |          '^='    (*   1          Right    *)
    |          '||'    (*   2          Left     *)
    |          '&&'    (*   3          Left     *)
    |          '|'     (*   4          Left     *)
    |          '^'     (*   5          Left     *)
    |          '&'     (*   6          Left     *)
    |          '=='    (*   7          Left     *)
    |          '!='    (*   7          Left     *)
    |          '<'     (*   8          Left     *)
    |          '>'     (*   8          Left     *)
    |          '>='    (*   8          Left     *)
    |          '<='    (*   8          Left     *)
    |          '+'     (*   10         Left     *)
    |          '-'     (*   10         Left     *)
    |          '*'     (*   11         Left     *)
    |          '/'     (*   11         Left     *)
    |          '%'     (*   11         Left     *)
    ;
