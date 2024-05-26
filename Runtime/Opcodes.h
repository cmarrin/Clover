/*-------------------------------------------------------------------------
    This source file is a part of Lucid
    For the latest info, see https://github.com/cmarrin/LucidVM
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#pragma once

#include <stdint.h>

namespace lucid {

/*

Machine Code for LucidVM

See README.md for details.

Opcodes:
    Param nomenclature:
        
        int8    - Byte after opcode. Int constant (-128 to 127)
        uint8   - Byte after opcode. Int constant (0 to 255)
        int16   - 2 bytes after opcode. Int constant (-32768 to 32767)
        uint16  - 2 bytes after opcode. Int constant (0 to 65535)
        int32   - 4 bytes after opcode. Int constant (-2^31 to 2^31-1)
        uint32  - 4 bytes after opcode. Int constant (0 to 2^32-1)
        half    - 2 bytes after opcode. 16 bit fixed point number (8:8 - +/-127.99)
        float   - 4 bytes after opcode. Floating point number in Float class format
        str     - Byte after opcode is length, followed by length bytes
        
        ref     - Reference to a value on the stack or a constant.

        p       - Byte after opcode. Num params passed to function.
        l       - Byte after opcode. Num locals in function.

    Stack is 8 bit. 16 and 32 bit values are stored in consecutive locations (HI to LO)
    
    Load and store values on the stack
    ==================================
    
    
    // Load and store rather than push/pop
    ex., a.b = c.d
    
    // With push/pop this is 
    
    
    PushDeref1              - uint16 tmp = pop16(), push1(mem1(tmp))
    PushDeref2              - uint16 tmp = pop16(), push2(mem2(tmp))
    PushDeref4              - uint16 tmp = pop16(), push4(mem4(tmp))
    PopDeref1               - uint16 tmp = pop16(), setmem1(tmp, pop1())
    PopDeref2               - uint16 tmp = pop16(), setmem2(tmp, pop2())
    PopDeref4               - uint16 tmp = pop16(), setmem4(tmp, pop4())
    
    PushL1 uint16           - push1(bp1(uint16))
    PushL2 uint16           - push2(bp2(uint16))
    PushL4 uint16           - push4(bp4(uint16))
    PopL1 uint16            - setbp1(uint16, pop1())
    PopL2 uint16            - setbp2(uint16, pop2())
    PopL4 uint16            - setbp4(uint16, pop4())
    
    PushC1 uint8            - push1(uint8)
    PushC2 uint8            - push2(uint8)
    PushC4 uint8            - push4(uint8)
    

    Stack manuipulation
    ===================
    
    Dup1                    - stack[sp] = stack[sp-1]; sp += 1
    Dup2                    - stack[sp] = stack[sp-2]; stack[sp+1] = stack[sp-1]; sp += 2
    Dup4                    - stack[sp] = stack[sp-4]; stack[sp+1] = stack[sp-3];
                              stack[sp+2] = stack[sp-2]; stack[sp+3] = stack[sp-1]; sp += 4
    Drop1                   - sp -= 1
    Drop2                   - sp -= 2
    Drop4                   - sp -= 4
    Swap1                   - tmp = stack[sp-1]; stack[sp-1] = stack[sp-2]; stack[sp-2] = tmp;
    Swap2                   - tmp = stack[sp-1]; stack[sp-1] = stack[sp-3]; stack[sp-3] = tmp;
                              tmp = stack[sp-2]; stack[sp-2] = stack[sp-4]; stack[sp-4] = tmp;
    Swap4                   - tmp = stack[sp-1]; stack[sp-1] = stack[sp-5]; stack[sp-5] = tmp;
                              tmp = stack[sp-2]; stack[sp-2] = stack[sp-6]; stack[sp-6] = tmp;
                              tmp = stack[sp-3]; stack[sp-3] = stack[sp-7]; stack[sp-7] = tmp;
                              tmp = stack[sp-4]; stack[sp-4] = stack[sp-8]; stack[sp-8] = tmp;


    Flow control
    ============
    
    If int16                - If stack[--sp] is non-zero continue execution. 
                              If zero jump to int16.
    
    Jump int16              - Jump size bytes (target is -2048 to 2047)
    
    Call uint16             - Call function at int16 address in executable, params on stack
    CallNative nativeId     - Call native function
    Return                  - Return from function, return value on TOS
    SetFrame p l            - Set the local frame with number of formal
                              params (p) and locals (l). This must be the 
                              first instruction of every function.


    Unary Operations - expect 1 value on stack (a = tos), pop before operation
    ================

    Not1                    - push1(~pop1())
    Not2                    - push2(~pop2())
    Not4                    - push4(~pop4())
    Neg1                    - push1(-pop1())
    Neg2                    - push2(-pop2())
    Neg4                    - push4(-pop4())
    NegF                    - pushF(-popF())  (float)
    NegH                    - pushH(-popH())  (half float)


    Binary Operations - expect 2 values on stack (a = tos-1, b = tos), pop before operation
    =================

    Or1                     - push1(pop1() | pop1())
    Or2                     - push2(pop2() | pop2())
    Or4                     - push4(pop4() | pop4())
    XOr1                    - push1(pop1() ^ pop1())
    XOr2                    - push2(pop2() ^ pop2())
    XOr4                    - push4(pop4() ^ pop4())
    And1                    - push1(pop1() & pop1())
    And2                    - push2(pop2() & pop2())
    And4                    - push4(pop4() & pop4())
    
    LTI1                    - push1(pop1() < pop1()) (signed, result is uint8 0 if false, 1 if true
    LTU1                    - push1(pop1() < pop1()) (unsigned, result is uint8 0 if false, 1 if true
    LTI2                    - push1(pop2() < pop2()) (signed, result is uint8 0 if false, 1 if true
    LTU2                    - push1(pop2() < pop2()) (unsigned, result is uint8 0 if false, 1 if true
    LTI4                    - push1(pop4() < pop4()) (signed, result is uint8 0 if false, 1 if true
    LTU4                    - push1(pop4() < pop4()) (unsigned, result is uint8 0 if false, 1 if true
    LTF                     - push1(popF() < popF()) (float, result is uint8 0 if false, 1 if true
    LTH                     - push1(popH() < popH()) (half float, result is uint8 0 if false, 1 if true
    
    
    LEI                     - stack[sp++] = a <= b (assumes int32_t, result is int32_t)
    LEF                     - stack[sp++] = a <= b (assumes float, result is int32_t)
    EQI                     - stack[sp++] = a == b (assumes int32_t, result is int32_t)
    EQF                     - stack[sp++] = a == b (assumes float, result is int32_t)
    NEI                     - stack[sp++] = a != b (assumes int32_t, result is int32_t)
    NEF                     - stack[sp++] = a != b (assumes float, result is int32_t)
    GEI                     - stack[sp++] = a >= b (assumes int32_t, result is int32_t)
    GEF                     - stack[sp++] = a >= b (assumes float, result is int32_t)
    GTI                     - stack[sp++] = a > b (assumes int32_t, result is int32_t)
    GTF                     - stack[sp++] = a > b (assumes float, result is int32_t)

    Add1                    - push1(pop1() + pop1())
    Add2                    - push2(pop2() + pop2())
    Add4                    - push4(pop4() + pop4())
    AddF                    - push4(popF() + popF())
    AddH                    - push4(popH() + popH())
    Sub1                    - push1(pop1() - pop1())
    Sub2                    - push2(pop2() - pop2())
    Sub4                    - push4(pop4() - pop4())
    SubF                    - push4(pop4() - pop4()) (float)
    SubH                    - push4(pop2() - pop2()) (half float)
    MulS1                   - push1(pop1() * pop1()) (signed multiply)
    MulU1                   - push1(pop1() * pop1()) (unsigned multiply)
    MulS2                   - push2(pop2() * pop2()) (signed multiply)
    MulU2                   - push2(pop2() * pop2()) (unsigned multiply)
    MulS4                   - push4(pop4() * pop4()) (signed multiply)
    MulU4                   - push4(pop4() * pop4()) (unsigned multiply)
    MulF                    - push4(pop4() * pop4()) (float multiply)
    MulH                    - push2(pop2() * pop2()) (half float multiply)
    DivS1                   - push1(pop1() / pop1()) (signed divide)
    DivU1                   - push1(pop1() / pop1()) (unsigned divide)
    DivS2                   - push2(pop2() / pop2()) (signed divide)
    DivU2                   - push2(pop2() / pop2()) (unsigned divide)
    DivS4                   - push4(pop4() / pop4()) (signed divide)
    DivU4                   - push4(pop4() / pop4()) (unsigned divide)
    DivF                    - push4(pop4() / pop4()) (float divide)
    DivH                    - push2(pop2() / pop2()) (half float divide)
    ModU1                   - push1(pop1() % pop1()) (unsigned mod)
    ModS2                   - push2(pop2() % pop2()) (signed mod)
    ModU2                   - push2(pop2() % pop2()) (unsigned mod)
    ModS4                   - push4(pop4() % pop4()) (signed mod)
    ModU4                   - push4(pop4() % pop4()) (unsigned mod)
    ModF                    - push4(pop4() % pop4()) (float mod)
    ModH                    - push2(pop2() % pop2()) (half float mod)



Add1: (a + b, stack has b, a)
    LDA 2,S
    ADDA 1,S
    LEAS 2,S
    PSHS A

Add2:
    LDD 2,S
    ADDD ,S
    LEAS 4,S
    PSHS D
    
Add4:
    JSR Add4

void Add4() // stack has b, a (4 bytes each)
{
    uint32_t b = pop4();
    uint32_t a = pop4();
    push4(a + b);
}

MulU1:
    PULS A,B
    MUL
    PSHS B

MulS1:
    JSR MulS1
    
void MulS1()
{
    int8_t b = pop1();
    int8_t a = pop1();
    push1(a * b);
}
    
    
    
    Increment/Decrement Operations
    ==============================
    Increment and decrement ops expect a ref on TOS which is popped. The "pre" 
    versions will load the value, increment or decrement, store the new value 
    and push that value. The "post" versions will load the value, push that value, 
    then increment or decrement and store the result. There are Int and Float 
    versions for all. The end result is one value left on the stack, which is 
    either incremented or decremented or not and one value at the location of the 
    ref which is either incremented or decremented.


PreInc1:
        Dup2
        Dup2
        PushDeref1
        Inc1
        PopDeref1
        PushDeref1

PreInc1: (X has Ref)
        LDA ,X
        INCA
        STA ,X

PreInc2: (X has Ref)
        LDD ,X
        ADDD #1
        STD ,X

    Executable format
    
    Format Id           - 4 bytes: 'lucd'
    Class proto psize   - 2 bytes, size in 16 bit entries of class prototype offsets
    Class proto table   - psize entries of 2 bytes each, absolute offset of each class
    Class protos        - list of class prototypes. Each prototype consists of:
    
                            Class size      - 2 bytes, size of a class instance
                            Constant size   - 2 bytes, size of constants
                            Constants       - list of each constant in the prototype
                            Function table  - array of indexes to each function
                                              Each function consists of:
                                              
                                                  Local size    - 2 bytes, size needed for params and locals
                                                  Code          executable code for function
*/

static constexpr uint16_t MaxIdSize = 4096;
static constexpr uint16_t ConstStart = 0x00;
static constexpr uint16_t ConstSize = 2048; // Max possible size
static constexpr uint16_t GlobalStart = ConstStart + ConstSize;
static constexpr uint16_t GlobalSize = 1024;
static constexpr uint16_t LocalStart = GlobalStart + GlobalSize;
static constexpr uint16_t LocalSize = MaxIdSize - LocalStart;
static constexpr uint8_t ExtOpcodeStart = 0x40;

enum class Op: uint8_t {

    None            = 0x00,

    PushIntConst    = 0x01,
    PushDeref       = 0x02,
    PopDeref        = 0x03,
    
    Dup1            = 0x04,
    Dup2            = 0x05,
    Dup4            = 0x06,
    Drop1           = 0x07,
    Drop2           = 0x08,
    Drop4           = 0x09,
    Swap1           = 0x0a,
    Swap2           = 0x0b,
    Swap4           = 0x0c,
    
    CallNative      = 0x0e,
    Return          = 0x0f,
    
// 0x0c to 0x0f unused

    Or              = 0x10,
    Xor             = 0x11,
    And             = 0x12,
    Not             = 0x13,

    LOr             = 0x14,
    LAnd            = 0x15,
    LNot            = 0x16,

    LTInt           = 0x17,
    LTFloat         = 0x18,
    LEInt           = 0x19,
    LEFloat         = 0x1a,
    EQInt           = 0x1b,
    EQFloat         = 0x1c,
    NEInt           = 0x1d,
    NEFloat         = 0x1e,
    GEInt           = 0x1f,
    GEFloat         = 0x20,
    GTInt           = 0x21,
    GTFloat         = 0x22,

    AddInt          = 0x23,
    AddFloat        = 0x24,
    SubInt          = 0x25,
    SubFloat        = 0x26,
    MulInt          = 0x27,
    MulFloat        = 0x28,
    DivInt          = 0x29,
    DivFloat        = 0x2a,

    NegInt          = 0x2b,
    NegFloat        = 0x2c,
    
    PreIncInt       = 0x2d,
    PreIncFloat     = 0x2e,
    PreDecInt       = 0x2f,
    PreDecFloat     = 0x30,
    PostIncInt      = 0x31,
    PostIncFloat    = 0x32,
    PostDecInt      = 0x33,
    PostDecFloat    = 0x34,
    
    // 0x40 - 0xf0 ops use lower 4 bits for data value

    PushRef         = ExtOpcodeStart + 0x00,
    Push            = ExtOpcodeStart + 0x10,
    Pop             = ExtOpcodeStart + 0x20,

    Call            = ExtOpcodeStart + 0x30,
    
    Offset          = ExtOpcodeStart + 0x40,
    Index           = ExtOpcodeStart + 0x50,
    PushIntConstS   = ExtOpcodeStart + 0x60,
    Log             = ExtOpcodeStart + 0x70,
    SetFrameS       = ExtOpcodeStart + 0x80,    // p and l are each 2 bits (0-3)
    Jump            = ExtOpcodeStart + 0x90,
    If              = ExtOpcodeStart + 0xa0,
};

enum class OpParams : uint8_t {
    None,       // No params
    Id,         // b+1 = <id>
    I,          // b+1[3:0] = <int> (0-3)
    Index,      // b[3:0] = <int> (0-15)
    Const,      // b+1 = 0-255
    AbsTarg,    // Lower 4 bits of opcode (bits 11:8) | byte after opcode
                // (bits 7:0). 12 bit absolute address (0 to 4095).
    RelTarg,    // Lower 4 bits of opcode (bits 11:8) | byte after opcode
                // (bits 7:0). 12 bit relative address (-2048 to 2047).
    P_L,        // b[3:0] = num params (0-15), b+1 = num locals (0-255)
    Idx_Len_S, // b[3:0] = <int> (0-15), b+1 = <int>, followed by Sz string bytes
};

}
