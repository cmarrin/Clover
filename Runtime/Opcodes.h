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
        int16   - Byte after opcode. Int constant (-128 to 127)
        uint16  - Byte after opcode. Int constant (0 to 255)
        int32   - Byte after opcode. Int constant (-128 to 127)
        uint32  - Byte after opcode. Int constant (0 to 255)
        float   - 4 bytes after opcode. Floating point number in Float class format
        str     - Byte after opcode is length, followed by length bytes
        
        ref     - Reference to a value on the stack or a constant.

        p       - Byte after opcode. Num params passed to function.
        l       - Byte after opcode. Num locals in function.

    Load and store values on the stack
    ==================================
    
    PushRef uint8           - stack[sp++] = uint8
    PushDeref               - tmp = stack[--sp], stack[sp++] = mem[tmp]
    PopDeref                - tmp = stack[--sp], mem[stack[--sp]] = tmp
    
    PushC uint16            - stack[sp++] = executable[uint16]
    PushF uint16            - stack[sp++] = bp[uint16]
    PopF uint16             - bp = stack[--sp]
    

    Stack manuipulation
    ===================
    
    Dup                     - stack[sp++] = stack[sp]
    Drop                    - --sp
    Swap                    - tmp = stack[sp]; stack[sp] = stack[sp-1]; stack[sp-1[ = tmp;


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

    Not                     - stack[sp++] = ~stack[--sp] (assumes int32_t)
    LNot                    - stack[sp++] = !stack[--sp] (assumes int32_t)
    NegI                    - stack[sp++] = -stack[--sp] (assumes int32_t, result is int32_t)
    NegF                    - stack[sp++] = -stack[--sp] (assumes float, result is float)


    Binary Operations - expect 2 values on stack (a = tos-1, b = tos), pop before operation
    =================

    Or                      - stack[sp++] = a | b (assumes int32_t)
    XOr                     - stack[sp++] = a ^ b (assumes int32_t)
    And                     - stack[sp++] = a & b (assumes int32_t)
    
    LTI                     - stack[sp++] = a < b (assumes int32_t, result is int32_t)
    LTF                     - stack[sp++] = a < b (assumes float, result is int32_t)
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
    
    AddI                    - stack[sp++] = a + b (assumes int32_t, result is int32_t)
    AddF                    - stack[sp++] = a + b (assumes float, result is float)
    SubI                    - stack[sp++] = a - b (assumes int32_t, result is int32_t)
    SubF                    - stack[sp++] = a - b (assumes float, result is float)
    MulI                    - stack[sp++] = a * b (assumes int32_t, result is int32_t)
    MulF                    - stack[sp++] = a * b (assumes float, result is float)
    DivI                    - stack[sp++] = a / b (assumes int32_t, result is int32_t)
    DivF                    - stack[sp++] = a / b (assumes float, result is float)
    ModI                    - stack[sp++] = a % b (assumes int32_t, result is int32_t)
    ModF                    - stack[sp++] = a % b (assumes float, result is float)


    Increment/Decrement Operations
    ==============================
    Increment and decrement ops expect a ref on TOS which is popped. The "pre" 
    versions will load the value, increment or decrement, store the new value 
    and push that value. The "post" versions will load the value, push that value, 
    then increment or decrement and store the result. There are Int and Float 
    versions for all. The end result is one value left on the stack, which is 
    either incremented or decremented or not and one value at the location of the 
    ref which is either incremented or decremented.
     
    PreIncI
    PreIncF
    PreDecI
    PreDecF
    PostIncI
    PostIncF
    PostDecI
    PostDecF
    
    
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
    
    Dup             = 0x04,
    Drop            = 0x05,
    Swap            = 0x06,
    
    CallNative      = 0x0a,
    Return          = 0x0b,
    
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
    SetFrame        = ExtOpcodeStart + 0x80,
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
