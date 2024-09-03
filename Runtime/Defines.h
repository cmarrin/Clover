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
#include <stdlib.h>

#ifdef ARDUINO
#include <Arduino.h>
#include <EEPROM.h>
#else
#include <cstdio>
#include <memory>
#endif

namespace lucid {

#ifdef ARDUINO
    static inline uint8_t rom(uint16_t addr) { return EEPROM.read(addr); }
    
    static inline void putChar(uint8_t c) { Serial.print(char(c)); }

    static inline void toString(char* s, float val, int8_t width = 0, uint8_t precision = 0)
    {
        dtostrf(val, width, precision, s);
    }
#else
    extern uint8_t* ROMBase;

    static inline uint8_t rom(uint16_t addr) { return ROMBase[addr]; }

    static inline void putChar(uint8_t c) { ::putchar(c); }
    
    static inline void toString(char* s, float val, int8_t width = 0, uint8_t precision = 0)
    {
        snprintf(s, 20, "%*.*g", width, precision, val);
    }

    static inline void randomSeed(uint32_t s) { srand(s); }
    static inline int32_t random(int32_t min, int32_t max)
    {
        if (min >= max) {
            return max;
        }
        int r = rand() % (max - min);
        return r + min;
    }
#endif

static inline float intToFloat(uint32_t i)
{
    return *(reinterpret_cast<float*>(&i));
}

static inline uint32_t floatToInt(float f)
{
    return *(reinterpret_cast<int32_t*>(&f));
}

enum class Index : uint8_t { C = 0x00, M = 0x01, A = 0x02, L = 0x03 };
enum class OpSize : uint8_t { i8 = 0, i16 = 1, i32 = 2, flt = 3 };

// Built-in types are 0x00 to EnumTypeStart-1, enum types are EnumTypeStart
// to StructTypeStart-1. Struct types are StructTypeStart-0xff
// Order the types so scalar types are all together

constexpr uint8_t EnumTypeStart = 0x20; // Where enum types start
constexpr uint8_t StructTypeStart = 0x80; // Where struct types start

enum class Type : uint8_t {
    None = 0,
    
    Float  = 0x08,
    Int8   = 0x09,
    UInt8  = 0x0a,
    Int16  = 0x0b,
    UInt16 = 0x0c,
    Int32  = 0x0d,
    UInt32 = 0x0e,
    
    String   = 0x10,
    Function = 0x11,
};

static constexpr bool isScalar(Type t) { return t >= Type::Float && t <= Type::UInt32; }
static constexpr bool isInteger(Type t) { return t >= Type::Int8 && t <= Type::UInt32; }
static constexpr bool isEnum(Type t) { return uint8_t(t) >= EnumTypeStart && uint8_t(t) < StructTypeStart; }
static constexpr bool isStruct(Type t) { return uint8_t(t) >= StructTypeStart; }

static constexpr uint8_t opSizeToBytes(OpSize opSize)
{
    return (opSize == OpSize::i8) ? 1 : ((opSize == OpSize::i16) ? 2 : 4);
}

static constexpr OpSize typeToOpSize(Type type)
{
    if (isEnum(type)) {
        // FIXME: For now Enums are always 1 byte
        return OpSize::i8;
    }
    return (type == Type::Int8 || type == Type::UInt8) ? OpSize::i8 : ((type == Type::Int16 || type == Type::UInt16) ? OpSize::i16 : OpSize::i32);
};

// Defines for size of addresses
using AddrNativeType = uint32_t;
constexpr Type AddrType = Type::UInt32;
constexpr OpSize AddrOpSize = typeToOpSize(AddrType);
constexpr uint8_t AddrSize = opSizeToBytes(AddrOpSize);

static inline uint8_t typeToBytes(Type type)
{
    switch (type) {
        case Type::Int8     : return 1;
        case Type::UInt8    : return 1;
        case Type::Int16    : return 2;
        case Type::UInt16   : return 2;
        case Type::Int32    : return 4;
        case Type::UInt32   : return 4;
        case Type::Float    : return 4;
        case Type::String   : return AddrSize;
        default:
            if (isStruct(type)) {
                return AddrSize;
            }
            if (isEnum(type)) {
                // FIXME: For now Enums are always 1 byte
                return 1;
            }
            return 0;
    }
};

static inline uint8_t typeToSizeBits(Type type)
{
    switch (type) {
        case Type::Int8     : return 0x00;
        case Type::UInt8    : return 0x00;
        case Type::Int16    : return 0x01;
        case Type::UInt16   : return 0x01;
        case Type::Int32    : return 0x02;
        case Type::UInt32   : return 0x02;
        case Type::Float    : return 0x03;
        default: return 0;
    }
};

/*
    Executable file format
    
        Bytes       Description
        
        0-3     : Signature - 'lucd'
        4-7     : Entry point of 'main' function, if any (0 if not)
        8-11    : Location of constructor function of top level struct
        12-15   : Bytes of storage needed for top-level struct
        16-17   : Size of constants in bytes
        18-n    : Bytes of constant structs and arrays
        n-<end> : Executable code
 */
 
constexpr AddrNativeType MainEntryPointAddr = 4;
constexpr AddrNativeType TopLevelCtorEntryPointAddr = 8;
constexpr AddrNativeType TopLevelStructSizeAddr = 12;
constexpr uint16_t ConstStart = 18; // Past signature, entry point and top level size


/*

Machine Code for LucidVM

See README.md for details.

Opcodes:
    Stack is 8 bit and grows down. 16 and 32 bit values are stored in consecutive
    locations in big-endian order (HI to LO).
    
    A stack frame has a U register pointing at the previous U register value. Above
    that is the return address, followed by args. The leftmost arg is pushed
    last and is therefore right above the return address. Args are accessed by
    negative offsets from U. Args are popped by the caller and therefore there
    can always be a variable number of arguments passed. Locals start just below
    the U register pointer and are accessed with positive offsets from U.
    
    Stack machine
    
    Example:
    
    int16 b = 10;
    int16 c = 20;
    int16 a = (b + 5) * (c + 6);
    
    6809:
    
    LDD     #10
    STD     b,U
    LDD     #20
    STD     c,U
    LDD     b,U
    ADDD    #5
    PSHS    D
    LDD     c,U
    ADDD    #6
    PSHS    D
    JSR     [$IMUL16]   // pass 16 bit args on stack, return value on stack but leave args on stack.
    PULS    D
    STD     a,U
    LEAS    4,S
    
    Generic register based (8 regs)
    
    LDK21   r0, #10
    ST2     r0, b,U
    LDK21   r1, #20
    ST2     r1, c,U
    LDK2    r2, #5
    ADD2    r0, r2
    LDK21   r2, #6
    ADD2    r1, r2
    IMUL2   r0, r1
    ST2     r0, a,U

    All addresses can be 2 bytes for a 64KB range or 4 bytes for a 2^32 byte range.
    This is determined at compile time. Also at compile time you can specify
    float support (16 or 32 bit) or not and max integer width (16 or 32 bits).
    
    - Address mode
        The byte following the opcode indicates the addressing mode. Bits1:0 indicate
        how the addressing value is used. See below for value of unsigned <addr>:
                        
            00 - Constant
            01 - M. EA = Y + <addr>
            10 - A. EA = U + <addr>
            11 - L. EA = U - <addr> - 1
                        
        if bit 2 is 0 then bits 7:3 are an unsigned offset from 0 to 31. If bit 2 is 1
        and bit 3 is 0, bits 7:4 are prepended to a following byte for a 12 bit
        unsigned address (0 to 4095). If bit 3 is 1 and bit 4 is 0 then the next 2 bytes
        is an unsigned address. If bit 4 is 1 then the next 4 bytes is an unsigned address.

    - Immediate value
        LDK<width><size> loads an immediate operand. <width> indicates the number
        of bytes to push (1, 2, or 4) and <size> indicates the number of bytes
        following the opcode (1, 2, 3, or 4). Operand is signed and is sign extended
        to fit in the desired number of bytes to push.

    - Stack frame and Base pointer
    
    Stack grows down so when you push a value the current SP points to it. On a call
    args are pushed from right to left so first param is at the lowest address.
    The caller then pushes the return address and calls the function.
    
    The first instruction of a function must be ENTER. This has a 4 bit (0-15), 1 or
    2 byte operand which is the number of bytes of local storage needed. The ENTER
    instruction pushes the current U reg, sets U = SP and subtracts the number of
    local bytes from SP. When indexing from BP, positive offsets address the args.
    Locals are addressed with negative offsets.
    
    On return, the callee sets SP = U, pops the stack into U and performs a
    return operation. The caller then adds the number of bytes of args to SP.
*/

enum class Op: uint8_t {
    NOP     = 0x00,

    // Load/store - bits 1:0 of opcode are Opsize of operation.
    // Next byte: 7:5 - offset mode, 4:3 - index reg, 2:0 - load/store reg
    // Following byte(s) - offset
    //
    // offset mode: 0 - 6   : byte offset
    //              7       : see next byte, if bit 7 = 0, byte is offset (0 - 127)
    //                      : if bit 7 = 1, append next byte to bits 6:0 for a 15 bit offset (0 - 32768)
    LD      = 0x04, // Load value from addr mode address
    ST      = 0x08, // Store value at addr mode address
    LEA     = 0x0c, // Load addr mode address (ignore bits 1:0 of opcode)
    LDX     = 0x10, // Load value at address in reg
    STX     = 0x14, // Store value at address in reg
    LDK     = 0x18, // Load constant - bits 1:0 of opcode are opsize of bytes to load.
                    // Next byte, bits 4:3 number of bytes following with signed value, bits 2:0 are destination reg
    LDSK    = 0x1c, // Load constant - bits 1:0 of opcode are opsize of bytes to load.
                    // Next byte, bits 7:3 signed value (-16 to 15), bits 2:0 are destination reg
                    
    PUSH    = 0x20, // Push value onto stack - bits 1:0 of opcode are opsize of bytes to load.
                    // Next byte bits 2:0 are source reg
    POP     = 0x24, // Pop value from stack - bits 1:0 of opcode are opsize of bytes to load.
                    // Next byte bits 2:0 are destination reg

    // Binary ops - bits 1:0 of opcode are Opsize of operation.
    // Next byte: 5:3 - register a, 2:0 - register b. Result is in register a
    ADD     = 0x28,
    SUB     = 0x2c,
    IMUL    = 0x30,
    UMUL    = 0x34,
    IDIV    = 0x38,
    UDIV    = 0x3c,
    
    AND     = 0x40,
    OR      = 0x44,
    XOR     = 0x48,

    // Unary ops - bits 1:0 of opcode are Opsize of operation.
    // Next byte: 2:0 - register a. Result is in register a
    NOT     = 0x4c,
    NEG     = 0x50,

    // Conditional branches. Bit 0 of opcode indicates that the next
    // byte (bit 0 = 0) or 2 bytes (bit 0 = 1) contain a signed
    // relative offset from the start of the next instruction.
    LE      = 0x54,
    LS      = 0x58,
    LT      = 0x5c,
    LO      = 0x60,
    GE      = 0x64,
    HS      = 0x68,
    GT      = 0x6c,
    HI      = 0x70,
    EQ      = 0x74,
    NE      = 0x78,
    
    BR      = 0x7c, // Operand:
                    //      00 - branch back always
                    //      01 - branch forward always
                    //      10 - branch forward if reg true
                    //      11 - branch forward if reg false

    SWITCH  = 0x80, // Following opcode is an 8 or 16 bit operand followed by a table.
                    // Bits 1:0 of opcode is OpSize of values in table. Bit 7 of first
                    // operand byte is addr size in table, 0 - 1 byte, 1 - 2 bytes. If
                    // Bit 6 of the operand is 0 then bits 5:0 are the number of entries
                    // in the table (0 - 63 entries). If bit 6 is 1 then there is a second
                    // operand byte bits 5:0 are prepended to this byte to give a 14 bit
                    // number of entried (0 - 16383). Immediately following the table is
                    // the code for the default clause. If there is none then this is a
                    // BRA to the end of the clauses.

    ENTER   = 0x84, // Enter function (adjust BP). Bit 0 is 0 for 1 byte operand, 1 for 2 byte
    CALL    = 0x88, // Call function. Bits 1:0 ignored, always 16 bit absolute operand
    NCALL   = 0x8c, // Call native function. Bit 0 is 0 for 1 byte operand, 1 for 2 byte
    RET     = 0x90, // Return from function (adjust BP).
// 5

    // Bits 1:0 are concatenated with 5:3 bits from next byte to give 32 type conversions
    // Bits 2:0 from next byte is register
    // Conversions:
    //
    //      FI32,  FI16,  FI8,  FU32, FU16, FU8 (negative float to unsigned return 0)
    //      I32F,  U32F,  I16F, U16F, I8F,  U8F (widening cast need signed/unsigned versions for sign ext.
    //      I1632, U1632, I832, U832, I816, U816
    CAST    = 0x94,

//
// available opcodes 98 - ff
};

static inline Op castOp(Type from, Type to)
{
    // FIXME: For now assume enum is 1 byte
    if (isEnum(from)) {
        from = Type::UInt8;
    }
    
    // Cast opcode are sparse, only the ones needed exist
    // return NOP for all the rest.
    if (from == Type::Float) {
        switch (to) {
            case Type::Int8:
            case Type::UInt8: return Op::CASTF8;
            case Type::Int16:
            case Type::UInt16: return Op::CASTF16;
            case Type::Int32:
            case Type::UInt32: return Op::CASTF32;
            default: return Op::NOP;
        }
    }
    
    if (from == Type::Int32 || from == Type::UInt32) {
        switch (to) {
            case Type::Float: return Op::CAST32F;
            case Type::Int16:
            case Type::UInt16: return Op::CAST3216;
            case Type::Int8:
            case Type::UInt8: return Op::CAST328;
            default: return Op::NOP;
        }
    }
    
    if (from == Type::UInt16) {
        switch (to) {
            case Type::Float: return Op::CASTU16F;
            case Type::Int32:
            case Type::UInt32: return Op::CASTU1632;
            case Type::Int8:
            case Type::UInt8: return Op::CAST168;
            default: return Op::NOP;
        }
    }
    
    if (from == Type::Int16) {
        switch (to) {
            case Type::Float: return Op::CASTI16F;
            case Type::Int32:
            case Type::UInt32: return Op::CASTI1632;
            case Type::Int8:
            case Type::UInt8: return Op::CAST168;
            default: return Op::NOP;
        }
    }

    if (from == Type::UInt8) {
        switch (to) {
            case Type::Float: return Op::CASTU8F;
            case Type::Int32:
            case Type::UInt32: return Op::CASTU832;
            case Type::Int16:
            case Type::UInt16: return Op::CASTU816;
            default: return Op::NOP;
        }
    }
    
    if (from == Type::Int8) {
        switch (to) {
            case Type::Float: return Op::CASTI8F;
            case Type::Int32:
            case Type::UInt32: return Op::CASTI832;
            case Type::Int16:
            case Type::UInt16: return Op::CASTI816;
            default: return Op::NOP;
        }
    }
    
    return Op::NOP;
}

static inline int32_t sex(int32_t& v, OpSize opSize)
{
    switch (opSize) {
        case OpSize::i32:
        case OpSize::flt:
            break;
        case OpSize::i16:
            if (v & 0x8000) {
                v |= 0xffff8000;
            }
            break;
        case OpSize::i8 :
            if (v & 0x80) {
                v |= 0xffffff80;
            }
            break;
    }
    return v;
}

enum class Reserved {
    None,
    Struct,
    Enum,
    Const,
    Import,
    As,
    Function,
    Return,
    Break,
    Continue,
    End,
    Loop,
    While,
    For,
    If,
    Else,
    Switch,
    Case,
    Default,
    Float,
    Fixed,
    True,
    False,
    Int8,
    UInt8,
    Int16,
    UInt16,
    Int32,
    UInt32,
    Bool,
};

// These must match the values for Token
enum class Operator : uint8_t {
    Equal   = '=',
    AddSto  = 0xa0,
    SubSto  = 0xa1,
    MulSto  = 0xa2,
    DivSto  = 0xa3,
    ModSto  = 0xa4,
    AndSto  = 0xa5,
    OrSto   = 0xa6,
    XorSto  = 0xa7,
    LOr     = 0xa8,
    LAnd    = 0xa9,
    Or      = '|',
    Xor     = '^',
    And     = '&',
    EQ      = 0xab,
    NE      = 0xac,
    LT      = '<',
    GT      = '>',
    GE      = 0xad,
    LE      = 0xaa,
    Plus    = '+',
    Minus   = '-',
    Mul     = '*',
    Div     = '/',
    Mod     = '%',
    Inc     = 0xae,
    Dec     = 0xaf,
    BNot    = '~',
    LNot    = '!',
    ArrIdx  = '[',
    Dot     = '.',
    
    // '&', '*' and '-' are used as unary operators. Represent those here.
    AddrOf  = 0xe0,
    Deref   = 0xe1,
    UMinus  = 0xe2,
};

class VarArg;

int32_t printf(AddrNativeType fmt, VarArg&);    
int32_t printf(const char* fmt, ...);
int32_t vprintf(const char* fmt, va_list args);

// Native Modules contain native functions that can be called
// by the interpreter. Modules are numbered starting at 0. There
// is a 'core' module always available as module 0. Others are
// loaded with an 'import' statement and are numbered starting
// at 1. The interpreter has a fixed array of modules which
// contain a CallNative function to execute the code. A NCALL
// opcode is used to incate the module function to call. The
// opcode has am 8 or 16 bit operand which encodes which module
// has the function and the id of the function.
//
// The number of bits used for the module index and function id
// as well as the max number of modules that can be loaded is
// contained in constants below.

class InterpreterBase;

using CallNative = void (*)(uint16_t id, InterpreterBase*);

static constexpr uint8_t ModuleCountMax     = 10;
static constexpr uint8_t BitsPerFunctionId  = 5;
static constexpr uint8_t FunctionIdMask     = (1 << BitsPerFunctionId) - 1;

#ifndef ARDUINO
class ASTNode;
using ASTPtr = std::shared_ptr<ASTNode>;
using ASTNodeList = std::vector<ASTPtr>;

class Symbol;
using SymbolPtr = std::shared_ptr<Symbol>;
using SymbolList = std::vector<Symbol>;

class Function;
using FunctionPtr = std::shared_ptr<Function>;
using FunctionList = std::vector<FunctionPtr>;

class Struct;
using StructPtr = std::shared_ptr<Struct>;
using StructList = std::vector<StructPtr>;

class Enum;
using EnumPtr = std::shared_ptr<Enum>;
using EnumList = std::vector<EnumPtr>;

#endif

}
