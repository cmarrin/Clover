/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
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


//#define ADDR32          // Uncomment for 32 bit addr, leave commented out for 16 bit
//#define SUPPORT_FLOAT   // Uncomment for float support, leave commented out for no floats
//#define SUPPORT_INT32   // Uncomment for int32/uint32 support, leave commented out for no support

namespace clvr {

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

// Defines for size of addresses
#ifdef ADDR32
using AddrNativeType = uint32_t;
using RelAddrNativeType = uint32_t;
constexpr Type AddrType = Type::UInt32;
constexpr Type RelAddrType = Type::Int32;
constexpr bool Is32BitAddr = true;
constexpr OpSize AddrOpSize = OpSize::i32;
constexpr uint8_t AddrSize = sizeof(AddrNativeType);
#else
using AddrNativeType = uint16_t;
using RelAddrNativeType = int16_t;
constexpr Type AddrType = Type::UInt16;
constexpr Type RelAddrType = Type::Int16;
constexpr bool Is32BitAddr = false;
constexpr OpSize AddrOpSize = OpSize::i16;
constexpr uint8_t AddrSize = sizeof(AddrNativeType);
#endif

#ifdef SUPPORT_INT32
using ArgUNativeType = uint32_t;
using ArgINativeType = int32_t;
constexpr Type ArgUType = Type::UInt32;
constexpr Type ArgIType = Type::Int32;
#else
using ArgUNativeType = uint16_t;
using ArgINativeType = int16_t;
constexpr Type ArgUType = Type::UInt16;
constexpr Type ArgIType = Type::Int16;
#endif

static constexpr uint8_t opSizeToBytes(OpSize opSize)
{
    return (opSize == OpSize::i8) ? 1 : ((opSize == OpSize::i16) ? 2 : 4);
}

static inline OpSize typeToOpSize(Type type)
{
    if (isEnum(type)) {
        // FIXME: For now Enums are always 1 byte
        return OpSize::i8;
    }
    if (type == Type::String) {
        return AddrOpSize;
    }
    return (type == Type::Int8 || type == Type::UInt8) ? OpSize::i8 : ((type == Type::Int16 || type == Type::UInt16) ? OpSize::i16 : OpSize::i32);
};

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
    
        Bytes (16 bit addr)     Bytes (32 bit addr)     Description
        
        0-3                     0-3                     : Signature - 'lucd'
        4-5                     4-5                     : Machine code format - major
        6                       6                       ; Machine code format - minor
        7                       7                       ; Address size (0 - 16 bit, 1 - 32 bit)
        8-9                     8-11                    : Entry point of 'main' function, if any (0 if not)
        10-11                   12-15                   : Location of constructor function of top level struct
        12-13                   16-19                   : Bytes of storage needed for top-level struct
        14-15                   20-21                   : Size of constants in bytes
        16-n                    22-n                    : Bytes of constant structs and arrays
        n-<end>                 n-<end>                 : Executable code
 */
 
constexpr AddrNativeType MajorVersionAddr = 4;
constexpr AddrNativeType MinorVersionAddr = 6;
constexpr AddrNativeType Is32BitAddrAddr = 7;
constexpr AddrNativeType MainEntryPointAddr = 8;

#ifdef ADDR32
constexpr AddrNativeType TopLevelCtorEntryPointAddr = 12;
constexpr AddrNativeType TopLevelStructSizeAddr = 16;
constexpr uint16_t ConstStart = 22;
#else
constexpr AddrNativeType TopLevelCtorEntryPointAddr = 10;
constexpr AddrNativeType TopLevelStructSizeAddr = 12;
constexpr uint16_t ConstStart = 16;
#endif

/*

Machine Code for Clover

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
    
    PUSHK<2>    #10
    POP<2>      b,L
    PUSHK<2>    #20
    POP<2>      c,L
    PUSH<2>     b,L
    PUSHK<2>    #5
    ADD<2>
    PUSH<2>     c,L
    PUSHK<2>    #6
    ADD<2>
    MUL<2>
    POP         a,L
    
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
        PUSHK<width><size> pushes an immediate operand. <width> indicates the number
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

// 0 bit opcodes start at 0x00
static constexpr uint8_t OneBitOperandStart  = 0x50;
static constexpr uint8_t TwoBitOperandStart  = 0x5c;
static constexpr uint8_t FoutBitOperandStart = 0xb0;

enum class Op: uint8_t {
    NOP     = 0x00,
    PUSHS   = 0x01,
    PUSHK11 = 0x02, // 1 byte operand, push 1 byte
    PUSHK12 = 0x03, // 1 byte operand, push 2 bytes
    PUSHK14 = 0x04, // 1 byte operand, push 4 bytes
    PUSHK22 = 0x05, // 2 byte operand, push 2 bytes
    PUSHK24 = 0x06, // 2 byte operand, push 4 bytes
    PUSHK44 = 0x07, // 4 byte operand, push 4 bytes
    
    DROP1   = 0x08, // Next byte is count (1 - 256)
    DROP2   = 0x09, // Next 2 bytes are count (1 - 65536)
    
    LNOT    = 0x0a,
    
    CALL    = 0x0b, // Absolute address of callee (16 bit)
    MCALL   = 0x0c, // Call a member function. TOS has struct instance address that must be put in the Y register
    INDEX1  = 0x0d, // Stack has a ref and 8 bit index. Operand is element size in bytes, push new ref offset by index * operand
    INDEX2  = 0x0e, // Stack has a ref and 16 bit index. Operand is element size in bytes, push new ref offset by index * operand
    PUSHREF = 0x0f, // Next byte is addr mode. Data width is used when computing negative offsets from U
    
    // These must have their '1', '2, '4' forms sequential in that order.
    // When generating code, the '1' form is passed in and we add one to
    // the opcode if we need the '2' form and add two if we need the '4'
    // form
    DEREF1  = 0x10, // TOS has ref, pop it, load the value at that address and push it
    DEREF2  = 0x11,
    DEREF4  = 0x12,
    POP1    = 0x13, // Next byte is addr mode, pop TOS and store at address
    POP2    = 0x14,
    POP4    = 0x15,
    POPDEREF1=0x16, // a = popaddr, v = pop1, mem1[a] = v
    POPDEREF2=0x17, // a = popaddr, v = pop2, mem2[a] = v
    POPDEREF4=0x18, // a = popaddr, v = pop4, mem4[a] = v
    PUSH1   = 0x19, // Next byte is addr mode, push value at addr
    PUSH2   = 0x1a,
    PUSH4   = 0x1b,

    DUP1    = 0x1c,
    DUP2    = 0x1d,
    DUP4    = 0x1e,
    
    PUSHR1  = 0x1f, // Push _returnValue (1 byte)
    PUSHR2  = 0x20, // Push _returnValue (2 bytes)
    PUSHR4  = 0x21, // Push _returnValue (4 bytes)

    SWITCH  = 0x22, // Following opcode is a 16 bit operand. Then there is a list of pairs: <value> (1-4 bytes) and <addr>
                    // (1 or 2 bytes). Bits 1:0 are value width (0 = 1 byte, 1 = 2 bytes, 2 = 4 bytes). This matches the 
                    // OpSize format. Bit 2 is addr size (0 = 1 byte, 1 = 2 bytes). Bits 15:3 is number of enties in list
                    // (0 - 8191 entries). Immediately following the entries is the code for the default clause. If
                    // there is none then this is a BRA to the end of the clauses.

    // Cast operators are sparse. For narrowing cast you don't
    // need to worry about sign.
    CASTF8  = 0x23,
    CASTF16 = 0x24,
    CASTF32 = 0x25,
    
    CAST32F = 0x26,
    CAST3216= 0x27,
    CAST328 = 0x28,
    
    CAST168 = 0x29,
    
    // For widening casts you need signed and unsigned
    // versions so you know when to sign extend
    CASTU16F= 0x2a,
    CASTU1632=0x2b,
    
    CASTI16F= 0x2c,
    CASTI1632=0x2d,

    CASTU8F = 0x2e,
    CASTU832= 0x2f,
    CASTU816= 0x30,

    CASTI8F = 0x31,
    CASTI832= 0x32,
    CASTI816= 0x33,
    
    RET     = 0x34, // Return without popping anything from the stack
    RETR1   = 0x35, // Return after popping TOS into _returnValue
    RETR2   = 0x36, // Return after popping TOS into _returnValue
    RETR4   = 0x37, // Return after popping TOS into _returnValue

    OFFSET1 = 0x38, // 8 bit ubnsigned offset added to ref on TOS
    OFFSET2 = 0x39, // 16 bit ubnsigned offset added to ref on TOS

    // These must have their '1', '2, '4' forms sequential in that order.
    // When generating code, the '1' form is passed in and we add one to
    // the opcode if we need the '2' form and add two if we need the '4'
    // form
    AND1    = 0x3a,
    AND2    = 0x3b,
    AND4    = 0x3c,
    OR1     = 0x3d,
    OR2     = 0x3e,
    OR4     = 0x3f,
    XOR1    = 0x40,
    XOR2    = 0x41,
    XOR4    = 0x42,
    NOT1    = 0x43,
    NOT2    = 0x44,
    NOT4    = 0x45,
    SHR1    = 0x46,
    SHR2    = 0x47,
    SHR4    = 0x48,
    ASR1    = 0x49,
    ASR2    = 0x4a,
    ASR4    = 0x4b,
    SHL1    = 0x4c,
    SHL2    = 0x4d,
    SHL4    = 0x4e,
    
//
//
// Available opcodes 4f
//
//

// Bit 0 is 0 if the operand is a 8 bits and 1 if 16 bits.
// Operand is sign extended
// This limits branches to the range -32768 to 32767.
// What happens if we go over that? do we fail or have some
// kind of trampoline support?
    BRF     = 0x50, // Branch if TOS is false, branch is always forward
    BRT     = 0x52, // Branch if TOS is true, branch is always forward
    FBRA    = 0x54, // Branch is always forward
    RBRA    = 0x56, // Branch is always reverse
    NCALL   = 0x58,
    ENTER   = 0x5a,

// Bits 1:0 is the width of the data: 00 - 1 byte, 01 - 2 bytes, 10 - 4 bytes, 11 float

    ADD     = 0x5c,
    SUB     = 0x60,
    IMUL    = 0x64,
    UMUL    = 0x68,
    IDIV    = 0x6c,
    UDIV    = 0x70,
    
    NEG     = 0x74,

    PREINC  = 0x78, // Next byte is addr mode
    PREDEC  = 0x7c, // Next byte is addr mode
    POSTINC = 0x80, // Next byte is addr mode
    POSTDEC = 0x84, // Next byte is addr mode
    
    LE      = 0x88,
    LS      = 0x8c,
    LT      = 0x90,
    LO      = 0x94,
    GE      = 0x98,
    HS      = 0x9c,
    GT      = 0xa0,
    HI      = 0xa4,
    EQ      = 0xa8,
    NE      = 0xac,

// These versions use the lower 4 bits of the opcode as a param (0-15)
    PUSHKS1 = 0xb0, // lower 4 bits is value from -8 to 7, push 1 byte
    PUSHKS2 = 0xc0, // lower 4 bits is value from -8 to 7, push 2 bytes
    PUSHKS4 = 0xd0, // lower 4 bits is value from -8 to 7, push 4 bytes
    DROPS   = 0xe0, // lower 4 bits is bytes to drop from 1 to 16
    ENTERS  = 0xf0,
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
    SHR     = 0xb0,
    SHL     = 0xb1,
    
    // '&', '*' and '-' are used as unary operators. Represent those here.
    AddrOf  = 0xe0,
    Deref   = 0xe1,
    UMinus  = 0xe2,
};

class VarArg;

int32_t printf(AddrNativeType fmt, VarArg&);    
int32_t format(AddrNativeType s, uint16_t n, AddrNativeType fmt, VarArg&);    
int32_t printf(const char* fmt, ...);
int32_t format(char* s, uint16_t n, const char* fmt, ...);
int32_t vprintf(const char* fmt, va_list args);
int32_t vformat(char* s, uint16_t n, const char* fmt, va_list args);
int32_t vformat(std::vector<uint8_t>&, const char* fmt, va_list args);

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
