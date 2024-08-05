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

    Stack is 8 bit and grows down. 16 and 32 bit values are stored in consecutive
    locations (HI to LO).
    
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
    
    PUSHREF   b,U
    PUSHK<2>  #10
    DEREF<2>
    PUSHREF   c,U
    PUSHK<2>  #20
    DEREF<2>
    PUSHREF   a,U
    PUSH<2>   b,U
    PUSHK<2>  #5
    ADD<2>
    PUSH<2>   c,BP
    PUSHK<2>  #6
    ADD<2>
    MUL<2>
    DEREF<2>
    
    All addresses can be 2 bytes for a 64KB range or 4 bytes for a 2^32 byte range.
    This is determined at compile time. Also at compile time you can specify
    float support (16 or 32 bit) or not and max integer width (16 or 32 bits).
    
    - Address mode
        The byte following the opcode indicates the addressing mode. Bits1:0 indicate
        how the addressing value is used:
                        
            00 - Constant
            01 - X. Add signed value to X to get EA
            10 - Y. Add signed value to Y to get EA
            11 - U. Add signed value to U to get EA
                        
        if bit 2 is 0 then bits 7:3 are a signed offset from -16 to 16. If bit 2 is 1
        then bits 4:3 are the number of bytes that follow, from 1 (00) to 4 (11).

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
    
    PUSHREF         - push EA of value (must be X, Y, or U addressing mode)
    DEREF<1,2,4>    - Value is on TOS, EA is TOS+1, store value at EA
    PUSH<1,2,4>     - Push value (indexed from X, y or BP)
    PUSHK<1,2,4>     - Push value (immediate value)
    
    DUP<1,2,4>      - Duplicate TOS
    DROP<1,2,4>     - Pop TOS
    SWAP<1,2,4>     - Exchange TOS and TOS+1
    
    ADD<1,2,4,F>    - Add TOS to TOS+1, result on TOS
    SUB<1,2,4,F>    - Sub TOS+1 from TOS, result on TOS
    UMUL<1,2,4>     - Unsigned multiple TOS+1 and TOS, result on TOS
    IMUL<1,2,4,F>   - Signed multiple TOS+1 and TOS, result on TOS
    UDIV<1,2,4>     - Unsigned divide TOS+1 by TOS, result on TOS
    IMUL<1,2,4,F>   - Signed divide TOS+1 by TOS, result on TOS
    OR<1,2,4>       - Bitwise or TOS and TOS+1, result on TOS
    XOR<1,2,4>      - Bitwise xor TOS and TOS+1, result on TOS
    AND<1,2,4>      - Bitwise and TOS and TOS+1, result on TOS
    
    NOT<1,2,4>      - Bitwise inversion of TOS, result on TOS
    NEG<1,2,4,F>    - Negate TOS, result on TOS

    LE<1,2,4,F>     - Test if signed TOS+1 is less than or equal to signed TOS, bool on TOS
    LS<1,2,4,F>     - Test if unsigned TOS+1 is less than or equal to unsigned TOS, bool on TOS
    LT<1,2,4,F>     - Test if signed TOS+1 is less than signed TOS, bool on TOS
    LO<1,2,4,F>     - Test if unsigned TOS+1 is less than unsigned TOS, bool on TOS
    EQ<1,2,4,F>     - Test if TOS+1 is equal to TOS, bool on TOS
    NE<1,2,4,F>     - Test if TOS+1 is not equal to TOS, bool on TOS
    GT<1,2,4,F>     - Test if signed TOS+1 is greater than signed TOS, bool on TOS
    HI<1,2,4,F>     - Test if unsigned TOS+1 is greater than unsigned TOS, bool on TOS
    GE<1,2,4,F>     - Test if signed TOS+1 is greater than or equal to signed TOS, bool on TOS
    HS<1,2,4,F>     - Test if unsigned TOS+1 is greater than or equal to unsigned TOS, bool on TOS

    IF<S,L>         - IF TOS is true, branch to 8 or 16 bit relative address in follow byte(s)
    BRA<S,L>        - Jump to 8 or 16 bit relative address in follow byte(s)
    CALL<S,L>       - Call function at 8 or 16 bit relative address in follow byte(s), push PC
    CALLNATIVE      - Call native function with id in next byte
    ENTER<I,S,L>    - Do enter operations using local count in lower 4 bits of opcode, or
                      following 8 or 16 bits
    RET             - Do leave operations and pop PC
    
    PREINC<1,2,4,F> - Takes an index byte. Like PUSH, but increments the value at EA and the pushed value
    PREDEC<1,2,4,F> - Takes an index byte. Like PUSH, but decrements the value at EA and the pushed value
    POSTINC<1,2,4,F>- Takes an index byte. Like PUSH, but increments the value at EA and pushes the unincremented value
    POSTDEC<1,2,4,F>- Takes an index byte. Like PUSH, but decrements the value at EA and pushes the undecremented value
*/

static constexpr uint16_t ConstStart = 14; // Past signature, entry point and top level size

// 0 bit opcodes start at 0x00
static constexpr uint8_t OneBitOperandStart  = 0x44;
static constexpr uint8_t TwoBitOperandStart  = 0x4c;
static constexpr uint8_t FoutBitOperandStart = 0xb0;

enum class Op: uint8_t {
    NOP     = 0x00,
    RET     = 0x01,
    PUSHS   = 0x02,
    PUSHK11 = 0x03, // 1 byte operand, push 1 byte
    PUSHK12 = 0x04, // 1 byte operand, push 2 bytes
    PUSHK14 = 0x05, // 1 byte operand, push 4 bytes
    PUSHK22 = 0x06, // 2 byte operand, push 2 bytes
    PUSHK24 = 0x07, // 2 byte operand, push 4 bytes
    PUSHK44 = 0x08, // 4 byte operand, push 4 bytes
    
    DROP1   = 0x09, // Next byte is count (1 - 256)
    DROP2   = 0x0a, // Next 2 bytes are count (1 - 65536)
    
    LAND    = 0x0b,
    LOR     = 0x0c,
    LNOT    = 0x0d,
    
    CALL    = 0x0e, // Absolute address of callee (16 bit)
    INDEX   = 0x0f, // Stack has a ref and index. Operand is 8 bit element size in bytes, push new ref offset by index * operand
    
    DEREF1  = 0x10, // TOS has ref, pop it, load the value at that address and push it
    DEREF2  = 0x11,
    DEREF4  = 0x12,
    POP1    = 0x13, // Next byte is addr mode, pop TOS and store at address
    POP2    = 0x14,
    POP4    = 0x15,
    PUSHREF1= 0x16, // Next byte is addr mode. Data width is used when computing negative offsets from U
    PUSHREF2= 0x17,
    PUSHREF4= 0x18,
    POPDEREF1=0x19, // TOS has value then addr. Store value at addr
    POPDEREF2=0x1a, 
    POPDEREF4=0x1b,
    PUSH1   = 0x1c, // Next byte is addr mode, push value at addr
    PUSH2   = 0x1d,
    PUSH4   = 0x1e,

    DUP1    = 0x1f,
    DUP2    = 0x20,
    DUP4    = 0x21,
    SWAP1   = 0x22,
    SWAP2   = 0x23,
    SWAP4   = 0x24,

    PUSHR1  = 0x25, // Push _returnValue (1 byte)
    PUSHR2  = 0x26, // Push _returnValue (2 bytes)
    PUSHR4  = 0x27, // Push _returnValue (4 bytes)

    // Cast operators are sparse. For narrowing cast you don't
    // need to worry about sign.
    CASTF8  = 0x28,
    CASTF16 = 0x29,
    CASTF32 = 0x2a,
    
    CAST32F = 0x2b,
    CAST3216= 0x2c,
    CAST328 = 0x2d,
    
    CAST168 = 0x2e,
    
    // For widening casts you need signed and unsigned
    // versions so you know when to sign extend
    CASTU16F= 0x2f,
    CASTU1632=0x30,
    
    CASTI16F= 0x31,
    CASTI1632=0x32,
    
    CASTU8F = 0x33,
    CASTU832= 0x34,
    CASTU816= 0x35,
    
    CASTI8F = 0x36,
    CASTI832= 0x37,
    CASTI816= 0x38,
    
//
//
// Available opcodes 39 - 43
//
//

// Bit 0 is 0 if the operand is a 8 bits and 1 if 16 bits.
// Operand is sign extended
// This limits branches to the range -32768 to 32767.
// What happens if we go over that? do we fail or have some
// kind of trampoline support?
    IF      = 0x44,
    BRA     = 0x46,
    NCALL   = 0x48,
    ENTER   = 0x4a,

// Bits 1:0 is the width of the data: 00 - 1 byte, 01 - 2 bytes, 10 - 4 bytes, 11 float

    ADD     = 0x4c,
    SUB     = 0x50,
    IMUL    = 0x54,
    UMUL    = 0x58,
    IDIV    = 0x5c,
    UDIV    = 0x60,
    
    AND     = 0x64,
    OR      = 0x68,
    XOR     = 0x6c,
    NOT     = 0x70,
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
    NCALLS  = 0xe0,
    ENTERS  = 0xf0,
};

// Built-in types are 0x00-StructTypeStart-1, custom types are StructTypeStart-0xff
enum class Type : uint8_t {
    None = 0,
    Float = 1,
    
    Int8 = 10,
    UInt8 = 11,
    Int16 = 12,
    UInt16 = 13,
    Int32 = 14,
    UInt32 = 15,
    String = 18,
    Function = 19,
};

constexpr uint8_t StructTypeStart = 0x80; // Where struct types start

static inline bool isStruct(Type type) { return uint8_t(type) >= StructTypeStart; }

enum class Index : uint8_t { C = 0x00, X = 0x01, Y = 0x02, U = 0x03 };
enum class OpSize : uint8_t { i8 = 0, i16 = 1, i32 = 2, flt = 3 };

static constexpr uint8_t opSizeToBytes(OpSize opSize)
{
    return (opSize == OpSize::i8) ? 1 : ((opSize == OpSize::i16) ? 2 : 4);
}

static constexpr OpSize typeToOpSize(Type type)
{
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
            if (uint8_t(type) >= StructTypeStart) {
                return AddrSize;
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

static inline Op castOp(Type from, Type to)
{
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
    Const,
    Import,
    As,
    Function,
    Initialize,
    Return,
    Break,
    Continue,
    End,
    Loop,
    While,
    For,
    If,
    Else,
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

// Native functions
//
// These are implemented in the ExecutionUnit and are recognized by the Compiler.
// CallNative op has an operand which is the Native enum value. All params are
// passed on the stack and must be the expected size and type. Return value is
// sent back as an int32_t but can represent any type of value as defined in the
// native function signature
//
// Functions:
//
//      void    print(string)       - prints the passed string to the console
//      string  int8ToString(int8)  - return passed int8 value converted to string

enum class NativeId : uint8_t {
    None            = 0,
    PrintF          = 1,
    MemSet          = 2,
    RandomInt       = 3,
    RandomFloat     = 4,
    MinInt          = 5,
    MaxInt          = 6,
    MinFloat        = 7,
    MaxFloat        = 8,
    InitArgs        = 9,
    ArgInt8         = 10,
    ArgInt16        = 11,
    ArgInt32        = 12,
    ArgFloat        = 13,
};

#ifndef ARDUINO
class ASTNode;
using ASTPtr = std::shared_ptr<ASTNode>;

class Symbol;
using SymbolPtr = std::shared_ptr<Symbol>;
using Symbols = std::vector<Symbol>;

class Function;
using FunctionPtr = std::shared_ptr<Function>;
using FunctionList = std::vector<FunctionPtr>;

class Struct;
using StructPtr = std::shared_ptr<Struct>;
using StructList = std::vector<StructPtr>;
#endif

}
