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
#include <memory>

namespace lucid {

#ifdef ARDUINO
    #include <Arduino.h>
    #include <EEPROM.h>

    static inline uint8_t rom(uint32_t addr) { return EEPROM[addr]; }
    
    static inline String to_string(int32_t v) { return String(v); }
    static inline String to_string(float v) { return String(v); }
#else
    extern uint8_t* ROMBase;

    static inline uint8_t rom(uint32_t addr) { return ROMBase[addr]; }
    
    static inline void randomSeed(uint32_t s) { srand(s); }
    static inline int32_t random(int32_t min, int32_t max)
    {
        if (min >= max) {
            return max;
        }
        int r = rand() % (max - min);
        return r + min;
    }
    
    template<class T> 
    const T& min(const T& a, const T& b)
    {
        return (b < a) ? b : a;
    }

    template<class T> 
    const T& max(const T& a, const T& b)
    {
        return (b > a) ? b : a;
    }
    
#endif

static inline float intToFloat(uint32_t i)
{
    float f;
    memcpy(&f, &i, sizeof(float));
    return f;
}

static inline uint32_t floatToInt(float f)
{
    uint32_t i;
    memcpy(&i, &f, sizeof(float));
    return i;
}

// PointerSize is needed to know how much space is taken
// by pointers on the stack and to compute function and
// struct variable sizes.
static constexpr uint8_t PointerSize = 2;

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
                        
            00 - Reserved
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

static constexpr uint16_t MaxIdSize = 4096;
static constexpr uint16_t ConstStart = 0x00;
static constexpr uint16_t ConstSize = 2048; // Max possible size
static constexpr uint16_t GlobalStart = ConstStart + ConstSize;
static constexpr uint16_t GlobalSize = 1024;
static constexpr uint16_t LocalStart = GlobalStart + GlobalSize;
static constexpr uint16_t LocalSize = MaxIdSize - LocalStart;
static constexpr uint8_t ExtOpcodeStart = 0x40;

// 0 bit opcodes start at 0x00
static constexpr uint8_t OneBitOperandStart = 0x10;
static constexpr uint8_t TwoBitOperandStart = 0x1c;
static constexpr uint8_t FoutBitOperandStart = 0xb0;

enum class Op: uint8_t {
    NOP     = 0x00,
    RET     = 0x01,
    PUSHS   = 0x02, // Following byte is length, followed by length chars
    PUSHK11 = 0x03, // 1 byte operand, push 1 byte
    PUSHK12 = 0x04, // 1 byte operand, push 2 bytes
    PUSHK14 = 0x05, // 1 byte operand, push 4 bytes
    PUSHK22 = 0x06, // 2 byte operand, push 2 bytes
    PUSHK24 = 0x07, // 2 byte operand, push 4 bytes
    PUSHK34 = 0x08, // 3 byte operand, push 4 bytes
    PUSHK44 = 0x09, // 4 byte operand, push 4 bytes
    
    DROP1   = 0x0a, // Next byte is count (1 - 256)
    DROP2   = 0x0b, // Next 2 bytes are count (1 - 65536)

// Bit 0 is 0 if the operand is a 8 bits and 1 if 16 bits.
// Operand is sign extended
// This limits branches to the range -32768 to 32767.
// What happens if we go over that? do we fail or have some
// kind of trampoline support?
    IF      = 0x10,
    BRA     = 0x12,
    CALL    = 0x14,
    NCALL   = 0x16,
    ENTER   = 0x18,

// Bits 1:0 is the width of the data: 00 - 1 byte, 01 - 2 bytes, 10 - 4 bytes, 11 float

    PUSHREF = 0x1c, // Next byte is addr mode. Data width is used when computing negative offsets from U
    DEREF   = 0x20,
    PUSH    = 0x24, // Next byte is addr mode
    DUP     = 0x28,
    SWAP    = 0x2c,
    
    ADD     = 0x30,
    SUB     = 0x34,
    IMUL    = 0x38,
    UMUL    = 0x3c,
    IDIV    = 0x40,
    UDIV    = 0x44,
    
    AND     = 0x48,
    OR      = 0x4c,
    XOR     = 0x50,
    NOT     = 0x54,
    NEG     = 0x58,

    PREINC  = 0x5c, // Next byte is addr mode
    PREDEC  = 0x60, // Next byte is addr mode
    POSTINC = 0x64, // Next byte is addr mode
    POSTDEC = 0x68, // Next byte is addr mode
    
    LE      = 0x6c,
    LS      = 0x70,
    LT      = 0x74,
    LO      = 0x78,
    GE      = 0x7c,
    HS      = 0x80,
    GT      = 0x84,
    HI      = 0x88,
    EQ      = 0x8c,
    NE      = 0x90,

    TOF     = 0x94,
    TOU8    = 0x98,
    TOI8    = 0x9c,
    TOU16   = 0xa0,
    TOI16   = 0xa4,
    TOU32   = 0xa8,
    TOI32   = 0xac,

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
    Fixed = 2,
    
    Int8 = 10,
    UInt8 = 11,
    Int16 = 12,
    UInt16 = 13,
    Int32 = 14,
    UInt32 = 15,
    String = 18,
    Function = 19,
    
    Struct = 20,
    
    Ptr = 30
};

enum class Index : uint8_t { X = 0x01, Y = 0x02, U = 0x03 };
enum class OpSize : uint8_t { i8 = 0, i16 = 1, i32 = 2, flt = 3 };

static constexpr uint8_t opSizeToBytes(OpSize opSize)
{
    switch (opSize) {
        case OpSize::i8:  return 1;
        case OpSize::i16: return 2;
        case OpSize::i32: return 4;
        case OpSize::flt: return 4;
    }
}

static constexpr OpSize typeToOpSize(Type type)
{
    switch (type) {
        case Type::Int8     : return OpSize::i8;
        case Type::UInt8    : return OpSize::i8;
        case Type::Int16    : return OpSize::i16;
        case Type::UInt16   : return OpSize::i16;
        case Type::Int32    : return OpSize::i32;
        case Type::UInt32   : return OpSize::i32;
        case Type::Float    : return OpSize::i32;
        default: return OpSize::i32;
    }
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
        default: return 0;
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

static inline Op castToOp(Type type)
{
    switch (type) {
        case Type::Float: return Op::TOF;
        case Type::UInt8: return Op::TOU8;
        case Type::Int8: return Op::TOI8;
        case Type::UInt16: return Op::TOU16;
        case Type::Int16: return Op::TOI16;
        case Type::UInt32: return Op::TOU32;
        case Type::Int32: return Op::TOI32;
        default: return Op::NOP;
    }
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
    Int8,
    UInt8,
    Int16,
    UInt16,
    Int32,
    UInt32,
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
};

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


}
