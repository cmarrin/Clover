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

namespace lucid {

#ifdef ARDUINO
    #include <Arduino.h>
    static inline String to_string(int32_t v) { return String(v); }
    static inline String to_string(float v) { return String(v); }
#else
    //#include <string>
    
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
    
    //using String = std::string;
    //static inline std::string to_string(int32_t v) { return std::to_string(v); }
    //static inline std::string to_string(float v) { return std::to_string(v); }
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

        p       - Byte after opcode. Num params passed to function.
        l       - Byte after opcode. Num locals in function.

    Stack is 8 bit. 16 and 32 bit values are stored in consecutive locations (HI to LO)
    
    Load and store values on the stack
    ==================================
    
    
    // Load and store rather than push/pop
    ex., a.b = c.d

    Non-stack machine (patterned after 6809)
    
    - Has an accumulator (A) that can hold 8, 16 or 32 bit value
    - Has an index register (X) that is the width of an absolute addr
    - Has a base pointer (L) register pointing to local vars
    - Has a stack pointer that grows down
    - Has a pc
    
    All addresses can be 2 bytes for a 64KB range or 4 bytes for a 2^32 byte range.
    This is determined at compile time. Also at compile time you can specify
    float support or not and max integer width (16 or 32 bits).
    
    - Address mode
        The byte following the opcode indicates the addressing mode. Bits1:0 indicate
        how the addressing value is used:
                        
            00 - Immediate. Value is the signed operand
            01 - X. Add signed value to X to get EA
            10 - Y. Add signed value to X to get EA
            11 - U. Add signed value to U to get EA
                        
        if bit 2 is 0 then bits 7:3 are a signed offset from -16 to 16. If bit 2 is 1
        then bits 4:3 are the number of bytes that follow, from 1 (00) to 4 (11). If
        1 byte follows then bits 7:5 are appended as the MSB of a 13 bit signed integer
        from -1024 to 1023. If 2, 3 or 4 bytes follow no appending is done.

    Stack frame and Base pointer
    
    Stack grows down so when you push a value the current SP points to it. On a call
    args are pushed from left to right so rightmost param is at the lowest address.
    The caller then pushes the return address and calls the function.
    
    The first
    instruction of a function must be ENTER. This has a 4 bit (0-15), 1 or 2 byte
    operand which is the number of bytes of local storage needed. The ENTER
    instruction pushes the current BP (U register), sets BP = SP and subtracts the
    number of local bytes from SP. When indexing from BP, positive offsets starting
    at 4 address the args. Locals are addressed with negative offsets starting
    at -1.
    
    On return, the callee sets SP = BP, pops the stack into BP and performs a
    return operation. The caller then adds the number of bytes of args to SP.
    
    
    
    
    
    
    
    
    
    
    LDAK<1,2,4>     - Load 1, 2 or 4 bytes into A immediate
    LDAX<1,2,4>     - Load 1, 2 or 4 bytes indexed from X
    LDAL<1,2,4>     - Load 1, 2 or 4 bytes indexed from B
    LDXK            - Load X immediate
    LDXX            - Load X indexed from X
    LDXL            - Load X indexed from B
    
    STAX<1,2,4>     - Store 1, 2 or 4 bytes of A into location indexed by X
    STAL<1,2,4>     - Store 1, 2 or 4 bytes of A into location indexed by L
    
    LEAXX           - Load X with the EA derived from the index mode using X
    LEAXL           - Load X with the EA derived from the index mode using L
    
    ADDAK<1,2,4>    - Add A (1, 2 or 4 bytes) to immediate value
    ADDAX<1,2,4>    - Add A (1, 2 or 4 bytes) to value indexed from X
    ADDAL<1,2,4>    - Add A (1, 2 or 4 bytes) to value indexed from L
    ADDAFK          - Add float A to immediate float value
    ADDAFX          - Add float A to float value indexed from X
    ADDAFL          - Add float A to float value indexed from L
    
    SUBAK<1,2,4>    - Sub A (1, 2 or 4 bytes) from immediate value
    SUBAX<1,2,4>    - Sub A (1, 2 or 4 bytes) from value indexed from X
    SUBAL<1,2,4>    - Sub A (1, 2 or 4 bytes) from value indexed from L
    SUBAFK          - Sub float A from immediate float value
    SUBAFX          - Sub float A from float value indexed from X
    SUBAFL          - Sub float A from float value indexed from L
    
    CMPAK<1,2,4>    - Cmp A (1, 2 or 4 bytes) to immediate value
    CMPAX<1,2,4>    - Cmp A (1, 2 or 4 bytes) to value indexed from X
    CMPAL<1,2,4>    - Cmp A (1, 2 or 4 bytes) to value indexed from L
    CMPAFK          - Cmp float A to immediate float value
    CMPAFX          - Cmp float A to float value indexed from X
    CMPAFL          - Cmp float A to float value indexed from L
    
    MULAK<1,2,4>    - Mul A (1, 2 or 4 bytes) unsigned by immediate value
    MULAX<1,2,4>    - Mul A (1, 2 or 4 bytes) unsigned by value indexed from X
    MULAL<1,2,4>    - Mul A (1, 2 or 4 bytes) unsigned by value indexed from L
    
    MULASK<1,2,4>   - Mul A (1, 2 or 4 bytes) signed by immediate value
    MULASX<1,2,4>   - Mul A (1, 2 or 4 bytes) signed by value indexed from X
    MULASL<1,2,4>   - Mul A (1, 2 or 4 bytes) signed by value indexed from L
    MULAFK          - Mul float A signed by immediate float value
    MULAFX          - Mul float A signed by float value indexed from X
    MULAFL          - Mul float A signed by float value indexed from L
    
    DIVAK<1,2,4>    - Div A (1, 2 or 4 bytes) unsigned by immediate value
    DIVAX<1,2,4>    - Div A (1, 2 or 4 bytes) unsigned by value indexed from X
    DIVAL<1,2,4>    - Div A (1, 2 or 4 bytes) unsigned by value indexed from L
    
    DIVAKS<1,2,4>    - Div A (1, 2 or 4 bytes) signed by immediate value
    DIVAXS<1,2,4>    - Div A (1, 2 or 4 bytes) signed by value indexed from X
    DIVALS<1,2,4>    - Div A (1, 2 or 4 bytes) signed by value indexed from L
    DIVAKF           - Div float A signed by immediate float value
    DIVAXF           - Div float A signed by float value indexed from X
    DIVALF           - Div float A signed by float value indexed from L
    
    MODAK<1,2,4>    - Mod A (1, 2 or 4 bytes) by immediate value
    MODAX<1,2,4>    - Mod A (1, 2 or 4 bytes) by value indexed from X
    MODAL<1,2,4>    - Mod A (1, 2 or 4 bytes) by value indexed from L
    
    NEGA<1,2,4>     - Negate A (1, 2 or 4 bytes)
    
    BLE             - Branch if previous signed result is less than or equal to relative address in next byte
    LBLE            - Branch if previous signed result is less than or equal to relative address in next 2 bytes
    BLS             - Branch if previous unsigned result is less than or equal to relative address in next byte
    LBLS            - Branch if previous unsigned result is less than or equal to relative address in next 2 bytes
    
    BLT             - Branch if previous signed result is less than to relative address in next byte
    LBLT            - Branch if previous signed result is less than to relative address in next 2 bytes
    BLO             - Branch if previous unsigned result is less than to relative address in next byte
    LBLO            - Branch if previous unsigned result is less than to relative address in next 2 bytes
    
    BEQ             - Branch if previous result is equal to relative address in next byte
    LBEQ            - Branch if previous result is equal to relative address in next 2 byte
    BNE             - Branch if previous result is not equal to relative address in next byte
    LBNE            - Branch if previous result is not equal to relative address in next 2 byte
    
    BGT             - Branch if previous signed result is greater than to relative address in next byte
    LBGT            - Branch if previous signed result is greater than to relative address in next 2 bytes
    BHI             - Branch if previous unsigned result is greater than to relative address in next byte
    LBHI            - Branch if previous unsigned result is greater than to relative address in next 2 bytes
    
    BGE             - Branch if previous signed result is greater than or equal to relative address in next byte
    LBGE            - Branch if previous signed result is greater than or equal to relative address in next 2 bytes
    BHS             - Branch if previous unsigned result is greater than or equal to relative address in next byte
    LBHS            - Branch if previous unsigned result is greater than or equal to relative address in next 2 bytes

    BSR             - Branch to subroutine at relative address in next byte
    LBSR            - Branch to subroutine at relative address in next 2 bytes
    JSR             - Jump to subroutine at absolute address in next 2 or 4 bytes
    RET             - Return from BSR or JSR
    SETFRAME        -

30 * 3 + 43 = 127







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
    Main struct size    - 2 bytes: size in bytes of main struct to be allocated in low memory
    Main init function  - 2 bytes: offset from start of code of the main struct initialize function
    Code                - Code for all functions. 
    Checksum            - 1 byte: checksum of entire file, including Format Id
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
    NOP     = 0x00,
    
// Bits 1:0 is the width of the data: 00 - 1 byte, 01 - 2 bytes, 10 - 3 bytes, 11 float
    LDA     = 0x10,
    LDX     = 0x14,
    STA     = 0x18,
    LEAX    = 0x1c,
    
    ADDA    = 0x20,
    SUBA    = 0x24,
    CMPA    = 0x28,
    MULA    = 0x2c,
    DIVA    = 0x30, // a % b => a - a / b * b
    
    ANDA    = 0x40,
    ORA     = 0x44,
    XORA    = 0x48,
    
// LSB = 0, branch address is 8 bit. LSB = 1, branch address is 16 bit
    BLE    = 0x40,
    BLS    = 0x42,
    BLT    = 0x44,
    BLO    = 0x46,
    BGE    = 0x48,
    BHS    = 0x4a,
    BGT    = 0x4c,
    BHI    = 0x4e,
    
    BEQ    = 0x50,
    BNE    = 0x52,
    BRA    = 0x54,
    
// LSB = 0, absolute jump address is 16 bit. LSB = 1, address is 32 bit.
    JMP    = 0x56,
    JSR    = 0x58,
    
    RET    = 0x5a,
    
// This version has the local count in bytes in the next byte (0-255 bytes)
    ENTER  = 0x5b,
    
// This version has the local count in bytes in the next 2 byte (0-65535 bytes)
    ENTERL = 0x5c,
    
// This version has the local count in the lower 4 bits of the opcode (0-15 bytes)
    ENTERS = 0x60,
    
// Next opcode is 0x70


    
    
    
    
//    None            = 0x00,
//
//    PushIntConst    = 0x01,
//    PushDeref       = 0x02,
//    PopDeref        = 0x03,
//
//    Dup1            = 0x04,
//    Dup2            = 0x05,
//    Dup4            = 0x06,
//    Drop1           = 0x07,
//    Drop2           = 0x08,
//    Drop4           = 0x09,
//    Swap1           = 0x0a,
//    Swap2           = 0x0b,
//    Swap4           = 0x0c,
//
//    CallNative      = 0x0e,
//    Return          = 0x0f,
//
//// 0x0c to 0x0f unused
//
//    Or              = 0x10,
//    Xor             = 0x11,
//    And             = 0x12,
//    Not             = 0x13,
//
//    LOr             = 0x14,
//    LAnd            = 0x15,
//    LNot            = 0x16,
//
//    LTInt           = 0x17,
//    LTFloat         = 0x18,
//    LEInt           = 0x19,
//    LEFloat         = 0x1a,
//    EQInt           = 0x1b,
//    EQFloat         = 0x1c,
//    NEInt           = 0x1d,
//    NEFloat         = 0x1e,
//    GEInt           = 0x1f,
//    GEFloat         = 0x20,
//    GTInt           = 0x21,
//    GTFloat         = 0x22,
//
//    AddInt          = 0x23,
//    AddFloat        = 0x24,
//    SubInt          = 0x25,
//    SubFloat        = 0x26,
//    MulInt          = 0x27,
//    MulFloat        = 0x28,
//    DivInt          = 0x29,
//    DivFloat        = 0x2a,
//
//    NegInt          = 0x2b,
//    NegFloat        = 0x2c,
//
//    PreIncInt       = 0x2d,
//    PreIncFloat     = 0x2e,
//    PreDecInt       = 0x2f,
//    PreDecFloat     = 0x30,
//    PostIncInt      = 0x31,
//    PostIncFloat    = 0x32,
//    PostDecInt      = 0x33,
//    PostDecFloat    = 0x34,
//
//    // 0x40 - 0xf0 ops use lower 4 bits for data value
//
//    PushRef         = ExtOpcodeStart + 0x00,
//    Push            = ExtOpcodeStart + 0x10,
//    Pop             = ExtOpcodeStart + 0x20,
//
//    Call            = ExtOpcodeStart + 0x30,
//
//    Offset          = ExtOpcodeStart + 0x40,
//    Index           = ExtOpcodeStart + 0x50,
//    PushIntConstS   = ExtOpcodeStart + 0x60,
//    Log             = ExtOpcodeStart + 0x70,
//    SetFrameS       = ExtOpcodeStart + 0x80,    // p and l are each 2 bits (0-3)
//    Jump            = ExtOpcodeStart + 0x90,
//    If              = ExtOpcodeStart + 0xa0,
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
    Int = 16, // Unspecified size
    UInt = 17, // Unspecified size
    String = 18,
    
    Struct = 20,
    
    Ptr = 30
};

enum class Index : uint8_t { K = 0x00, X = 0x01, Y = 0x02, U = 0x03 };

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

}
