/*-------------------------------------------------------------------------
    This source file is a part of Lucid
    For the latest info, see https://github.com/cmarrin/Lucid
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// Interpreter base class
//
#pragma once

#include "Defines.h"
#include "Memory.h"

#include <stdlib.h>
#include <string.h>

namespace lucid {

class InterpreterBase
{
  public:
    enum class Error {
        None,
        InvalidSignature,
        NoEntryPoint,
        UnexpectedOpInIf,
		InvalidOp,
        OnlyMemAddressesAllowed,
        AddressOutOfRange,
        ExpectedSetFrame,
        InvalidModuleOp,
        InvalidNativeFunction,
        NotEnoughArgs,
        WrongNumberOfArgs,
        StackOverrun,
        StackUnderrun,
        StackOutOfRange,
        ImmedNotAllowedHere,
        InternalError,
    };

    InterpreterBase(uint8_t* mem, uint32_t memSize);
    
    enum class ExecMode : uint8_t { Start, Continue };
    int32_t execute(ExecMode);

    void addArg(uint32_t v, Type type);
    void addArg(float v);
    void dropArgs(uint32_t bytes);
    
    Memory* memMgr() { return &_memMgr; }
    VarArg* topLevelArgs() { return &_topLevelArgs; }
    void setReturnValue(uint32_t v) { _returnValue = v; }
    
  protected:
    void callNative(uint16_t);
    
    // TOS has from type value, type cast and push to type
    void typeCast(Type from, Type to);


    // Addr is in bytes
    uint8_t getUInt8ROM(uint16_t addr) const
    {
        return rom(addr);
    }
    
    uint32_t getUOpnd(OpSize opSize)
    {
        int32_t v = 0;
        
        switch (opSize) {
            case OpSize::i32:
            case OpSize::flt: v = getUInt8ROM(_pc++);
                              v = (v << 8) | getUInt8ROM(_pc++);
            case OpSize::i16: v = (v << 8) | getUInt8ROM(_pc++);
            case OpSize::i8 : v = (v << 8) | getUInt8ROM(_pc++);
                break;
        }
        return v;
    }
    
    int32_t getIOpnd(OpSize opSize)
    {
        int32_t v = getUOpnd(opSize);
        
        // Sign extend
        sex(v, opSize);
        return v;
    }

    // if bit 2 is 0 then bits 7:3 are a signed offset from -16 to 15. If bit 2 is 1
    // and bit 3 is 0, bits 7:4 are prepended to a following byte for a 12 bit
    // address (-2048 to 2047). If bit 3 is 1 then if bit 4 is 0 the next 2 bytes
    // is a signed address. If bit 4 is 1 then the next 4 bytes is a signed address.
    int32_t addrMode(Index& index)
    {
        int8_t mode = getUInt8ROM(_pc++);
        index = Index(mode & 0x03);
        int32_t v;
        
        if ((mode & 0x04) == 0) {
            // Short
            v = mode >> 3;
        } else if ((mode & 0x08) == 0) {
            // Upper 4 bits of mode prepended to next byte
            v = (int32_t(mode & 0xf0) << 4) | getUOpnd(OpSize::i8);
            if ((v & 0x800) != 0) {
                // Sign extend
                v |= 0xfffff000;
            }
        } else {
            v = getIOpnd(((mode & 0x10) == 0) ? OpSize::i16 : OpSize::i32);
        }
        
        return v;
    }
    
    AddrNativeType ea(OpSize opSize)
    {
        Index index;
        int32_t addr = addrMode(index);
        return _memMgr.index(addr, index, opSize);
    }
    
    uint32_t value(OpSize opSize)
    {
        return _memMgr.getAbs(ea(opSize), opSize);
    }
    
    void handleReturn()
    {
        _memMgr.restoreFrame();
        _pc = _memMgr.stack().pop(AddrOpSize);
    }
    
    uint16_t _pc = 0;

    Error _error = Error::None;
    int16_t _errorAddr = -1;

    Memory _memMgr;
    CallNative _modules[ModuleCountMax];
    
    uint32_t _returnValue;
    
    VarArg _topLevelArgs;
    AddrNativeType _entryPoint;
};

template <uint32_t memSize> class Interpreter : public InterpreterBase
{
public:
    Interpreter() : InterpreterBase(_mem, memSize) { }

    int32_t interp(ExecMode mode)
    {
        return execute(mode);
    }

    Error error() const { return _error; }
    
    // Returns -1 if error was not at any pc addr
    int16_t errorAddr() const { return _errorAddr; }
    
	// Return a float with a random number between min and max.
	// The random function takes ints. Multiply inputs by 1000 then divide the result
    // by the same to get a floating point result. That effectively makes the range
    // +/-2,000,000.
//	static float random(float min, float max)
//	{
//		return float(::random(int32_t(min * 1000), int32_t(max * 1000))) / 1000;
//	}
//	
//	static int32_t random(int32_t min, int32_t max)
//	{
//		int32_t r = ::random(min, max);
//		return r;
//	}

    void setError(Error error) { _error = error; }

private:
    uint16_t getId(uint8_t i)
    {
        return uint16_t(getUInt8ROM(_pc++)) | (uint16_t(i) << 8);
    }
    
    bool isNextOpcodeSetFrame() const
    {
        uint8_t op = getUInt8ROM(_pc);
        return (Op(op & 0xfe) == Op::ENTER) || (Op(op & 0xf0) == Op::ENTERS);
    }
    
    uint8_t _mem[memSize];
};

}
