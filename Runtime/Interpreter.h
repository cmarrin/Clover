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
        InvalidVersion,
        NoEntryPoint,
        NotInstantiated,
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
    
    virtual ~InterpreterBase() { }
    
    virtual void setLight(uint8_t i, uint32_t rgb) = 0;

    // This method is the first called. It sets up the stack and registers
    // then loads the executable and finally allocates the top level struct
    // on the stack. But the constructor is not called.
    void instantiate();
    
    // After instantiate is called the top-level struct instance is ready to be
    // constructed. The caller first pushes args for the constructor and then
    // calls the construct method.
    void construct();

    // The execute method can be called over and over, as it would be from the
    // loop() function on Arduino.
    enum class ExecMode : uint8_t { Start, Continue };
    uint32_t execute(ExecMode);

    void addArg(uint32_t v, Type type);
    void addArg(float v);
    void dropArgs(uint32_t bytes);
    
    Memory* memMgr() { return &_memMgr; }
    VarArg* topLevelArgs() { return &_topLevelArgs; }
    void setReturnValue(uint32_t v) { _returnValue = v; }
    
  protected:
    bool isNextOpcodeSetFrame() const
    {
        uint8_t op = getUInt8ROM(_pc);
        return (Op(op & 0xfe) == Op::ENTER) || (Op(op & 0xf0) == Op::ENTERS);
    }
    
    void callNative(uint16_t);
    
    // TOS has from type value, type cast and push to type
    void typeCast(Type from, Type to);

    int32_t getROM(AddrNativeType addr, uint8_t size) const
    {
        int32_t v = 0;
        
        switch (size) {
            case 4: v = getUInt8ROM(addr++);
                    v = (v << 8) | getUInt8ROM(addr++);
            case 2: v = (v << 8) | getUInt8ROM(addr++);
            case 1: v = (v << 8) | getUInt8ROM(addr++);
                break;
        }
        return v;
    }

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

    // Determine the addr mode. Addr is unsigned. See Defines.h (Address Mode)
    // for details
    int32_t addrMode(Index& index)
    {
        uint8_t mode = getUInt8ROM(_pc++);
        index = Index(mode & 0x03);
        uint32_t v;
        
        if ((mode & 0x04) == 0) {
            // Short
            v = mode >> 3;
        } else if ((mode & 0x08) == 0) {
            // Upper 4 bits of mode prepended to next byte
            v = (uint32_t(mode & 0xf0) << 4) | getUOpnd(OpSize::i8);
        } else {
            v = getIOpnd(((mode & 0x10) == 0) ? OpSize::i16 : OpSize::i32);
        }
        
        return v;
    }
    
    AddrNativeType ea()
    {
        Index index;
        uint32_t addr = addrMode(index);
        return _memMgr.index(addr, index);
    }
    
    uint32_t value(OpSize opSize)
    {
        return _memMgr.getAbs(ea(), opSize);
    }
    
    void handleReturn()
    {
        _memMgr.restoreFrame();
        _pc = _memMgr.stack().pop(AddrOpSize);
    }
    
    template <uint8_t valueSize, uint8_t addrSize>
    AddrNativeType switchSearch(int32_t v, AddrNativeType addr, uint16_t n)
    {
        static constexpr uint8_t entrySize = valueSize + addrSize;
        uint16_t low = 0;
        uint16_t high = n - 1;
        
        while (low <= high) {
            uint16_t mid = low + (high - low) / 2;
            int32_t testValue = getROM(addr + mid * entrySize, valueSize);
            if (testValue == v) {
                return getROM(addr + mid * entrySize + valueSize, addrSize);
            }
            
            if (testValue < v) {
                low = mid + 1;
            } else {
                high = mid - 1;
            }
        }
        return 0;
    }

    uint16_t _pc = 0;

    Error _error = Error::None;
    int16_t _errorAddr = -1;

    Memory _memMgr;
    CallNative _modules[ModuleCountMax];
    
    uint32_t _returnValue;
    
    VarArg _topLevelArgs;
    AddrNativeType _mainEntryPoint = 0;
    AddrNativeType _topLevelCtorEntryPoint = 0;
    
    enum class State { BeforeInstantiate, Instantiated, Constructed };
    State _state = State::BeforeInstantiate;
};

template <uint32_t memSize> class Interpreter : public InterpreterBase
{
public:
    Interpreter() : InterpreterBase(_mem, memSize) { }
    
    uint32_t interp(ExecMode mode)
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
    
    uint8_t _mem[memSize];
};

}
