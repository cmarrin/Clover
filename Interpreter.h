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

    };

    InterpreterBase(uint8_t* mem, uint32_t memSize) : _memMgr(mem, memSize) { }
    int32_t execute(uint16_t addr);

    // On entry args are pushed on stack followed by retAddr.
    // Push _bp and then set _bp to _sp. The subtract locals
    // from _sp.
    void setFrame(uint16_t locals)
    {
        _memMgr.stack().push(_u, 4);
        _u = _memMgr.stack().sp();
        _memMgr.stack().ensurePush(locals);
        _memMgr.stack().sp() -= locals;
    }

    void restoreFrame()
    {
        _memMgr.stack().sp() = _u;
        _u = _memMgr.stack().pop(4);
    }

  protected:
    virtual uint8_t rom(uint16_t i) const = 0;

    // Index is in bytes
    uint8_t getUInt8ROM(uint16_t index) const
    {
        return rom(index);
    }
    
    uint16_t getUInt16ROM(uint16_t index) const
    {
        // Little endian
        return uint32_t(getUInt8ROM(index)) |
               (uint32_t(getUInt8ROM(index + 1)) << 8);
    }

    uint32_t getOpnd(uint8_t bytes)
    {
        int32_t v = 0;
        uint32_t sex = 0x01;
        
        switch (bytes) {
            case 3: v = getUInt8ROM(_pc++); sex <<= 8;
            case 2: v = (v << 8) | getUInt8ROM(_pc++); sex <<= 8;
            case 1: v = (v << 8) | getUInt8ROM(_pc++); sex <<= 8;
            case 0: v = (v << 8) | getUInt8ROM(_pc++); sex <<= 8;
                // Sign extend
                sex >>= 1;
                if ((v & sex) != 0) {
                    v |= ~(sex - 1);
                }
                break;
        }
        return v;
    }

    int32_t addrMode(Index& index)
    {
        int8_t mode = getUInt8ROM(_pc++);
        index = Index(mode & 0x03);
        int32_t v;
        
        if ((mode & 0x04) == 0) {
            // Short
            v = mode >> 3;
        } else {
            v = getOpnd((mode & 18) >> 3);
        }
        
        return v;
    }
    
    // If immediate the immediate value is returned, otherwise it is the value at the ea
    uint32_t ea(uint8_t count)
    {
        Index index;
        int32_t addr = addrMode(index);
        
        switch (index) {
            case Index::X: return uint32_t(int32_t(_x) + addr);
            case Index::Y: return uint32_t(int32_t(_y) + addr);
            case Index::U: return uint32_t(int32_t(_u) + addr);
        }
    }
    
    uint32_t value(uint8_t count)
    {
        return _memMgr.getAbs(ea(count), count);
    }
    
    uint32_t immed(uint8_t opnd, uint8_t& count)
    {
        uint8_t bytes;
        
        switch (opnd) {
            case 0x00: bytes = 1; count = 1; break;
            case 0x01: bytes = 1; count = 2; break;
            case 0x02: bytes = 1; count = 4; break;
            case 0x03: bytes = 2; count = 2; break;
            case 0x04: bytes = 2; count = 4; break;
            case 0x05: bytes = 3; count = 4; break;
            case 0x06: bytes = 4; count = 4; break;
            default: return 0;
        }
        
        return getOpnd(bytes);
    }
    
    uint16_t _pc = 0;
    uint16_t _u = 0;
    uint16_t _x = 0;
    uint16_t _y = 0;

    Error _error = Error::None;
    int16_t _errorAddr = -1;

    Memory _memMgr;
};

template <uint32_t memSize> class Interpreter : public InterpreterBase
{
public:
    Interpreter() : InterpreterBase(_mem, memSize) { }
    virtual ~Interpreter() { }
    
    int32_t interp()
    {
        return execute(4);
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

    uint32_t stackLocal(uint16_t addr) const { return _mem.local(addr); }

    void setError(Error error) { _error = error; }

//    virtual void log(const char* s) const = 0;

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
