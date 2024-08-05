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

namespace lucid {

// Stack grows down from the top. Heap grows up from the bottom
class Memory
{
  public:
    enum class Error {
        None,
        InternalError,
        StackOverrun,
        StackUnderrun,
        StackOutOfRange,
    };
    
    class Stack
    {
      public:
        Stack(uint8_t* mem, uint32_t size) : _mem(mem), _size(size), _sp(size) { }
        
        void setHeapEnd(int32_t addr) { _heapEnd = addr; }
        
        const uint8_t& getAbs(uint32_t addr) const { return _mem[addr]; }
        uint8_t& getAbs(uint32_t addr) { return _mem[addr]; }
        
        void push(uint32_t v, Type type) { push(v, typeToOpSize(type)); }
        void push(float v) { push(*(reinterpret_cast<uint32_t*>(&v)), typeToOpSize(Type::Float)); }

        void push(uint32_t v, OpSize size)
        {
            push(uint8_t(v));
            if (size == OpSize::i8) {
                return;
            }
            push(uint8_t(v >> 8));
            if (size == OpSize::i16) {
                return;
            }
            push(uint8_t(v >> 16));
            push(uint8_t(v >> 24));
        }
                
        uint32_t pop(OpSize size)
        {
            uint32_t v = 0;
            
            switch (size) {
                case OpSize::flt  :
                case OpSize::i32: v |= uint32_t(pop()) << 24;
                                  v |= uint32_t(pop()) << 16;
                case OpSize::i16: v |= uint32_t(pop()) << 8;
                case OpSize::i8 : v |= uint32_t(pop());
            }
            
            // Sign extend
            sex(v, size);
            
            return v;
        }
        
        void drop(uint16_t count) { _sp += count; }
        
        void swap(OpSize size)
        {
            uint32_t a = pop(size);
            uint32_t b = pop(size);
            push(a, size);
            push(b, size);
        }
        
        void dup(OpSize size)
        {
            uint32_t v = pop(size);
            push(v, size);
            push(v, size);
        }

        bool empty() const { return _sp == _size; }
        Memory::Error error() const { return _error; }
        
        const uint16_t& sp() const { return _sp; }
        uint16_t& sp() { return _sp; }
                
        void ensurePush(uint8_t n = 1) const
        {
            if (_sp >= _size || _sp < 0) {
                _error = Error::InternalError;
            }
            if (_sp - n < _heapEnd) {
                _error = Error::StackOverrun;
            }
        }
        
      private:
        void push(uint8_t v) { ensurePush(1); _mem[--_sp] = v; }
        uint8_t pop() { ensureCount(1); return _mem[_sp++]; }

        static uint32_t floatToInt(float f) { return *(reinterpret_cast<uint32_t*>(&f)); }
        
        void ensureCount(uint8_t n) const
        {
            if (_sp < 0) {
                _error = Error::InternalError;
            }
            if (_sp + n > _size) {
                _error = Error::StackUnderrun;
            }
        }
        
        uint8_t* _mem = nullptr;
        int32_t _size = 0;
        uint16_t _sp = 0;
        uint16_t _heapEnd = 0;
        mutable Error _error = Error::None;
    };

    Memory(uint8_t* mem, uint32_t memSize) : _stack(mem, memSize) { }
    
    Error error() const { return _stack.error(); }
    const Stack& stack() const { return _stack; }
    Stack& stack() { return _stack; }
    void setAbs(uint32_t addr, uint32_t v, OpSize opSize)
    {
        switch (opSize) {
            case OpSize::flt:
            case OpSize::i32: _stack.getAbs(addr++) = uint8_t(v >> 24);
                              _stack.getAbs(addr++) = uint8_t(v >> 16);
            case OpSize::i16: _stack.getAbs(addr++) = uint8_t(v >> 8);
            case OpSize::i8 : _stack.getAbs(addr++) = uint8_t(v);
        }
    }
    
    uint32_t getAbs(uint32_t addr, OpSize opSize)
    {
        uint32_t v = 0;
        
        switch (opSize) {
            case OpSize::flt:
            case OpSize::i32: v |= uint32_t(_stack.getAbs(addr++)) << 24;
                              v |= uint32_t(_stack.getAbs(addr++)) << 16;
            case OpSize::i16: v |= uint32_t(_stack.getAbs(addr++)) << 8;
            case OpSize::i8 : v |= uint32_t(_stack.getAbs(addr++));
        }
        
        return v;
    }
    
    uint32_t getLocal(int32_t offset, Type type)
    {
        return getAbs(localAddr(offset, typeToOpSize(type)), typeToOpSize(type));
    }
    
    // On entry args are pushed on stack followed by retAddr.
    // Push _bp and then set _bp to _sp. The subtract locals
    // from _sp.
    void setFrame(uint16_t locals)
    {
        _stack.push(_u, AddrOpSize);
        _u = _stack.sp();
        _stack.ensurePush(locals);
        _stack.sp() -= locals;
    }

    void restoreFrame()
    {
        _stack.sp() = _u;
        _u = _stack.pop(AddrOpSize);
    }

    AddrNativeType index(int32_t offset, Index idx, OpSize opSize) const
    {
        switch (idx) {
            case Index::C: return 0;
            case Index::X: return _x + offset;
            case Index::Y: return _y + offset;
            case Index::U: return localAddr(offset, opSize);
        }
    }
    
    // Local offsets are negative and args are non-negative so
    // the first local byte (or the LSB if 16 or 32 bit) is -1
    // and the first arg (or the MSB if 16 or 32 bit) is 0.
    AddrNativeType localAddr(int32_t offset, OpSize opSize = OpSize::i8) const
    {
        if (offset < 0) {
            return _u + offset - opSizeToBytes(opSize) + 1;
        }
        return _u + AddrSize * 2 + offset;
    }

    const AddrNativeType& self() const { return _y; }
    AddrNativeType& self() { return _y; }
    
  private:
    Stack _stack;
    AddrNativeType _u = 0;
    AddrNativeType _x = 0;
    AddrNativeType _y = 0;
};

// Stack grows down. On a function call U points to previous U pushed onto the stack.
// Above that is the return address and then the args. The MSB of the leftmost arg
// is first since it is pushed last. Locals are negative offsets from U, so
// U - AddrSize is the LSB of the first local variable.

class VarArg
{
  public:
    VarArg(Memory* memMgr, uint32_t lastArgOffset, Type lastArgType)
        : _memMgr(memMgr)
    {
        _nextAddr = memMgr->localAddr(lastArgOffset, typeToOpSize(lastArgType)) + typeToBytes(lastArgType);
    }
    
    VarArg(Memory* memMgr)
        : _memMgr(memMgr)
    {
        _nextAddr = memMgr->localAddr(0);
    }
    
    // Type returned is always uint32_t. Use reinterpret_cast to convert to the proper type
    uint32_t arg(Type type)
    {
        AddrNativeType argAddr = _nextAddr;
        _nextAddr += typeToBytes(type);
        return _memMgr->getAbs(argAddr, typeToOpSize(type));
    }
    
    
    void initialize() { _nextAddr = _memMgr->localAddr(0); _initialAddr = _nextAddr; }

    void initialize(uint32_t lastArgOffset, Type lastArgType)
    {
        _nextAddr = _memMgr->localAddr(lastArgOffset, typeToOpSize(lastArgType)) + typeToBytes(lastArgType);
        _initialAddr = _nextAddr;
    }
    
    void reset() { _nextAddr = _initialAddr; }

  private:
    AddrNativeType _nextAddr;
    Memory* _memMgr;
    AddrNativeType _initialAddr = 0;
};
    
}
