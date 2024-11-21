/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// Interpreter base class
//
#pragma once

#include "Defines.h"

namespace clvr {

// Stack grows down from the top. Heap grows up from the bottom
class Memory
{
  public:
    enum class Error {
        None,
        InvalidSignature,
        InvalidVersion,
        WrongAddressSize,
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
                
        int32_t pop(OpSize size)
        {
            int32_t v = 0;
            
            switch (size) {
                case OpSize::flt  :
                case OpSize::i32: v |= int32_t(pop()) << 24;
                                  v |= int32_t(pop()) << 16;
                case OpSize::i16: v |= int32_t(pop()) << 8;
                case OpSize::i8 : v |= int32_t(pop());
            }
            
            // sign extend
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
        
        int32_t size() const { return _size; }
                
        bool ensurePush(uint8_t n = 1) const
        {
            if (_sp >= _size || _sp < 0) {
                _error = Error::InternalError;
                return false;
            }
            if (_sp - n < _heapEnd) {
                _error = Error::StackOverrun;
                return false;
            }
            return true;
        }
        
      private:
        void push(uint8_t v) { if (!ensurePush(1)) return; _mem[--_sp] = v; }
        uint8_t pop() { if (!ensureCount(1)) return 0; return _mem[_sp++]; }

        static uint32_t floatToInt(float f) { return *(reinterpret_cast<uint32_t*>(&f)); }
        
        bool ensureCount(uint8_t n) const
        {
            if (_sp < 0) {
                _error = Error::InternalError;
                return false;
            }
            if (_sp + n > _size) {
                _error = Error::StackUnderrun;
                return false;
            }
            return true;
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
    
    uint32_t getAbs(uint32_t addr, uint8_t size)
    {
        uint32_t v = 0;
        
        if (addr >= stack().size()) {
            // This is a constant
            addr -= stack().size();
            switch (size) {
                case 4: v |= uint32_t(rom(addr++)) << 24;
                                  v |= uint32_t(rom(addr++)) << 16;
                case 2: v |= uint32_t(rom(addr++)) << 8;
                case 1: v |= uint32_t(rom(addr++));
            }
        } else {
            switch (size) {
                case 4: v |= uint32_t(_stack.getAbs(addr++)) << 24;
                                  v |= uint32_t(_stack.getAbs(addr++)) << 16;
                case 2: v |= uint32_t(_stack.getAbs(addr++)) << 8;
                case 1: v |= uint32_t(_stack.getAbs(addr++));
            }
        }
        
        return v;
    }
    
    uint32_t getArg(int32_t offset, uint8_t size)
    {
        return getAbs(index(offset, Index::A), size);
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
        
        // Restore the previous self pointer if needed
        AddrNativeType prevSelf = _stack.pop(AddrOpSize);
        if (prevSelf) {
            _y = prevSelf;
        }
    }

    AddrNativeType index(uint32_t offset, Index idx) const
    {
        // U points at the previous U, then the previous self, then the return address
        switch (idx) {
            // Constants offset addresses by the size of stack memory so you can distinguish them
            case Index::C: return offset + ConstStart + stack().size();
            case Index::M: return _y + offset;
            case Index::A: return _u + (AddrSize * 3) + offset;
            case Index::L: return _u - offset - 1;
        }
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
    VarArg(Memory* memMgr, uint32_t lastArgOffset, Type lastArgType, bool isLastArgPtr)
        : _memMgr(memMgr)
    {
        _nextAddr = memMgr->index(lastArgOffset, Index::A) + typeToBytes(lastArgType, isLastArgPtr);
    }
    
    VarArg(Memory* memMgr)
        : _memMgr(memMgr)
    {
        _nextAddr = memMgr->index(0, Index::A);
    }
    
    // Type returned is always ArgUNativeType. Use reinterpret_cast to convert to the proper type
    ArgUNativeType arg(uint8_t bytes)
    {
        AddrNativeType argAddr = _nextAddr;
        _nextAddr += bytes;
        return _memMgr->getAbs(argAddr, bytes);
    }
    
    
    void initialize() { _nextAddr = _memMgr->stack().sp(); _initialAddr = _nextAddr; }

    void initialize(uint32_t lastArgOffset, Type lastArgType, bool isLastArgPtr)
    {
        _nextAddr = _memMgr->index(lastArgOffset, Index::A) + typeToBytes(lastArgType, isLastArgPtr);
        _initialAddr = _nextAddr;
    }
    
    void reset() { _nextAddr = _initialAddr; }
    
    void putChar(AddrNativeType addr, uint8_t c) { _memMgr->setAbs(addr, c, OpSize::i8); }
    
    Memory* memMgr() const { return _memMgr; }

  private:
    AddrNativeType _nextAddr;
    Memory* _memMgr;
    AddrNativeType _initialAddr = 0;
};
    
}
