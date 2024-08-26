/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "Interpreter.h"

#include "NativeColor.h"
#include "NativeCore.h"

using namespace lucid;

InterpreterBase::InterpreterBase(uint8_t* mem, uint32_t memSize)
    : _memMgr(mem, memSize)
    , _topLevelArgs(&_memMgr)
{ }

void
InterpreterBase::instantiate()
{
    _error = Error::None;
    _state = State::BeforeInstantiate;
    
    // Reset everything
    
    // Init module list
    memset(_modules, 0, sizeof(CallNative) * ModuleCountMax);
    
    // Install core
    _modules[0] = NativeCore::callNative;
    
    // FIXME: For now install NativeColor
    _modules[1] = NativeColor::callNative;
    
    // Check signature
    if (getUInt8ROM(0) != 'l' || getUInt8ROM(1) != 'u' || getUInt8ROM(2) != 'c' || getUInt8ROM(3) != 'd') {
        _error = Error::InvalidSignature;
        return;
    }

    _mainEntryPoint = getROM(MainEntryPointAddr, 4);
    _topLevelCtorEntryPoint = getROM(TopLevelCtorEntryPointAddr, 4);

    uint32_t topLevelStructSize = getROM(TopLevelStructSizeAddr, 4);


    // Instantiate the top-level struct, but don't call the ctor
    _memMgr.stack().sp() = _memMgr.stack().size() - topLevelStructSize;    
    _pc = _topLevelCtorEntryPoint;
    _memMgr.self() = _memMgr.stack().sp();
    
    // The only thing on the stack at this point is space for the
    // top-level struct. The self pointer points at it. The next
    // step is to push args for the ctor and call construct.

    _state = State::Instantiated;
}

void
InterpreterBase::construct()
{
    if (_state != State::Instantiated) {
        _error = Error::NotInstantiated;
        return;
    }
    
    _state = State::Constructed;
    
    // If there's no ctor, that's ok, we just have nothing to do.
    if (_topLevelCtorEntryPoint == 0) {
        return;
    }

    // At this point the stack has memory for the top-level struct
    // instantiation (pointed to by Y) and any args pushed by the
    // caller. Now we need to push a return addr of 0, so when we
    // return from the ctor we will see the 0 and exit the interpreter.
    _topLevelArgs.initialize();
    _memMgr.stack().push(0, AddrOpSize);
    
    // Set the pc to the ctor address and call the interpreter
    // with ExecMode::Continue, so it doesn't try to execute main().
    _pc = _topLevelCtorEntryPoint;
    execute(ExecMode::Continue);
}

void
InterpreterBase::addArg(uint32_t v, Type type)
{
    _memMgr.stack().push(v, type);
}

void
InterpreterBase::addArg(float v)
{
    _memMgr.stack().push(v);
}

void
InterpreterBase::dropArgs(uint32_t bytes)
{
    _memMgr.stack().drop(bytes);
}

void
InterpreterBase::callNative(uint16_t id)
{
    // Add a dummy return address to make everything work
    _memMgr.stack().push(0, AddrOpSize);
    
    // Set the frame
    _memMgr.setFrame(0);

    uint8_t moduleId = id >> BitsPerFunctionId;
    uint8_t functionId = id & FunctionIdMask;
    if (moduleId < ModuleCountMax) {
        CallNative call = _modules[moduleId];
        if (call) {
            call(functionId, this);
        }
    }
    
    // Restore the frame and pop the dummy return address
    _memMgr.restoreFrame();
    _memMgr.stack().pop(AddrOpSize);
}

void
InterpreterBase::typeCast(Type from, Type to)
{
    int32_t v = _memMgr.stack().pop(typeToOpSize(from));
    
    // handle from float case
    if (from == Type::Float) {
        v = intToFloat(v);
    } else if (from == Type::UInt16) {
        // v is signed. Clear upper 16 bits
        v &= 0xffff;
    } else if (from == Type::UInt8) {
        // v is signed. Clear upper 24 bits
        v &= 0xff;
    } else if (typeToBytes(from) < typeToBytes(to)) {
        // Handle widening
        if (from == Type::Int16 || from == Type::Int8) {
            sex(v, typeToOpSize(from));
        }
    }
    
    // v is now a 32 bit value, sign extended if needed
    
    // handle to float case
    if (to == Type::Float) {
        v = floatToInt(float(v));
    }
    
    _memMgr.stack().push(v, typeToOpSize(to));
}

uint32_t
InterpreterBase::execute(ExecMode mode)
{
    if (_state != State::Constructed) {
        _error = Error::NotInstantiated;
        return 0;
    }
    
    if (mode == ExecMode::Start) {
        _pc = _mainEntryPoint;
        if (!isNextOpcodeSetFrame()) {
            _error = Error::NoEntryPoint;
            return 0;
        }
    }
    
    // If we're continuing the stack is already setup
    if (mode == ExecMode::Start) {
        // Init the top level arg pointer. Any params pushed will be accessible using _topLevelArgs
        // At this point any args are directly on the top of stack init VarArgs to that point
        _topLevelArgs.initialize();

        // Push a dummy return address
        _memMgr.stack().push(0, AddrOpSize);
    }
    
    _returnValue = 0;
    
    while(1) {
        if (_memMgr.error() != Memory::Error::None) {
            // FIXME: Deal with errors
            // _error = _memMgr.error();
        }
        if (_error != Error::None) {
            _errorAddr = _pc - 1;
            return 0;
        }

        // If address is 0, we exit
        if (_pc == 0) {
            return _returnValue;
        }

        uint8_t opInt = getUInt8ROM(_pc++);
        uint8_t operand = 0;
        Op opcode = Op::NOP;
        OpSize opSize = OpSize::i8;

        if (opInt < OneBitOperandStart) {
            opcode = Op(opInt);
        } else if (opInt < TwoBitOperandStart) {
            operand = opInt & 0x01;
            opcode = Op(opInt & 0xfe);
            opSize = OpSize(operand);
        } else if (opInt < FoutBitOperandStart) {
            operand = opInt & 0x03;
            opcode = Op(opInt & 0xfc);
            opSize = OpSize(operand);
        } else {
            operand = opInt & 0x0f;
            opcode = Op(opInt & 0xf0);
        }
        
        int32_t left, right;
        
        switch (opcode) {
            case Op::NOP:
                break;
            case Op::PUSHREF1:
                _memMgr.stack().push(ea(OpSize::i8), AddrOpSize);
                break;
            case Op::PUSHREF2:
                _memMgr.stack().push(ea(OpSize::i16), AddrOpSize);
                break;
            case Op::PUSHREF4:
                _memMgr.stack().push(ea(OpSize::i32), AddrOpSize);
                break;
            case Op::DEREF1:
                left = _memMgr.stack().pop(AddrOpSize);
                right = _memMgr.getAbs(left, OpSize::i8);
                _memMgr.stack().push(right, OpSize::i8);
                break;
            case Op::DEREF2:
                left = _memMgr.stack().pop(AddrOpSize);
                right = _memMgr.getAbs(left, OpSize::i16);
                _memMgr.stack().push(right, OpSize::i16);
                break;
            case Op::DEREF4:
                left = _memMgr.stack().pop(AddrOpSize);
                right = _memMgr.getAbs(left, OpSize::i32);
                _memMgr.stack().push(right, OpSize::i32);
                break;
            case Op::RETR1:
                _returnValue = _memMgr.stack().pop(OpSize::i8);
                handleReturn();
                break;
            case Op::RETR2:
                _returnValue = _memMgr.stack().pop(OpSize::i16);
                handleReturn();
                break;
            case Op::RETR4:
                _returnValue = _memMgr.stack().pop(OpSize::i32);
                handleReturn();
                break;
            case Op::RET:
                handleReturn();
                break;
            case Op::PUSHR1:
                _memMgr.stack().push(_returnValue, OpSize::i8);
                break;
            case Op::PUSHR2:
                _memMgr.stack().push(_returnValue, OpSize::i16);
                break;
            case Op::PUSHR4:
                _memMgr.stack().push(_returnValue, OpSize::i32);
                break;
            case Op::POP1: {
                uint32_t v = _memMgr.stack().pop(OpSize::i8);
                _memMgr.setAbs(ea(OpSize::i8), v, OpSize::i8);
                break;
            }
            case Op::POP2: {
                uint32_t v = _memMgr.stack().pop(OpSize::i16);
                _memMgr.setAbs(ea(OpSize::i16), v, OpSize::i16);
                break;
            }
            case Op::POP4: {
                uint32_t v = _memMgr.stack().pop(OpSize::i32);
                _memMgr.setAbs(ea(OpSize::i32), v, OpSize::i32);
                break;
            }
            case Op::POPDEREF1: {
                uint32_t a = _memMgr.stack().pop(AddrOpSize);
                uint32_t v = _memMgr.stack().pop(OpSize::i8);
                _memMgr.setAbs(a, v, OpSize::i8);
                break;
            }
            case Op::POPDEREF2: {
                uint32_t a = _memMgr.stack().pop(AddrOpSize);
                uint32_t v = _memMgr.stack().pop(OpSize::i16);
                _memMgr.setAbs(a, v, OpSize::i16);
                break;
            }
            case Op::POPDEREF4: {
                uint32_t a = _memMgr.stack().pop(AddrOpSize);
                uint32_t v = _memMgr.stack().pop(OpSize::i32);
                _memMgr.setAbs(a, v, OpSize::i32);
                break;
            }
            case Op::PUSH1: _memMgr.stack().push(value(OpSize::i8), OpSize::i8); break;
            case Op::PUSH2: _memMgr.stack().push(value(OpSize::i16), OpSize::i16); break;
            case Op::PUSH4: _memMgr.stack().push(value(OpSize::i32), OpSize::i32); break;
            case Op::INDEX1:
            case Op::INDEX2: {
                uint8_t elementSize = getUOpnd(OpSize::i8);
                uint32_t index = _memMgr.stack().pop((opcode == Op::INDEX1) ? OpSize::i8 : OpSize::i16);
                AddrNativeType addr = _memMgr.stack().pop(AddrOpSize);
                addr += index * elementSize;
                _memMgr.stack().push(addr, AddrOpSize);
                break;
            }
            case Op::OFFSET1:
            case Op::OFFSET2: {
                uint8_t offset = getUOpnd((opcode == Op::OFFSET1) ? OpSize::i8 : OpSize::i16);
                AddrNativeType addr = _memMgr.stack().pop(AddrOpSize);
                addr += offset;
                _memMgr.stack().push(addr, AddrOpSize);
                break;
            }
            case Op::PREINC:
            case Op::PREDEC:
            case Op::POSTINC:
            case Op::POSTDEC: {
                AddrNativeType addr = _memMgr.stack().pop(AddrOpSize);
                uint32_t oldValue = _memMgr.getAbs(addr, opSize);
                uint32_t newValue;
                
                if (opSize == OpSize::flt) {
                    float oldFloatValue = intToFloat(oldValue);
                    float newFloatValue = oldFloatValue + ((opcode == Op::PREINC || opcode == Op::POSTINC) ? 1 : -1);
                    newValue = floatToInt(newFloatValue);
                } else {
                    newValue = oldValue + ((opcode == Op::PREINC || opcode == Op::POSTINC) ? 1 : -1);
                }
                _memMgr.setAbs(addr, newValue, opSize);
                _memMgr.stack().push((opcode == Op::PREINC || opcode == Op::PREDEC) ? newValue : oldValue, opSize);
                break;
            }
            case Op::DUP1    : _memMgr.stack().dup(OpSize::i8); break;
            case Op::DUP2    : _memMgr.stack().dup(OpSize::i16); break;
            case Op::DUP4    : _memMgr.stack().dup(OpSize::i32); break;
            case Op::ADD:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);
                
                if (opSize == OpSize::flt) {
                    _memMgr.stack().push(floatToInt(intToFloat(left) + intToFloat(right)), opSize);
                } else {
                    _memMgr.stack().push(left + right, opSize);
                }
                break;
            case Op::SUB:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);
                
                if (opSize == OpSize::flt) {
                    _memMgr.stack().push(floatToInt(intToFloat(left) - intToFloat(right)), opSize);
                } else {
                    _memMgr.stack().push(left - right, opSize);
                }
                break;
            case Op::IMUL:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);

                if (opSize == OpSize::flt) {
                    _memMgr.stack().push(floatToInt(intToFloat(left) * intToFloat(right)), opSize);
                } else {
                    _memMgr.stack().push(left * right, opSize);
                }
                break;
            case Op::UMUL:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);
                _memMgr.stack().push(uint32_t(left) * uint32_t(right), opSize);
                break;
            case Op::IDIV:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);

                if (opSize == OpSize::flt) {
                    _memMgr.stack().push(floatToInt(intToFloat(left) / intToFloat(right)), opSize);
                } else {
                    _memMgr.stack().push(left / right, opSize);
                }
                break;
            case Op::UDIV:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);
                _memMgr.stack().push(uint32_t(left) / uint32_t(right), opSize);
                break;
            case Op::AND:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);
                _memMgr.stack().push(left & right, opSize);
                break;
            case Op::OR:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);
                _memMgr.stack().push(left | right, opSize);
                break;
            case Op::XOR:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);
                _memMgr.stack().push(left ^ right, opSize);
                break;
            case Op::LAND:
                right = _memMgr.stack().pop(OpSize::i8);
                left = _memMgr.stack().pop(OpSize::i8);
                _memMgr.stack().push(left && right, OpSize::i8);
                break;
            case Op::LOR:
                right = _memMgr.stack().pop(OpSize::i8);
                left = _memMgr.stack().pop(OpSize::i8);
                _memMgr.stack().push(left || right, OpSize::i8);
                break;
            case Op::NOT:
                left = _memMgr.stack().pop(opSize);
                _memMgr.stack().push(~left, opSize);
                break;
            case Op::NEG:
                left = _memMgr.stack().pop(opSize);
                if (opSize == OpSize::flt) {
                    _memMgr.stack().push(floatToInt(-intToFloat(left)), opSize);
                } else {
                    _memMgr.stack().push(-left, opSize);
                }
                break;
            case Op::LNOT:
                left = _memMgr.stack().pop(OpSize::i8);
                _memMgr.stack().push(left == 0, OpSize::i8);
                break;
            case Op::LE:
            case Op::LS:
            case Op::LT:
            case Op::LO:
            case Op::GE:
            case Op::HS:
            case Op::GT:
            case Op::HI:
            case Op::EQ:
            case Op::NE:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);
                if (opSize == OpSize::flt) {
                    float l = intToFloat(left);
                    float r = intToFloat(right);
                    switch(opcode) {
                        case Op::LE: left = l <= r; break;
                        case Op::LS: _error = Error::InternalError; break;
                        case Op::LT: left = l < r; break;
                        case Op::LO: _error = Error::InternalError; break;
                        case Op::GE: left = l >= r; break;
                        case Op::HS: _error = Error::InternalError; break;
                        case Op::GT: left = l > r; break;
                        case Op::HI: _error = Error::InternalError; break;
                        case Op::EQ: left = l == r; break;
                        case Op::NE: left = l != r; break;
                        default: break;
                    }
                } else {
                    switch(opcode) {
                        case Op::LE: left = left <= right; break;
                        case Op::LS: left = uint32_t(left) <= uint32_t(right); break;
                        case Op::LT: left = left < right; break;
                        case Op::LO: left = uint32_t(left) < uint32_t(right); break;
                        case Op::GE: left = left >= right; break;
                        case Op::HS: left = uint32_t(left) >= uint32_t(right); break;
                        case Op::GT: left = left > right; break;
                        case Op::HI: left = uint32_t(left) > uint32_t(right); break;
                        case Op::EQ: left = left == right; break;
                        case Op::NE: left = left != right; break;
                        default: break;
                    }
                }
                _memMgr.stack().push(left, OpSize::i8);
                break;
            case Op::IF:
                right = getUOpnd(opSize);
                left = _memMgr.stack().pop(OpSize::i8);
                if (left == 0) {
                    _pc += right;
                }
                break;
            case Op::FBRA:
                right = getUOpnd(opSize);
                _pc += right;
                break;
            case Op::RBRA:
                right = getUOpnd(opSize);
                _pc -= right;
                break;
            case Op::SWITCH  : {
                uint16_t operand = getUOpnd(OpSize::i16);
                uint16_t n = operand >> 3;
                bool isLongAddr = (operand & 0x04) != 0;
                OpSize opSize = OpSize(operand & 0x03);
                
                uint32_t value = _memMgr.stack().pop(opSize);
                
                // We're pointing at the jump table, do a binary search
                AddrNativeType addr = 0;
                
                switch ((opSizeToBytes(opSize) << 4) | (isLongAddr ? 2 : 1)) {
                    case 0x11: addr = switchSearch<1, 1>(value, _pc, n); break;
                    case 0x21: addr = switchSearch<2, 1>(value, _pc, n); break;
                    case 0x41: addr = switchSearch<4, 1>(value, _pc, n); break;
                    case 0x12: addr = switchSearch<1, 2>(value, _pc, n); break;
                    case 0x22: addr = switchSearch<2, 2>(value, _pc, n); break;
                    case 0x42: addr = switchSearch<4, 2>(value, _pc, n); break;
                }
                
                if (addr == 0) {
                    // If there's no match, this is the default clause, which is
                    // right after the jump table
                    _pc += n * (opSizeToBytes(opSize) + (isLongAddr ? 2 : 1));
                } else {
                    _pc += addr;
                }
                break;
            }
            case Op::CALL:
                right = getUOpnd(OpSize::i16);
                _memMgr.stack().push(_pc, AddrOpSize);
                _pc = right;
                break;
            case Op::DROPS   : _memMgr.stack().drop(operand + 1); break;
            case Op::DROP1   : left = getUOpnd(OpSize::i8); _memMgr.stack().drop(left); break;
            case Op::DROP2   : left = getUOpnd(OpSize::i16); _memMgr.stack().drop(left); break;

            case Op::ENTER   : operand = getUOpnd(opSize);
            case Op::ENTERS  : _memMgr.setFrame(operand); break;
            
            case Op::NCALL   : callNative(getUOpnd(opSize)); break;
            
            case Op::PUSHK11 : left = getIOpnd(OpSize::i8); _memMgr.stack().push(left, OpSize::i8); break;
            case Op::PUSHK12 : left = getIOpnd(OpSize::i8); _memMgr.stack().push(left, OpSize::i16); break;
            case Op::PUSHK14 : left = getIOpnd(OpSize::i8); _memMgr.stack().push(left, OpSize::i32); break;
            case Op::PUSHK22 : left = getIOpnd(OpSize::i16); _memMgr.stack().push(left, OpSize::i16); break;
            case Op::PUSHK24 : left = getIOpnd(OpSize::i16); _memMgr.stack().push(left, OpSize::i32); break;
            case Op::PUSHK44 : left = getIOpnd(OpSize::i32); _memMgr.stack().push(left, OpSize::i32); break;
            case Op::PUSHKS1:
                left = operand;
                _memMgr.stack().push(left, OpSize::i8);
                break;
            case Op::PUSHKS2:
                left = operand;
                if (left & 0x08) {
                    left |= 0xfffffff0;
                }
                _memMgr.stack().push(left, OpSize::i16);
                break;
            case Op::PUSHKS4:
                left = operand;
                if (left & 0x08) {
                    left |= 0xfffffff0;
                }
                _memMgr.stack().push(left, OpSize::i32);
                break;
            case Op::PUSHS: {
                // String is in ROM memory. Push a pointer to it and
                // set pc past it.
                uint8_t size = getUInt8ROM(_pc++);
                _memMgr.stack().push(_pc, AddrOpSize);
                _pc += size;
                break;
            }
            case Op::CASTF8   : typeCast(Type::Float,  Type::Int8); break;
            case Op::CASTF16  : typeCast(Type::Float,  Type::Int16); break;
            case Op::CASTF32  : typeCast(Type::Float,  Type::Int32); break;
            case Op::CAST32F  : typeCast(Type::Int32,  Type::Float); break;
            case Op::CAST3216 : typeCast(Type::Int32,  Type::Int16); break;
            case Op::CAST328  : typeCast(Type::Int32,  Type::Int8); break;
            case Op::CAST168  : typeCast(Type::Int16,  Type::Int8); break;
            case Op::CASTU16F : typeCast(Type::UInt16, Type::Float); break;
            case Op::CASTU1632: typeCast(Type::UInt16, Type::Int32); break;
            case Op::CASTI16F : typeCast(Type::Int16,  Type::Float); break;
            case Op::CASTI1632: typeCast(Type::Int16,  Type::Int32); break;
            case Op::CASTU8F  : typeCast(Type::UInt8,  Type::Float); break;
            case Op::CASTU832 : typeCast(Type::UInt8,  Type::Int32); break;
            case Op::CASTU816 : typeCast(Type::UInt8,  Type::Int16); break;
            case Op::CASTI8F  : typeCast(Type::Int8,   Type::Float); break;
            case Op::CASTI832 : typeCast(Type::Int8,   Type::Int32); break;
            case Op::CASTI816 : typeCast(Type::Int8,   Type::Int16); break;
        }
    }
}
