/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "Interpreter.h"

#include "Formatter.h"
#include "Module.h"

using namespace lucid;

static int32_t typeCast(int32_t v, OpSize from, Type to)
{
    // handle from float case
    if (from == OpSize::flt) {
        float f = intToFloat(v);
        if (to != Type::Float) {
            v = int32_t(f);
        }
        return v;
    }
    
    // handle to float case (assume from is int)
    if (to == Type::Float) {
        float f = float(v);
        return floatToInt(f);
    }
    
    // This is just an int to int case, nothing to do
    return v;
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
InterpreterBase::callNative(NativeId id)
{
    // Add a dummy return address to make everything work
    _memMgr.stack().push(0, AddrOpSize);
    
    // Set the frame
    _memMgr.setFrame(0);
    
    switch (id) {
        default: _error = Error::None; break;
        case NativeId::PrintF: {
            VarArg va(&_memMgr, 0, Type::String);
            AddrNativeType fmt = _memMgr.getLocal(0, AddrType);
            fmt::Formatter::format([this](uint8_t c) { putc(c); }, fmt, va);
            break;
        }
        case NativeId::RandomInt: {
            int32_t a = _memMgr.getLocal(0, Type::Int32);
            int32_t b = _memMgr.getLocal(4, Type::Int32);
            _returnValue = random(a, b);
            break;
        }
        case NativeId::RandomFloat: {
            float a = intToFloat(_memMgr.getLocal(0, Type::Float));
            float b = intToFloat(_memMgr.getLocal(4, Type::Float));
            _returnValue = floatToInt(float(random(int32_t(a * 1000), int32_t(b * 1000))) / 1000);
            break;
        }
    }

    // Restore the frame and pop the dummy return address
    _memMgr.restoreFrame();
    _memMgr.stack().pop(AddrOpSize);
}

int32_t
InterpreterBase::execute()
{
    // Check signature
    if (getUInt8ROM(0) != 'l' || getUInt8ROM(1) != 'u' || getUInt8ROM(2) != 'c' || getUInt8ROM(3) != 'd') {
        _error = Error::InvalidSignature;
        return -1;
    }
    
    AddrNativeType entryPoint = 0;
    entryPoint |= uint32_t(getUInt8ROM(4)) << 24;
    entryPoint |= uint32_t(getUInt8ROM(5)) << 16;
    entryPoint |= uint32_t(getUInt8ROM(6)) << 8;
    entryPoint |= uint32_t(getUInt8ROM(7));
    
    if (entryPoint == 0) {
        _error = Error::NoEntryPoint;
        return -1;
    }
    
    // Push a dummy return address
    _memMgr.stack().push(0, AddrOpSize);
    
    _pc = entryPoint;
    
    while(1) {
        if (_memMgr.error() != Memory::Error::None) {
            // FIXME: Deal with errors
            // _error = _memMgr.error();
        }
        if (_error != Error::None) {
            _errorAddr = _pc - 1;
            return -1;
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
        } else if (opInt < FoutBitOperandStart) {
            operand = opInt & 0x03;
            opcode = Op(opInt & 0xfc);
            opSize = OpSize(operand);
        } else {
            operand = opInt & 0x0f;
            opcode = Op(opInt & 0xf0);
        }
        
        int32_t left, right;
        uint32_t addr;
        
        switch (opcode) {
            case Op::NOP:
                break;
            case Op::PUSHREF:
                _memMgr.stack().push(ea(opSize), AddrOpSize);
                break;
            case Op::RET:
                _memMgr.restoreFrame();
                _pc = _memMgr.stack().pop(AddrOpSize);
                
                // If return address is 0, we exit
                if (_pc == 0) {
                    return 0;
                }
                break;
            case Op::PUSHR:
                _memMgr.stack().push(_returnValue, OpSize::i32);
                break;
            case Op::DEREF: {
                uint32_t v = _memMgr.stack().pop(opSize);
                uint32_t a = _memMgr.stack().pop(AddrOpSize);
                _memMgr.setAbs(a, v, opSize);
                break;
            }
            case Op::PREINC:
                addr = ea(opSize);
            case Op::PREDEC  :
            case Op::POSTINC :
            case Op::POSTDEC :
            case Op::PUSH    : _memMgr.stack().push(value(opSize), opSize); break;
            case Op::DUP     : _memMgr.stack().dup(opSize); break;
            case Op::SWAP    : _memMgr.stack().swap(opSize); break;
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
                _memMgr.stack().push(left - right, opSize);
                break;
            case Op::IMUL:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);
                _memMgr.stack().push(left * right, opSize);
                break;
            case Op::UMUL:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);
                _memMgr.stack().push(uint32_t(left) * uint32_t(right), opSize);
                break;
            case Op::IDIV:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);
                _memMgr.stack().push(left / right, opSize);
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
            case Op::OR:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);
                _memMgr.stack().push(left | right, opSize);
            case Op::XOR:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);
                _memMgr.stack().push(left ^ right, opSize);
            case Op::NOT     :
                left = _memMgr.stack().pop(opSize);
                _memMgr.stack().push(~left, opSize);
            case Op::NEG     :
                left = _memMgr.stack().pop(opSize);
                _memMgr.stack().push(-left, opSize);
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
                        case Op::LE: l = l <= r; break;
                        case Op::LS: _error = Error::InternalError; break;
                        case Op::LT: l = l < r; break;
                        case Op::LO: _error = Error::InternalError; break;
                        case Op::GE: l = l >= r; break;
                        case Op::HS: _error = Error::InternalError; break;
                        case Op::GT: l = l > r; break;
                        case Op::HI: _error = Error::InternalError; break;
                        case Op::EQ: l = l == r; break;
                        case Op::NE: l = l != r; break;
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
                right = getOpnd(operand);
                left = _memMgr.stack().pop(OpSize::i8);
                if (left == 0) {
                    _pc += right - 2;
                }
                break;
            case Op::BRA:
                right = getOpnd(operand);
                _pc += right - 2;
                break;
            case Op::CALL:
                right = getOpnd(operand);
                _memMgr.stack().push(_pc, AddrOpSize);
                _pc += right;
                break;
            case Op::DROP1   : left = getOpnd(0); _memMgr.stack().drop(left); break;
            case Op::DROP2   : left = getOpnd(1); _memMgr.stack().drop(left); break;

            case Op::ENTER   : operand = getOpnd(operand);
            case Op::ENTERS  : _memMgr.setFrame(operand); break;
            
            case Op::NCALL   : operand = getOpnd(operand);
            case Op::NCALLS  : callNative(NativeId(operand)); break;
            
            case Op::PUSHK11 : left = getOpnd(0); _memMgr.stack().push(left, OpSize::i8); break;
            case Op::PUSHK12 : left = getOpnd(0); _memMgr.stack().push(left, OpSize::i16); break;
            case Op::PUSHK14 : left = getOpnd(0); _memMgr.stack().push(left, OpSize::i32); break;
            case Op::PUSHK22 : left = getOpnd(1); _memMgr.stack().push(left, OpSize::i16); break;
            case Op::PUSHK24 : left = getOpnd(1); _memMgr.stack().push(left, OpSize::i32); break;
            case Op::PUSHK34 : left = getOpnd(2); _memMgr.stack().push(left, OpSize::i32); break;
            case Op::PUSHK44 : left = getOpnd(3); _memMgr.stack().push(left, OpSize::i32); break;
            case Op::PUSHKS1:
                left = operand;
                if (left & 0x08) {
                    left |= 0xfffffff0;
                }
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
            case Op::TOF   : left = _memMgr.stack().pop(opSize); _memMgr.stack().push(typeCast(left, opSize, Type::Float), OpSize::flt); break;
            case Op::TOU8  : left = _memMgr.stack().pop(opSize); _memMgr.stack().push(typeCast(left, opSize, Type::UInt8), OpSize::i8); break;
            case Op::TOI8  : left = _memMgr.stack().pop(opSize); _memMgr.stack().push(typeCast(left, opSize, Type::Int8), OpSize::i8); break;
            case Op::TOU16 : left = _memMgr.stack().pop(opSize); _memMgr.stack().push(typeCast(left, opSize, Type::UInt16), OpSize::i16); break;
            case Op::TOI16 : left = _memMgr.stack().pop(opSize); _memMgr.stack().push(typeCast(left, opSize, Type::Int16), OpSize::i16); break;
            case Op::TOU32 : left = _memMgr.stack().pop(opSize); _memMgr.stack().push(typeCast(left, opSize, Type::UInt32), OpSize::i32); break;
            case Op::TOI32 : left = _memMgr.stack().pop(opSize); _memMgr.stack().push(typeCast(left, opSize, Type::Int32), OpSize::i32); break;
        }
    }
}
