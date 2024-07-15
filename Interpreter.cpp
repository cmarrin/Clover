/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "Interpreter.h"
#include "Module.h"

using namespace lucid;

void
InterpreterBase::callNative(NativeId id)
{
    switch (id) {
        default: _error = Error::None; break;
        case NativeId::PrintF:
            break;
    }
}

int32_t
InterpreterBase::execute(uint16_t addr)
{
    _pc = addr;
    
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
            opSize = OpSize(operand);
        } else if (opInt < FoutBitOperandStart) {
            operand = opInt & 0x03;
            opcode = Op(opInt & 0xfc);
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
                _memMgr.stack().push(ea(), AddrOpSize);
                break;
            case Op::RET:
                restoreFrame();
                if (_memMgr.stack().empty()) {
                    return 0;
                }
                _pc = _memMgr.stack().pop(AddrOpSize);
                break;
            case Op::DEREF: {
                uint32_t v = _memMgr.stack().pop(opSize);
                uint32_t a = _memMgr.stack().pop(AddrOpSize);
                _memMgr.setAbs(a, v, opSize);
                break;
            }
            case Op::PREINC:
                addr = ea();
            case Op::PREDEC  :
            case Op::POSTINC :
            case Op::POSTDEC :
            case Op::PUSH    : _memMgr.stack().push(value(opSize), opSize); break;
            case Op::DUP     : _memMgr.stack().dup(opSize); break;
            case Op::SWAP    : _memMgr.stack().swap(opSize); break;
            case Op::ADD:
                right = _memMgr.stack().pop(opSize);
                left = _memMgr.stack().pop(opSize);
                _memMgr.stack().push(left + right, opSize);
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
            case Op::LE      :
            case Op::LS      :
            case Op::LT      :
            case Op::LO      :
            case Op::GE      :
            case Op::HS      :
            case Op::GT      :
            case Op::HI      :
            case Op::EQ      :
            case Op::NE      :
            
            case Op::IF      : getOpnd(operand); break; // FIXME: Implement
            case Op::BRA     : getOpnd(operand); break; // FIXME: Implement
            case Op::CALL    : getOpnd(operand); break; // FIXME: Implement
            case Op::DROP1   : left = getOpnd(0); _memMgr.stack().drop(left); break;
            case Op::DROP2   : left = getOpnd(1); _memMgr.stack().drop(left); break;

            case Op::ENTER   : operand = getOpnd(operand);
            case Op::ENTERS  : setFrame(operand); break;
            
            case Op::NCALL   : operand = getOpnd(operand);
            case Op::NCALLS  : callNative(NativeId(operand)); break;
            
            case Op::PUSHK11 : left = getOpnd(1); _memMgr.stack().push(left, OpSize::i8); break;
            case Op::PUSHK12 : left = getOpnd(1); _memMgr.stack().push(left, OpSize::i16); break;
            case Op::PUSHK14 : left = getOpnd(1); _memMgr.stack().push(left, OpSize::i32); break;
            case Op::PUSHK22 : left = getOpnd(2); _memMgr.stack().push(left, OpSize::i16); break;
            case Op::PUSHK24 : left = getOpnd(2); _memMgr.stack().push(left, OpSize::i32); break;
            case Op::PUSHK34 : left = getOpnd(3); _memMgr.stack().push(left, OpSize::i32); break;
            case Op::PUSHK44 : left = getOpnd(4); _memMgr.stack().push(left, OpSize::i32); break;
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
            case Op::TOF   : left = _memMgr.stack().pop(opSize); _memMgr.stack().push(float(left), OpSize::flt); break;
            case Op::TOU8  : left = _memMgr.stack().pop(opSize); _memMgr.stack().push(uint8_t(left), OpSize::i8); break;
            case Op::TOI8  : left = _memMgr.stack().pop(opSize); _memMgr.stack().push(int8_t(left), OpSize::i8); break;
            case Op::TOU16 : left = _memMgr.stack().pop(opSize); _memMgr.stack().push(uint16_t(left), OpSize::i16); break;
            case Op::TOI16 : left = _memMgr.stack().pop(opSize); _memMgr.stack().push(int16_t(left), OpSize::i16); break;
            case Op::TOU32 : left = _memMgr.stack().pop(opSize); _memMgr.stack().push(uint32_t(left), OpSize::i32); break;
            case Op::TOI32 : left = _memMgr.stack().pop(opSize); _memMgr.stack().push(int32_t(left), OpSize::i32); break;
        }
    }
}
