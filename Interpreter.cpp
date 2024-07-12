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

        if (opInt < OneBitOperandStart) {
            opcode = Op(opInt);
        } else if (opInt < TwoBitOperandStart) {
            operand = opInt & 0x01;
            opcode = Op(opInt & 0xfe);
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
                _memMgr.stack().push(ea(operand), 4);
                break;
            case Op::RET:
                restoreFrame();
                _pc = _memMgr.stack().pop(4);
                break;
            case Op::DEREF: {
                uint32_t v = _memMgr.stack().pop(operand);
                uint32_t a = _memMgr.stack().pop(4);
                _memMgr.setAbs(a, v, operand);
                break;
            }
            case Op::PREINC:
                addr = ea(operand);
            case Op::PREDEC  :
            case Op::POSTINC :
            case Op::POSTDEC :
            case Op::PUSH    : _memMgr.stack().push(value(operand), operand); break;
            case Op::DUP     : _memMgr.stack().dup(operand); break;
            case Op::DROP    : _memMgr.stack().drop(operand); break;
            case Op::SWAP    : _memMgr.stack().swap(operand); break;
            case Op::ADD:
                right = _memMgr.stack().pop(operand);
                left = _memMgr.stack().pop(operand);
                _memMgr.stack().push(left + right, operand);
                break;
            case Op::SUB:
                right = _memMgr.stack().pop(operand);
                left = _memMgr.stack().pop(operand);
                _memMgr.stack().push(left - right, operand);
                break;
            case Op::IMUL:
                right = _memMgr.stack().pop(operand);
                left = _memMgr.stack().pop(operand);
                _memMgr.stack().push(left * right, operand);
                break;
            case Op::UMUL:
                right = _memMgr.stack().pop(operand);
                left = _memMgr.stack().pop(operand);
                _memMgr.stack().push(uint32_t(left) * uint32_t(right), operand);
                break;
            case Op::IDIV:
                right = _memMgr.stack().pop(operand);
                left = _memMgr.stack().pop(operand);
                _memMgr.stack().push(left / right, operand);
                break;
            case Op::UDIV:
                right = _memMgr.stack().pop(operand);
                left = _memMgr.stack().pop(operand);
                _memMgr.stack().push(uint32_t(left) / uint32_t(right), operand);
                break;
            case Op::AND:
                right = _memMgr.stack().pop(operand);
                left = _memMgr.stack().pop(operand);
                _memMgr.stack().push(left & right, operand);
            case Op::OR:
                right = _memMgr.stack().pop(operand);
                left = _memMgr.stack().pop(operand);
                _memMgr.stack().push(left | right, operand);
            case Op::XOR:
                right = _memMgr.stack().pop(operand);
                left = _memMgr.stack().pop(operand);
                _memMgr.stack().push(left ^ right, operand);
            case Op::NOT     :
                left = _memMgr.stack().pop(operand);
                _memMgr.stack().push(~left, operand);
            case Op::NEG     :
                left = _memMgr.stack().pop(operand);
                _memMgr.stack().push(-left, operand);
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
            case Op::IF      :
            case Op::BRA     :
            case Op::CALL    :
            case Op::ENTER   :
            case Op::ENTERS  :
            case Op::NCALL   :
            case Op::NCALLS  :
            case Op::PUSHS   :
            case Op::PUSHK11 : left = getOpnd(1); _memMgr.stack().push(left, 1); break;
            case Op::PUSHK12 : left = getOpnd(1); _memMgr.stack().push(left, 2); break;
            case Op::PUSHK14 : left = getOpnd(1); _memMgr.stack().push(left, 4); break;
            case Op::PUSHK22 : left = getOpnd(2); _memMgr.stack().push(left, 2); break;
            case Op::PUSHK24 : left = getOpnd(2); _memMgr.stack().push(left, 4); break;
            case Op::PUSHK34 : left = getOpnd(3); _memMgr.stack().push(left, 4); break;
            case Op::PUSHK44 : left = getOpnd(4); _memMgr.stack().push(left, 4); break;
            case Op::PUSHKS1:
                left = operand;
                if (left & 0x08) {
                    left |= 0xfffffff0;
                }
                _memMgr.stack().push(left, 1);
                break;
            case Op::PUSHKS2:
                left = operand;
                if (left & 0x08) {
                    left |= 0xfffffff0;
                }
                _memMgr.stack().push(left, 2);
                break;
            case Op::PUSHKS4:
                left = operand;
                if (left & 0x08) {
                    left |= 0xfffffff0;
                }
                _memMgr.stack().push(left, 4);
                break;
            case Op::TOF   : left = _memMgr.stack().pop(operand); _memMgr.stack().push(float(left), 4); break;
            case Op::TOU8  : left = _memMgr.stack().pop(operand); _memMgr.stack().push(uint8_t(left), 1); break;
            case Op::TOI8  : left = _memMgr.stack().pop(operand); _memMgr.stack().push(int8_t(left), 1); break;
            case Op::TOU16 : left = _memMgr.stack().pop(operand); _memMgr.stack().push(uint16_t(left), 2); break;
            case Op::TOI16 : left = _memMgr.stack().pop(operand); _memMgr.stack().push(int16_t(left), 2); break;
            case Op::TOU32 : left = _memMgr.stack().pop(operand); _memMgr.stack().push(uint32_t(left), 4); break;
            case Op::TOI32 : left = _memMgr.stack().pop(operand); _memMgr.stack().push(int32_t(left), 4); break;
        }
    }
}
