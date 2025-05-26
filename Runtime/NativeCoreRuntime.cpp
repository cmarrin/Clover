/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "NativeCore.h"

#include "Interpreter.h"

using namespace clvr;

void
NativeCore::callNative(uint16_t id, InterpreterBase* interp)
{
    switch (Id(id)) {
        default: break;
        case Id::Monitor: {
            // General Monitor access
            uint16_t cmd = interp->memMgr()->getArg(0, 2);
            uint16_t param0 = interp->memMgr()->getArg(VarArgSize, 2);
            uint16_t param1 = interp->memMgr()->getArg(VarArgSize * 2, 2);
            (void) param0;
            (void) param1;
            switch (cmd) {
                default: break;
                case 0:
                    break;
            }
            break;
        }
        case Id::PrintF: {
            VarArg va(interp->memMgr(), 0, Type::UInt8, true);
            AddrNativeType fmt = interp->memMgr()->getArg(0, AddrSize);
            clvr::printf(fmt, va);
            break;
        }
        case Id::Format: {
            VarArg va(interp->memMgr(), AddrSize + 2, Type::UInt8, true);
            AddrNativeType s = interp->memMgr()->getArg(0, AddrSize);
            uint16_t n = interp->memMgr()->getArg(AddrSize, 2);
            AddrNativeType fmt = interp->memMgr()->getArg(AddrSize + 2, AddrSize);
            clvr::format(s, n, fmt, va);
            break;
        }
        case Id::RandomInt: {
            int32_t a = interp->memMgr()->getArg(0, 4);
            int32_t b = interp->memMgr()->getArg(4, 4);
            interp->setReturnValue(random(a, b));
            break;
        }
        case Id::MemSet: {
            AddrNativeType addr = interp->memMgr()->getArg(0, AddrSize);
            uint8_t v = interp->memMgr()->getArg(AddrSize, 1);
            uint32_t n = interp->memMgr()->getArg(AddrSize + 1, 4);
            if (n == 0) {
                break;
            }
            while (n--) {
                interp->memMgr()->setAbs(addr++, v, OpSize::i8);
            }
            break;
        }
        case Id::MinInt: {
            int32_t a = interp->memMgr()->getArg(0, 4);
            int32_t b = interp->memMgr()->getArg(4, 4);
            
            interp->setReturnValue((a < b) ? a : b);
            break;
        }
        case Id::MaxInt: {
            int32_t a = interp->memMgr()->getArg(0, 4);
            int32_t b = interp->memMgr()->getArg(4, 4);
            
            interp->setReturnValue((a > b) ? a : b);
            break;
        }
        case Id::InitArgs:
            interp->topLevelArgs()->reset();
            break;
        case Id::ArgInt8:
            interp->setReturnValue(interp->topLevelArgs()->arg(1));
            break;
        case Id::ArgInt16:
            interp->setReturnValue(interp->topLevelArgs()->arg(2));
            break;
        case Id::ArgInt32:
            interp->setReturnValue(interp->topLevelArgs()->arg(4));
            break;
        case Id::ArgFloat:
            interp->setReturnValue(interp->topLevelArgs()->arg(4));
            break;
    }
}
