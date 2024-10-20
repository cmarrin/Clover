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

#if COMPILE == 1
ModulePtr
NativeCore::createModule()
{
    ModulePtr coreModule = std::make_shared<NativeCore>();
    coreModule->addNativeFunction("printf", uint16_t(Id::PrintF), Type::None, {{ "fmt", Type::String, true, AddrSize, 1 }});
    coreModule->addNativeFunction("format", uint16_t(Id::Format), Type::None, {{ "s", Type::UInt8, true, AddrSize, 1 },
                                                                           { "n", Type::UInt16, false, 2, 1 },
                                                                           { "fmt", Type::String, true, AddrSize, 1 }});
    coreModule->addNativeFunction("memset", uint16_t(Id::MemSet), Type::None, {{ "p", Type::None, true, 1, 1 },
                                                                           { "v", Type::UInt8, false, 1, 1 },
                                                                           { "n", Type::UInt32, false, 1, 1 }});
    coreModule->addNativeFunction("irand", uint16_t(Id::RandomInt), Type::Int32, {{ "min", Type::Int32, false, 1, 1 }, { "max", Type::Int32, false, 1, 1 }});
    coreModule->addNativeFunction("imin", uint16_t(Id::MinInt), Type::Int32, {{ "a", Type::Int32, false, 1, 1 }, { "b", Type::Int32, false, 1, 1 }});
    coreModule->addNativeFunction("imax", uint16_t(Id::MaxInt), Type::Int32, {{ "a", Type::Int32, false, 1, 1 }, { "b", Type::Int32, false, 1, 1 }});
    coreModule->addNativeFunction("initArgs", uint16_t(Id::InitArgs), Type::None, { });
    coreModule->addNativeFunction("argint8", uint16_t(Id::ArgInt8), Type::Int8, { });
    coreModule->addNativeFunction("argint16", uint16_t(Id::ArgInt16), Type::Int16, { });
    coreModule->addNativeFunction("argint32", uint16_t(Id::ArgInt32), Type::Int32, { });
    coreModule->addNativeFunction("argFloat", uint16_t(Id::ArgFloat), Type::Float, { });
    return coreModule;
}
#endif
#if RUNTIME == 1
void
NativeCore::callNative(uint16_t id, InterpreterBase* interp)
{
    switch (Id(id)) {
        default: break;
        case Id::PrintF: {
            VarArg va(interp->memMgr(), 0, Type::String);
            AddrNativeType fmt = interp->memMgr()->getArg(0, AddrSize);
            clvr::printf(fmt, va);
            break;
        }
        case Id::Format: {
            VarArg va(interp->memMgr(), AddrSize + 2, Type::String);
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
#endif
