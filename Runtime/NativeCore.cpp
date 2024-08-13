/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "NativeCore.h"

#include "Formatter.h"
#include "Interpreter.h"

using namespace lucid;

ModulePtr
NativeCore::createModule()
{
    ModulePtr coreModule = std::make_shared<NativeCore>();
    coreModule->addNativeFunction("printf", uint16_t(Id::PrintF), Type::None, {{ "s", Type::String, true, 1, 1 }});
    coreModule->addNativeFunction("memset", uint16_t(Id::MemSet), Type::None, {{ "p", Type::None, true, 1, 1 },
                                                                           { "v", Type::UInt8, false, 1, 1 },
                                                                           { "n", Type::UInt32, false, 1, 1 }});
    coreModule->addNativeFunction("irand", uint16_t(Id::RandomInt), Type::Int32, {{ "min", Type::Int32, false, 1, 1 }, { "max", Type::Int32, false, 1, 1 }});
    coreModule->addNativeFunction("frand", uint16_t(Id::RandomFloat), Type::Float, {{ "min", Type::Float, false, 1, 1 }, { "max", Type::Float, false, 1, 1 }});
    coreModule->addNativeFunction("imin", uint16_t(Id::MinInt), Type::Int32, {{ "a", Type::Int32, false, 1, 1 }, { "b", Type::Int32, false, 1, 1 }});
    coreModule->addNativeFunction("imax", uint16_t(Id::MaxInt), Type::Int32, {{ "a", Type::Int32, false, 1, 1 }, { "b", Type::Int32, false, 1, 1 }});
    coreModule->addNativeFunction("fmin", uint16_t(Id::MinFloat), Type::Float, {{ "a", Type::Float, false, 1, 1 }, { "b", Type::Float, false, 1, 1 }});
    coreModule->addNativeFunction("fmax", uint16_t(Id::MaxFloat), Type::Float, {{ "a", Type::Float, false, 1, 1 }, { "b", Type::Float, false, 1, 1 }});
    coreModule->addNativeFunction("initArgs", uint16_t(Id::InitArgs), Type::None, { });
    coreModule->addNativeFunction("argint8", uint16_t(Id::ArgInt8), Type::Int8, { });
    coreModule->addNativeFunction("argint16", uint16_t(Id::ArgInt16), Type::Int16, { });
    coreModule->addNativeFunction("argint32", uint16_t(Id::ArgInt32), Type::Int32, { });
    coreModule->addNativeFunction("argFloat", uint16_t(Id::ArgFloat), Type::Float, { });
    coreModule->addNativeFunction("animate", uint16_t(Id::Animate), Type::Int8, { });
    return coreModule;
}

void
NativeCore::callNative(uint16_t id, InterpreterBase* interp)
{
    switch (Id(id)) {
        default: break;
        case Id::PrintF: {
            VarArg va(interp->memMgr(), 0, Type::String);
            AddrNativeType fmt = interp->memMgr()->getLocal(0, AddrType);
            fmt::Formatter::format(fmt, va);
            break;
        }
        case Id::RandomInt: {
            int32_t a = interp->memMgr()->getLocal(0, Type::Int32);
            int32_t b = interp->memMgr()->getLocal(4, Type::Int32);
            interp->setReturnValue(random(a, b));
            break;
        }
        case Id::RandomFloat: {
            float a = intToFloat(interp->memMgr()->getLocal(0, Type::Float));
            float b = intToFloat(interp->memMgr()->getLocal(4, Type::Float));
            interp->setReturnValue(floatToInt(float(random(int32_t(a * 1000), int32_t(b * 1000))) / 1000));
            break;
        }
        case Id::MemSet: {
            AddrNativeType addr = interp->memMgr()->getLocal(0, AddrType);
            uint8_t v = interp->memMgr()->getLocal(AddrSize, Type::UInt8);
            uint32_t n = interp->memMgr()->getLocal(AddrSize + 1, Type::UInt32);
            if (n == 0) {
                break;
            }
            while (n--) {
                interp->memMgr()->setAbs(addr++, v, OpSize::i8);
            }
            break;
        }
        case Id::MinInt: {
            int32_t a = interp->memMgr()->getLocal(0, Type::Int32);
            int32_t b = interp->memMgr()->getLocal(4, Type::Int32);
            
            interp->setReturnValue((a < b) ? a : b);
            break;
        }
        case Id::MaxInt: {
            int32_t a = interp->memMgr()->getLocal(0, Type::Int32);
            int32_t b = interp->memMgr()->getLocal(4, Type::Int32);
            
            interp->setReturnValue((a > b) ? a : b);
            break;
        }
        case Id::MinFloat: {
            float a = intToFloat(interp->memMgr()->getLocal(0, Type::Float));
            float b = intToFloat(interp->memMgr()->getLocal(4, Type::Float));
            interp->setReturnValue(floatToInt((a < b) ? a : b));
            break;
        }
        case Id::MaxFloat: {
            float a = intToFloat(interp->memMgr()->getLocal(0, Type::Float));
            float b = intToFloat(interp->memMgr()->getLocal(4, Type::Float));
            interp->setReturnValue(floatToInt((a > b) ? a : b));
            break;
        }
        case Id::InitArgs:
            interp->topLevelArgs()->reset();
            break;
        case Id::ArgInt8:
            interp->setReturnValue(interp->topLevelArgs()->arg(Type::Int8));
            break;
        case Id::ArgInt16:
            interp->setReturnValue(interp->topLevelArgs()->arg(Type::Int16));
            break;
        case Id::ArgInt32:
            interp->setReturnValue(interp->topLevelArgs()->arg(Type::Int32));
            break;
        case Id::ArgFloat:
            interp->setReturnValue(interp->topLevelArgs()->arg(Type::Float));
            break;
        case Id::Animate: {
            // Passes in a pointer to a struct with cur, inc, min and max
            // values, all floats. Perform one animation iteration of cur
            // by adding inc to it. When it hits min or max, negate inc
            // to go the other direction next time. Return -1 if we just
            // finished going down, or 1 if we just finished going up.
            // Otherwise return 0.
            float cur, inc, min, max;
            AddrNativeType addr = interp->memMgr()->getLocal(0, AddrType);
            cur = intToFloat(interp->memMgr()->getAbs(addr, OpSize::flt));
            inc = intToFloat(interp->memMgr()->getAbs(addr + 4, OpSize::flt));
            min = intToFloat(interp->memMgr()->getAbs(addr + 8, OpSize::flt));
            max = intToFloat(interp->memMgr()->getAbs(addr + 12, OpSize::flt));

            cur += inc;
            interp->memMgr()->setAbs(addr, floatToInt(cur), OpSize::flt);
            interp->setReturnValue(0);

            if (0 < inc) {
                if (cur >= max) {
                    cur = max;
                    inc = -inc;
                    interp->memMgr()->setAbs(addr, floatToInt(cur), OpSize::flt);
                    interp->memMgr()->setAbs(addr + 4, floatToInt(inc), OpSize::flt);
                    interp->setReturnValue(1);
                }
            } else {
                if (cur <= min) {
                    cur = min;
                    inc = -inc;
                    interp->memMgr()->setAbs(addr, floatToInt(cur), OpSize::flt);
                    interp->memMgr()->setAbs(addr + 4, floatToInt(inc), OpSize::flt);
                    interp->setReturnValue(-1);
                }
            }
            break;
        }
    }
}