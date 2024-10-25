/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "NativeColor.h"

#include "Interpreter.h"

#include <math.h>

using namespace clvr;

#if COMPILE == 1
ModulePtr
NativeColor::createModule()
{
    ModulePtr module = std::make_shared<NativeColor>();
    module->addNativeFunction("setLight", uint16_t(Id::SetLight), Type::None, {{ "i", Type::UInt8, false, 1, 1 }, { "c", Type::None, true, 1, 1 }});
    return module;
}
#endif
#if RUNTIME == 1
void
NativeColor::callNative(uint16_t id, InterpreterBase* interp)
{
    switch (Id(id)) {
        case Id::SetLight: {
            // First arg is byte index of light to set. Next is a ptr to a struct of
            // h, s, v byte values (0-255)
            uint8_t i = interp->memMgr()->getArg(0, 1);
            AddrNativeType addr = interp->memMgr()->getArg(1, AddrSize);
            uint8_t h = interp->memMgr()->getAbs(addr, 1);
            uint8_t s = interp->memMgr()->getAbs(addr + 1, 1);
            uint8_t v = interp->memMgr()->getAbs(addr + 2, 1);
            
            interp->setLight(i, h, s, v);
            break;
        }
    }
}

#endif
