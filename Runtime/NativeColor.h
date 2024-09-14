/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// NativeColor
//
// static color specific methods

#pragma once

#ifndef RUNTIME
#include "Module.h"
#endif

#include <stdint.h>

namespace clvr {

class InterpreterBase;

class NativeColor
#ifndef RUNTIME
    : public Module
#endif
{
public:
    enum class Id {
        LoadColorArg  = 0,
        SetLight      = 1,
    };
    
    NativeColor()
#ifndef RUNTIME
        : Module("clr")
#endif
    { }

#ifndef RUNTIME
    static ModulePtr createModule();
#else
    static void callNative(uint16_t id, InterpreterBase* interp);
    static uint32_t hsvToRGB(float h, float s, float v);
#endif
};

}
