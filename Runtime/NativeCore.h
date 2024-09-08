/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// NativeCore
//
// core methods

// Native functions
//
// These are implemented in the ExecutionUnit and are recognized by the Compiler.
// CallNative op has an operand which is the Native enum value. All params are
// passed on the stack and must be the expected size and type. Return value is
// sent back as an int32_t but can represent any type of value as defined in the
// native function signature
//
// Functions:
//
//      void    print(string)       - prints the passed string to the console
//      string  int8ToString(int8)  - return passed int8 value converted to string

#pragma once

#ifndef ARDUINO
#include "Module.h"
#endif

#include <stdint.h>

namespace clvr {

class InterpreterBase;

class NativeCore
#ifndef ARDUINO
    : public Module
#endif
{
  public:
    enum class Id {
        None            = 0,
        PrintF          = 1,
        Format          = 2,
        MemSet          = 3,
        RandomInt       = 4,
        RandomFloat     = 5,
        MinInt          = 6,
        MaxInt          = 7,
        MinFloat        = 8,
        MaxFloat        = 9,
        InitArgs        = 10,
        ArgInt8         = 11,
        ArgInt16        = 12,
        ArgInt32        = 13,
        ArgFloat        = 14,
        Animate         = 15,
    };
    
    NativeCore()
#ifndef ARDUINO
        : Module("core")
#endif
    { }
    
#ifndef ARDUINO
    static ModulePtr createModule();
#endif

    static void callNative(uint16_t id, InterpreterBase* interp);


  private:
};

}
