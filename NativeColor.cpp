/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "NativeColor.h"

#include "Symbol.h"

#include <math.h>

using namespace clvr;

const FunctionList
NativeColor::create()
{
    FunctionList list;
    
    Function::addNativeFunction(list, "setLight", uint16_t(Id::SetLight), Type::None, {{ "i", Type::UInt8, false, 1, 1 }, { "c", Type::None, true, 1, 1 }});
    return list;
}
