/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "CompileEngine.h"

#include <cmath>
#include <map>

using namespace lucid;

static void emitUInt16(std::vector<uint8_t>& executable, uint16_t v)
{
    executable.push_back(uint8_t(v));
    executable.push_back(uint8_t(v >> 8));
}
