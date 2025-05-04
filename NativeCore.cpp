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
#include "Symbol.h"

using namespace clvr;

ModulePtr
NativeCore::createModule()
{
    ModulePtr coreModule = std::make_shared<Module>("core");
    coreModule->addNativeFunction("monitor", uint16_t(Id::Monitor), Type::UInt16, {{ "cmd", Type::UInt8, false, 1, 1 },
                                                                                   { "param0", Type::UInt16, false, 2, 1 },
                                                                                   { "param1", Type::UInt16, false, 2, 1 }});
    coreModule->addNativeFunction("printf", uint16_t(Id::PrintF), Type::None, {{ "fmt", Type::UInt8, true, AddrSize, 1 }});
    coreModule->addNativeFunction("format", uint16_t(Id::Format), Type::None, {{ "s", Type::UInt8, true, AddrSize, 1 },
                                                                           { "n", Type::UInt16, false, 2, 1 },
                                                                           { "fmt", Type::UInt8, true, AddrSize, 1 }});
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
