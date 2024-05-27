/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "Compiler.h"

#include "CompileEngine.h"

#include <map>
#include <vector>

using namespace lucid;

bool Compiler::compile(std::istream* istream,
                       std::vector<uint8_t>& executable, uint32_t maxExecutableSize,
                       const std::vector<NativeModule*>& modules,
                       AnnotationList* annotations)
{
    CompileEngine* engine = new CompileEngine(istream, annotations);
    
    // Install the modules in the engine
    // First add the core
    for (const auto& it : modules) {
        it->addFunctions(engine);
    }
    
    engine->program();
    _error = engine->error();
    _expectedToken = engine->expectedToken();
    _expectedString = engine->expectedString();
    _lineno = engine->lineno();
    _charno = engine->charno();
    
    if (_error == Error::None && executable.size() > maxExecutableSize) {
        _error = Error::ExecutableTooBig;
    }
    
    delete engine;
    
    return _error == Error::None;
}
