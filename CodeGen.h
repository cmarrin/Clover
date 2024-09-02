/*-------------------------------------------------------------------------
    This source file is a part of Lucid
    For the latest info, see https://github.com/cmarrin/Lucid
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#pragma once

#include "AST.h"

#include <vector>

namespace lucid {

//static inline void appendValue(std::vector<uint8_t>& container, uint32_t v, uint8_t bytes)
//{
//    switch (bytes) {
//        case 4: container.push_back(v >> 24);
//                container.push_back(v >> 16);
//        case 2: container.push_back(v >> 8);
//        case 1: container.push_back(v);
//    }
//}

class CodeGen
{
  public:
    virtual ~CodeGen() { }
    
    virtual void emitCode(const ASTPtr& node, bool isLHS) { }

    std::vector<uint8_t>& code() { return _code; }
    
  private:
    std::vector<uint8_t> _code;
};

}
