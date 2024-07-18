/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// Lucid code generator
//

#pragma once

#include "AST.h"
#include "Defines.h"
#include "Symbol.h"

namespace lucid {

//*********************************
//
//  Class: Codegen
//
//*********************************

/*

Input is an AST, which represents a statement. Output is virtual machine code.
The VM is register based. There is an accumulator (8, 16 or 32 bit) which
holds signed or unsigned integers or a single precision float.

NOTE:   I'm considering having a 16 bit fixed point float (8:8 format) to save
        space. If I do that it might be a good idea to have a 32 bit fixed
        format and get rid of native floating point support entirely. But
        in tests on Arduino and ESP, I'm not sure if my fixed point library
        is any better than the floating point libraries on those platforms.
        
NOTE:   I'm also considering a bool type. It would be 8 bits, so maybe not
        worth it. Maybe I'll just add true and false keywords that evaluate
        to uint8, 1 and 0.
*/

/*
    Executable format
    
    Signature       4 bytes             "lucd"
    Addr size       1 byte              0 - 16 bit, 1 - 32 bit
    Entry point     Addr size bytes     Address of first instruction to execute
    Code            Rest of file        Executable code
 */

class Codegen {
public:
    Codegen(std::vector<uint8_t>* code) : _code(code) { }
  	
    bool processAST(const ASTPtr&);

private:
    bool processNextASTNode(const ASTPtr&, bool isLHS);

    std::vector<uint8_t>* _code;
};

}
