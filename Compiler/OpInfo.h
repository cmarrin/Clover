/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// Clover compiler
//
// A simple imperative language which generates code that can be 
// executed by the Interpreter
//

#pragma once

namespace lucid {

class OpInfo {
public:        
    OpInfo() { }
    
    enum class Assign { None, Only, Op };
    
    // assign says this is an assignmentOperator, opAssign says it also has a binary op
    OpInfo(Token token, uint8_t prec, Op intOp, Op floatOp, Assign assign, Type resultType)
        : _token(token)
        , _intOp(intOp)
        , _floatOp(floatOp)
        , _prec(prec)
        , _assign(assign)
        , _resultType(resultType)
    {
    }
    
    bool operator==(const Token& t)
    {
        return static_cast<Token>(_token) == t;
    }
    
    Token token() const { return _token; }
    uint8_t prec() const { return _prec; }
    Op intOp() const { return _intOp; }
    Op floatOp() const { return _floatOp; }
    Assign assign() const { return _assign; }
    Type resultType() const { return _resultType; }
    
private:
    Token _token;
    Op _intOp;
    Op _floatOp;
    uint8_t _prec;
    Assign _assign;
    Type _resultType;
};

}
