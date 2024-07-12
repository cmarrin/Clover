/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Lucid
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#pragma once

#include "AST.h"
#include "Defines.h"

namespace lucid {

class ExprEntry
{
public:
    struct Ref
    {
        Ref(Type type, bool ptr = false) : _type(type), _ptr(ptr) { }
        
        Type _type;
        bool _ptr;
    };
    
    struct Function
    {
        Function(const std::string& s) : _name(s) { }
        std::string _name;
    };
    
    enum class ExprEntryType {
        None = 0,
        AST = 1,
        Ref = 2,
        Function = 3,
    };
    
    ExprEntry() { _variant = std::monostate(); }
    ExprEntry(const ASTPtr& ast) { _variant = ast; }
    ExprEntry(const Ref& ref) { _variant = ref; }
    ExprEntry(const Function& fun) { _variant = fun; }
            
    ASTPtr ast() const { return std::get<ASTPtr>(_variant); }
    const Ref& ref() const { return std::get<Ref>(_variant); }
    const Function& function() const { return std::get<Function>(_variant); }
    
    ExprEntryType exprEntryType() const { return ExprEntryType(_variant.index()); }

    void addCode(std::vector<uint8_t>& code, bool isLHS) const
    {
        if (exprEntryType() == ExprEntryType::AST) {
            ast()->addCode(code, isLHS);
        }
    }

private:
    std::variant<std::monostate
                 , ASTPtr
                 , Ref
                 , Function
                 > _variant;
};

}
