/*-------------------------------------------------------------------------
    This source file is a part of Lucid
    For the latest info, see https://github.com/cmarrin/Lucid
    Copyright (c) 2021-2024, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "Function.h"

#include "AST.h"

using namespace lucid;

Function::Function(const char* name, NativeId nativeId, Type returnType, const SymbolList& locals)
    : _name(name)
    , _returnType(returnType)
    , _native(true)
    , _addr(int32_t(nativeId))
{
    for (const auto& it : locals) {
        addArg(it.name(), it.type(), it.size(), it.isPointer());
    }
}

void Function::addASTNode(const ASTPtr& node)
{
    if (!_astNode) {
        _astNode = std::make_shared<StatementsNode>();
    }
    _astNode->addNode(node);
}

void Function::pruneLocals(uint32_t n)
{
    // remove the last n locals and reduce _localSize
    while (n-- > 0) {
        _localSize -= _locals.back()->size();
        _locals.pop_back();
    }
}

SymbolPtr Function::findLocal(const std::string& s) const
{
    const auto& it = find_if(_locals.begin(), _locals.end(),
            [s](const SymbolPtr& p) { return p->name() == s; });

    if (it != _locals.end()) {
        return *it;
    }
    return nullptr;
}

