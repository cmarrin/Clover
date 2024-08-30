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
#include "Symbol.h"

using namespace lucid;

Function::Function(const char* name, uint16_t nativeId, Type returnType, const SymbolList& locals)
    : _name(name)
    , _returnType(returnType)
    , _native(true)
    , _addr(nativeId)
{
    for (const auto& it : locals) {
        SymbolPtr sym = std::make_shared<Symbol>(it.name(), it.type(), it.isPointer(), 1, 1);
        addArg(sym);
    }
}

const ASTPtr&
Function::astNode()
{
    if (!_astNode) {
        _astNode = std::make_shared<StatementsNode>(-1);
    }
    return _astNode;
}

void
Function::addASTNode(const ASTPtr& node)
{
    astNode()->addNode(node);
}

void
Function::pruneLocals(uint32_t n)
{
    // remove the last n locals and reduce _localSize
    while (n-- > 0) {
        _localSize -= typeToBytes(_locals.back()->type());
        _locals.pop_back();
    }
}

SymbolPtr
Function::findLocal(const std::string& s) const
{
    const auto& it = find_if(_locals.begin(), _locals.end(),
            [s](const SymbolPtr& p) { return p->name() == s; });

    if (it != _locals.end()) {
        return *it;
    }
    return nullptr;
}

bool
Function::addLocal(const SymbolPtr& sym, AddrNativeType addr, uint16_t nElements)
{
    // Check for duplicates
    if (findLocal(sym->name())) {
        return false;
    }
    
    _locals.push_back(sym);
    
    if (sym->kind() != Symbol::Kind::Var) {
        sym->setAddr(addr, Index::C);
        sym->setNElements(nElements);
        return true;
    }
    
    // Locals are negative offsets from the U register. But here they
    // are positive. But they still point at locations going down
    // in memory. So the addr point at the "highest" location of the
    // var. So if the first var is an int8 it is at addr 0, int16 is at
    // addr is 1, and int32 is at 3. When accessing at runtime addrs
    // are translated with 'U - addr - 1'. So the values would be -1,
    // -2 and -4.
    uint8_t bytes = sym->sizeInBytes();
    sym->setAddr(_localSize + bytes - 1, Index::L);
    _localSize += bytes;
    
    if (_localHighWaterMark < _localSize) {
        _localHighWaterMark = _localSize;
    }
    return true;
}

bool
Function::addArg(const SymbolPtr& sym)
{
    // Check for duplicates
    if (findLocal(sym->name())) {
        return false;
    }
    
    _locals.push_back(sym);
    
    // Args start at 0 and go positive
    sym->setAddr(_argSize, Index::A);
    
    _argSize += sym->sizeInBytes();
    _argCount += 1;
    return true;
}

SymbolPtr
Function::arg(uint32_t index) const
{
    if (index >= _argCount) {
        return nullptr;
    }
    return _locals[index];
}

