/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// Decompile executable into source string
//

#pragma once

#include <vector>
#include "AST.h"
#include "Scanner.h"
#include "Struct.h"

namespace clvr {

class Compiler;

class Decompiler
{
public:
    enum class Error {
        None,
        InvalidSignature,
        WrongAddressSize,
        InvalidOp,
        PrematureEOF,
    };
    
    Decompiler(const std::vector<uint8_t>* in, std::string* out, const Annotations* annotations)
        : _in(in)
        , _out(out)
        , _annotations(annotations)
    {
        _it = _in->begin();
    }
    
    bool decompile();
    
    Error error() const { return _error; }

private:
    void firstPassStruct(const StructPtr&);
    void printASTNode(const ASTPtr& ast);
    
    const char* typeToString(Type) const;
    
    void statement();
    
    void emitOp(const char* opString);
    void emitRelAddr(uint8_t size, bool isSigned = true);
    void emitNumber(int32_t number);
    void emitConstant(uint8_t bytes, bool isSigned = true);
    void emitShortConstant(uint8_t value);
    
    void emitSize(uint8_t size);
    void emitIndex();
    
    bool atEnd() { return (_in->end() - _it) <= 0; }

    AddrNativeType addrMode(Index& index);

    int32_t getInt32()
    {
        if (_in->end() - _it < 2) {
            _error = Error::PrematureEOF;
            throw true;
        }
        
        // Big endian
        return (int32_t(getUInt16()) << 16) | int32_t(getUInt16());
    }
    
    uint32_t getUInt32() { return getInt32(); }
    
    int16_t getInt16()
    {
        if (_in->end() - _it < 2) {
            _error = Error::PrematureEOF;
            throw true;
        }
        
        // Big endian
        return (uint32_t(*_it++) << 8) | uint32_t(*_it++);
    }
    
    uint16_t getUInt16()
    {
        if (_in->end() - _it < 2) {
            _error = Error::PrematureEOF;
            throw true;
        }
        
        // Big endian
        return (uint32_t(*_it++) << 8) | uint32_t(*_it++);
    }
    
    int8_t getInt8()
    {
        if (_in->end() - _it < 1) {
            _error = Error::PrematureEOF;
            throw true;
        }
        

        return int8_t(*_it++);
    }
    
    uint8_t getUInt8()
    {
        if (_in->end() - _it < 1) {
            _error = Error::PrematureEOF;
            throw true;
        }
        

        return *_it++;
    }
    
    std::string regString(uint8_t r);
    std::string colorString(uint8_t c);
    
    void doIndent()
    {
        for (uint32_t i = 0; i < _indent; ++i) {
            _out->append("    ");
        }
    }
    
    void decIndent()
    {
        if (_indent == 0) {
            _out->append("*** Error, tried to indent past 0!!!\n");
        } else {
            _indent--;
        }
    }

    void incIndent()
    {
        _indent++;
    }
    
    uint16_t addr() const { return (_it - _in->begin()); }
    
    void outputAddr()
    {
        _out->append("[");
        _out->append(std::to_string(addr() - 1));
        _out->append("] ");
    }

    Error _error = Error::None;
    const std::vector<uint8_t>* _in;
    std::vector<uint8_t>::const_iterator _it;
    std::string* _out;
    int32_t _indent = 0;
    uint16_t _codeOffset = 0; // Used by Call
    const Annotations* _annotations = nullptr;
    int _annotationIndex = 0;
    const Compiler* _compiler = nullptr;
};

}
