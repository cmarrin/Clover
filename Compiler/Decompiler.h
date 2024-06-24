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
#include "Scanner.h"
#include "Struct.h"

namespace lucid {

class Compiler;

class Decompiler
{
public:
    enum class Error {
        None,
        InvalidSignature,
        InvalidOp,
        PrematureEOF,
    };
    
    Decompiler(const std::vector<uint8_t>* in, std::string* out, const AnnotationList& annotations)
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
    void emitSizeValue(uint8_t size);
    void emitIndexValue(uint8_t index);
    void emitRelAddr(const char* opString, uint8_t size);
    void emitNumber(const char* opString, int32_t number);
    
    void emitNoParams(const char* opString) { emitOp(opString); }
    void emitSize(const char* opString, uint8_t size) { emitOp(opString); emitSizeValue(size); }
    void emitIndex(const char* opString, uint8_t index) { emitOp(opString); emitIndexValue(index); }
    void emitSizeIndex(const char* opString, uint8_t size, uint8_t index) { emitOp(opString); emitSizeValue(size); emitIndexValue(index); }
    
    bool atEnd() { return (_in->end() - _it) <= 0; }
    uint32_t getUInt32()
    {
        if (_in->end() - _it < 4) {
            _error = Error::PrematureEOF;
            throw true;
        }
        
        // Little endian
        return uint32_t(*_it++) | (uint32_t(*_it++) << 8) | 
                (uint32_t(*_it++) << 16) | (uint32_t(*_it++) << 24);
    }
    
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
        
        // Little endian
        return uint32_t(*_it++) | (uint32_t(*_it++) << 8);
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
    int32_t _indent = 1;
    uint16_t _codeOffset = 0; // Used by Call
    const AnnotationList& _annotations;
    int _annotationIndex = 0;
    const Compiler* _compiler = nullptr;
};

}
