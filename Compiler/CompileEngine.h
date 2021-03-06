/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// base compiler internals

#pragma once

#include "Compiler.h"
#include "Interpreter.h"
#include "Scanner.h"
#include <cstdint>
#include <istream>
#include <vector>

namespace clvr {

class CompileEngine
{
public:
    // Built-in types are 0x00-0x7f, custom types are 0x80-0xff
    enum class Type : uint8_t { None = 0, Float = 1, Int = 2, UInt8 = 3, Ptr = 5 };

    class Symbol
    {
    public:
        enum class Storage { None, Const, Global, Local };
        
        Symbol() { }
        
        Symbol(const std::string& name, uint16_t addr, Type type, Storage storage = Storage::Local, bool ptr = false, uint8_t size = 1)
            : _name(name)
            , _addr(addr)
            , _type(type)
            , _ptr(ptr)
            , _storage(storage)
            , _size(size)
        {
            if (type == Type::Ptr) {
                _ptr = true;
            }
        }
        
        const std::string& name() const { return _name; }
        uint16_t addr() const;
        Type type() const { return _type; }
        bool isPointer() const { return _ptr; }
        Storage storage() const { return _storage; }
        uint8_t size() const { return _size; }
        
    private:
        std::string _name;
        uint16_t _addr = 0;
        Type _type = Type::None;
        bool _ptr = false;
        Storage _storage = Storage::None;
        uint8_t _size = 0;
    };
    
    using SymbolList = std::vector<Symbol>;
    
    CompileEngine(std::istream* stream, std::vector<std::pair<int32_t, std::string>>* annotations);
    
    virtual ~CompileEngine() { }
    
    virtual bool program() = 0;
    
    void emit(std::vector<uint8_t>& executable);

    Compiler::Error error() const { return _error; }
    Token expectedToken() const { return _expectedToken; }
    const std::string& expectedString() const { return _expectedString; }
    uint32_t lineno() const { return _scanner.lineno(); }
    uint32_t charno() const { return _scanner.charno(); }
    
    static bool opDataFromOp(const Op op, OpData& data);

    void addNative(const char* name, uint8_t nativeId, Type type, const SymbolList& locals)
    {
        _functions.emplace_back(name, nativeId, type, locals);
    }

protected:
    enum class Reserved {
        None,
        Struct,
        Const,
        Table,
        Function,
        Return,
        Break,
        Continue,
        Log,
        Command,
        End,
        Loop,
        While,
        For,
        If,
        Else,
        Float,
        Int,
        R0, R1, R2, R3,
        C0, C1, C2, C3,
    };
    
    virtual bool statement() = 0;
    virtual bool function() = 0;
    virtual bool table() = 0;
    virtual bool type(Type& t);
    
    bool constant();
    bool command();

    bool values(Type);

    // Value is returned as an int32_t, but it might be a float
    bool value(int32_t& i, Type);
        
    // The expect methods validate the passed param and if
    // there is no match, the passed error is saved and
    // throw is called. The first version also retires the
    // current token.
    void expect(Token token, const char* str = nullptr);
    void expect(bool passed, Compiler::Error error);
    void expectWithoutRetire(Token token);
    bool match(Reserved r);
    bool match(Token r);
    void ignoreNewLines();
    
    // These methods check to see if the next token is of the
    // appropriate type. Some versions just return true or
    // false, others also return details about the token
    bool identifier(std::string& id, bool retire = true);
    bool integerValue(int32_t& i);
    bool floatValue(float& f);
    bool stringValue(std::string&);
    bool reserved();
    bool reserved(Reserved &r);
    
    // This assumes the last op is a single byte op
    Op lastOp() const { return _rom8.size() ? Op(_rom8.back()) : Op::None; }
    uint16_t romSize() const { return _rom8.size(); }
    
    void addOp(Op op) { annotate(); _rom8.push_back(uint8_t(op)); }
    
    void addOpSingleByteIndex(Op op, uint8_t i)
    {
        annotate();
        _rom8.push_back(uint8_t(op) | (i & 0x0f));
    }

    void addOpTarg(Op op, uint16_t targ)
    {
        annotate();
        _rom8.push_back(uint8_t(op) | ((targ >> 8) & 0x0f));
        _rom8.push_back(uint8_t(targ));
    }
    
    void addOpIdI(Op op, uint8_t id, uint8_t i)
    {
        addOp(op);
        _rom8.push_back(id);
        _rom8.push_back(uint8_t(i & 0x0f));
    }

    void addOpInt(Op op, uint8_t i)
    {
        addOp(op);
        _rom8.push_back(i);
    }
    
    void addInt(uint8_t i) { _rom8.push_back(i); }
    
    void addOpI(Op op, uint8_t i) { addOpInt(op, i); }
    void addOpConst(Op op, uint8_t c) { addOpInt(op, c); }
    void addOpPL(Op op, uint8_t p, uint8_t l)
    {
        addOpSingleByteIndex(op, p);
        _rom8.push_back(l);
    }

    void addOpId(Op op, uint16_t id)
    {
        addOpSingleByteIndex(op, uint8_t(id >> 8));
        _rom8.push_back(uint8_t(id));
    }
    
    virtual bool isReserved(Token token, const std::string str, Reserved& r);

    class Function;
    const Function& handleFunctionName();

    static bool opDataFromString(const std::string str, OpData& data);

    bool addGlobal(const std::string& name, uint8_t addr, Type type, Symbol::Storage storage, bool ptr = false, uint8_t size = 1)
    {
        // Check for duplicates
        Symbol sym;
        if (findSymbol(name, sym)) {
            return false;
        }
        _globals.emplace_back(name, addr, type, storage, ptr, size);
        return true;
    }

    struct Def
    {
        Def() { }
        Def(std::string name, uint8_t value)
            : _name(name)
            , _value(value)
        { }
        std::string _name;
        uint8_t _value;
    };
    
    class Function
    {
    public:
        Function() { }
        
        Function(const std::string& name, uint16_t addr, Type type = Type::None)
            : _name(name)
            , _addr(addr)
            , _type(type)
            , _native(false)
        { }

        // Used to create built-in native functions
        Function(const char* name, uint8_t nativeId, Type type, const SymbolList& locals)
            : _name(name)
            , _addr(int16_t(nativeId))
            , _locals(locals)
            , _args(locals.size())
            , _type(type)
            , _native(true)
        { }

        const std::string& name() const { return _name; }
        int16_t addr() const { return _addr; }
        uint8_t& args() { return _args; }
        const uint8_t& args() const { return _args; }
        Type type() const { return _type; }
        uint8_t localSize() const { return _localHighWaterMark; }
        const Symbol& local(uint8_t i) const { return _locals[i]; }

        uint32_t numLocals() const { return uint32_t(_locals.size()); }
        void pruneLocals(uint32_t n)
        {
            // remove the last n locals and reduce _localSize
            while (n-- > 0) {
                _localSize -= _locals.back().size();
                _locals.pop_back();
            }
        }
        
        bool isNative() const { return _native; }
        int16_t nativeId() const { return _addr; }

        // Args are always 1 word and will always come before locals. So the
        // addr is the current locals size.
        void addArg(const std::string& name, Type type, bool isPtr)
        {
            _locals.emplace_back(name, _locals.size(), type, Symbol::Storage::Local, isPtr);
            _args++;
        }
        
        bool addLocal(const std::string& name, Type type, bool ptr, uint8_t size)
        {
            // Check for duplicates
            Symbol sym;
            if (findLocal(name, sym)) {
                return false;
            }
            _locals.emplace_back(name, _localSize + _args, type, Symbol::Storage::Local, ptr, size);
            _localSize += size;
            if (_localHighWaterMark < _localSize) {
                _localHighWaterMark = _localSize;
            }
            return true;
        }

        bool findLocal(const std::string& s, Symbol& sym)
        {
            const auto& it = find_if(_locals.begin(), _locals.end(),
                    [s](const Symbol& p) { return p.name() == s; });

            if (it != _locals.end()) {
                sym = *it;
                return true;
            }
            return false;
        }
        
    private:
        std::string _name;
        int16_t _addr = 0;
        std::vector<Symbol> _locals;
        uint8_t _args = 0;
        Type _type;
        bool _native = false;
        uint8_t _localSize = 0;
        uint8_t _localHighWaterMark = 0;
    };
    
    struct Command
    {
        Command(const std::string& cmd, uint8_t count, uint16_t initAddr, uint16_t loopAddr)
            : _cmd(cmd)
            , _count(count)
            , _initAddr(initAddr)
            , _loopAddr(loopAddr)
        { }
        
        std::string _cmd;
        uint8_t _count;
        uint16_t _initAddr = 0;
        uint16_t _loopAddr = 0;
    };
    
    Function& currentFunction()
    {
        if (_functions.empty()) {
            _error = Compiler::Error::InternalError;
            throw true;
        }
        return _functions.back();
    }
    
    void annotate()
    {
        if (_scanner.annotation() == -1) {
            _scanner.setAnnotation(int32_t(_rom8.size()));
        }
    }
    
    uint8_t allocNativeId() { return _nextNativeId++; }
        
    bool findSymbol(const std::string&, Symbol&);
    bool findDef(const std::string&, Def&);
    bool findFunction(const std::string&, Function&);

    Compiler::Error _error = Compiler::Error::None;
    Token _expectedToken = Token::None;
    std::string _expectedString;
    
    Scanner _scanner;

    std::vector<Def> _defs;
    std::vector<Symbol> _globals;
    std::vector<Function> _functions;
    std::vector<Command> _commands;
    std::vector<uint32_t> _rom32;
    std::vector<uint8_t> _rom8;

    // Vars are defined in 2 places. At global scope (when there are
    // no active functions) they are placed in _global memory.
    // When a function is being defined the vars are placed on the
    // stack.
    
    uint16_t _nextMem = 0; // next available location in mem
    uint16_t _localHighWaterMark = 0;
    uint16_t _globalSize = 0;
    bool _inFunction = false;
    uint8_t _nextNativeId = 0;
};

}
