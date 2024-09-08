/*-------------------------------------------------------------------------
    This source file is a part of Clover

    For the latest info, see http:www.marrin.org/Clover

    Copyright (c) 2018-2024, Chris Marrin
    All rights reserved.

    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

//#include "bare.h"

#include "Defines.h"
#include "Memory.h"

#include <assert.h>
#include <string.h>
#include <stdarg.h>

using namespace clvr;

class FormatterArgs
{
  public:
    virtual ~FormatterArgs() { }
    
    virtual uint8_t getChar(uint32_t i) const = 0;
    virtual void putChar(uint8_t c) = 0;
    virtual uint8_t getStringChar(uintptr_t p) const = 0;
    virtual uintptr_t getArg(clvr::Type type) = 0;
    virtual void end() { }
};

class InterpPrintArgs : public FormatterArgs
{
  public:
    InterpPrintArgs(clvr::AddrNativeType fmt, clvr::VarArg& args)
        : _fmt(fmt)
        , _args(&args)
    { }
        
    virtual ~InterpPrintArgs() { }
    virtual uint8_t getChar(uint32_t i) const override { return getStringChar(uintptr_t(_fmt + i)); }
    virtual void putChar(uint8_t c) override { clvr::putChar(c); }
    virtual uintptr_t getArg(clvr::Type type) override { return _args->arg(type); }

    // The interpreter keeps strings in ROM. The p pointer is actually an offset in the rom
    virtual uint8_t getStringChar(uintptr_t p) const override
    {
        return _args->memMgr()->getAbs(clvr::AddrNativeType(p), OpSize::i8);
    }
    
    clvr::VarArg* args() { return _args; }

  private:
    clvr::AddrNativeType _fmt;
    clvr::VarArg* _args;
};

class InterpFormatArgs : public InterpPrintArgs
{
  public:
    InterpFormatArgs(AddrNativeType s, uint16_t n, clvr::AddrNativeType fmt, clvr::VarArg& args)
        : InterpPrintArgs(fmt, args)
        , _buf(s)
        , _size(n)
        , _index(0)
    { }
        
    virtual ~InterpFormatArgs() { }

    virtual void putChar(uint8_t c) override
    {
        if (_index < _size - 1) {
            args()->putChar(_buf + _index++, c);
        }
    }

    virtual void end() override { putChar('\0'); }

  private:
    AddrNativeType _buf;
    uint16_t _size;
    uint16_t _index;
};

class NativePrintArgs : public FormatterArgs
{
  public:
    NativePrintArgs(const char* fmt, va_list args)
        : _fmt(fmt)
    {
        va_copy(_args, args);
    }
        
    virtual ~NativePrintArgs() { }
    virtual uint8_t getChar(uint32_t i) const override { return _fmt[i]; }
    virtual void putChar(uint8_t c) override { clvr::putChar(c); }
    virtual uintptr_t getArg(clvr::Type type) override
    {
        if (type == clvr::Type::Float) {
            double d = va_arg(_args, double);
            return clvr::floatToInt(float(d));
        }
        if (type == clvr::Type::String) {
            return uintptr_t(va_arg(_args, const char*));
        }
        return va_arg(_args, uint32_t);
    }

    // For native, p is the actual pointer to the char
    virtual uint8_t getStringChar(uintptr_t p) const override
    {
        const char* addr = reinterpret_cast<const char*>(p);
        return *addr;
    }

  private:
    const char* _fmt;
    va_list _args;
};

class NativeFormatArgs : public NativePrintArgs
{
  public:
    NativeFormatArgs(char* buf, uint16_t n, const char* fmt, va_list args)
        : NativePrintArgs(fmt, args)
        , _buf(buf)
        , _size(n)
        , _index(0)
    { }
    
    virtual ~NativeFormatArgs() { }

    virtual void putChar(uint8_t c) override
    {
        if (_index < _size - 1) {
            _buf[_index++] = c;
        }
    }

    virtual void end() override { putChar('\0'); }

  private:
    char* _buf;
    uint16_t _size;
    uint16_t _index;
};

enum class Flag {
    leftJustify = 0x01,
    plus = 0x02,
    space = 0x04,
    alt = 0x08,
    zeroPad = 0x10,
};

static bool isFlag(uint8_t flags, Flag flag) { return (flags & static_cast<uint8_t>(flag)) != 0; }
static void setFlag(uint8_t& flags, Flag flag) { flags |= static_cast<uint8_t>(flag); }

enum class Capital { Yes, No };

static constexpr uint32_t MaxIntegerBufferSize = 24; // Big enough for a 64 bit integer in octal

int32_t doprintf(FormatterArgs*);

namespace clvr {

int32_t
printf(clvr::AddrNativeType fmt, clvr::VarArg& args)
{
    InterpPrintArgs f(fmt, args);
    return doprintf(&f);
}

int32_t
format(AddrNativeType s, uint16_t n, clvr::AddrNativeType fmt, clvr::VarArg& args)
{
    InterpFormatArgs f(s, n, fmt, args);
    return doprintf(&f);
}

int32_t
printf(const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    return clvr::vprintf(fmt, args);
}

int32_t
format(char* s, uint16_t n, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    return clvr::vformat(s, n, fmt, args);
}

int32_t
vprintf(const char* fmt, va_list args)
{
    NativePrintArgs f(fmt, args);
    return doprintf(&f);
}

int32_t
vformat(char* s, uint16_t n, const char* fmt, va_list args)
{
    NativeFormatArgs f(s, n, fmt, args);
    return doprintf(&f);
}

}

bool
toNumber(FormatterArgs* f, uint32_t& fmt, uint32_t& n)
{
    n = 0;
    bool haveNumber = false;
    while (1) {
        uint8_t c = f->getChar(fmt);
        if (c < '0' || c > '9') {
            return haveNumber;
        }
        n = n * 10 + c - '0';
        fmt += 1;
        haveNumber = true;
    }
}

enum class Signed { Yes, No };
enum class FloatType { Float, Exp, Shortest };
 
static void handleFlags(FormatterArgs* f, uint32_t& fmt, uint8_t& flags)
{
    while (1) {
        switch (f->getChar(fmt)) {
        case '-': setFlag(flags, Flag::leftJustify); break;
        case '+': setFlag(flags, Flag::plus); break;
        case ' ': setFlag(flags, Flag::space); break;
        case '#': setFlag(flags, Flag::alt); break;
        case '0': setFlag(flags, Flag::zeroPad); break;
        default: return;
        }
        ++fmt;
    }
}

static int32_t handleWidth(FormatterArgs* f, uint32_t& fmt)
{
    if (f->getChar(fmt) == '*') {
        ++fmt;
        return int32_t(f->getArg(clvr::Type::UInt32));
    }
    
    uint32_t n;
    return toNumber(f, fmt, n) ? static_cast<int32_t>(n) : -1;
}

enum class Length { None, H, HH, L, LL, J, Z, T };

static Length handleLength(FormatterArgs* f, uint32_t& fmt)
{
    Length length = Length::None;
    if (f->getChar(fmt) == 'h') {
        ++fmt;
        if (f->getChar(fmt) == 'h') {
            ++fmt;
            length = Length::HH;
        } else {
            length = Length::H;
        }
    } else if (f->getChar(fmt) == 'l') {
        ++fmt;
        if (f->getChar(fmt) == 'l') {
            ++fmt;
            length = Length::LL;
        } else {
            length = Length::L;
        }
    } else if (f->getChar(fmt) == 'j') {
        length = Length::J;
    } else if (f->getChar(fmt) == 'z') {
        length = Length::Z;
    } else if (f->getChar(fmt) == 't') {
        length = Length::T;
    } else {
        return length;
    }
    return length;
}

// 8 and 16 bit integers are upcast by the caller to 32 bit. Ignore the length field
static int32_t getInteger(Length length, FormatterArgs* f)
{
    return int32_t(f->getArg(clvr::Type::UInt32));
}

static char* intToString(uint64_t value, char* buf, size_t size, uint8_t base = 10, Capital cap = Capital::No)
{
    if (value == 0) {
        buf[0] = '0';
        buf[1] = '\0';
        return buf;
    }
    
    char hexBase = (cap == Capital::Yes) ? 'A' : 'a';
    char* p = buf + size;
    *--p = '\0';
    
    while (value) {
        uint8_t digit = value % base;
        *--p = (digit > 9) ? (digit - 10 + hexBase) : (digit + '0');
        value /= base;
    }
    return p;
}

static int32_t outInteger(FormatterArgs* f, uintmax_t value, Signed sign, int32_t width, int32_t precision, uint8_t flags, uint8_t base, Capital cap)
{
    uint32_t size = 0;
    if (sign == Signed::Yes) {
        intmax_t signedValue = value;
        if (signedValue < 0) {
            value = -signedValue;
            f->putChar('-');
            size = 1;
            width--;
        }
    }
    
    if (isFlag(flags, Flag::alt) && base != 10) {
        f->putChar('0');
        size++;
        width--;
        if (base == 16) {
            f->putChar((cap == Capital::Yes) ? 'X' : 'x');
            size++;
            width--;
        }
    }
    
    char buf[MaxIntegerBufferSize];
    char* p = intToString(static_cast<uint64_t>(value), buf, MaxIntegerBufferSize, base, cap);
    size += static_cast<uint32_t>(p - buf);

    int32_t pad = static_cast<int32_t>(width) - static_cast<int32_t>(strlen(p));
    
    char padChar = isFlag(flags, Flag::zeroPad) ? '0' : ' ';
    
    while (pad > 0) {
        f->putChar(padChar);
        size++;
        pad--;
    }
    
    for ( ; *p; ++p) {
        f->putChar(*p);
    }

    return size;
}

static int32_t outString(FormatterArgs* f, uintptr_t p, int32_t width, int32_t precision, uint8_t flags)
{
    // FIXME: Handle flags.leftJustify
    // FIXME: Handle width
    // FIXME: Handle precision
    
    int32_t size = 0;
    while (true) {
        uint8_t c = f->getStringChar(p++);
        if (c == '\0') {
            break;
        }
        f->putChar(c);
        ++size;
    }
    
    if (width > size) {
        width -= size;
        while (width--) {
            f->putChar(' ');
        }
    }

    return size;
}

// Unsupported features:
//
//     'n' specifier - returns number of characters written so far
//     'a', 'A' specifiers - prints hex floats
//     'L' length - long double
//     'l' length for 'c' and 's' specifiers - wide characters
//
// 
 
int32_t doprintf(FormatterArgs* f)
{
    uint8_t flags = 0;
    int32_t size = 0;
    uint32_t fmt = 0;
    uint8_t c;
    
    while ((c = f->getChar(fmt)) != '\0') {
        if (c != '%') {
            f->putChar(c);
            fmt++;
            size++;
            continue;
        }
        
        fmt++;
        
        // We have a format, do the optional part
        handleFlags(f, fmt, flags);
        int32_t width = handleWidth(f, fmt);
        int32_t precision = -1;
        if (f->getChar(fmt) == '.') {
            precision = handleWidth(f, ++fmt);
        }
        Length length = handleLength(f, fmt);
        
        // Handle the specifier
        switch (f->getChar(fmt))
        {
        case 'd':
        case 'i':
            size += outInteger(f, getInteger(length, f), Signed::Yes, width, precision, flags, 10, Capital::No);
            break;
        case 'u':
            size += outInteger(f, getInteger(length, f), Signed::No, width, precision, flags, 10, Capital::No);
            break;
        case 'o':
            size += outInteger(f, getInteger(length, f), Signed::No, width, precision, flags, 8, Capital::No);
            break;
        case 'x':
        case 'X':
            size += outInteger(f, getInteger(length, f), Signed::No, width, precision, flags, 16, (f->getChar(fmt) == 'X') ? Capital::Yes : Capital::No);
            break;
        case 'f':
        case 'F':
        case 'e':
        case 'E':
        case 'g':
        case 'G': {
            Capital cap = Capital::No;
            FloatType type = FloatType::Shortest;
            switch(f->getChar(fmt))
            {
            case 'f': cap = Capital::No; type = FloatType::Float; break;
            case 'F': cap = Capital::Yes; type = FloatType::Float; break;
            case 'e': cap = Capital::No; type = FloatType::Exp; break;
            case 'E': cap = Capital::Yes; type = FloatType::Exp; break;
            case 'g': cap = Capital::No; type = FloatType::Shortest; break;
            case 'G': cap = Capital::Yes; type = FloatType::Shortest; break;
            }

            char buf[20];
            clvr::toString(buf, clvr::intToFloat(int32_t(f->getArg(clvr::Type::Float))), width, (precision < 0) ? 6 : precision);
            for (int i = 0; buf[i] != '\0'; ++i) {
                f->putChar(buf[i]);
            }
            //size += outFloat(gen, flt::Float::fromArg(va_arg(va.value, flt::Float::arg_type)), width, precision, flags, cap, type);
            break;
        }
        case 'c':
            // Chars are passed in as uint32
            f->putChar(static_cast<char>(f->getArg(clvr::Type::UInt32)));
            size++;
            break;
        case 'b': {
            // Booleans are passed in as uint32
            const char* s = f->getArg(clvr::Type::UInt32) ? "true" : "false";
            for (int i = 0; s[i] != '\0'; ++i) {
                f->putChar(s[i]);
            }
            break;
        }
        case 's':
            size += outString(f, f->getArg(clvr::Type::String), width, precision, flags);
            break;
        case 'p':
            size += outInteger(f, f->getArg(clvr::Type::UInt32), Signed::No, width, precision, flags, 16, Capital::No);
            break;
        default:
            f->putChar(f->getChar(fmt++));
            size++;
            break;
        }
        ++fmt;
    }
    
    f->end();
    return size;
}
