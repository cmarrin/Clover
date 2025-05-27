/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "InterpretedEffect.h"

#include "NativeColor.h"

void
MyInterpreter::callNativeColor(uint16_t id, InterpreterBase* interp)
{
    MyInterpreter* self = reinterpret_cast<MyInterpreter*>(interp);
    switch (clvr::NativeColor::Id(id)) {
        case clvr::NativeColor::Id::SetLight: {
            // First arg is byte index of light to set. Next is a ptr to a struct of
            // h, s, v byte values (0-255)
            uint8_t i = interp->memMgr()->getArg(0, 1);
            clvr::AddrNativeType addr = interp->memMgr()->getArg(1, clvr::AddrSize);
            uint8_t h = interp->memMgr()->getAbs(addr, 1);
            uint8_t s = interp->memMgr()->getAbs(addr + 1, 1);
            uint8_t v = interp->memMgr()->getAbs(addr + 2, 1);
            
            self->_pixels->setPixelColor(i, Adafruit_NeoPixel::gamma32(Adafruit_NeoPixel::ColorHSV(uint16_t(h) * 256, s, v)));
            self->_pixels->show();
            break;
        }
    }
}

bool
InterpretedEffect::init(uint8_t cmd, const uint8_t* buf, uint32_t size)
{
    _interp.instantiate();
    if (_interp.error() != clvr::Memory::Error::None) {
        return false;
    }
    
    _interp.addModule(MyInterpreter::callNativeColor);

    for (int i = size - 1; i >= 0; --i) {
        _interp.addArg(buf[i], clvr::Type::UInt8);
    }
    _interp.addArg(cmd, clvr::Type::UInt8); // cmd
    _interp.construct();
    _interp.dropArgs(size + 1);
    
    if (_interp.error() != clvr::Memory::Error::None) {
        return false;
    }
    
    _interp.interp(MyInterpreter::ExecMode::Start);

	if (_interp.error() != clvr::Memory::Error::None) {
		return false;
	}

    Serial.print(F("InterpretedEffect started: cmd='"));
    Serial.print(char(cmd));
    Serial.println("'");

	return true;
}

int32_t
InterpretedEffect::loop()
{
    uint32_t result = _interp.interp(MyInterpreter::ExecMode::Start);
    return (_interp.error() != clvr::Memory::Error::None) ? -1 : result;
}
