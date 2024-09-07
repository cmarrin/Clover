/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "InterpretedEffect.h"

bool
InterpretedEffect::init(uint8_t cmd, const uint8_t* buf, uint32_t size)
{
    _interp.init();

    for (int i = size - 1; i >= 0; --i) {
        _interp.addArg(buf[i], clvr::Type::UInt8);
    }
    _interp.addArg(cmd, clvr::Type::UInt8); // cmd
    
    _interp.interp(MyInterpreter::ExecMode::Start);
    _interp.dropArgs(size + 1);

	if (_interp.error() != MyInterpreter::Error::None) {
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
    _interp.addArg('*', clvr::Type::UInt8);
    uint32_t result = _interp.interp(MyInterpreter::ExecMode::Start);
    _interp.dropArgs(1);
    return (_interp.error() != MyInterpreter::Error::None) ? -1 : result;
}
