/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

#include "InterpretedEffect.h"

InterpretedEffect::InterpretedEffect(Adafruit_NeoPixel* pixels)
	: Effect(pixels)
	, _interp(pixels)
{
}

bool
InterpretedEffect::init(uint8_t cmd, const uint8_t* buf, uint32_t size)
{
	Effect::init(cmd, buf, size);
	
    char c[2];
    c[0] = cmd;
    c[1] = '\0';
	if (!_interp.init(c, buf, size)) {
		return false;
	}

    lucid::printf(F("InterpretedEffect started: cmd='*c'\n"), cmd);

	return true;
}

int32_t
InterpretedEffect::loop()
{
    return _interp.loop();
}
