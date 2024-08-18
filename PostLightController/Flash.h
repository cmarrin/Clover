/*-------------------------------------------------------------------------
    This source file is a part of Clover
    For the latest info, see https://github.com/cmarrin/Clover
    Copyright (c) 2021-2022, Chris Marrin
    All rights reserved.
    Use of this source code is governed by the MIT license that can be
    found in the LICENSE file.
-------------------------------------------------------------------------*/

// Flash Class
//
// This class flashes the lights a given number of times (for error output)

#pragma once

#include "Effect.h"

class Flash : public Effect
{
public:
	Flash(Adafruit_NeoPixel* pixels) : Effect(pixels) { }
	virtual ~Flash() { }
	
	virtual bool init(uint8_t cmd, const uint8_t* buf, uint32_t size) override;
	virtual int32_t loop() override;
		
private:
	void setAllLights(uint32_t color);
	
    uint32_t _color;
	uint8_t _count;
	uint32_t _duration; // in ms
	
	uint32_t _lastFlash = 0;
	bool _on = false;
	uint8_t _countCompleted = 0;
};
