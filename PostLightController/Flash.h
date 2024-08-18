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

#include <stdint.h>

class Adafruit_NeoPixel;

class Flash
{
public:
	bool init(Adafruit_NeoPixel* pixels, uint8_t h, uint8_t s, uint8_t v, uint8_t count, uint16_t duration);
	int32_t loop(Adafruit_NeoPixel* pixels);
		
private:
	void setAllLights(Adafruit_NeoPixel* pixels, uint32_t color);
	
    uint32_t _color;
	uint8_t _count;
	uint16_t _duration; // in ms
	
	uint32_t _lastFlash = 0;
	bool _on = false;
	uint8_t _countCompleted = 0;
};
