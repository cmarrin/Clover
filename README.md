# LucidVM
Lucid is a virtual machine for C-like languages intended to run on a variety of embedded platforms. It has grown out of two other interpreted language I wrote in other repositories: m8rscript and Clover. I wanted to make a runtime that could run on Arduino hardware (e.g., ATMega128 based), ESP8266/ESP32, and a bare-metal Raspberry Pi. I also want to run it on a Mac to allow code to be simulated. It is feature rich, but expensive features can be optionally excluded to allow the runtime to work on small platforms like Arduino.

