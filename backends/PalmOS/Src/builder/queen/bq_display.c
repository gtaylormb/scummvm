#include <PalmOS.h>
#include "b_globals.h"

typedef UInt8 uint8;

static void addDisplay_fontRegular() {
	uint8 _fontRegular[] = {
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC0, 0xC0, 0xC0, 0xC0, 0x00, 0xC0, 0xC0, 0x00, 
		0xD8, 0xD8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x6C, 0x6C, 0xFE, 0x6C, 0xFE, 0x6C, 0x6C, 0x00, 
		0x30, 0x7C, 0xC0, 0x78, 0x0C, 0xF8, 0x30, 0x00, 0x00, 0xC6, 0xCC, 0x18, 0x30, 0x66, 0xC6, 0x00, 
		0x38, 0x6C, 0x68, 0x36, 0xDC, 0xCC, 0x76, 0x00, 0x60, 0x60, 0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x30, 0x60, 0xC0, 0xC0, 0xC0, 0x60, 0x30, 0x00, 0xC0, 0x60, 0x30, 0x30, 0x30, 0x60, 0xC0, 0x00, 
		0x00, 0x6C, 0x38, 0xFE, 0x38, 0x6C, 0x00, 0x00, 0x00, 0x30, 0x30, 0xFC, 0x30, 0x30, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x60, 0x60, 0xC0, 0x00, 0x00, 0x00, 0xFC, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0xC0, 0xC0, 0x00, 0x03, 0x06, 0x0C, 0x18, 0x30, 0x60, 0xC0, 0x00, 
		0x78, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0x78, 0x00, 0x30, 0x70, 0xF0, 0x30, 0x30, 0x30, 0x30, 0x00, 
		0x78, 0xCC, 0x0C, 0x78, 0xC0, 0xC0, 0xFC, 0x00, 0x78, 0xCC, 0x0C, 0x38, 0x0C, 0xCC, 0x78, 0x00, 
		0x1C, 0x3C, 0x6C, 0xCC, 0xFC, 0x0C, 0x0C, 0x00, 0xFC, 0xC0, 0xF8, 0x0C, 0x0C, 0xCC, 0x78, 0x00, 
		0x78, 0xCC, 0xC0, 0xF8, 0xCC, 0xCC, 0x78, 0x00, 0xFC, 0xCC, 0x0C, 0x18, 0x30, 0x30, 0x30, 0x00, 
		0x78, 0xCC, 0xCC, 0x78, 0xCC, 0xCC, 0x78, 0x00, 0x78, 0xCC, 0xCC, 0x7C, 0x0C, 0xCC, 0x78, 0x00, 
		0x00, 0xC0, 0xC0, 0x00, 0x00, 0xC0, 0xC0, 0x00, 0x00, 0x60, 0x60, 0x00, 0x00, 0x60, 0x60, 0xC0, 
		0x18, 0x30, 0x60, 0xC0, 0x60, 0x30, 0x18, 0x00, 0x00, 0x00, 0xFC, 0x00, 0xFC, 0x00, 0x00, 0x00, 
		0xC0, 0x60, 0x30, 0x18, 0x30, 0x60, 0xC0, 0x00, 0x78, 0xCC, 0x0C, 0x18, 0x30, 0x00, 0x30, 0x00, 
		0x6C, 0xFE, 0xFE, 0xFE, 0x7C, 0x38, 0x10, 0x00, 0x38, 0x7C, 0xC6, 0xC6, 0xFE, 0xC6, 0xC6, 0x00, 
		0xF8, 0xCC, 0xCC, 0xF8, 0xCC, 0xCC, 0xF8, 0x00, 0x78, 0xCC, 0xC0, 0xC0, 0xC0, 0xCC, 0x78, 0x00, 
		0xF8, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xF8, 0x00, 0xFC, 0xC0, 0xC0, 0xF0, 0xC0, 0xC0, 0xFC, 0x00, 
		0xFC, 0xC0, 0xC0, 0xF0, 0xC0, 0xC0, 0xC0, 0x00, 0x78, 0xCC, 0xC0, 0xDC, 0xCC, 0xCC, 0x7C, 0x00, 
		0xCC, 0xCC, 0xCC, 0xFC, 0xCC, 0xCC, 0xCC, 0x00, 0xF0, 0x60, 0x60, 0x60, 0x60, 0x60, 0xF0, 0x00, 
		0x0C, 0x0C, 0x0C, 0x0C, 0xCC, 0xCC, 0x78, 0x00, 0xC6, 0xCC, 0xD8, 0xF8, 0xD8, 0xCC, 0xC6, 0x00, 
		0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0xFC, 0x00, 0x82, 0xC6, 0xEE, 0xFE, 0xD6, 0xC6, 0xC6, 0x00, 
		0xC6, 0xE6, 0xF6, 0xDE, 0xCE, 0xC6, 0xC6, 0x00, 0x78, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0x78, 0x00, 
		0xF8, 0xCC, 0xCC, 0xF8, 0xC0, 0xC0, 0xC0, 0x00, 0x78, 0xCC, 0xCC, 0xCC, 0xCC, 0xDC, 0x78, 0x0C, 
		0xF8, 0xCC, 0xCC, 0xF8, 0xD8, 0xCC, 0xCC, 0x00, 0x78, 0xCC, 0xC0, 0x78, 0x0C, 0xCC, 0x78, 0x00, 
		0xFC, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x00, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0x7C, 0x00, 
		0xC6, 0xC6, 0x6C, 0x6C, 0x38, 0x38, 0x10, 0x00, 0xC6, 0xC6, 0xC6, 0xD6, 0xFE, 0xEE, 0xC6, 0x00, 
		0xC6, 0x6C, 0x38, 0x10, 0x38, 0x6C, 0xC6, 0x00, 0xCC, 0xCC, 0xCC, 0x78, 0x30, 0x30, 0x30, 0x00, 
		0xFC, 0x0C, 0x18, 0x30, 0x60, 0xC0, 0xFC, 0x00, 0xF0, 0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0xF0, 0x00, 
		0xC0, 0x60, 0x30, 0x18, 0x0C, 0x06, 0x03, 0x00, 0xF0, 0x30, 0x30, 0x30, 0x30, 0x30, 0xF0, 0x00, 
		0xE8, 0x4D, 0x4A, 0x48, 0x00, 0x00, 0x00, 0x00, 0x80, 0x80, 0x80, 0x80, 0x00, 0x00, 0x00, 0x00, 
		0xC0, 0xC0, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x78, 0x0C, 0x7C, 0xCC, 0x7C, 0x00, 
		0xC0, 0xC0, 0xF8, 0xCC, 0xCC, 0xCC, 0xF8, 0x00, 0x00, 0x00, 0x78, 0xCC, 0xC0, 0xCC, 0x78, 0x00, 
		0x0C, 0x0C, 0x7C, 0xCC, 0xCC, 0xCC, 0x7C, 0x00, 0x00, 0x00, 0x78, 0xCC, 0xFC, 0xC0, 0x78, 0x00, 
		0x38, 0x6C, 0x60, 0xF8, 0x60, 0x60, 0x60, 0x00, 0x00, 0x00, 0x7C, 0xCC, 0xCC, 0x7C, 0x0C, 0x78, 
		0xC0, 0xC0, 0xF8, 0xCC, 0xCC, 0xCC, 0xCC, 0x00, 0xC0, 0x00, 0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0x00, 
		0x0C, 0x00, 0x0C, 0x0C, 0x0C, 0x0C, 0xCC, 0x78, 0xC0, 0xC0, 0xCC, 0xD8, 0xF0, 0xD8, 0xCC, 0x00, 
		0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0x00, 0x00, 0x00, 0xCC, 0xEE, 0xD6, 0xC6, 0xC6, 0x00, 
		0x00, 0x00, 0xF8, 0xCC, 0xCC, 0xCC, 0xCC, 0x00, 0x00, 0x00, 0x78, 0xCC, 0xCC, 0xCC, 0x78, 0x00, 
		0x00, 0x00, 0xF8, 0xCC, 0xCC, 0xF8, 0xC0, 0xC0, 0x00, 0x00, 0x7C, 0xCC, 0xCC, 0x7C, 0x0C, 0x0C, 
		0x00, 0x00, 0xF8, 0xCC, 0xC0, 0xC0, 0xC0, 0x00, 0x00, 0x00, 0x7C, 0xC0, 0x78, 0x0C, 0x78, 0x00, 
		0x30, 0x30, 0xFC, 0x30, 0x30, 0x30, 0x30, 0x00, 0x00, 0x00, 0xCC, 0xCC, 0xCC, 0xCC, 0x7C, 0x00, 
		0x00, 0x00, 0xCC, 0xCC, 0xCC, 0x78, 0x30, 0x00, 0x00, 0x00, 0xC6, 0xD6, 0xD6, 0x6C, 0x6C, 0x00, 
		0x00, 0x00, 0xCC, 0x78, 0x30, 0x78, 0xCC, 0x00, 0x00, 0x00, 0xCC, 0xCC, 0xCC, 0x78, 0x30, 0xE0, 
		0x00, 0x00, 0xFC, 0x18, 0x30, 0x60, 0xFC, 0x00, 0x38, 0x60, 0x60, 0xC0, 0x60, 0x60, 0x38, 0x00, 
		0xC0, 0xC0, 0xC0, 0x00, 0xC0, 0xC0, 0xC0, 0x00, 0xE0, 0x30, 0x30, 0x18, 0x30, 0x30, 0xE0, 0x00, 
		0x38, 0x44, 0xBA, 0xAA, 0xBA, 0x44, 0x38, 0x00, 0x00, 0x98, 0x30, 0x60, 0xC8, 0x98, 0x30, 0x00, 
		0x1E, 0x30, 0x60, 0x60, 0x30, 0x1E, 0x0C, 0x18, 0x00, 0x66, 0x00, 0x66, 0x66, 0x66, 0x3E, 0x00, 
		0x0C, 0x18, 0x3C, 0x66, 0x7E, 0x60, 0x3C, 0x00, 0x18, 0x66, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x18, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1E, 0x30, 0x60, 0x60, 0x30, 0x1E, 0x0C, 0x18, 
		0x18, 0x66, 0x3C, 0x66, 0x7E, 0x60, 0x3C, 0x00, 0x66, 0x00, 0x3C, 0x66, 0x7E, 0x60, 0x3C, 0x00, 
		0x30, 0x18, 0x3C, 0x66, 0x7E, 0x60, 0x3C, 0x00, 0x00, 0x66, 0x00, 0x18, 0x18, 0x18, 0x18, 0x00, 
		0x18, 0x66, 0x00, 0x18, 0x18, 0x18, 0x18, 0x00, 0x30, 0x18, 0x00, 0x18, 0x18, 0x18, 0x18, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x18, 0x30, 0xFC, 0xC0, 0xF0, 0xC0, 0xFC, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x66, 0x00, 0x3C, 0x66, 0x66, 0x3C, 0x00, 
		0x00, 0x66, 0x00, 0x3C, 0x66, 0x66, 0x3C, 0x00, 0x30, 0x18, 0x00, 0x3C, 0x66, 0x66, 0x3C, 0x00, 
		0x18, 0x66, 0x00, 0x66, 0x66, 0x66, 0x3E, 0x00, 0x30, 0x18, 0x00, 0x66, 0x66, 0x66, 0x3E, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x18, 0x30, 0x78, 0x0C, 0x7C, 0xCC, 0x7C, 0x00, 0x0C, 0x18, 0x00, 0x18, 0x18, 0x18, 0x18, 0x00, 
		0x18, 0x30, 0x00, 0x78, 0xCC, 0xCC, 0x78, 0x00, 0x18, 0x30, 0x00, 0xCC, 0xCC, 0xCC, 0x7C, 0x00, 
		0x71, 0x8E, 0x00, 0x7C, 0x66, 0x66, 0x66, 0x00, 0x71, 0xCE, 0xE6, 0xF6, 0xDE, 0xCE, 0xC6, 0x00, 
		0x18, 0x18, 0x18, 0x00, 0x18, 0x18, 0x18, 0x00, 0x3C, 0x60, 0x3C, 0x66, 0x3C, 0x06, 0x3C, 0x00, 
		0x18, 0x00, 0x18, 0x0C, 0x06, 0x66, 0x3C, 0x00, 0x3F, 0x40, 0x4E, 0x58, 0x4E, 0x40, 0x3F, 0x00, 
		0x1C, 0xA4, 0xC4, 0xBC, 0x80, 0xFE, 0x00, 0x00, 0x00, 0x33, 0x66, 0xCC, 0x66, 0x33, 0x00, 0x00, 
		0x3E, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC0, 0xC0, 0x00, 0xC0, 0xC0, 0xC0, 0xC0, 0x00, 
		0x81, 0xB9, 0xA5, 0xB9, 0xA5, 0x81, 0x7E, 0x00, 0xFC, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x78, 0xCC, 0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x30, 0xFC, 0x30, 0x30, 0x00, 0xFC, 0x00, 
		0xF0, 0x18, 0x30, 0x60, 0xF8, 0x00, 0x00, 0x00, 0xF0, 0x18, 0x30, 0x18, 0xF0, 0x00, 0x00, 0x00, 
		0x30, 0x60, 0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xCC, 0xCC, 0xCC, 0xCC, 0xFE, 0xC0, 
		0x3E, 0x7A, 0x7A, 0x3A, 0x0A, 0x0A, 0x0A, 0x00, 0x00, 0x00, 0x18, 0x18, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x60, 0x60, 0xE0, 0x60, 0x60, 0x60, 0x00, 0x00, 0x00, 
		0x38, 0x44, 0x44, 0x38, 0x00, 0x7C, 0x00, 0x00, 0x00, 0xCC, 0x66, 0x33, 0x66, 0xCC, 0x00, 0x00, 
		0x40, 0xC6, 0x4C, 0x58, 0x32, 0x66, 0xCF, 0x02, 0x40, 0xC6, 0x4C, 0x58, 0x3E, 0x62, 0xC4, 0x0E, 
		0xC0, 0x23, 0x66, 0x2C, 0xD9, 0x33, 0x67, 0x01, 0x18, 0x00, 0x18, 0x30, 0x60, 0x66, 0x3C, 0x00, 
		0x60, 0x30, 0x7C, 0xC6, 0xFE, 0xC6, 0xC6, 0x00, 0x0C, 0x18, 0x7C, 0xC6, 0xFE, 0xC6, 0xC6, 0x00, 
		0x38, 0xC6, 0x7C, 0xC6, 0xFE, 0xC6, 0xC6, 0x00, 0x71, 0x8E, 0x7C, 0xC6, 0xFE, 0xC6, 0xC6, 0x00, 
		0x6C, 0x00, 0x7C, 0xC6, 0xFE, 0xC6, 0xC6, 0x00, 0x38, 0x44, 0x7C, 0xC6, 0xFE, 0xC6, 0xC6, 0x00, 
		0x1F, 0x3C, 0x3C, 0x6F, 0x7C, 0xCC, 0xCF, 0x00, 0x1E, 0x30, 0x60, 0x60, 0x30, 0x1E, 0x0C, 0x18, 
		0x60, 0x30, 0xFC, 0xC0, 0xF0, 0xC0, 0xFC, 0x00, 0x18, 0x30, 0xFC, 0xC0, 0xF0, 0xC0, 0xFC, 0x00, 
		0x30, 0xCC, 0xFC, 0xC0, 0xF0, 0xC0, 0xFC, 0x00, 0xCC, 0x00, 0xFC, 0xC0, 0xF0, 0xC0, 0xFC, 0x00, 
		0x60, 0x30, 0x78, 0x30, 0x30, 0x30, 0x78, 0x00, 0x18, 0x30, 0x78, 0x30, 0x30, 0x30, 0x78, 0x00, 
		0x30, 0xCC, 0x78, 0x30, 0x30, 0x30, 0x78, 0x00, 0xCC, 0x00, 0x78, 0x30, 0x30, 0x30, 0x78, 0x00, 
		0x78, 0x6C, 0x66, 0xF6, 0x66, 0x6C, 0x78, 0x00, 0x71, 0xCE, 0xE6, 0xF6, 0xDE, 0xCE, 0xC6, 0x00, 
		0x30, 0x18, 0x3C, 0x66, 0x66, 0x66, 0x3C, 0x00, 0x0C, 0x18, 0x3C, 0x66, 0x66, 0x66, 0x3C, 0x00, 
		0x18, 0x66, 0x3C, 0x66, 0x66, 0x66, 0x3C, 0x00, 0x71, 0x8E, 0x3C, 0x66, 0x66, 0x66, 0x3C, 0x00, 
		0xC3, 0x3C, 0x66, 0x66, 0x66, 0x66, 0x3C, 0x00, 0x00, 0xC6, 0x6C, 0x38, 0x6C, 0xC6, 0x00, 0x00, 
		0x3F, 0x66, 0x6E, 0x7E, 0x76, 0x66, 0xFC, 0x00, 0x30, 0x18, 0x66, 0x66, 0x66, 0x66, 0x3E, 0x00, 
		0x0C, 0x18, 0x66, 0x66, 0x66, 0x66, 0x3E, 0x00, 0x18, 0x24, 0x66, 0x66, 0x66, 0x66, 0x3E, 0x00, 
		0x66, 0x00, 0x66, 0x66, 0x66, 0x66, 0x3E, 0x00, 0x06, 0x08, 0xC3, 0x66, 0x3C, 0x18, 0x18, 0x00, 
		0x60, 0x60, 0x7E, 0x63, 0x7E, 0x60, 0x60, 0x00, 0x3C, 0x66, 0x66, 0x6C, 0x66, 0x66, 0x6C, 0x60, 
		0x30, 0x18, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00, 0x0C, 0x18, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00, 
		0x18, 0x66, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00, 0x71, 0x8E, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00, 
		0x66, 0x00, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00, 0x18, 0x24, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00, 
		0x00, 0x00, 0x7E, 0x1B, 0x7F, 0xD8, 0x77, 0x00, 0x00, 0x00, 0x3C, 0x60, 0x60, 0x60, 0x3C, 0x18, 
		0x30, 0x18, 0x3C, 0x66, 0x7E, 0x60, 0x3C, 0x00, 0x0C, 0x18, 0x3C, 0x66, 0x7E, 0x60, 0x3C, 0x00, 
		0x18, 0x66, 0x3C, 0x66, 0x7E, 0x60, 0x3C, 0x00, 0x66, 0x00, 0x3C, 0x66, 0x7E, 0x60, 0x3C, 0x00, 
		0x30, 0x18, 0x00, 0x18, 0x18, 0x18, 0x18, 0x00, 0x0C, 0x18, 0x00, 0x18, 0x18, 0x18, 0x18, 0x00, 
		0x18, 0x66, 0x00, 0x18, 0x18, 0x18, 0x18, 0x00, 0x00, 0x66, 0x00, 0x18, 0x18, 0x18, 0x18, 0x00, 
		0x60, 0xFC, 0x18, 0x3C, 0x66, 0x66, 0x3C, 0x00, 0x71, 0x8E, 0x00, 0x7C, 0x66, 0x66, 0x66, 0x00, 
		0x30, 0x18, 0x00, 0x3C, 0x66, 0x66, 0x3C, 0x00, 0x0C, 0x18, 0x00, 0x3C, 0x66, 0x66, 0x3C, 0x00, 
		0x18, 0x66, 0x00, 0x3C, 0x66, 0x66, 0x3C, 0x00, 0x71, 0x8E, 0x00, 0x3C, 0x66, 0x66, 0x3C, 0x00, 
		0x00, 0x66, 0x00, 0x3C, 0x66, 0x66, 0x3C, 0x00, 0x00, 0x18, 0x00, 0x7E, 0x00, 0x18, 0x00, 0x00, 
		0x00, 0x02, 0x7C, 0xCE, 0xD6, 0xE6, 0x7C, 0x80, 0x30, 0x18, 0x00, 0x66, 0x66, 0x66, 0x3E, 0x00, 
		0x0C, 0x18, 0x00, 0x66, 0x66, 0x66, 0x3E, 0x00, 0x18, 0x66, 0x00, 0x66, 0x66, 0x66, 0x3E, 0x00, 
		0x00, 0x66, 0x00, 0x66, 0x66, 0x66, 0x3E, 0x00, 0x0C, 0x18, 0x00, 0x66, 0x66, 0x3C, 0x18, 0x30, 
		0x60, 0x60, 0x7C, 0x66, 0x66, 0x7C, 0x60, 0x60, 0x00, 0x66, 0x00, 0x66, 0x66, 0x3C, 0x18, 0x30
	};
	writeRecord(_fontRegular, sizeof(_fontRegular), GBVARS_DISPLAYFONTREGULAR_INDEX , GBVARS_QUEEN);
}

static void addDisplay_fontHebrew() {
	uint8 _fontHebrew[] = {
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 0xF8, 0xB0, 0xB0, 0x80, 0xB0, 0xB0, 0xC0, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xC0, 0xC0, 0xC0, 0xC0, 0x00, 0xC0, 0xC0, 0x00, 
		0xD8, 0xD8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x6C, 0x6C, 0xFE, 0x6C, 0xFE, 0x6C, 0x6C, 0x00, 
		0x30, 0x7C, 0xC0, 0x78, 0x0C, 0xF8, 0x30, 0x00, 0x00, 0xC6, 0xCC, 0x18, 0x30, 0x66, 0xC6, 0x00, 
		0x38, 0x6C, 0x68, 0x36, 0xDC, 0xCC, 0x76, 0x00, 0x60, 0x60, 0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x30, 0x60, 0xC0, 0xC0, 0xC0, 0x60, 0x30, 0x00, 0xC0, 0x60, 0x30, 0x30, 0x30, 0x60, 0xC0, 0x00, 
		0x00, 0x6C, 0x38, 0xFE, 0x38, 0x6C, 0x00, 0x00, 0x00, 0x30, 0x30, 0xFC, 0x30, 0x30, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x60, 0x60, 0xC0, 0x00, 0x00, 0x00, 0xFC, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0xC0, 0xC0, 0x00, 0x03, 0x06, 0x0C, 0x18, 0x30, 0x60, 0xC0, 0x00, 
		0x78, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0x78, 0x00, 0x30, 0x70, 0xF0, 0x30, 0x30, 0x30, 0x30, 0x00, 
		0x78, 0xCC, 0x0C, 0x78, 0xC0, 0xC0, 0xFC, 0x00, 0x78, 0xCC, 0x0C, 0x38, 0x0C, 0xCC, 0x78, 0x00, 
		0x1C, 0x3C, 0x6C, 0xCC, 0xFC, 0x0C, 0x0C, 0x00, 0xFC, 0xC0, 0xF8, 0x0C, 0x0C, 0xCC, 0x78, 0x00, 
		0x78, 0xCC, 0xC0, 0xF8, 0xCC, 0xCC, 0x78, 0x00, 0xFC, 0xCC, 0x0C, 0x18, 0x30, 0x30, 0x30, 0x00, 
		0x78, 0xCC, 0xCC, 0x78, 0xCC, 0xCC, 0x78, 0x00, 0x78, 0xCC, 0xCC, 0x7C, 0x0C, 0xCC, 0x78, 0x00, 
		0x00, 0xC0, 0xC0, 0x00, 0x00, 0xC0, 0xC0, 0x00, 0x00, 0x60, 0x60, 0x00, 0x00, 0x60, 0x60, 0xC0, 
		0x18, 0x30, 0x60, 0xC0, 0x60, 0x30, 0x18, 0x00, 0x00, 0x00, 0xFC, 0x00, 0xFC, 0x00, 0x00, 0x00, 
		0xC0, 0x60, 0x30, 0x18, 0x30, 0x60, 0xC0, 0x00, 0x78, 0xCC, 0x0C, 0x18, 0x30, 0x00, 0x30, 0x00, 
		0x6C, 0xFE, 0xFE, 0xFE, 0x7C, 0x38, 0x10, 0x00, 0x38, 0x7C, 0xC6, 0xC6, 0xFE, 0xC6, 0xC6, 0x00, 
		0xF8, 0xCC, 0xCC, 0xF8, 0xCC, 0xCC, 0xF8, 0x00, 0x78, 0xCC, 0xC0, 0xC0, 0xC0, 0xCC, 0x78, 0x00, 
		0xF8, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xF8, 0x00, 0xFC, 0xC0, 0xC0, 0xF0, 0xC0, 0xC0, 0xFC, 0x00, 
		0xFC, 0xC0, 0xC0, 0xF0, 0xC0, 0xC0, 0xC0, 0x00, 0x78, 0xCC, 0xC0, 0xDC, 0xCC, 0xCC, 0x7C, 0x00, 
		0xCC, 0xCC, 0xCC, 0xFC, 0xCC, 0xCC, 0xCC, 0x00, 0xF0, 0x60, 0x60, 0x60, 0x60, 0x60, 0xF0, 0x00, 
		0x0C, 0x0C, 0x0C, 0x0C, 0xCC, 0xCC, 0x78, 0x00, 0xC6, 0xCC, 0xD8, 0xF8, 0xD8, 0xCC, 0xC6, 0x00, 
		0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0xFC, 0x00, 0x82, 0xC6, 0xEE, 0xFE, 0xD6, 0xC6, 0xC6, 0x00, 
		0xC6, 0xE6, 0xF6, 0xDE, 0xCE, 0xC6, 0xC6, 0x00, 0x78, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0x78, 0x00, 
		0xF8, 0xCC, 0xCC, 0xF8, 0xC0, 0xC0, 0xC0, 0x00, 0x78, 0xCC, 0xCC, 0xCC, 0xCC, 0xDC, 0x78, 0x0C, 
		0xF8, 0xCC, 0xCC, 0xF8, 0xD8, 0xCC, 0xCC, 0x00, 0x78, 0xCC, 0xC0, 0x78, 0x0C, 0xCC, 0x78, 0x00, 
		0xFC, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x00, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0xCC, 0x7C, 0x00, 
		0xC6, 0xC6, 0x6C, 0x6C, 0x38, 0x38, 0x10, 0x00, 0xC6, 0xC6, 0xC6, 0xD6, 0xFE, 0xEE, 0xC6, 0x00, 
		0xC6, 0x6C, 0x38, 0x10, 0x38, 0x6C, 0xC6, 0x00, 0xCC, 0xCC, 0xCC, 0x78, 0x30, 0x30, 0x30, 0x00, 
		0xFC, 0x0C, 0x18, 0x30, 0x60, 0xC0, 0xFC, 0x00, 0xF0, 0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0xF0, 0x00, 
		0xC0, 0x60, 0x30, 0x18, 0x0C, 0x06, 0x03, 0x00, 0xF0, 0x30, 0x30, 0x30, 0x30, 0x30, 0xF0, 0x00, 
		0xE8, 0x4D, 0x4A, 0x48, 0x00, 0x00, 0x00, 0x00, 0x80, 0x80, 0x80, 0x80, 0x00, 0x00, 0x00, 0x00, 
		0xC0, 0xC0, 0x60, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x78, 0x0C, 0x7C, 0xCC, 0x7C, 0x00, 
		0xC0, 0xC0, 0xF8, 0xCC, 0xCC, 0xCC, 0xF8, 0x00, 0x00, 0x00, 0x78, 0xCC, 0xC0, 0xCC, 0x78, 0x00, 
		0x0C, 0x0C, 0x7C, 0xCC, 0xCC, 0xCC, 0x7C, 0x00, 0x00, 0x00, 0x78, 0xCC, 0xFC, 0xC0, 0x78, 0x00, 
		0x38, 0x6C, 0x60, 0xF8, 0x60, 0x60, 0x60, 0x00, 0x00, 0x00, 0x7C, 0xCC, 0xCC, 0x7C, 0x0C, 0x78, 
		0xC0, 0xC0, 0xF8, 0xCC, 0xCC, 0xCC, 0xCC, 0x00, 0xC0, 0x00, 0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0x00, 
		0x0C, 0x00, 0x0C, 0x0C, 0x0C, 0x0C, 0xCC, 0x78, 0xC0, 0xC0, 0xCC, 0xD8, 0xF0, 0xD8, 0xCC, 0x00, 
		0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0x00, 0x00, 0x00, 0xCC, 0xEE, 0xD6, 0xC6, 0xC6, 0x00, 
		0x00, 0x00, 0xF8, 0xCC, 0xCC, 0xCC, 0xCC, 0x00, 0x00, 0x00, 0x78, 0xCC, 0xCC, 0xCC, 0x78, 0x00, 
		0x00, 0x00, 0xF8, 0xCC, 0xCC, 0xF8, 0xC0, 0xC0, 0x00, 0x00, 0x7C, 0xCC, 0xCC, 0x7C, 0x0C, 0x0C, 
		0x00, 0x00, 0xF8, 0xCC, 0xC0, 0xC0, 0xC0, 0x00, 0x00, 0x00, 0x7C, 0xC0, 0x78, 0x0C, 0x78, 0x00, 
		0x30, 0x30, 0xFC, 0x30, 0x30, 0x30, 0x30, 0x00, 0x00, 0x00, 0xCC, 0xCC, 0xCC, 0xCC, 0x7C, 0x00, 
		0x00, 0x00, 0xCC, 0xCC, 0xCC, 0x78, 0x30, 0x00, 0x00, 0x00, 0xC6, 0xD6, 0xD6, 0x6C, 0x6C, 0x00, 
		0x00, 0x00, 0xCC, 0x78, 0x30, 0x78, 0xCC, 0x00, 0x00, 0x00, 0xCC, 0xCC, 0xCC, 0x78, 0x30, 0xE0, 
		0x00, 0x00, 0xFC, 0x18, 0x30, 0x60, 0xFC, 0x00, 0x38, 0x60, 0x60, 0xC0, 0x60, 0x60, 0x38, 0x00, 
		0xC0, 0xC0, 0xC0, 0x00, 0xC0, 0xC0, 0xC0, 0x00, 0xE0, 0x30, 0x30, 0x18, 0x30, 0x30, 0xE0, 0x00, 
		0x38, 0x44, 0xBA, 0xAA, 0xBA, 0x44, 0x38, 0x00, 0x00, 0x98, 0x30, 0x60, 0xC8, 0x98, 0x30, 0x00, 
		0xCC, 0x66, 0x76, 0xBC, 0x98, 0x8C, 0xE6, 0x00, 0xFC, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0xFE, 0x00, 
		0x78, 0x18, 0x18, 0x18, 0x38, 0x78, 0xD8, 0x00, 0xFE, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x00, 
		0xFE, 0x06, 0x06, 0xC6, 0xC6, 0xC6, 0xC6, 0x00, 0x78, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x00, 
		0x7C, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x00, 0xFE, 0x66, 0x66, 0x66, 0x66, 0x66, 0x66, 0x00, 
		0xDC, 0x66, 0xE6, 0xC6, 0xC6, 0xC6, 0x7C, 0x00, 0xF0, 0x30, 0x30, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0xFE, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0xF8, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0xF8, 0x00, 
		0xC0, 0xFE, 0x06, 0x06, 0x0C, 0x18, 0x18, 0x00, 0xFE, 0x66, 0x66, 0x66, 0x66, 0x66, 0x7E, 0x00, 
		0xFC, 0x76, 0x66, 0x66, 0x66, 0x66, 0x6E, 0x00, 0x78, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 0x18, 
		0x78, 0x18, 0x18, 0x18, 0x18, 0x18, 0x78, 0x00, 0xFE, 0x66, 0x66, 0x66, 0x66, 0x66, 0x3C, 0x00, 
		0xEE, 0x66, 0x66, 0x66, 0x66, 0x6C, 0xF8, 0x00, 0xFE, 0xC6, 0xC6, 0xF6, 0x06, 0x06, 0x06, 0x06, 
		0xFE, 0xC6, 0xC6, 0xFE, 0x06, 0x06, 0xFE, 0x00, 0xFE, 0x66, 0x6C, 0x78, 0x60, 0x60, 0x60, 0x60, 
		0xEE, 0x66, 0x3C, 0x18, 0x0C, 0x06, 0xFE, 0x00, 0xFE, 0x06, 0x0E, 0xD8, 0xF0, 0xF0, 0xC0, 0xC0, 
		0xFC, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x0C, 0x00, 0xEE, 0xCA, 0xCA, 0xCA, 0xCA, 0xCA, 0x7C, 0x00, 
		0xFF, 0x67, 0x67, 0x67, 0x67, 0x67, 0xE7, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x18, 0x00, 0x18, 0x18, 0x18, 0x18, 0x18, 0x00, 
		0x00, 0x0C, 0x3E, 0x6C, 0x3E, 0x0C, 0x00, 0x00, 0x38, 0x6C, 0x60, 0xF0, 0x60, 0x60, 0xFC, 0x00, 
		0x42, 0x3C, 0x66, 0x3C, 0x42, 0x00, 0x00, 0x00, 0xC3, 0x66, 0x3C, 0x18, 0x3C, 0x18, 0x18, 0x00, 
		0x18, 0x18, 0x18, 0x00, 0x18, 0x18, 0x18, 0x00, 0x3C, 0x60, 0x3C, 0x66, 0x3C, 0x06, 0x3C, 0x00, 
		0x66, 0x66, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3F, 0x40, 0x4E, 0x58, 0x4E, 0x40, 0x3F, 0x00, 
		0x1C, 0xA4, 0xC4, 0xBC, 0x80, 0xFE, 0x00, 0x00, 0x00, 0x33, 0x66, 0xCC, 0x66, 0x33, 0x00, 0x00, 
		0x3E, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x7E, 0x00, 0x00, 0x00, 0x00, 
		0x81, 0xB9, 0xA5, 0xB9, 0xA5, 0x81, 0x7E, 0x00, 0xFC, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
		0x78, 0xCC, 0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x30, 0xFC, 0x30, 0x30, 0x00, 0xFC, 0x00, 
		0xF0, 0x18, 0x30, 0x60, 0xF8, 0x00, 0x00, 0x00, 0xF0, 0x18, 0x30, 0x18, 0xF0, 0x00, 0x00, 0x00, 
		0x30, 0x60, 0xC0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xCC, 0xCC, 0xCC, 0xCC, 0xFE, 0xC0, 
		0x3E, 0x7A, 0x7A, 0x3A, 0x0A, 0x0A, 0x0A, 0x00, 0x00, 0x00, 0x18, 0x18, 0x00, 0x00, 0x00, 0x00, 
		0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30, 0x60, 0x60, 0xE0, 0x60, 0x60, 0x60, 0x00, 0x00, 0x00, 
		0x38, 0x44, 0x44, 0x38, 0x00, 0x7C, 0x00, 0x00, 0x00, 0xCC, 0x66, 0x33, 0x66, 0xCC, 0x00, 0x00, 
		0x40, 0xC6, 0x4C, 0x58, 0x32, 0x66, 0xCF, 0x02, 0x40, 0xC6, 0x4C, 0x58, 0x3E, 0x62, 0xC4, 0x0E, 
		0xC0, 0x23, 0x66, 0x2C, 0xD9, 0x33, 0x67, 0x01, 0x18, 0x00, 0x18, 0x30, 0x60, 0x66, 0x3C, 0x00, 
		0x60, 0x30, 0x7C, 0xC6, 0xFE, 0xC6, 0xC6, 0x00, 0x0C, 0x18, 0x7C, 0xC6, 0xFE, 0xC6, 0xC6, 0x00, 
		0x38, 0xC6, 0x7C, 0xC6, 0xFE, 0xC6, 0xC6, 0x00, 0x71, 0x8E, 0x7C, 0xC6, 0xFE, 0xC6, 0xC6, 0x00, 
		0x6C, 0x00, 0x7C, 0xC6, 0xFE, 0xC6, 0xC6, 0x00, 0x38, 0x44, 0x7C, 0xC6, 0xFE, 0xC6, 0xC6, 0x00, 
		0x1F, 0x3C, 0x3C, 0x6F, 0x7C, 0xCC, 0xCF, 0x00, 0x1E, 0x30, 0x60, 0x60, 0x30, 0x1E, 0x0C, 0x18, 
		0x60, 0x30, 0xFC, 0xC0, 0xF0, 0xC0, 0xFC, 0x00, 0x18, 0x30, 0xFC, 0xC0, 0xF0, 0xC0, 0xFC, 0x00, 
		0x30, 0xCC, 0xFC, 0xC0, 0xF0, 0xC0, 0xFC, 0x00, 0xCC, 0x00, 0xFC, 0xC0, 0xF0, 0xC0, 0xFC, 0x00, 
		0x60, 0x30, 0x78, 0x30, 0x30, 0x30, 0x78, 0x00, 0x18, 0x30, 0x78, 0x30, 0x30, 0x30, 0x78, 0x00, 
		0x30, 0xCC, 0x78, 0x30, 0x30, 0x30, 0x78, 0x00, 0xCC, 0x00, 0x78, 0x30, 0x30, 0x30, 0x78, 0x00, 
		0x78, 0x6C, 0x66, 0xF6, 0x66, 0x6C, 0x78, 0x00, 0x71, 0xCE, 0xE6, 0xF6, 0xDE, 0xCE, 0xC6, 0x00, 
		0x30, 0x18, 0x3C, 0x66, 0x66, 0x66, 0x3C, 0x00, 0x0C, 0x18, 0x3C, 0x66, 0x66, 0x66, 0x3C, 0x00, 
		0x18, 0x66, 0x3C, 0x66, 0x66, 0x66, 0x3C, 0x00, 0x71, 0x8E, 0x3C, 0x66, 0x66, 0x66, 0x3C, 0x00, 
		0xC3, 0x3C, 0x66, 0x66, 0x66, 0x66, 0x3C, 0x00, 0x00, 0xC6, 0x6C, 0x38, 0x6C, 0xC6, 0x00, 0x00, 
		0x3F, 0x66, 0x6E, 0x7E, 0x76, 0x66, 0xFC, 0x00, 0x30, 0x18, 0x66, 0x66, 0x66, 0x66, 0x3E, 0x00, 
		0x0C, 0x18, 0x66, 0x66, 0x66, 0x66, 0x3E, 0x00, 0x18, 0x24, 0x66, 0x66, 0x66, 0x66, 0x3E, 0x00, 
		0x66, 0x00, 0x66, 0x66, 0x66, 0x66, 0x3E, 0x00, 0x06, 0x08, 0xC3, 0x66, 0x3C, 0x18, 0x18, 0x00, 
		0x60, 0x60, 0x7E, 0x63, 0x7E, 0x60, 0x60, 0x00, 0x3C, 0x66, 0x66, 0x6C, 0x66, 0x66, 0x6C, 0x60, 
		0x30, 0x18, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00, 0x0C, 0x18, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00, 
		0x18, 0x66, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00, 0x71, 0x8E, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00, 
		0x66, 0x00, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00, 0x18, 0x24, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00, 
		0x00, 0x00, 0x7E, 0x1B, 0x7F, 0xD8, 0x77, 0x00, 0x00, 0x00, 0x3C, 0x60, 0x60, 0x60, 0x3C, 0x18, 
		0x30, 0x18, 0x3C, 0x66, 0x7E, 0x60, 0x3C, 0x00, 0x0C, 0x18, 0x3C, 0x66, 0x7E, 0x60, 0x3C, 0x00, 
		0x18, 0x66, 0x3C, 0x66, 0x7E, 0x60, 0x3C, 0x00, 0x66, 0x00, 0x3C, 0x66, 0x7E, 0x60, 0x3C, 0x00, 
		0x30, 0x18, 0x00, 0x18, 0x18, 0x18, 0x18, 0x00, 0x0C, 0x18, 0x00, 0x18, 0x18, 0x18, 0x18, 0x00, 
		0x18, 0x66, 0x00, 0x18, 0x18, 0x18, 0x18, 0x00, 0x00, 0x66, 0x00, 0x18, 0x18, 0x18, 0x18, 0x00, 
		0x60, 0xFC, 0x18, 0x3C, 0x66, 0x66, 0x3C, 0x00, 0x71, 0x8E, 0x00, 0x7C, 0x66, 0x66, 0x66, 0x00, 
		0x30, 0x18, 0x00, 0x3C, 0x66, 0x66, 0x3C, 0x00, 0x0C, 0x18, 0x00, 0x3C, 0x66, 0x66, 0x3C, 0x00, 
		0x18, 0x66, 0x00, 0x3C, 0x66, 0x66, 0x3C, 0x00, 0x71, 0x8E, 0x00, 0x3C, 0x66, 0x66, 0x3C, 0x00, 
		0x00, 0x66, 0x00, 0x3C, 0x66, 0x66, 0x3C, 0x00, 0x00, 0x18, 0x00, 0x7E, 0x00, 0x18, 0x00, 0x00, 
		0x00, 0x02, 0x7C, 0xCE, 0xD6, 0xE6, 0x7C, 0x80, 0x30, 0x18, 0x00, 0x66, 0x66, 0x66, 0x3E, 0x00, 
		0x0C, 0x18, 0x00, 0x66, 0x66, 0x66, 0x3E, 0x00, 0x18, 0x66, 0x00, 0x66, 0x66, 0x66, 0x3E, 0x00, 
		0x00, 0x66, 0x00, 0x66, 0x66, 0x66, 0x3E, 0x00, 0x0C, 0x18, 0x00, 0x66, 0x66, 0x3C, 0x18, 0x30, 
		0x60, 0x60, 0x7C, 0x66, 0x66, 0x7C, 0x60, 0x60, 0x00, 0x66, 0x00, 0x66, 0x66, 0x3C, 0x18, 0x30
	};
	writeRecord(_fontHebrew, sizeof(_fontHebrew), GBVARS_DISPLAYFONTHEBREW_INDEX , GBVARS_QUEEN);
}

static void addDisplay_palJoeClothes() {
	uint8 _palJoeClothes[] = {
		0x00, 0x00, 0x00, 0x60, 0x60, 0x60, 0x87, 0x87, 0x87, 0xB0, 0xB0, 0xB0, 0xDA, 0xDA, 0xDA, 0x43,
		0x34, 0x20, 0x77, 0x33, 0x1F, 0xA3, 0x43, 0x27, 0x80, 0x45, 0x45, 0x9E, 0x5D, 0x5B, 0xB9, 0x78,
		0x75, 0xDF, 0x97, 0x91,	0x17, 0x27, 0x63, 0x1F, 0x3F, 0x83, 0x27, 0x5B, 0xA7, 0x98, 0xD4, 0xFF
	};
	writeRecord(_palJoeClothes, sizeof(_palJoeClothes), GBVARS_DISPLAYPALJOECLOTHES_INDEX , GBVARS_QUEEN);
}

static void addDisplay_palJoeDress() {
	uint8 _palJoeDress[] = {
		0x00, 0x00, 0x00, 0x50, 0x50, 0x50, 0x70, 0x70, 0x70, 0x90, 0x90, 0x90, 0xC6, 0xC6, 0xC6, 0xFF,
		0xFF, 0xFF, 0x30, 0x30, 0x90, 0x47, 0x49, 0xD0, 0x40, 0x24, 0x00, 0x79, 0x34, 0x0B, 0xB2, 0x3D,
		0x22, 0xED, 0x42, 0x42,	0x80, 0x45, 0x45, 0xA3, 0x5F, 0x5F, 0xC8, 0x7C, 0x7C, 0xEC, 0x9C, 0x9C
	};
	writeRecord(_palJoeDress, sizeof(_palJoeDress), GBVARS_DISPLAYPALJOEDRESS_INDEX , GBVARS_QUEEN);
}

void Queen_addDisplay() {
	addDisplay_fontRegular();
	addDisplay_fontHebrew();
	addDisplay_palJoeClothes();
	addDisplay_palJoeDress();
}