/* ScummVM - Graphic Adventure Engine
 *
 * ScummVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

#include "common/scummsys.h"
#include "xeen/resources.h"

namespace Xeen {

const char *const CREDITS =
	"\013""012\010""000\003""c\014""35Designed and Directed By:\n"
	"\014""17Jon Van Caneghem\003""l\n"
	"\n"
	"\t025\014""35Programming:\n"
	"\t035\014""17Mark Caldwell\n"
	"\t035Dave Hathaway\n"
	"\n"
	"\t025\014""35Sound System & FX:\n"
	"\t035\014""17Mike Heilemann\n"
	"\n"
	"\t025\014""35Music & Speech:\n"
	"\t035\014""17Tim Tully\n"
	"\n"
	"\t025\014""35Writing:\n"
	"\t035\014""17Paul Rattner\n"
	"\t035Debbie Van Caneghem\n"
	"\t035Jon Van Caneghem\013""012\n"
	"\n"
	"\n"
	"\t180\014""35Graphics:\n"
	"\t190\014""17Jonathan P. Gwyn\n"
	"\t190Bonita Long-Hemsath\n"
	"\t190Julia Ulano\n"
	"\t190Ricardo Barrera\n"
	"\n"
	"\t180\014""35Testing:\n"
	"\t190\014""17Benjamin Bent\n"
	"\t190Christian Dailey\n"
	"\t190Mario Escamilla\n"
	"\t190Marco Hunter\n"
	"\t190Robert J. Lupo\n"
	"\t190Clayton Retzer\n"
	"\t190David Vela\003""c";

const char *const OPTIONS_TITLE = 
	"\x0D\x01\003""c\014""dMight and Magic Options\n"
	"World of Xeen\x02\n"
	"\v117Copyright (c) 1993 NWC, Inc.\n"
	"All Rights Reserved\x01";

const char *const TERRAIN_TYPES[6] = {
	"town", "cave", "towr", "cstl", "dung", "scfi"
};

const char *const OUTDOOR_WALLS[15] = {
	nullptr, "mount", "ltree", "dtree", "grass", "snotree", "snomnt",
	"dedltree", "mount", "lavamnt", "palm", "dmount", "dedltree",
	"dedltree", "dedltree"
};

const char *const OUTDOOR_SURFACES[16] = {
	"water.srf", "dirt.srf", "grass.srf", "snow.srf", "swamp.srf",
	"lava.srf", "desert.srf", "road.srf", "dwater.srf", "tflr.srf",
	"sky.srf", "croad.srf", "sewer.srf", "cloud.srf", "scortch.srf",
	"space.srf"
};

const byte SYMBOLS[20][64] = {
	{ // 0
		0x00, 0x00, 0xA8, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0x00, 0xA8, 0x9E, 0x9C, 0x9C, 0x9E, 0x9E, 0x9E,
		0xAC, 0x9C, 0xA4, 0xAC, 0xAC, 0x9A, 0x9A, 0x9A, 0xAC, 0x9E, 0xAC, 0xA8, 0xA8, 0xA6, 0x97, 0x98,
		0xAC, 0xA0, 0xAC, 0xAC, 0xA4, 0xA6, 0x98, 0x99, 0x00, 0xAC, 0xA0, 0xA0, 0xA8, 0xAC, 0x9A, 0x9A,
		0x00, 0x00, 0xAC, 0xAC, 0xAC, 0xA4, 0x9B, 0x9A, 0x00, 0x00, 0x00, 0x00, 0xAC, 0xA0, 0x9B, 0x9B,
	},
	{ // 1
		0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E,
		0x99, 0x9A, 0x9A, 0x99, 0x99, 0x99, 0x9A, 0x99, 0x98, 0x98, 0x98, 0x97, 0x97, 0x97, 0x97, 0x97,
		0x99, 0x98, 0x98, 0x99, 0x98, 0x98, 0x99, 0x99, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A,
		0x9A, 0x9B, 0x9B, 0x9C, 0x9B, 0x9A, 0x9C, 0x9A, 0x9B, 0x9A, 0x99, 0x99, 0x99, 0x9A, 0x9A, 0x9B,
	},
	{ // 2
		0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E,
		0x99, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x99, 0x98, 0x98, 0x99, 0x98, 0x98, 0x97, 0x98, 0x98,
		0x99, 0x98, 0x98, 0x98, 0x99, 0x99, 0x98, 0x99, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A,
		0x9B, 0x9B, 0x9C, 0x9C, 0x9B, 0x9B, 0x9B, 0x9B, 0x99, 0x9A, 0x9B, 0x9B, 0x9A, 0x9A, 0x99, 0x9A,
	},
	{ // 3
		0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E,
		0x99, 0x9A, 0x9A, 0x9A, 0x99, 0x99, 0x99, 0x9A, 0x98, 0x98, 0x97, 0x97, 0x98, 0x98, 0x98, 0x98,
		0x99, 0x99, 0x98, 0x99, 0x98, 0x98, 0x99, 0x99, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A,
		0x9B, 0x9C, 0x9B, 0x9B, 0x9C, 0x9C, 0x9C, 0x9C, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x99, 0x99, 0x9A,
	},
	{ // 4
		0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E,
		0x9A, 0x9A, 0x9A, 0x99, 0x99, 0x99, 0x99, 0x9A, 0x97, 0x97, 0x97, 0x97, 0x97, 0x98, 0x98, 0x98,
		0x99, 0x99, 0x98, 0x99, 0x99, 0x98, 0x98, 0x98, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A,
		0x9A, 0x9C, 0x9B, 0x9B, 0x9C, 0x9B, 0x9B, 0x9B, 0x9A, 0x99, 0x9B, 0x9B, 0x9A, 0x99, 0x9A, 0x9A,
	},
	{ // 5
		0xA4, 0xA4, 0xA8, 0xA8, 0x00, 0x00, 0x00, 0x00, 0x9E, 0x9E, 0x9E, 0xA0, 0xA8, 0xAC, 0x00, 0x00,
		0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9E, 0xAC, 0x00, 0x97, 0x97, 0x97, 0x98, 0x9C, 0x9C, 0xA0, 0xAC,
		0x99, 0x98, 0x99, 0x99, 0x99, 0x9B, 0xA0, 0xAC, 0x9A, 0x9A, 0x9A, 0x9A, 0x9A, 0x9B, 0xA0, 0xAC,
		0x9C, 0x9B, 0x9C, 0x9C, 0x9C, 0xA0, 0xAC, 0x00, 0x99, 0x9A, 0x9A, 0x9B, 0x9B, 0xA4, 0xAC, 0x00,
	},
	{ // 6
		0x00, 0x00, 0x00, 0xAC, 0xA4, 0x9C, 0x99, 0x99, 0x00, 0x00, 0x00, 0xAC, 0xA0, 0x9C, 0x9B, 0x99,
		0x00, 0x00, 0xAC, 0xA0, 0x9C, 0x99, 0x99, 0x99, 0x00, 0xAC, 0xA0, 0x9C, 0x99, 0x98, 0x99, 0x99,
		0x00, 0xAC, 0xA0, 0x9C, 0x9C, 0xA0, 0x9C, 0x9A, 0x00, 0x00, 0xAC, 0xA4, 0xA0, 0x99, 0x99, 0x99,
		0x00, 0xAC, 0xA0, 0x9C, 0x99, 0x99, 0x99, 0x99, 0x00, 0xAC, 0xA4, 0x9C, 0x99, 0x99, 0x99, 0x99,
	},
	{ // 7
		0xAC, 0xA0, 0x9C, 0x99, 0x99, 0x99, 0x99, 0x99, 0xAC, 0xA4, 0x9C, 0x99, 0x99, 0x99, 0x99, 0x99,
		0x00, 0xAC, 0xA0, 0x9C, 0x99, 0x99, 0x99, 0x99, 0x00, 0x00, 0xAC, 0xA4, 0x9C, 0x9C, 0x99, 0x99,
		0x00, 0x00, 0xAC, 0xA0, 0x9C, 0x99, 0x99, 0x99, 0x00, 0x00, 0x00, 0xAC, 0xA4, 0x9C, 0x99, 0x99,
		0x00, 0x00, 0xAC, 0xA0, 0x9B, 0xA0, 0x9E, 0x9C, 0x00, 0xAC, 0xA4, 0x9C, 0x99, 0x9C, 0x99, 0x99,
	},
	{ // 8
		0x00, 0xAC, 0xA0, 0x9C, 0x99, 0x99, 0x9B, 0x99, 0xAC, 0xA4, 0x9C, 0x99, 0x99, 0x99, 0x99, 0x99,
		0xAC, 0xA0, 0x9C, 0x99, 0x99, 0x99, 0x99, 0x99, 0xAC, 0xA4, 0x9C, 0x99, 0x99, 0x99, 0x99, 0x99,
		0x00, 0xAC, 0xA4, 0x9C, 0x99, 0x99, 0x99, 0x99, 0xAC, 0xA0, 0x9C, 0x99, 0x99, 0x99, 0x99, 0x99,
		0x00, 0xAC, 0xA0, 0x9C, 0x99, 0x99, 0x9C, 0x99, 0x00, 0xAC, 0xA4, 0x9C, 0x99, 0x9E, 0x9C, 0x99,
	},
	{ // 9
		0x00, 0x00, 0xAC, 0xA4, 0xA0, 0x9C, 0x99, 0x99, 0x00, 0xAC, 0xA0, 0x9C, 0x9C, 0xA0, 0x9C, 0x9A,
		0xAC, 0xA4, 0x9C, 0x9A, 0x99, 0x99, 0x99, 0x99, 0xAC, 0xA0, 0x9C, 0x99, 0x99, 0x99, 0x99, 0x99,
		0xAC, 0xA4, 0x9C, 0x99, 0x99, 0x99, 0x99, 0x99, 0x00, 0xAC, 0xA0, 0x9C, 0x99, 0x99, 0x99, 0x99,
		0x00, 0xAC, 0xA4, 0x9C, 0x9A, 0x9C, 0x99, 0x99, 0x00, 0x00, 0xAC, 0xA0, 0x9C, 0x9A, 0x99, 0x99,
	},
	{ // 10
		0x99, 0x99, 0x99, 0x9A, 0xA0, 0xAC, 0x00, 0x00, 0x99, 0x99, 0x99, 0x9C, 0xA0, 0xAC, 0x00, 0x00,
		0x99, 0x99, 0x9C, 0x9E, 0xA4, 0xAC, 0x00, 0x00, 0x99, 0x99, 0x9C, 0x99, 0x9C, 0xA4, 0xAC, 0x00,
		0x99, 0x99, 0x99, 0x99, 0x9C, 0xA0, 0xAC, 0x00, 0x99, 0x99, 0x99, 0x9C, 0xA0, 0xAC, 0x00, 0x00,
		0x99, 0x99, 0x99, 0xA0, 0xA4, 0xAC, 0x00, 0x00, 0x9A, 0x9B, 0x9E, 0x9C, 0x9C, 0xA4, 0xAC, 0x00,
	},
	{ // 11
		0x99, 0x99, 0x99, 0x99, 0x9C, 0xA0, 0xAC, 0x00, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9C, 0x9E, 0xAC,
		0x99, 0x99, 0x99, 0x99, 0x9C, 0xA0, 0xAC, 0x00, 0x99, 0x99, 0x99, 0x99, 0x9C, 0xA0, 0xAC, 0x00,
		0x99, 0x99, 0x99, 0x99, 0x99, 0x9C, 0xA4, 0xAC, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9C, 0xA0, 0xAC,
		0x9C, 0x99, 0x99, 0x99, 0x9C, 0x9C, 0xA4, 0xAC, 0x99, 0x9E, 0x9E, 0x9C, 0x9C, 0xA0, 0xAC, 0x00,
	},
	{ // 12
		0x99, 0x99, 0x9C, 0xA0, 0xA4, 0xAC, 0x00, 0x00, 0x9B, 0x9C, 0x9E, 0x9C, 0x9C, 0xA4, 0xAC, 0x00,
		0x99, 0x99, 0x99, 0x99, 0x99, 0xA0, 0xAC, 0x00, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9C, 0xA0, 0xAC,
		0x99, 0x99, 0x99, 0x99, 0x9C, 0x9C, 0xA4, 0xAC, 0x99, 0x99, 0x99, 0x9C, 0xA0, 0xA4, 0xAC, 0x00,
		0x99, 0x99, 0x9C, 0x99, 0x99, 0x9C, 0xA0, 0xAC, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9C, 0xA0, 0xAC,
	},
	{ // 13
		0x99, 0x99, 0x99, 0x99, 0x9C, 0xA0, 0xAC, 0x00, 0x99, 0x99, 0x99, 0x9C, 0xA0, 0xAC, 0x00, 0x00,
		0x99, 0x9B, 0x9C, 0xA0, 0xA4, 0xAC, 0x00, 0x00, 0x99, 0x99, 0x9A, 0x99, 0x9C, 0xA0, 0xAC, 0x00,
		0x99, 0x99, 0x99, 0x99, 0x99, 0x9C, 0xA4, 0xAC, 0x99, 0x99, 0x99, 0x99, 0x99, 0x9C, 0xA0, 0xAC,
		0x99, 0x99, 0x99, 0x99, 0x9A, 0x9C, 0xA4, 0xAC, 0x99, 0x99, 0x99, 0x9A, 0x9C, 0xA4, 0xAC, 0x00,
	},
	{ // 14
		0x00, 0x00, 0xAC, 0x9E, 0x9C, 0x9C, 0x9C, 0x9B, 0x00, 0xAC, 0x9C, 0xA0, 0x9E, 0xA4, 0xA4, 0xA4,
		0xAC, 0x9C, 0xA4, 0xAC, 0xAC, 0xAC, 0x9C, 0x9E, 0xAC, 0xA0, 0xAC, 0xA8, 0x9E, 0xA8, 0xAC, 0x99,
		0xAC, 0x9E, 0xAC, 0xA8, 0xAC, 0x9E, 0xA4, 0xAC, 0xAC, 0xA4, 0xA0, 0xAC, 0xAC, 0xA0, 0xA4, 0xAC,
		0x00, 0xAC, 0xA4, 0xA0, 0xA0, 0xA4, 0xAC, 0xA4, 0x00, 0x00, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC,
	},
	{ // 15
		0x9C, 0x9C, 0x9C, 0x9B, 0x9C, 0x9C, 0x9C, 0x9B, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4,
		0x9E, 0x9E, 0x9E, 0x9C, 0x9E, 0x9E, 0x9E, 0x9E, 0x99, 0x99, 0x99, 0x99, 0x99, 0x98, 0x99, 0x98,
		0x9C, 0x9C, 0x9B, 0x9B, 0x9B, 0x9C, 0x9C, 0x9C, 0xA0, 0xA0, 0xA0, 0xA0, 0xA0, 0x9E, 0x9E, 0xA0,
		0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC,
	},
	{ // 16
		0x9B, 0x9B, 0x9B, 0x9B, 0x9C, 0x9B, 0x9C, 0x9C, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4,
		0x9C, 0x9C, 0x9C, 0x9C, 0x9C, 0x9C, 0x9C, 0x9E, 0x98, 0x98, 0x98, 0x98, 0x99, 0x99, 0x99, 0x99,
		0x9C, 0x9B, 0x9C, 0x9C, 0x9C, 0x9C, 0x9C, 0x9C, 0xA0, 0xA0, 0xA0, 0x9E, 0xA0, 0x9E, 0x9E, 0xA0,
		0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC,
	},
	{ // 17
		0x9C, 0x9C, 0x9C, 0x9B, 0x9B, 0x9B, 0x9C, 0x9B, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4,
		0x9E, 0x9E, 0x9E, 0x9C, 0x9C, 0x9C, 0x9E, 0x9E, 0x98, 0x98, 0x98, 0x99, 0x9A, 0x9A, 0x99, 0x98,
		0x9C, 0x9B, 0x9C, 0x9C, 0x9C, 0x9B, 0x9B, 0x9C, 0xA0, 0x9E, 0x9E, 0xA0, 0xA0, 0xA0, 0xA0, 0x9E,
		0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC,
	},
	{ // 18
		0x9B, 0x9B, 0x9C, 0x9C, 0x9C, 0x9B, 0x9B, 0x9B, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4,
		0x9E, 0x9E, 0x9E, 0x9E, 0x9C, 0x9C, 0x9C, 0x9E, 0x98, 0x98, 0x98, 0x98, 0x9A, 0x9A, 0x98, 0x99,
		0x9C, 0x9C, 0x9C, 0x9C, 0x9C, 0x9C, 0x9B, 0x9C, 0x9E, 0x9E, 0x9E, 0x9E, 0x9E, 0xA0, 0xA0, 0xA0,
		0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC,
	},
	{ // 19
		0x9C, 0x9B, 0x9C, 0x9C, 0xA0, 0xA4, 0xAC, 0x00, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xAC, 0x00, 0x00,
		0x9E, 0x9E, 0x9C, 0x9C, 0x9E, 0xA0, 0xAC, 0x00, 0x99, 0x98, 0x98, 0x99, 0x9A, 0x9A, 0xA0, 0xAC,
		0x9C, 0x9C, 0x9C, 0x9C, 0x9C, 0x9C, 0xA0, 0xAC, 0xA0, 0xA0, 0x9E, 0xA0, 0xA0, 0xA0, 0xA0, 0xAC,
		0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xA4, 0xAC, 0x00, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0xAC, 0x00, 0x00,
	}
};

const byte TEXT_COLORS[40][4] = {
	{ 0x00, 0x19, 0x19, 0x19 },
	{ 0x00, 0x08, 0x08, 0x08 },
	{ 0x00, 0x0F, 0x0F, 0x0F },
	{ 0x00, 0x15, 0x15, 0x15 },
	{ 0x00, 0x01, 0x01, 0x01 },
	{ 0x00, 0x21, 0x21, 0x21 },
	{ 0x00, 0x26, 0x26, 0x26 },
	{ 0x00, 0x2B, 0x2B, 0x2B },
	{ 0x00, 0x31, 0x31, 0x31 },
	{ 0x00, 0x36, 0x36, 0x36 },
	{ 0x00, 0x3D, 0x3D, 0x3D },
	{ 0x00, 0x41, 0x41, 0x41 },
	{ 0x00, 0x46, 0x46, 0x46 },
	{ 0x00, 0x4C, 0x4C, 0x4C },
	{ 0x00, 0x50, 0x50, 0x50 },
	{ 0x00, 0x55, 0x55, 0x55 },
	{ 0x00, 0x5D, 0x5D, 0x5D },
	{ 0x00, 0x60, 0x60, 0x60 },
	{ 0x00, 0x65, 0x65, 0x65 },
	{ 0x00, 0x6C, 0x6C, 0x6C },
	{ 0x00, 0x70, 0x70, 0x70 },
	{ 0x00, 0x75, 0x75, 0x75 },
	{ 0x00, 0x7B, 0x7B, 0x7B },
	{ 0x00, 0x80, 0x80, 0x80 },
	{ 0x00, 0x85, 0x85, 0x85 },
	{ 0x00, 0x8D, 0x8D, 0x8D },
	{ 0x00, 0x90, 0x90, 0x90 },
	{ 0x00, 0x97, 0x97, 0x97 },
	{ 0x00, 0x9D, 0x9D, 0x9D },
	{ 0x00, 0xA4, 0xA4, 0xA4 },
	{ 0x00, 0xAB, 0xAB, 0xAB },
	{ 0x00, 0xB0, 0xB0, 0xB0 },
	{ 0x00, 0xB6, 0xB6, 0xB6 },
	{ 0x00, 0xBD, 0xBD, 0xBD },
	{ 0x00, 0xC0, 0xC0, 0xC0 },
	{ 0x00, 0xC6, 0xC6, 0xC6 },
	{ 0x00, 0xCD, 0xCD, 0xCD },
	{ 0x00, 0xD0, 0xD0, 0xD0 },
	{ 0x00, 0xD6, 0xD6, 0xD6 },
	{ 0x00, 0xDB, 0xDB, 0xDB },
};

const char *const DIRECTION_TEXT[4] = { "NORTH", "EAST", "SOUTH", "WEST" };

const char *const RACE_NAMES[5] = { "Human", "Elf", "Dwarf", "Gnome", "H-Orc" };

const char *const ALIGNMENT_NAMES[3] = { "Good", "Neutral", "Evil" };

const char *const SEX_NAMES[2] = { "Male", "Female" };

const char *const CLASS_NAMES[11] = {
	"Knight", "Paladin", "Archer", "Cleric", "Sorcerer", "Robber", 
	"Ninja", "Barbarian", "Druid", "Ranger", nullptr
};

const char *const CONDITION_NAMES[18] = {
	nullptr, "Cursed", "Heart Broken", "Weak", "Poisoned", "Diseased", 
	"Insane", "In Love", "Drunk", "Asleep", "Depressed", "Confused", 
	"Paralyzed", "Unconscious", "Dead", "Stone", "Eradicated", "Good"
};

const char *const IN_PARTY = "\014""15In Party\014""d";

const char *const PARTY_DETAILS = "\015\003l\002\014""00"
	"\013""001""\011""035%s" 
	"\013""009""\011""035%s" 
	"\013""017""\011""035%s" 
	"\013""025""\011""035%s" 
	"\013""001""\011""136%s" 
	"\013""009""\011""136%s" 
	"\013""017""\011""136%s" 
	"\013""025""\011""136%s" 
	"\013""044""\011""035%s" 
	"\013""052""\011""035%s" 
	"\013""060""\011""035%s" 
	"\013""068""\011""035%s" 
	"\013""044""\011""136%s" 
	"\013""052""\011""136%s" 
	"\013""060""\011""136%s" 
	"\013""068""\011""136%s";

const int FACE_CONDITION_FRAMES[17] = { 
	2, 2, 2, 1, 1, 4, 4, 4, 3, 2, 4, 3, 3, 5, 6, 7, 0 
};

const int CHAR_FACES_X[6] = { 10, 45, 81, 117, 153, 189 };

const int HP_BARS_X[6] = { 13, 50, 86, 122, 158, 194 };

const char *const NO_ONE_TO_ADVENTURE_WITH = "You have no one to adventure with";

const char *const YOUR_ROSTER_IS_FULL = "Your Roster is full!";

const byte BACKGROUND_XLAT[] = {
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
	0x00, 0x00, 0xF7, 0xFF, 0x09, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0xF9, 0xFF, 0x07, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0xF7, 0xFF, 0x09, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0xF5, 0xFF, 0x0B, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0xF3, 0xFF, 0x0D, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00
};

const char *const PLEASE_WAIT = "\014""d\003""c\011""000"
	"\013""002Please Wait...";

const char *const OOPS = "\003""c\011""000\013""002Oops...";

const int8 SCREEN_POSITIONING_X[4][48] = {
	{
	-1,  0,  0,  0,  1, -1,  0,  0,  0,  1, -2, -1,
	-1,  0,  0,  0,  1,  1,  2, -4, -3, -3, -2, -2,
	-1, -1,  0,  0,  0,  1,  1,  2,  2,  3,  3,  4, 
	-3, -2, -1,  0,  0,  1,  2,  3, -4,  4,  0,  0
	}, {
	 0,  0,  0,  0,  0,  1,  1,  1,  1,  1,  2,  2,
	 2,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3,
	 3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
	 4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  0,  1
	}, {
	 1,  0,  0,  0, -1,  1,  0,  0,  0, -1,  2,  1,
	 1,  0,  0,  0, -1, -1, -2,  4,  3,  3,  2,  2,
	 1,  1,  0,  0,  0, -1, -1, -2, -2, -3, -3, -4,
	 3,  2,  1,  0,  0, -1, -2, -3,  4, -4,  0,  0
	}, {
	 0,  0,  0,  0,  0, -1, -1, -1, -1, -1, -2, -2, 
	-2, -2, -2, -2, -2, -2, -2, -3, -3, -3, -3, -3,
	-3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3,
	-4, -4, -4, -4, -4, -4, -4, -4, -4, -4,  0, -1
	}
};

const int8 SCREEN_POSITIONING_Y[4][48] = {
	{
	 0,  0,  0,  0,  0,  1,  1,  1,  1,  1,  2,  2,
	 2,  2,  2,  2,  2,  2,  2,  3,  3,  3,  3,  3,
	 3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
	 4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  0,  1
	}, {
	 1,  0,  0,  0, -1,  1,  0,  0,  0, -1,  2,  1,
	 1,  0,  0,  0, -1, -1, -2,  4,  3,  3,  2,  2,
	 1,  1,  0,  0,  0, -1, -1, -2, -2, -3, -3, -4,
	 3,  2,  1,  0,  0, -1, -2, -3,  4, -4,  0,  0
	}, {
	 0,  0,  0,  0,  0, -1, -1, -1, -1, -1, -2, -2,
	-2, -2, -2, -2, -2, -2, -2, -3, -3, -3, -3, -3,
	-3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3, -3,
	-4, -4, -4, -4, -4, -4, -4, -4, -4, -4,  0, -1
	}, {
	-1,  0,  0,  0,  1, -1,  0,  0,  0,  1, -2, -1,
	-1,  0,  0,  0,  1,  1,  2, -4, -3, -3, -2, -2,
	-1, -1,  0,  0,  0,  1,  1,  2,  2,  3,  3,  4,
	-3, -2, -1,  0,  0,  1,  2,  3, -4,  4,  0,  0
	}
};

const int INDOOR_OBJECT_X[2][12] = {
	{ 5, -7, -112, 98, -8, -65, 49, -9, -34, 16, -58, 40 },
	{ -35, -35, -142, 68, -35, -95, 19, -35, -62, -14, -98, 16 }
};

const int INDOOR_OBJECT_Y[2][12] = {
	{ 2, 25, 25, 25, 50, 50, 50, 58, 58, 58, 58, 58 },
	{ -65, -6, -6, -6, 36, 36, 36, 54, 54, 54, 54, 54 }
};

const int OUTDOOR_OBJECT_X[2][12] = {
	{ -5, -7, -112, 98, -8, -77, 61, -9, -43, 25, -74, 56 },
	{ -35, -35, -142, 68, -35, -95, 19, -35, -62, -24, -98, 16 }
};

const int OUTDOOR_OBJECT_Y[2][12] = {
	{ 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 69 },
	{ 70, 71, 72, 73, 74, 75, 90, 91, 92, 93, 94, 112 }
};

const int DIRECTION_ANIM_POSITIONS[4][4] = {
	{ 0, 1, 2, 3 }, { 3, 0, 1, 2 }, { 2, 3, 0, 1 }, { 1, 2, 3, 0 }
};

const byte WALL_NUMBERS[4][96] = {
	{
	3, 0, 0, 0, 3, 0, 2, 0, 3, 0, 3, 0,
	0, 0, 3, 0, 2, 0, 3, 0, 3, 0, 0, 0,
	3, 0, 0, 0, 3, 0, 2, 0, 3, 0, 2, 0,
	3, 0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0,
	0, 0, 3, 0, 0, 0, 3, 0, 2, 0, 3, 0,
	2, 0, 3, 0, 2, 0, 3, 0, 2, 0, 3, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 0,
	2, 0, 2, 0, 0, 0, 0, 0, 1, 0, 1, 0
	}, {
	2, 0, 3, 0, 2, 0, 1, 0, 2, 0, 2, 0,
	3, 0, 2, 0, 1, 0, 2, 0, 2, 0, 3, 0,
	2, 0, 3, 0, 2, 0, 1, 0, 2, 0, 1, 0,
	2, 0, 2, 0, 3, 0, 2, 0, 3, 0, 2, 0,
	3, 0, 2, 0, 3, 0, 2, 0, 1, 0, 2, 0,
	1, 0, 2, 0, 1, 0, 2, 0, 1, 0, 2, 0,
	3, 0, 3, 0, 3, 0, 3, 0, 1, 0, 1, 0,
	1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0
	}, {
	1, 0, 2, 0, 1, 0, 0, 0, 1, 0, 1, 0,
	2, 0, 1, 0, 0, 0, 1, 0, 1, 0, 2, 0,
	1, 0, 2, 0, 1, 0, 0, 0, 1, 0, 0, 0,
	1, 0, 1, 0, 2, 0, 1, 0, 2, 0, 1, 0,
	2, 0, 1, 0, 2, 0, 1, 0, 0, 0, 1, 0,
	0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0,
	2, 0, 2, 0, 2, 0, 2, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 3, 0
	}, {
	0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 0, 0,
	1, 0, 0, 0, 3, 0, 0, 0, 0, 0, 1, 0,
	0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 3, 0,
	0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0,
	1, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0,
	3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0,
	1, 0, 1, 0, 1, 0, 1, 0, 3, 0, 3, 0,
	3, 0, 3, 0, 0, 0, 0, 0, 2, 0, 2, 0
	}
};

} // End of namespace Xeen
