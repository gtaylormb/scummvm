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

#ifndef ADL_HIRES1_H
#define ADL_HIRES1_H

#include "common/str.h"

#include "adl/adl_v2.h"
#include "adl/disk.h"

namespace Common {
class ReadStream;
class Point;
}

namespace Adl {

#define IDS_HR2_DISK_IMAGE "WIZARD.DSK"

#define IDI_HR2_NUM_ROOMS 135
#define IDI_HR2_NUM_MESSAGES 254
#define IDI_HR2_NUM_VARS 40
#define IDI_HR2_NUM_ITEM_PICS 38
#define IDI_HR2_NUM_ITEM_OFFSETS 16

// Messages used outside of scripts
#define IDI_HR2_MSG_CANT_GO_THERE      123
#define IDI_HR2_MSG_DONT_UNDERSTAND     19
#define IDI_HR2_MSG_ITEM_DOESNT_MOVE   242
#define IDI_HR2_MSG_ITEM_NOT_HERE        4
#define IDI_HR2_MSG_THANKS_FOR_PLAYING 239

struct Picture2 {
	byte nr;
	DataBlockPtr data;
};

struct RoomData {
	Common::String description;
	Common::Array<Picture2> pictures;
	Commands commands;
};

typedef Common::ScopedPtr<Common::SeekableReadStream> StreamPtr;

class HiRes2Engine : public AdlEngine_v2 {
public:
	HiRes2Engine(OSystem *syst, const AdlGameDescription *gd) : AdlEngine_v2(syst, gd) { }

private:
	// AdlEngine
	void runIntro() const;
	void init();
	void initState();
	void restartGame();
	void drawPic(byte pic, Common::Point pos) const;
	void drawItem(const Item &item, const Common::Point &pos) const;
	void showRoom();
	void printMessage(uint idx, bool wait);
	void checkInput(byte verb, byte noun);

	void loadRoom(byte roomNr);
	DataBlockPtr readDataBlockPtr(Common::ReadStream &f) const;
	void readPictureMeta(Common::ReadStream &f, Picture2 &pic) const;

	DiskImage_DSK _disk;
	RoomData _roomData;
	Common::Array<Picture2> _itemPics;
};

} // End of namespace Adl

#endif
