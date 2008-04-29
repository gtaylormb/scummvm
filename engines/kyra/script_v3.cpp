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

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *
 */

#include "kyra/kyra_v3.h"
#include "kyra/script.h"
#include "kyra/screen_v3.h"
#include "kyra/text_v3.h"
#include "kyra/wsamovie.h"
#include "kyra/timer.h"

#include "common/endian.h"

namespace Kyra {

int KyraEngine_v3::o3_getMalcolmShapes(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getMaloclmShapes(%p) ()", (const void *)script);
	return _malcolmShapes;
}

int KyraEngine_v3::o3_setCharacterPos(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setCharacterPos(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	int x = stackPos(0);
	int y = stackPos(1);

	if (x != -1 && y != -1) {
		x &= ~3;
		y &= ~1;
	}

	_mainCharacter.x1 = _mainCharacter.x2 = x;
	_mainCharacter.y1 = _mainCharacter.y2 = y;

	return 0;
}

int KyraEngine_v3::o3_defineObject(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_defineObject(%p) (%d, '%s', %d, %d, %d, %d, %d, %d)", (const void *)script,
			stackPos(0), stackPosString(1), stackPos(2), stackPos(3), stackPos(4), stackPos(5), stackPos(6), stackPos(7));
	TalkObject &obj = _talkObjectList[stackPos(0)];
	strcpy(obj.filename, stackPosString(1));
	obj.sceneAnim = stackPos(2);
	obj.sceneScript = stackPos(3);
	obj.x = stackPos(4);
	obj.y = stackPos(5);
	obj.color = stackPos(6);
	obj.sceneId = stackPos(7);
	return 0;
}

int KyraEngine_v3::o3_refreshCharacter(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_refreshCharacter(%p) (%d, %d, %d)", (const void *)script, stackPos(0), stackPos(1), stackPos(2));
	const int frame = stackPos(0);
	const int facing = stackPos(1);
	const bool updateNeed = stackPos(2) != 0;

	if (facing >= 0)
		_mainCharacter.facing = facing;

	if (frame >= 0 && frame != 87)
		_mainCharacter.animFrame = _characterFrameTable[_mainCharacter.facing];
	else
		_mainCharacter.animFrame = 87;

	updateCharacterAnim(0);

	if (updateNeed)
		refreshAnimObjectsIfNeed();
	return 0;
}

int KyraEngine_v3::o3_getCharacterX(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getCharacterX(%p) ()", (const void *)script);
	return _mainCharacter.x1;
}

int KyraEngine_v3::o3_getCharacterY(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getCharacterY(%p) ()", (const void *)script);
	return _mainCharacter.y1;
}

int KyraEngine_v3::o3_getCharacterFacing(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getCharacterFacing(%p) ()", (const void *)script);
	return _mainCharacter.facing;
}

int KyraEngine_v3::o3_getCharacterScene(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getCharacterScene(%p) ()", (const void *)script);
	return _mainCharacter.sceneId;
}

int KyraEngine_v3::o3_getMalcolmsMood(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getMalcolmsMood(%p) ()", (const void *)script);
	return _malcolmsMood;
}

int KyraEngine_v3::o3_getCharacterFrameFromFacing(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getCharacterFrameFromFacing(%p) ()", (const void *)script);
	return _characterFrameTable[_mainCharacter.facing];
}

int KyraEngine_v3::o3_setCharacterFacingOverwrite(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setCharacterFacingOverwrite(%p) (%d)", (const void *)script, stackPos(0));
	_mainCharacter.facing = stackPos(0);
	_overwriteSceneFacing = true;
	return 0;
}

int KyraEngine_v3::o3_trySceneChange(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_trySceneChange(%p) (%d, %d, %d, %d)", (const void *)script,
			stackPos(0), stackPos(1), stackPos(2), stackPos(3));

	_unkHandleSceneChangeFlag = 1;
	int success = inputSceneChange(stackPos(0), stackPos(1), stackPos(2), stackPos(3));
	_unkHandleSceneChangeFlag = 0;

	if (success) {
		_emc->init(script, script->dataPtr);
		_unk4 = 0;
		_unk3 = -1;
		_unk5 = 1;
		return 0;
	} else {
		return (_unk4 != 0) ? 1 : 0;
	}
}

int KyraEngine_v3::o3_moveCharacter(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_moveCharacter(%p) (%d, %d, %d)", (const void *)script, stackPos(0), stackPos(1), stackPos(2));
	moveCharacter(stackPos(0), stackPos(1), stackPos(2));
	return 0;
}

int KyraEngine_v3::o3_setCharacterFacing(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setCharacterFacing(%p) (%d)", (const void *)script, stackPos(0));
	_mainCharacter.facing = stackPos(0);
	return 0;
}

int KyraEngine_v3::o3_showSceneFileMessage(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_showSceneFileMessage(%p) (%d)", (const void *)script, stackPos(0));
	showMessage((const char*)getTableEntry(_scenesFile, stackPos(0)), 0xFF, 0xF0);
	return 0;
}

int KyraEngine_v3::o3_setCharacterAnimFrameFromFacing(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setCharacterAnimFrameFromFacing(%p) ()", (const void *)script);
	updateCharPal(0);
	_mainCharacter.animFrame = _characterFrameTable[_mainCharacter.facing];
	updateCharacterAnim(0);
	refreshAnimObjectsIfNeed();
	return 0;
}

int KyraEngine_v3::o3_showBadConscience(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_showBadConscience(%p) ()", (const void *)script);
	showBadConscience();
	return 0;
}

int KyraEngine_v3::o3_hideBadConscience(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_hideBadConscience(%p) ()", (const void *)script);
	hideBadConscience();
	return 0;
}

int KyraEngine_v3::o3_setInventorySlot(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setInventorySlot(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	const int slot = MAX<int16>(0, MIN<int16>(10, stackPos(0)));
	return (_mainCharacter.inventory[slot] = stackPos(1));
}

int KyraEngine_v3::o3_getInventorySlot(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getInventorySlot(%p) (%d)", (const void *)script, stackPos(0));
	return _mainCharacter.inventory[stackPos(0)];
}

int KyraEngine_v3::o3_addItemToInventory(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_addItemToInventory(%p) (%d)", (const void *)script, stackPos(0));
	int slot = findFreeInventorySlot();
	if (slot >= 0) {
		_mainCharacter.inventory[slot] = stackPos(0);
		if (_inventoryState) {
			_screen->hideMouse();
			redrawInventory(0);
			_screen->showMouse();
		}
	}
	return slot;
}

int KyraEngine_v3::o3_addItemToCurScene(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_addItemToCurScene(%p) (%d, %d, %d)", (const void *)script, stackPos(0), stackPos(1), stackPos(2));
	const uint16 item = stackPos(0);
	int x = stackPos(1);
	int y = stackPos(2);
	int itemSlot = findFreeItem();

	if (x < 20)
		x = 20;
	else if (x > 299)
		x = 299;

	if (y < 18)
		y = 18;
	else if (y > 187)
		y = 187;

	if (itemSlot >= 0) {
		_itemList[itemSlot].x = x;
		_itemList[itemSlot].y = y;
		_itemList[itemSlot].id = item;
		_itemList[itemSlot].sceneId = _mainCharacter.sceneId;
		addItemToAnimList(itemSlot);
		refreshAnimObjectsIfNeed();
	}

	return itemSlot;
}

int KyraEngine_v3::o3_objectChat(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_objectChat(%p) (%d)", (const void *)script, stackPos(0));
	int id = stackPos(0);
	const char *str = (const char*)getTableEntry(_useActorBuffer ? _actorFile : _sceneStrings, id);
	if (str) {
		objectChat(str, 0, _vocHigh, id);
		playStudioSFX(str);
	}
	return 0;
}

int KyraEngine_v3::o3_checkForItem(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_checkForItem(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	return findItem(stackPos(0), stackPos(1)) == -1 ? 0 : 1;
}

int KyraEngine_v3::o3_resetInventory(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_resetInventory(%p) ()", (const void *)script);
	memset(_mainCharacter.inventory, -1, sizeof(_mainCharacter.inventory));
	return 0;
}

int KyraEngine_v3::o3_defineItem(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_defineItem(%p) (%d, %d, %d, %d)", (const void *)script, stackPos(0), stackPos(1), stackPos(2), stackPos(3));
	int freeItem = findFreeItem();
	if (freeItem != -1) {
		_itemList[freeItem].id = stackPos(0);
		_itemList[freeItem].x = stackPos(1);
		_itemList[freeItem].y = stackPos(2);
		_itemList[freeItem].sceneId = stackPos(3);
	}
	return freeItem;
}

int KyraEngine_v3::o3_removeInventoryItemInstances(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_removeInventoryItemInstances(%p) (%d)", (const void *)script, stackPos(0));
	const int item = stackPos(0);
	for (int i = 0; i < 10; ++i) {
		if (_mainCharacter.inventory[i] == item)
			_mainCharacter.inventory[i] = 0xFFFF;
	}
	return 0;
}

int KyraEngine_v3::o3_countInventoryItemInstances(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_countInventoryItemInstances(%p) (%d)", (const void *)script, stackPos(0));
	const int item = stackPos(0);
	int count = 0;

	for (int i = 0; i < 10; ++i) {
		if (_mainCharacter.inventory[i] == item)
			++count;
	}

	if (_itemInHand == item)
		++count;

	return count;
}

int KyraEngine_v3::o3_npcChatSequence(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_npcChatSequence(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	const int id = stackPos(0);
	const char *str = (const char*)getTableEntry(_sceneStrings, id);
	if (str)
		npcChatSequence(str, stackPos(1), _vocHigh, id);
	return 0;
}

int KyraEngine_v3::o3_queryGameFlag(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_queryGameFlag(%p) (%d)", (const void *)script, stackPos(0));
	return queryGameFlag(stackPos(0));
}

int KyraEngine_v3::o3_resetGameFlag(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_resetGameFlag(%p) (%d)", (const void *)script, stackPos(0));
	resetGameFlag(stackPos(0));
	return 0;
}

int KyraEngine_v3::o3_setGameFlag(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setGameFlag(%p) (%d)", (const void *)script, stackPos(0));
	setGameFlag(stackPos(0));
	return 1;
}

int KyraEngine_v3::o3_setHandItem(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setHandItem(%p) (%d)", (const void *)script, stackPos(0));
	setHandItem(stackPos(0));
	return 0;
}

int KyraEngine_v3::o3_removeHandItem(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_removeHandItem(%p) ()", (const void *)script);
	removeHandItem();
	return 0;
}

int KyraEngine_v3::o3_handItemSet(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_handItemSet(%p) ()", (const void *)script);
	return _handItemSet;
}

int KyraEngine_v3::o3_hideMouse(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_hideMouse(%p) ()", (const void *)script);
	_screen->hideMouse();
	return 0;
}

int KyraEngine_v3::o3_addSpecialExit(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_addSpecialExit(%p) (%d, %d, %d, %d, %d)", (const void *)script,
		stackPos(0), stackPos(1), stackPos(2), stackPos(3), stackPos(4));
	if (_specialExitCount < 5) {
		_specialExitTable[_specialExitCount+0] = stackPos(0);
		_specialExitTable[_specialExitCount+5] = stackPos(1);
		_specialExitTable[_specialExitCount+10] = stackPos(2) + stackPos(0) - 1;
		_specialExitTable[_specialExitCount+15] = stackPos(3) + stackPos(1) - 1;
		_specialExitTable[_specialExitCount+20] = stackPos(4);
		++_specialExitCount;
	}
	return 0;
}

int KyraEngine_v3::o3_setMousePos(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setMousePos(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	setMousePos(stackPos(0), stackPos(1));
	return 0;
}

int KyraEngine_v3::o3_showMouse(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_showMouse(%p) ()", (const void *)script);
	_screen->showMouse();
	return 0;
}

int KyraEngine_v3::o3_badConscienceChat(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_badConscienceChat(%p) (%d)", (const void *)script, stackPos(0));
	int id = stackPos(0);
	const char *str = (const char*)getTableEntry(_useActorBuffer ? _actorFile : _sceneStrings, id);
	badConscienceChat(str, _vocHigh, id);
	return 0;
}

int KyraEngine_v3::o3_wipeDownMouseItem(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v2::o3_wipeDownMouseItem(%p) (-, %d, %d)", (const void *)script, stackPos(1), stackPos(2));
	_screen->hideMouse();
	const int x = stackPos(1) - 12;
	const int y = stackPos(2) - 19;

	if (_itemInHand >= 0) {
		backUpGfxRect32x32(x, y);
		uint8 *shape = getShapePtr(_itemInHand+248);
		for (int curY = y, height = 20; height > 0; height -= 2, curY += 2) {
			restoreGfxRect32x32(x, y);
			_screen->setNewShapeHeight(shape, height);
			uint32 waitTime = _system->getMillis() + _tickLength;
			_screen->drawShape(0, shape, x, curY, 0, 0);
			_screen->updateScreen();
			delayUntil(waitTime);
		}
		restoreGfxRect32x32(x, y);
		_screen->resetShapeHeight(shape);
	}

	_screen->showMouse();
	removeHandItem();

	return 0;
}

int KyraEngine_v3::o3_setMalcolmsMood(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setMalcolmsMood(%p) (%d)", (const void *)script, stackPos(0));
	return (_malcolmsMood = stackPos(0));
}

int KyraEngine_v3::o3_delay(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_delay(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	if (stackPos(1)) {
		uint32 maxWaitTime = _system->getMillis() + stackPos(0) * _tickLength;
		while (_system->getMillis() < maxWaitTime) {
			int inputFlag = checkInput(0);
			removeInputTop();

			if (inputFlag == 198 || inputFlag == 199)
				return 1;

			if (_chatText)
				updateWithText();
			else
				update();
			_system->delayMillis(10);
		}
	} else {
		delay(stackPos(0) * _tickLength, true);
	}
	return 0;
}

int KyraEngine_v3::o3_updateScore(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_updateScore(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	return updateScore(stackPos(0), stackPos(1)) ? 1 : 0;
}

int KyraEngine_v3::o3_makeSecondChanceSave(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_makeSecondChanceSave(%p) ()", (const void *)script);
	saveGame(getSavegameFilename(999), "SECOND CHANCE SAVE GAME");
	return 0;
}

int KyraEngine_v3::o3_setSceneFilename(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setSceneFilename(%p) (%d, '%s')", (const void *)script, stackPos(0), stackPosString(1));
	strcpy(_sceneList[stackPos(0)].filename1, stackPosString(1));
	_sceneList[stackPos(0)].filename1[9] = 0;
	return 0;
}

int KyraEngine_v3::o3_removeItemsFromScene(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_removeItemsFromScene(%p) (%d, %d, %d)", (const void *)script, stackPos(0), stackPos(1), stackPos(2));
	const uint16 itemId = stackPos(0);
	const uint16 sceneId = stackPos(1);
	const bool allItems = (stackPos(2) != 0);

	int retValue = 0;

	for (int i = 0; i < 50; ++i) {
		if (_itemList[i].sceneId == sceneId && _itemList[i].id == itemId) {
			resetItem(i);
			retValue = 1;
			if (!allItems)
				return 1;
		}
	}

	return retValue;
}

int KyraEngine_v3::o3_disguiseMalcolm(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v2::o3_disguiseMalcolm(%p) (%d)", (const void *)script, stackPos(0));
	loadMalcolmShapes(stackPos(0));
	updateDlgIndex();
	return 0;
}

int KyraEngine_v3::o3_drawSceneShape(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v2::o3_drawSceneShape(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));

	int shape = stackPos(0);
	int flag = (stackPos(1) != 0) ? 1 : 0;

	_screen->hideMouse();
	restorePage3();

	const int x = _sceneShapeDescs[shape].drawX;
	const int y = _sceneShapeDescs[shape].drawY;

	_screen->drawShape(2, _sceneShapes[shape], x, y, 2, flag);

	_screen->copyRegionToBuffer(3, 0, 0, 320, 200, _gamePlayBuffer);

	_screen->drawShape(0, _sceneShapes[shape], x, y, 2, flag);

	flagAnimObjsForRefresh();
	refreshAnimObjectsIfNeed();
	_screen->showMouse();
	return 0;
}

int KyraEngine_v3::o3_drawSceneShapeOnPage(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_drawSceneShapeOnPage(%p) (%d, %d, %d)", (const void *)script, stackPos(0), stackPos(1), stackPos(2));
	const int shape = stackPos(0);
	
	int x = _sceneShapeDescs[shape].drawX;
	int y = _sceneShapeDescs[shape].drawY;
	_screen->drawShape(stackPos(2), _sceneShapes[shape], x, y, 2, (stackPos(1) != 0) ? 1 : 0);
	return 0;
}

int KyraEngine_v3::o3_checkInRect(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_checkInRect(%p) (%d, %d, %d, %d, %d, %d)", (const void *)script,
			stackPos(0), stackPos(1), stackPos(2), stackPos(3), stackPos(4), stackPos(5));
	const int x1 = stackPos(0);
	const int y1 = stackPos(1);
	const int x2 = stackPos(2);
	const int y2 = stackPos(3);
	int x = stackPos(4), y = stackPos(5);
	if (_itemInHand >= 0) {
		const int8 *desc = &_itemBuffer2[_itemInHand*2];
		x -= 12;
		x += desc[0];
		y -= 19;
		y += desc[1];
	}

	if (x >= x1 && x <= x2 && y >= y1 && y <= y2) {
		//XXX
		return 1;
	} else {
		//XXX
		return 0;
	}
}

int KyraEngine_v3::o3_updateConversations(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_updateConversations(%p) (%d)", (const void *)script, stackPos(0));
	int dlgIndex = stackPos(0);
	switch (_currentChapter-2) {
	case 0:
		dlgIndex -= 34;
		break;
	
	case 1:
		dlgIndex -= 54;
		break;

	case 2:
		dlgIndex -= 55;
		break;

	case 3:
		dlgIndex -= 70;
		break;

	default:
		break;
	}

	int convs[4];
	Common::set_to(convs, convs+4, -1);

	if (_currentChapter == 1) {
		switch (_mainCharacter.dlgIndex) {
		case 0:
			convs[0] = 6;
			convs[1] = 12;
			break;

		case 2:
			convs[0] = 8;
			convs[1] = 14;
			break;

		case 3:
			convs[0] = 9;
			convs[1] = 15;
			break;

		case 4:
			convs[0] = 10;
			convs[1] = 16;
			break;

		case 5:
			convs[0] = 11;
			convs[1] = 17;
			break;

		case 6:
			convs[0] = 0;
			convs[1] = 12;
			break;

		case 8:
			convs[0] = 2;
			convs[1] = 14;
			break;

		case 9:
			convs[0] = 3;
			convs[1] = 15;
			break;

		case 10:
			convs[0] = 4;
			convs[1] = 16;
			break;

		case 11:
			convs[0] = 5;
			convs[1] = 17;
			break;

		case 12:
			convs[0] = 0;
			convs[1] = 6;
			break;

		case 14:
			convs[0] = 2;
			convs[1] = 8;
			break;

		case 15:
			convs[0] = 3;
			convs[1] = 9;
			break;

		case 16:
			convs[0] = 4;
			convs[1] = 10;
			break;

		case 17:
			convs[0] = 5;
			convs[1] = 11;
			break;

		default:
			break;
		}
	} else if (_currentChapter == 2) {
		switch (_mainCharacter.dlgIndex) {
		case 0:
			convs[0] = 4;
			convs[1] = 8;
			convs[2] = 5;
			convs[3] = 9;
			break;

		case 1:
			convs[0] = 4;
			convs[1] = 8;
			convs[2] = 0;
			convs[3] = 5;
			break;

		case 2:
			convs[0] = 6;
			convs[2] = 11;
			break;

		case 3:
			convs[0] = 7;
			convs[2] = 12;
			break;

		case 4:
			convs[0] = 0;
			convs[1] = 8;
			convs[2] = 1;
			convs[3] = 9;
			break;

		case 5:
			convs[0] = 0;
			convs[1] = 8;
			convs[2] = 4;
			convs[3] = 1;
			break;

		case 6:
			convs[0] = 2;
			convs[1] = 10;
			break;

		case 7:
			convs[0] = 3;
			convs[1] = 11;
			break;

		case 8:
			convs[0] = 0;
			convs[1] = 4;
			convs[2] = 1;
			break;

		case 9:
			convs[0] = 0;
			convs[1] = 4;
			convs[2] = 0;
			convs[4] = 1;
			break;

		case 10:
			convs[0] = 2;
			convs[1] = 6;
			break;

		case 11:
			convs[0] = 3;
			convs[1] = 7;
			break;

		default:
			break;
		}
	} else if (_currentChapter == 4) {
		if (_malcolmsMood == 0) {
			convs[0] = _mainCharacter.dlgIndex - 10;
			convs[1] = _mainCharacter.dlgIndex - 5;
		} else if (_malcolmsMood == 1) {
			convs[0] = _mainCharacter.dlgIndex + 5;
			convs[1] = _mainCharacter.dlgIndex + 10;
		} else if (_malcolmsMood == 2) {
			convs[0] = _mainCharacter.dlgIndex - 5;
			convs[1] = _mainCharacter.dlgIndex + 5;
		}
	}

	for (int i = 0; i < 4; ++i) {
		if (convs[i] != -1)
			_conversationState[dlgIndex][convs[i]] = 0;
	}

	return 1;
}

int KyraEngine_v3::o3_setSceneDim(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setSceneDim(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	_sceneMinX = stackPos(0);
	_sceneMaxX = stackPos(1);
	return 0;
}

int KyraEngine_v3::o3_setSceneAnimPosAndFrame(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setSceneAnimPosAndFrame(%p) (%d, %d, %d, %d, %d, %d)", (const void *)script,
			stackPos(0), stackPos(1), stackPos(2), stackPos(3), stackPos(4), stackPos(5));
	SceneAnim &anim = _sceneAnims[stackPos(0)];
	const int newX2 = stackPos(1);
	const int newY2 = stackPos(2);
	const int newX = stackPos(3);
	const int newY = stackPos(4);
	
	if (newX2 >= 0)
		anim.x2 = newX2;
	if (newY2 >= 0)
		anim.y2 = newY2;

	if (newX >= 0)
		anim.x = newX;
	else
		anim.x = anim.x2 + (anim.width >> 1);

	if (newY >= 0)
		anim.y = newY;
	else
		anim.y = anim.y2 + anim.height - 1;

	updateSceneAnim(stackPos(0), stackPos(5));
	_specialSceneScriptRunFlag = false;
	return 0;
}

int KyraEngine_v3::o3_update(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_update(%p) (%d)", (const void *)script, stackPos(0));
	for (int times = stackPos(0); times != 0; --times) {
		if (_chatText)
			updateWithText();
		else
			update();
	}
	return 0;
}

int KyraEngine_v3::o3_removeItemInstances(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_removeItemInstances(%p) (%d)", (const void *)script, stackPos(0));
	const int16 item = stackPos(0);

	int deleted = 0;

	for (int i = 0; i < 10; ++i) {
		if (_mainCharacter.inventory[i] == item) {
			_mainCharacter.inventory[i] = 0xFFFF;
			++deleted;
		}
	}

	if (_itemInHand == item) {
		removeHandItem();
		++deleted;
	}

	for (int i = 0; i < 50; ++i) {
		if (_itemList[i].id == item) {
			_itemList[i].id = 0xFFFF;
			++deleted;
		}
	}

	return deleted;
}

int KyraEngine_v3::o3_disableInventory(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_disableInventory(%p) ()", (const void *)script);
	_enableInventory = false;
	return 0;
}

int KyraEngine_v3::o3_enableInventory(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_enableInventory(%p) ()", (const void *)script);
	_enableInventory = true;
	return 1;
}

int KyraEngine_v3::o3_enterNewScene(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_enterNewScene(%p) (%d, %d, %d, %d, %d)", (const void *)script, stackPos(0),
		stackPos(1), stackPos(2), stackPos(3), stackPos(4));

	_screen->hideMouse();
	enterNewScene(stackPos(0), stackPos(1), stackPos(2), stackPos(3), stackPos(4));

	_unk5 = 1;

	if (_mainCharX == -1 || _mainCharY == -1) {
		_mainCharacter.animFrame = _characterFrameTable[_mainCharacter.facing];
		updateCharacterAnim(0);
	}
	_screen->showMouse();

	return 0;
}

int KyraEngine_v3::o3_switchScene(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_switchScene(%p) (%d)", (const void *)script, stackPos(0));
	setGameFlag(1);
	_mainCharX = _mainCharacter.x1;
	_mainCharY = _mainCharacter.y1;
	_noScriptEnter = false;
	enterNewScene(stackPos(0), _mainCharacter.facing, 0, 0, 0);
	_noScriptEnter = true;
	return 0;
}

int KyraEngine_v3::o3_getShapeFlag1(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getShapeFlag1(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	return _screen->getShapeFlag1(stackPos(0), stackPos(1));
}

int KyraEngine_v3::o3_setMalcolmPos(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setMalcolmPos(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	_mainCharX = stackPos(0);
	_mainCharY = stackPos(1);

	if (_mainCharX == -1 && _mainCharY == -1)
		_mainCharacter.animFrame = 87;
	else
		_mainCharacter.animFrame = _characterFrameTable[_mainCharacter.facing];

	return 0;
}

int KyraEngine_v3::o3_stopMusic(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_stopMusic(%p) ()", (const void *)script);
	stopMusicTrack();
	return 0;
}

int KyraEngine_v3::o3_playWanderScoreViaMap(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_playWanderScoreViaMap(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	snd_playWanderScoreViaMap(stackPos(0), stackPos(1));
	return 0;
}

int KyraEngine_v3::o3_playSoundEffect(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_playSoundEffect(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	snd_playSoundEffect(stackPos(0), stackPos(1));
	return 0;
}

int KyraEngine_v3::o3_getScore(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getScore(%p) ()", (const void *)script);
	return _score;
}

int KyraEngine_v3::o3_blockOutRegion(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_blockOutRegion(%p) (%d, %d, %d, %d)", (const void *)script, stackPos(0), stackPos(1), stackPos(2), stackPos(3));
	const int x1 = stackPos(0);
	int y1 = stackPos(1);
	const int x2 = stackPos(2);
	int y2 = stackPos(3);

	if (y1 < _maskPageMinY)
		y1 = _maskPageMinY;
	if (y2 > _maskPageMaxY)
		y2 = _maskPageMaxY;

	_screen->blockOutRegion(x1, y1, x2-x1+1, y2-y1+1);
	return 0;
}

int KyraEngine_v3::o3_showSceneStringsMessage(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_showSceneStringsMessage(%p) (%d)", (const void *)script, stackPos(0));
	showMessage((const char*)getTableEntry(_sceneStrings, stackPos(0)), 0xFF, 0xF0);
	return 0;
}

int KyraEngine_v3::o3_getRand(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getRand(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	assert(stackPos(0) < stackPos(1));
	return _rnd.getRandomNumberRng(stackPos(0), stackPos(1));
}

int KyraEngine_v3::o3_setDeathHandler(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setDeathHandler(%p) (%d)", (const void *)script, stackPos(0));
	_deathHandler = stackPos(0);
	return 0;
}

int KyraEngine_v3::o3_showGoodConscience(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_showGoodConscience(%p) ()", (const void *)script);
	showGoodConscience();
	return 0;
}

int KyraEngine_v3::o3_goodConscienceChat(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_goodConscienceChat(%p) (%d)", (const void *)script, stackPos(0));
	int id = stackPos(0);
	const char *str = (const char*)getTableEntry(_useActorBuffer ? _actorFile : _sceneStrings, id);
	goodConscienceChat(str, _vocHigh, id);
	return 0;
}

int KyraEngine_v3::o3_hideGoodConscience(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_hideGoodConscience(%p) ()", (const void *)script);
	hideGoodConscience();
	return 0;
}

int KyraEngine_v3::o3_waitForConfirmationClick(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o2_waitForConfirmationClick(%p) (%d)", (const void *)script, stackPos(0));
	resetSkipFlag();
	uint32 maxWaitTime = _system->getMillis() + stackPos(0) * _tickLength;

	while (_system->getMillis() < maxWaitTime) {
		int inputFlag = checkInput(0);
		removeInputTop();

		if (inputFlag == 198 || inputFlag == 199) {
			_sceneScriptState.regs[1] = _mouseX;
			_sceneScriptState.regs[2] = _mouseY;
			return 0;
		}

		update();
		_system->delayMillis(10);
	}

	_sceneScriptState.regs[1] = _mouseX;
	_sceneScriptState.regs[2] = _mouseY;
	return 1;
}

int KyraEngine_v3::o3_defineRoomEntrance(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_defineRoomEntrance(%p) (%d, %d, %d)", (const void *)script, stackPos(0), stackPos(1), stackPos(2));
	switch (stackPos(0)) {
	case 0:
		_sceneEnterX1 = stackPos(1);
		_sceneEnterY1 = stackPos(2);
		break;

	case 1:
		_sceneEnterX2 = stackPos(1);
		_sceneEnterY2 = stackPos(2);
		break;

	case 2:
		_sceneEnterX3 = stackPos(1);
		_sceneEnterY3 = stackPos(2);
		break;

	case 3:
		_sceneEnterX4 = stackPos(1);
		_sceneEnterY4 = stackPos(2);
		break;

	default:
		break;
	}
	return 0;
}

int KyraEngine_v3::o3_runTemporaryScript(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_runTemporaryScript(%p) ('%s', %d, %d, %d)", (const void *)script,
			stackPosString(0), stackPos(1), stackPos(2), stackPos(3));
	const int newShapes = stackPos(1);
	const int unloadShapes = stackPos(2);
	const int allowSkip = stackPos(3);
	runTemporaryScript(stackPosString(0), allowSkip, (unloadShapes != 0) ? 1 : 0, newShapes, unloadShapes);
	return 0;
}

int KyraEngine_v3::o3_setSpecialSceneScriptRunTime(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setSpecialSceneScriptRunTime(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	assert(stackPos(0) >= 0 && stackPos(0) < 10);
	_sceneSpecialScriptsTimer[stackPos(0)] = _system->getMillis() + stackPos(1) * _tickLength;
	return 0;
}

int KyraEngine_v3::o3_defineSceneAnim(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_defineSceneAnim(%p) (%d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, '%s')",
		(const void *)script, stackPos(0), stackPos(1), stackPos(2), stackPos(3), stackPos(4), stackPos(5), stackPos(6), stackPos(7),
		stackPos(8), stackPos(9), stackPos(10), stackPos(11), stackPosString(12));
	const int animId = stackPos(0);
	SceneAnim &anim = _sceneAnims[animId];

	musicUpdate(0);

	uint16 flags = anim.flags = stackPos(1);
	int x = anim.x = stackPos(2);
	int y = anim.y = stackPos(3);
	int x2 = anim.x2 = stackPos(4);
	int y2 = anim.y2 = stackPos(5);
	int w = anim.width = stackPos(6);
	int h = anim.height = stackPos(7);
	anim.unk10 = stackPos(8);
	anim.specialSize = stackPos(9);
	anim.unk14 = stackPos(10);
	anim.shapeIndex = stackPos(11);
	const char *filename = stackPosString(12);

	if (filename)
		strcpy(anim.filename, filename);

	if (flags & 8) {
		_sceneAnimMovie[animId]->open(filename, 1, 0);
		musicUpdate(0);
		if (_sceneAnimMovie[animId]->opened()) {
			anim.wsaFlag = 1;
			if (x2 == -1)
				x2 = _sceneAnimMovie[animId]->xAdd();
			if (y2 == -1)
				y2 = _sceneAnimMovie[animId]->yAdd();
			if (w == -1)
				w = _sceneAnimMovie[animId]->width();
			if (h == -1)
				h = _sceneAnimMovie[animId]->height();
			if (x == -1)
				x = (w >> 1) + x2;
			if (y == -1)
				y = y2 + h - 1;

			anim.x = x;
			anim.y = y;
			anim.x2 = x2;
			anim.y2 = y2;
			anim.width = w;
			anim.height = h;
		}
	}

	musicUpdate(0);

	return 9;
}

int KyraEngine_v3::o3_updateSceneAnim(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_updateSceneAnim(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	updateSceneAnim(stackPos(0), stackPos(1));
	_specialSceneScriptRunFlag = false;
	return 0;
}

int KyraEngine_v3::o3_runActorScript(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_runActorScript(%p) ()", (const void *)script);
	EMCData data;
	EMCState state;
	memset(&data, 0, sizeof(data));
	memset(&state, 0, sizeof(state));

	_res->exists("_ACTOR.EMC", true);
	_emc->load("_ACTOR.EMC", &data, &_opcodes);
	_emc->init(&state, &data);
	_emc->start(&state, 0);

	state.regs[4] = _itemInHand;
	state.regs[0] = _mainCharacter.sceneId;

	int vocHigh = _vocHigh;
	_vocHigh = 200;
	_useActorBuffer = true;

	while (_emc->isValid(&state))
		_emc->run(&state);

	_useActorBuffer = false;
	_vocHigh = vocHigh;
	_emc->unload(&data);

	if (queryGameFlag(0x218)) {
		resetGameFlag(0x218);
		enterNewScene(78, -1, 0, 0, 0);
	}

	return 0;
}

int KyraEngine_v3::o3_runDialog(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_runDialog(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	runDialog(stackPos(0), stackPos(1));
	return 0;
}

int KyraEngine_v3::o3_malcolmRandomChat(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_malcolmRandomChat(%p) ()", (const void *)script);
	malcolmRandomChat();
	return 0;
}

int KyraEngine_v3::o3_setDlgIndex(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setDlgIndex(%p) (%d)", (const void *)script, stackPos(0));
	setDlgIndex(stackPos(0));
	return 0;
}

int KyraEngine_v3::o3_getDlgIndex(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getDlgIndex(%p) ()", (const void *)script);
	return _mainCharacter.dlgIndex;
}

int KyraEngine_v3::o3_defineScene(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_defineScene(%p) (%d, '%s', %d, %d, %d, %d, %d, %d)",
		(const void *)script, stackPos(0), stackPosString(1), stackPos(2), stackPos(3), stackPos(4), stackPos(5), stackPos(6), stackPos(7));
	const int scene = stackPos(0);
	strcpy(_sceneList[scene].filename1, stackPosString(1));
	_sceneList[scene].filename1[9] = 0;
	strcpy(_sceneList[scene].filename2, stackPosString(1));
	_sceneList[scene].filename2[9] = 0;

	_sceneList[scene].exit1 = stackPos(2);
	_sceneList[scene].exit2 = stackPos(3);
	_sceneList[scene].exit3 = stackPos(4);
	_sceneList[scene].exit4 = stackPos(5);
	_sceneList[scene].flags = stackPos(6);
	_sceneList[scene].sound = stackPos(7);

	if (_mainCharacter.sceneId == scene) {
		_sceneExit1 = _sceneList[scene].exit1;
		_sceneExit2 = _sceneList[scene].exit2;
		_sceneExit3 = _sceneList[scene].exit3;
		_sceneExit4 = _sceneList[scene].exit4;
	}

	return 0;
}

int KyraEngine_v3::o3_setConversationState(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setConversationState(%p) (%d, %d, %d)", (const void *)script, stackPos(0), stackPos(1), stackPos(2));
	int id = stackPos(0);
	const int dlgIndex = stackPos(1);
	const int value = stackPos(2);

	switch (_currentChapter-2) {
	case 0:
		id -= 34;
		break;

	case 1:
		id -= 54;
		break;

	case 2:
		id -= 55;
		break;

	case 3:
		id -= 70;
		break;

	default:
		break;
	}

	return (_conversationState[id][dlgIndex] = value);
}

int KyraEngine_v3::o3_getConversationState(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getConversationState(%p) (%d)", (const void *)script, stackPos(0));
	int id = stackPos(0);
	const int dlgIndex = _mainCharacter.dlgIndex;

	switch (_currentChapter-2) {
	case 0:
		id -= 34;
		break;

	case 1:
		id -= 54;
		break;

	case 2:
		id -= 55;
		break;

	case 3:
		id -= 70;
		break;

	default:
		break;
	}

	return _conversationState[id][dlgIndex];
}

int KyraEngine_v3::o3_changeChapter(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_changeChapter(%p) (%d, %d, %d, %d)", (const void *)script, stackPos(0), stackPos(1), stackPos(2), stackPos(3));
	changeChapter(stackPos(0), stackPos(1), stackPos(2), stackPos(3));
	return 0;
}

int KyraEngine_v3::o3_countItemInstances(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_countItemInstances(%p) (%d)", (const void *)script, stackPos(0));
	int count = 0;
	const int16 item = stackPos(0);

	for (int i = 0; i < 10; ++i) {
		if (_mainCharacter.inventory[i] == item)
			++count;
	}

	if (_itemInHand == item)
		++count;

	for (int i = 0; i < 50; ++i) {
		if (_itemList[i].id == item)
			++count;
	}

	return count;
}

int KyraEngine_v3::o3_dialogStartScript(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_dialogStartScript(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	dialogStartScript(stackPos(0), stackPos(1));
	return 0;
}

int KyraEngine_v3::o3_dialogEndScript(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_dialogEndScript(%p) (%d)", (const void *)script, stackPos(0));
	dialogEndScript(stackPos(0));
	return 0;
}

int KyraEngine_v3::o3_setSpecialSceneScriptState(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setSpecialSceneScriptState(%p) (%d)", (const void *)script, stackPos(0));
	_specialSceneScriptState[stackPos(0)] = 1;
	return 1;
}

int KyraEngine_v3::o3_clearSpecialSceneScriptState(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_clearSpecialSceneScriptState(%p) (%d)", (const void *)script, stackPos(0));
	_specialSceneScriptState[stackPos(0)] = 0;
	return 0;
}

int KyraEngine_v3::o3_querySpecialSceneScriptState(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_querySpecialSceneScriptState(%p) (%d)", (const void *)script, stackPos(0));
	return _specialSceneScriptState[stackPos(0)];
}

int KyraEngine_v3::o3_setHiddenItemsEntry(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setHiddenItemsEntry(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	return (_hiddenItems[stackPos(0)] = (uint16)stackPos(1));
}

int KyraEngine_v3::o3_getHiddenItemsEntry(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getHiddenItemsEntry(%p) (%d)", (const void *)script, stackPos(0));
	return (int16)_hiddenItems[stackPos(0)];
}

int KyraEngine_v3::o3_customChat(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_customChat(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	const int id = stackPos(0);
	const int object = stackPos(1);
	const char *str = (const char *)getTableEntry(_sceneStrings, id);

	if (!str)
		return 0;

	strcpy(_stringBuffer, str);
	_chatText = _stringBuffer;
	_chatObject = object;
	_chatVocHigh = _chatVocLow = -1;
	objectChatInit(_stringBuffer, object, _vocHigh, id);
	playVoice(_vocHigh, id);
	return 0;
}

int KyraEngine_v3::o3_customChatFinish(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_customChatFinish(%p) ()", (const void *)script);
	_text->restoreScreen();
	_chatText = 0;
	_chatObject = -1;
	return 0;
}

int KyraEngine_v3::o3_setupSceneAnimObject(EMCState *script) {
	debugC(9, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setupSceneAnimObject(%p) (%d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d, '%s')", (const void *)script,
			stackPos(0), stackPos(1), stackPos(2), stackPos(3), stackPos(4), stackPos(5), stackPos(6), stackPos(7), stackPos(8), stackPos(9),
			stackPos(10), stackPos(11), stackPosString(12));
	musicUpdate(0);
	setupSceneAnimObject(stackPos(0), stackPos(1), stackPos(2), stackPos(3), stackPos(4), stackPos(5), stackPos(6), stackPos(7), stackPos(8),
						stackPos(9), stackPos(10), stackPos(11), stackPosString(12));
	return 0;
}

int KyraEngine_v3::o3_removeSceneAnimObject(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_removeSceneAnimObject(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	removeSceneAnimObject(stackPos(0), stackPos(1));
	return 0;
}

int KyraEngine_v3::o3_disableTimer(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_disableTimer(%p) (%d)", (const void *)script, stackPos(0));
	_timer->disable(stackPos(0));
	return 0;
}

int KyraEngine_v3::o3_enableTimer(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_enableTimer(%p) (%d)", (const void *)script, stackPos(0));
	_timer->enable(stackPos(0));
	return 0;
}

int KyraEngine_v3::o3_setTimerCountdown(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setTimerCountdown(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	_timer->setCountdown(stackPos(0), stackPos(1));
	return 0;
}

int KyraEngine_v3::o3_setVocHigh(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_setVocHigh(%p) (%d)", (const void *)script, stackPos(0));
	_vocHigh = stackPos(0);
	return 0;
}

int KyraEngine_v3::o3_getVocHigh(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_getVocHigh(%p) ()", (const void *)script);
	return _vocHigh;
}

int KyraEngine_v3::o3_dummy(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3_dummy(%p) ()", (const void *)script);
	return 0;
}

#pragma mark -

int KyraEngine_v3::o3t_defineNewShapes(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3t_defineNewShapes(%p) ('%s', %d, %d, %d, %d, %d)", (const void *)script,
			stackPosString(0), stackPos(1), stackPos(2), stackPos(3), stackPos(4), stackPos(5));
	strcpy(_newShapeFilename, stackPosString(0));
	_newShapeLastEntry = stackPos(1);
	_newShapeWidth = stackPos(2);
	_newShapeHeight = stackPos(3);
	_newShapeXAdd = stackPos(4);
	_newShapeYAdd = stackPos(5);
	return 0;
}

int KyraEngine_v3::o3t_setCurrentFrame(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3t_setCurrentFrame(%p) (%d, %d)", (const void *)script, stackPos(0), stackPos(1));
	static const uint8 frameTable[] = {
		0x58, 0xD8, 0xD8, 0x98, 0x78, 0x78, 0xB8, 0xB8
	};

	_newShapeAnimFrame = stackPos(0);
	if (_useFrameTable)
		_newShapeAnimFrame += frameTable[_mainCharacter.facing];

	_newShapeDelay = stackPos(1);
	_temporaryScriptExecBit = true;
	return 0;
}

int KyraEngine_v3::o3t_setNewShapeFlag(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3t_setNewShapeFlag(%p) (%d)", (const void *)script, stackPos(0));
	_newShapeFlag = stackPos(0);
	return 0;
}

#pragma mark -

int KyraEngine_v3::o3d_updateAnim(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3d_updateAnim(%p) (%d)", (const void *)script, stackPos(0));
	if (_dialogSceneAnim >= 0)
		updateSceneAnim(_dialogSceneAnim, stackPos(0));
	return 0;
}

int KyraEngine_v3::o3d_delay(EMCState *script) {
	debugC(3, kDebugLevelScriptFuncs, "KyraEngine_v3::o3d_delay(%p) (%d)", (const void *)script, stackPos(0));
	const uint32 endTime = _system->getMillis() + stackPos(0) * _tickLength;
	while (_system->getMillis() < endTime) {
		if (_chatText)
			updateWithText();
		else
			update();
		_system->delayMillis(10);
	}
	return 0;
}

#pragma mark -

typedef Common::Functor1Mem<EMCState*, int, KyraEngine_v3> OpcodeV3;
#define SetOpcodeTable(x) table = &x;
#define Opcode(x) table->push_back(new OpcodeV3(this, &KyraEngine_v3::x))
#define OpcodeUnImpl() table->push_back(new OpcodeV3(this, 0))
void KyraEngine_v3::setupOpcodeTable() {
	Common::Array<const Opcode*> *table = 0;

	SetOpcodeTable(_opcodes);
	// 0x00
	Opcode(o3_getMalcolmShapes);
	Opcode(o3_setCharacterPos);
	Opcode(o3_defineObject);
	Opcode(o3_refreshCharacter);
	// 0x04
	Opcode(o3_getCharacterX);
	Opcode(o3_getCharacterY);
	Opcode(o3_getCharacterFacing);
	Opcode(o3_getCharacterScene);
	// 0x08
	Opcode(o3_getMalcolmsMood);
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	Opcode(o3_getCharacterFrameFromFacing);
	// 0x0c
	Opcode(o3_setCharacterFacingOverwrite);
	Opcode(o3_trySceneChange);
	Opcode(o3_moveCharacter);
	Opcode(o3_setCharacterFacing);
	// 0x10
	OpcodeUnImpl();
	Opcode(o3_showSceneFileMessage);
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	// 0x14
	Opcode(o3_setCharacterAnimFrameFromFacing);
	Opcode(o3_showBadConscience);
	Opcode(o3_dummy);
	Opcode(o3_hideBadConscience);
	// 0x18
	OpcodeUnImpl();
	OpcodeUnImpl();
	Opcode(o3_setInventorySlot);
	Opcode(o3_getInventorySlot);
	// 0x1c
	Opcode(o3_addItemToInventory);
	OpcodeUnImpl();
	Opcode(o3_addItemToCurScene);
	Opcode(o3_objectChat);
	// 0x20
	Opcode(o3_checkForItem);
	Opcode(o3_dummy);
	Opcode(o3_resetInventory);
	Opcode(o3_defineItem);
	// 0x24
	Opcode(o3_removeInventoryItemInstances);
	Opcode(o3_countInventoryItemInstances);
	Opcode(o3_npcChatSequence);
	Opcode(o3_queryGameFlag);
	// 0x28
	Opcode(o3_resetGameFlag);
	Opcode(o3_setGameFlag);
	Opcode(o3_setHandItem);
	Opcode(o3_removeHandItem);
	// 0x2c
	Opcode(o3_handItemSet);
	Opcode(o3_hideMouse);
	Opcode(o3_addSpecialExit);
	Opcode(o3_setMousePos);
	// 0x30
	Opcode(o3_showMouse);
	Opcode(o3_badConscienceChat);
	Opcode(o3_wipeDownMouseItem);
	Opcode(o3_dummy);
	// 0x34
	Opcode(o3_setMalcolmsMood);
	Opcode(o3_playSoundEffect);
	Opcode(o3_dummy);
	Opcode(o3_delay);
	// 0x38
	Opcode(o3_updateScore);
	Opcode(o3_makeSecondChanceSave);
	Opcode(o3_setSceneFilename);
	OpcodeUnImpl();
	// 0x3c
	Opcode(o3_removeItemsFromScene);
	Opcode(o3_disguiseMalcolm);
	Opcode(o3_drawSceneShape);
	Opcode(o3_drawSceneShapeOnPage);
	// 0x40
	Opcode(o3_checkInRect);
	Opcode(o3_updateConversations);
	OpcodeUnImpl();
	Opcode(o3_dummy);
	// 0x44
	Opcode(o3_dummy);
	Opcode(o3_setSceneDim);
	OpcodeUnImpl();
	Opcode(o3_dummy);
	// 0x48
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	Opcode(o3_setSceneAnimPosAndFrame);
	Opcode(o3_update);
	// 0x4c
	Opcode(o3_removeItemInstances);
	Opcode(o3_dummy);
	Opcode(o3_disableInventory);
	Opcode(o3_enableInventory);
	// 0x50
	Opcode(o3_enterNewScene);
	Opcode(o3_switchScene);
	Opcode(o3_getShapeFlag1);
	Opcode(o3_dummy);
	// 0x54
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	Opcode(o3_setMalcolmPos);
	Opcode(o3_stopMusic);
	// 0x58
	Opcode(o3_playWanderScoreViaMap);
	Opcode(o3_playSoundEffect);
	Opcode(o3_getScore);
	OpcodeUnImpl();
	// 0x5c
	Opcode(o3_blockOutRegion);
	Opcode(o3_dummy);
	Opcode(o3_showSceneStringsMessage);
	OpcodeUnImpl();
	// 0x60
	Opcode(o3_getRand);
	Opcode(o3_dummy);
	Opcode(o3_setDeathHandler);
	Opcode(o3_showGoodConscience);
	// 0x64
	Opcode(o3_goodConscienceChat);
	Opcode(o3_hideGoodConscience);
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	// 0x68
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	Opcode(o3_waitForConfirmationClick);
	// 0x6c
	Opcode(o3_dummy);
	Opcode(o3_defineRoomEntrance);
	Opcode(o3_runTemporaryScript);
	Opcode(o3_setSpecialSceneScriptRunTime);
	// 0x70
	Opcode(o3_defineSceneAnim);
	Opcode(o3_dummy);
	Opcode(o3_updateSceneAnim);
	Opcode(o3_dummy);
	// 0x74
	Opcode(o3_runActorScript);
	Opcode(o3_runDialog);
	Opcode(o3_malcolmRandomChat);
	Opcode(o3_setDlgIndex);
	// 0x78
	Opcode(o3_getDlgIndex);
	Opcode(o3_defineScene);
	Opcode(o3_setConversationState);
	OpcodeUnImpl();
	// 0x7c
	OpcodeUnImpl();
	Opcode(o3_getConversationState);
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	// 0x80
	Opcode(o3_dummy);
	Opcode(o3_changeChapter);
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	// 0x84
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	// 0x88
	Opcode(o3_countItemInstances);
	Opcode(o3_dummy);
	Opcode(o3_dialogStartScript);
	Opcode(o3_dummy);
	// 0x8c
	Opcode(o3_dialogEndScript);
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	Opcode(o3_setSpecialSceneScriptState);
	// 0x90
	Opcode(o3_clearSpecialSceneScriptState);
	Opcode(o3_querySpecialSceneScriptState);
	Opcode(o3_dummy);
	Opcode(o3_setHiddenItemsEntry);
	// 0x94
	Opcode(o3_getHiddenItemsEntry);
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	OpcodeUnImpl();
	// 0x98
	Opcode(o3_customChat);
	Opcode(o3_customChatFinish);
	Opcode(o3_setupSceneAnimObject);
	Opcode(o3_removeSceneAnimObject);
	// 0x9c
	Opcode(o3_disableTimer);
	Opcode(o3_enableTimer);
	Opcode(o3_setTimerCountdown);
	OpcodeUnImpl();
	// 0xa0
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	Opcode(o3_dummy);
	// 0xa4
	OpcodeUnImpl();
	OpcodeUnImpl();
	OpcodeUnImpl();
	Opcode(o3_setVocHigh);
	// 0xa8
	Opcode(o3_getVocHigh);
	OpcodeUnImpl();
	OpcodeUnImpl();
	OpcodeUnImpl();
	// 0xac
	OpcodeUnImpl();
	Opcode(o3_dummy);
	OpcodeUnImpl();
	Opcode(o3_dummy);
	
	SetOpcodeTable(_opcodesTemporary);
	// 0x00
	Opcode(o3t_defineNewShapes);
	Opcode(o3t_setCurrentFrame);
	Opcode(o3_playSoundEffect);
	Opcode(o3_dummy);
	// 0x0a
	Opcode(o3t_setNewShapeFlag);
	Opcode(o3_getRand);
	Opcode(o3_getMalcolmShapes);
	Opcode(o3_dummy);

	SetOpcodeTable(_opcodesDialog);
	// 0x00
	Opcode(o3d_updateAnim);
	Opcode(o3d_delay);
	Opcode(o3_getRand);
	Opcode(o3_queryGameFlag);
	// 0x04
	Opcode(o3_dummy);
}

} // end of namespace Kyra
