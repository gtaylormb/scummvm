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

/*
* Based on the Reverse Engineering work of Christophe Fontanel,
* maintainer of the Dungeon Master Encyclopaedia (http://dmweb.free.fr/)
*/

#include "dm/group.h"
#include "dm/dungeonman.h"
#include "dm/champion.h"
#include "dm/movesens.h"
#include "dm/projexpl.h"
#include "dm/timeline.h"
#include "dm/objectman.h"
#include "dm/menus.h"
#include "dm/sounds.h"


namespace DM {
int32 M32_setTime(int32 &map_time, int32 time) {
	return map_time = (map_time & 0xFF000000) | time;
}

GroupMan::GroupMan(DMEngine *vm) : _vm(vm) {
	for (uint16 i = 0; i < 4; ++i)
		_dropMovingCreatureFixedPossessionsCell[i] = 0;
	_dropMovingCreatureFixedPossCellCount = 0;
	_fluxCageCount = 0;
	for (uint16 i = 0; i < 4; ++i)
		_fluxCages[i] = 0;
	_currentGroupMapX = 0;
	_currentGroupMapY = 0;
	_currGroupThing = Thing(0);
	for (uint16 i = 0; i < 4; ++i)
		_groupMovementTestedDirections[i] = 0;
	_currGroupDistanceToParty = 0;
	_currGroupPrimaryDirToParty = 0;
	_currGroupSecondaryDirToParty = 0;
	_groupMovementBlockedByGroupThing = Thing(0);
	_groupMovementBlockedByDoor = false;
	_groupMovementBlockedByParty = false;
	_groupMovBlockedByWallStairsPitFakeWalFluxCageTeleporter = false;
	_maxActiveGroupCount = 60;
	_activeGroups = nullptr;
	_currActiveGroupCount = 0;
	twoHalfSquareSizedCreaturesGroupLastDirectionSetTime = 0;
}

GroupMan::~GroupMan() {
	delete[] _activeGroups;
}

uint16 GroupMan::toggleFlag(uint16& val, uint16 mask) {
	return val ^= mask;
}

void GroupMan::initActiveGroups() {
	if (_vm->_newGameFl)
		_maxActiveGroupCount = 60;

	if (_activeGroups)
		delete[] _activeGroups;

	_activeGroups = new ActiveGroup[_maxActiveGroupCount];
	for (uint16 i = 0; i < _maxActiveGroupCount; ++i)
		_activeGroups[i]._groupThingIndex = -1;
}

uint16 GroupMan::getGroupCells(Group *group, int16 mapIndex) {
	byte cells = group->_cells;
	if (mapIndex == _vm->_dungeonMan->_partyMapIndex)
		cells = _activeGroups[cells]._cells;
	return cells;
}

uint16 GroupMan::getGroupDirections(Group *group, int16 mapIndex) {
	static byte groupDirections[4] = {0x00, 0x55, 0xAA, 0xFF}; // @ G0258_auc_Graphic559_GroupDirections

	if (mapIndex == _vm->_dungeonMan->_partyMapIndex)
		return _activeGroups[group->getActiveGroupIndex()]._directions;

	return groupDirections[group->getDir()];
}

int16 GroupMan::getCreatureOrdinalInCell(Group *group, uint16 cell) {
	uint16 currMapIndex = _vm->_dungeonMan->_currMapIndex;
	byte groupCells = getGroupCells(group, currMapIndex);
	if (groupCells == k255_CreatureTypeSingleCenteredCreature)
		return _vm->indexToOrdinal(0);

	int retval = 0;
	byte creatureIndex = group->getCount();
	if (getFlag(_vm->_dungeonMan->_creatureInfos[group->_type]._attributes, k0x0003_MaskCreatureInfo_size) == k1_MaskCreatureSizeHalf) {
		if ((getGroupDirections(group, currMapIndex) & 1) == (cell & 1))
			cell = _vm->turnDirLeft(cell);

		do {
			byte creatureCell = getCreatureValue(groupCells, creatureIndex);
			if (creatureCell == cell || creatureCell == _vm->turnDirRight(cell)) {
				retval = _vm->indexToOrdinal(creatureIndex);
				break;
			}
		} while (creatureIndex--);
	} else {
		do {
			if (getCreatureValue(groupCells, creatureIndex) == cell) {
				retval = _vm->indexToOrdinal(creatureIndex);
				break;
			}
		} while (creatureIndex--);
	}

	return retval;
}

uint16 GroupMan::getCreatureValue(uint16 groupVal, uint16 creatureIndex) {
	return (groupVal >> (creatureIndex << 1)) & 0x3;
}

void GroupMan::dropGroupPossessions(int16 mapX, int16 mapY, Thing groupThing, SoundMode soundMode) {
	Group *group = (Group *)_vm->_dungeonMan->getThingData(groupThing);
	uint16 creatureType = group->_type;
	if ((soundMode != kDMSoundModeDoNotPlaySound) && getFlag(_vm->_dungeonMan->_creatureInfos[creatureType]._attributes, k0x0200_MaskCreatureInfo_dropFixedPoss)) {
		int16 creatureIndex = group->getCount();
		uint16 groupCells = getGroupCells(group, _vm->_dungeonMan->_currMapIndex);
		do {
			dropCreatureFixedPossessions(creatureType, mapX, mapY,
				(groupCells == k255_CreatureTypeSingleCenteredCreature) ? k255_CreatureTypeSingleCenteredCreature : getCreatureValue(groupCells, creatureIndex), soundMode);
		} while (creatureIndex--);
	}

	Thing currentThing = group->_slot;
	if ((currentThing) != Thing::_endOfList) {
		bool weaponDropped = false;
		Thing nextThing;
		do {
			nextThing = _vm->_dungeonMan->getNextThing(currentThing);
			currentThing = _vm->thingWithNewCell(currentThing, _vm->getRandomNumber(4));
			if ((currentThing).getType() == kDMThingTypeWeapon) {
				weaponDropped = true;
			}
			_vm->_moveSens->getMoveResult(currentThing, kDMMapXNotOnASquare, 0, mapX, mapY);
		} while ((currentThing = nextThing) != Thing::_endOfList);

		if (soundMode != kDMSoundModeDoNotPlaySound)
			_vm->_sound->requestPlay(weaponDropped ? k00_soundMETALLIC_THUD : k04_soundWOODEN_THUD_ATTACK_TROLIN_ANTMAN_STONE_GOLEM, mapX, mapY, soundMode);
	}
}

void GroupMan::dropCreatureFixedPossessions(uint16 creatureType, int16 mapX, int16 mapY, uint16 cell, SoundMode soundMode) {
	static uint16 fixedPossessionCreature12Skeleton[3] = { // @ G0245_aui_Graphic559_FixedPossessionsCreature12Skeleton
		kDMObjectInfoIndexFirstWeapon + kDMWeaponFalchion,
		kDMObjectInfoIndexFirstArmour + kDMArmourWoodenShield,
		0}
	;
	static uint16 fixedPossessionCreature9StoneGolem[2] = { // @ G0246_aui_Graphic559_FixedPossessionsCreature09StoneGolem
		kDMObjectInfoIndexFirstWeapon + kDMWeaponStoneClub,
		0
	};
	static uint16 fixedPossessionCreatur16TrolinAntman[2] = { // @ G0247_aui_Graphic559_FixedPossessionsCreature16Trolin_Antman
		kDMObjectInfoIndexFirstWeapon + kDMWeaponClub,
		0
	};
	static uint16 fixedPossessionCreature18AnimatedArmourDethKnight[7] = { // @ G0248_aui_Graphic559_FixedPossessionsCreature18AnimatedArmour_DethKnight
		kDMObjectInfoIndexFirstArmour + kDMArmourFootPlate,
		kDMObjectInfoIndexFirstArmour + kDMArmourLegPlate,
		kDMObjectInfoIndexFirstArmour + kDMArmourTorsoPlate,
		kDMObjectInfoIndexFirstWeapon + kDMWeaponSword,
		kDMObjectInfoIndexFirstArmour + kDMArmourArmet,
		kDMObjectInfoIndexFirstWeapon + kDMWeaponSword,
		0
	};
	static uint16 fixedPossessionCreature7rockRockPile[5] = { // @ G0249_aui_Graphic559_FixedPossessionsCreature07Rock_RockPile
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeBoulder,
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeBoulder | kDMMaskRandomDrop,
		kDMObjectInfoIndexFirstWeapon + kDMWeaponRock | kDMMaskRandomDrop,
		kDMObjectInfoIndexFirstWeapon + kDMWeaponRock | kDMMaskRandomDrop,
		0
	};
	static uint16 fixedPossessionCreature4PainRatHellHound[3] = { // @ G0250_aui_Graphic559_FixedPossessionsCreature04PainRat_Hellhound
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeDrumstickShank,
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeDrumstickShank | kDMMaskRandomDrop,
		0
	};
	static uint16 fixedPossessionCreature6screamer[3] = { // @ G0251_aui_Graphic559_FixedPossessionsCreature06Screamer
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeScreamerSlice,
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeScreamerSlice | kDMMaskRandomDrop,
		0
	};
	static uint16 fixedPossessionCreature15MagnetaWormWorm[4] = { // @ G0252_aui_Graphic559_FixedPossessionsCreature15MagentaWorm_Worm
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeWormRound,
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeWormRound | kDMMaskRandomDrop,
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeWormRound | kDMMaskRandomDrop,
		0
	};
	static uint16 fixedPossessionCreature24RedDragon[11] = { // @ G0253_aui_Graphic559_FixedPossessionsCreature24RedDragon
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeDragonSteak,
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeDragonSteak,
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeDragonSteak,
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeDragonSteak,
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeDragonSteak,
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeDragonSteak,
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeDragonSteak,
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeDragonSteak,
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeDragonSteak | kDMMaskRandomDrop,
		kDMObjectInfoIndexFirstJunk + kDMJunkTypeDragonSteak | kDMMaskRandomDrop, 0};

	uint16 *fixedPossessions;
	bool cursedPossessions = false;
	switch (creatureType) {
	case k4_CreatureTypePainRatHellHound:
		fixedPossessions = fixedPossessionCreature4PainRatHellHound;
		break;
	case k6_CreatureTypeScreamer:
		fixedPossessions = fixedPossessionCreature6screamer;
		break;
	case k7_CreatureTypeRockpile:
		fixedPossessions = fixedPossessionCreature7rockRockPile;
		break;
	case k9_CreatureTypeStoneGolem:
		fixedPossessions = fixedPossessionCreature9StoneGolem;
		break;
	case k12_CreatureTypeSkeleton:
		fixedPossessions = fixedPossessionCreature12Skeleton;
		break;
	case k16_CreatureTypeTrolinAntman:
		fixedPossessions = fixedPossessionCreatur16TrolinAntman;
		break;
	case k15_CreatureTypeMagnetaWormWorm:
		fixedPossessions = fixedPossessionCreature15MagnetaWormWorm;
		break;
	case k18_CreatureTypeAnimatedArmourDethKnight:
		cursedPossessions = true;
		fixedPossessions = fixedPossessionCreature18AnimatedArmourDethKnight;
		break;
	case k24_CreatureTypeRedDragon:
		fixedPossessions = fixedPossessionCreature24RedDragon;
		break;
	default:
		return;
	}

	uint16 currFixedPossession = *fixedPossessions++;
	bool weaponDropped = false;
	while (currFixedPossession) {
		if (getFlag(currFixedPossession, kDMMaskRandomDrop) && _vm->getRandomNumber(2))
			continue;

		int16 currThingType;
		if (clearFlag(currFixedPossession, kDMMaskRandomDrop) >= kDMObjectInfoIndexFirstJunk) {
			currThingType = kDMThingTypeJunk;
			currFixedPossession -= kDMObjectInfoIndexFirstJunk;
		} else if (currFixedPossession >= kDMObjectInfoIndexFirstArmour) {
			currThingType = kDMThingTypeArmour;
			currFixedPossession -= kDMObjectInfoIndexFirstArmour;
		} else {
			weaponDropped = true;
			currThingType = kDMThingTypeWeapon;
			currFixedPossession -= kDMObjectInfoIndexFirstWeapon;
		}

		Thing nextUnusedThing = _vm->_dungeonMan->getUnusedThing(currThingType);
		if ((nextUnusedThing) == Thing::_none)
			continue;

		Weapon *currWeapon = (Weapon *)_vm->_dungeonMan->getThingData(nextUnusedThing);
		/* The same pointer type is used no matter the actual type k5_WeaponThingType, k6_ArmourThingType or k10_JunkThingType */
		currWeapon->setType(currFixedPossession);
		currWeapon->setCursed(cursedPossessions);
		nextUnusedThing = _vm->thingWithNewCell(nextUnusedThing, ((cell == k255_CreatureTypeSingleCenteredCreature) || !_vm->getRandomNumber(4)) ? _vm->getRandomNumber(4) : cell);
		_vm->_moveSens->getMoveResult(nextUnusedThing, kDMMapXNotOnASquare, 0, mapX, mapY);
		currFixedPossession = *fixedPossessions++;
	}
	_vm->_sound->requestPlay(weaponDropped ? k00_soundMETALLIC_THUD : k04_soundWOODEN_THUD_ATTACK_TROLIN_ANTMAN_STONE_GOLEM, mapX, mapY, soundMode);
}

int16 GroupMan::getDirsWhereDestIsVisibleFromSource(int16 srcMapX, int16 srcMapY, int16 destMapX, int16 destMapY) {
	if (srcMapX == destMapX) {
		_vm->_projexpl->_secondaryDirToOrFromParty = (_vm->getRandomNumber(65536) & 0x0002) + 1; /* Resulting direction may be 1 or 3 (East or West) */
		if (srcMapY > destMapY)
			return kDMDirNorth;

		return kDMDirSouth;
	}
	if (srcMapY == destMapY) {
		_vm->_projexpl->_secondaryDirToOrFromParty = (_vm->getRandomNumber(65536) & 0x0002) + 0; /* Resulting direction may be 0 or 2 (North or South) */
		if (srcMapX > destMapX)
			return kDMDirWest;

		return kDMDirEast;
	}

	int16 curDirection = kDMDirNorth;
	for (;;) {
		if (isDestVisibleFromSource(curDirection, srcMapX, srcMapY, destMapX, destMapY)) {
			_vm->_projexpl->_secondaryDirToOrFromParty = _vm->turnDirRight(curDirection);
			if (!isDestVisibleFromSource(_vm->_projexpl->_secondaryDirToOrFromParty, srcMapX, srcMapY, destMapX, destMapY)) {
				_vm->_projexpl->_secondaryDirToOrFromParty = _vm->turnDirLeft(curDirection);
				if ((curDirection != kDMDirNorth) || !isDestVisibleFromSource(_vm->_projexpl->_secondaryDirToOrFromParty, srcMapX, srcMapY, destMapX, destMapY)) {
					_vm->_projexpl->_secondaryDirToOrFromParty = _vm->turnDirRight((_vm->getRandomNumber(65536) & 0x0002) + curDirection);
					return curDirection;
				}
			}
			if (_vm->getRandomNumber(2)) {
				int16 primaryDirection = _vm->_projexpl->_secondaryDirToOrFromParty;
				_vm->_projexpl->_secondaryDirToOrFromParty = curDirection;
				return primaryDirection;
			}
			return curDirection;
		}
		curDirection++;
	}
}

bool GroupMan::isDestVisibleFromSource(uint16 dir, int16 srcMapX, int16 srcMapY, int16 destMapX, int16 destMapY) {
	switch (dir) { /* If direction is not 'West' then swap variables so that the same test as for west can be applied */
	case kDMDirSouth:
		SWAP(srcMapX, destMapY);
		SWAP(destMapX, srcMapY);
		break;
	case kDMDirEast:
		SWAP(srcMapX, destMapX);
		SWAP(destMapY, srcMapY);
		break;
	case kDMDirNorth:
		SWAP(srcMapX, srcMapY);
		SWAP(destMapX, destMapY);
		break;
	}
	return ((srcMapX -= (destMapX - 1)) > 0) && ((((srcMapY -= destMapY) < 0) ? -srcMapY : srcMapY) <= srcMapX);
}

bool GroupMan::groupIsDoorDestoryedByAttack(uint16 mapX, uint16 mapY, int16 attack, bool magicAttack, int16 ticks) {
	Door *curDoor = (Door *)_vm->_dungeonMan->getSquareFirstThingData(mapX, mapY);
	if ((magicAttack && !curDoor->isMagicDestructible()) || (!magicAttack && !curDoor->isMeleeDestructible()))
		return false;

	if (attack >= _vm->_dungeonMan->_currMapDoorInfo[curDoor->getType()]._defense) {
		byte *curSquare = &_vm->_dungeonMan->_currMapData[mapX][mapY];
		if (Square(*curSquare).getDoorState() == k4_doorState_CLOSED) {
			if (ticks) {
				TimelineEvent newEvent;
				_vm->setMapAndTime(newEvent._mapTime, _vm->_dungeonMan->_currMapIndex, _vm->_gameTime + ticks);
				newEvent._type = k2_TMEventTypeDoorDestruction;
				newEvent._priority = 0;
				newEvent._Bu._location._mapX = mapX;
				newEvent._Bu._location._mapY = mapY;
				_vm->_timeline->addEventGetEventIndex(&newEvent);
			} else {
				((Square *)curSquare)->setDoorState(k5_doorState_DESTROYED);
			}
			return true;
		}
	}
	return false;
}

Thing GroupMan::groupGetThing(int16 mapX, int16 mapY) {
	Thing curThing = _vm->_dungeonMan->getSquareFirstThing(mapX, mapY);
	while ((curThing != Thing::_endOfList) && (curThing.getType() != kDMThingTypeGroup))
		curThing = _vm->_dungeonMan->getNextThing(curThing);

	return curThing;
}

int16 GroupMan::groupGetDamageCreatureOutcome(Group *group, uint16 creatureIndex, int16 mapX, int16 mapY, int16 damage, bool notMoving) {
	uint16 creatureType = group->_type;
	CreatureInfo *creatureInfo = &_vm->_dungeonMan->_creatureInfos[creatureType];
	if (getFlag(creatureInfo->_attributes, k0x2000_MaskCreatureInfo_archenemy)) /* Lord Chaos cannot be damaged */
		return k0_outcomeKilledNoCreaturesInGroup;

	if (group->_health[creatureIndex] <= damage) {
		uint16 groupCells = getGroupCells(group, _vm->_dungeonMan->_currMapIndex);
		uint16 cell = (groupCells == k255_CreatureTypeSingleCenteredCreature) ? k255_CreatureTypeSingleCenteredCreature : getCreatureValue(groupCells, creatureIndex);
		uint16 creatureCount = group->getCount();
		uint16 retVal;

		if (!creatureCount) { /* If there is a single creature in the group */
			if (notMoving) {
				dropGroupPossessions(mapX, mapY, groupGetThing(mapX, mapY), kDMSoundModePlayOneTickLater);
				groupDelete(mapX, mapY);
			}
			retVal = k2_outcomeKilledAllCreaturesInGroup;
		} else { /* If there are several creatures in the group */
			uint16 groupDirections = getGroupDirections(group, _vm->_dungeonMan->_currMapIndex);
			if (getFlag(creatureInfo->_attributes, k0x0200_MaskCreatureInfo_dropFixedPoss)) {
				if (notMoving)
					dropCreatureFixedPossessions(creatureType, mapX, mapY, cell, kDMSoundModePlayOneTickLater);
				else
					_dropMovingCreatureFixedPossessionsCell[_dropMovingCreatureFixedPossCellCount++] = cell;
			}
			bool currentMapIsPartyMap = (_vm->_dungeonMan->_currMapIndex == _vm->_dungeonMan->_partyMapIndex);
			ActiveGroup *activeGroup = nullptr;
			if (currentMapIsPartyMap)
				activeGroup = &_activeGroups[group->getActiveGroupIndex()];

			if (group->getBehaviour() == k6_behavior_ATTACK) {
				TimelineEvent *curEvent = _vm->_timeline->_events;
				for (uint16 eventIndex = 0; eventIndex < _vm->_timeline->_eventMaxCount; eventIndex++) {
					uint16 curEventType = curEvent->_type;
					if ((_vm->getMap(curEvent->_mapTime) == _vm->_dungeonMan->_currMapIndex) &&
						(curEvent->_Bu._location._mapX == mapX) &&
						(curEvent->_Bu._location._mapY == mapY) &&
						(curEventType > k32_TMEventTypeUpdateAspectGroup) &&
						(curEventType < k41_TMEventTypeUpdateBehaviour_3 + 1)) {
						uint16 nextCreatureIndex;
						if (curEventType < k37_TMEventTypeUpdateBehaviourGroup)
							nextCreatureIndex = curEventType - k33_TMEventTypeUpdateAspectCreature_0; /* Get creature index for events 33 to 36 */
						else
							nextCreatureIndex = curEventType - k38_TMEventTypeUpdateBehaviour_0; /* Get creature index for events 38 to 41 */

						if (nextCreatureIndex == creatureIndex)
							_vm->_timeline->deleteEvent(eventIndex);
						else if (nextCreatureIndex > creatureIndex) {
							curEvent->_type -= 1;
							_vm->_timeline->fixChronology(_vm->_timeline->getIndex(eventIndex));
						}
					}
					curEvent++;
				}

				uint16 fearResistance = creatureInfo->getFearResistance();
				if (currentMapIsPartyMap && (fearResistance) != k15_immuneToFear) {
					fearResistance += creatureCount - 1;
					if (fearResistance < _vm->getRandomNumber(16)) { /* Test if the death of a creature frightens the remaining creatures in the group */
						activeGroup->_delayFleeingFromTarget = _vm->getRandomNumber(100 - (fearResistance << 2)) + 20;
						group->setBehaviour(k5_behavior_FLEE);
					}
				}
			}
			uint16 nextCreatureIndex = creatureIndex;
			for (uint16 curCreatureIndex = creatureIndex; curCreatureIndex < creatureCount; curCreatureIndex++) {
				nextCreatureIndex++;
				group->_health[curCreatureIndex] = group->_health[nextCreatureIndex];
				groupDirections = getGroupValueUpdatedWithCreatureValue(groupDirections, curCreatureIndex, getCreatureValue(groupDirections, nextCreatureIndex));
				groupCells = getGroupValueUpdatedWithCreatureValue(groupCells, curCreatureIndex, getCreatureValue(groupCells, nextCreatureIndex));
				if (currentMapIsPartyMap)
					activeGroup->_aspect[curCreatureIndex] = activeGroup->_aspect[nextCreatureIndex];
			}
			groupCells &= 0x003F;
			_vm->_dungeonMan->setGroupCells(group, groupCells, _vm->_dungeonMan->_currMapIndex);
			_vm->_dungeonMan->setGroupDirections(group, groupDirections, _vm->_dungeonMan->_currMapIndex);
			group->setCount(group->getCount() - 1);
			retVal = k1_outcomeKilledSomeCreaturesInGroup;
		}

		uint16 creatureSize = getFlag(creatureInfo->_attributes, k0x0003_MaskCreatureInfo_size);
		uint16 attack;
		if (creatureSize == k0_MaskCreatureSizeQuarter)
			attack = 110;
		else if (creatureSize == k1_MaskCreatureSizeHalf)
			attack = 190;
		else
			attack = 255;

		_vm->_projexpl->createExplosion(Thing::_explSmoke, attack, mapX, mapY, cell); /* BUG0_66 Smoke is placed on the source map instead of the destination map when a creature dies by falling through a pit. The game has a special case to correctly drop the creature possessions on the destination map but there is no such special case for the smoke. Note that the death must be caused by the damage of the fall (there is no smoke if the creature is removed because its type is not allowed on the destination map). However this bug has no visible consequence because of BUG0_26: the smoke explosion falls in the pit right after being placed in the dungeon and before being drawn on screen so it is only visible on the destination square */
		return retVal;
	}

	if (damage > 0)
		group->_health[creatureIndex] -= damage;

	return k0_outcomeKilledNoCreaturesInGroup;
}

void GroupMan::groupDelete(int16 mapX, int16 mapY) {
	Thing groupThing = groupGetThing(mapX, mapY);
	if (groupThing == Thing::_endOfList)
		return;

	Group *group = (Group *)_vm->_dungeonMan->getThingData(groupThing);
	for (uint16 i = 0; i < 4; ++i)
		group->_health[i] = 0;
	_vm->_moveSens->getMoveResult(groupThing, mapX, mapY, kDMMapXNotOnASquare, 0);
	group->_nextThing = Thing::_none;
	if (_vm->_dungeonMan->_currMapIndex == _vm->_dungeonMan->_partyMapIndex) {
		_activeGroups[group->getActiveGroupIndex()]._groupThingIndex = -1;
		_currActiveGroupCount--;
	}
	groupDeleteEvents(mapX, mapY);
}

void GroupMan::groupDeleteEvents(int16 mapX, int16 mapY) {
	TimelineEvent *curEvent = _vm->_timeline->_events;
	for (int16 eventIndex = 0; eventIndex < _vm->_timeline->_eventMaxCount; eventIndex++) {
		uint16 curEventType = curEvent->_type;
		if ((_vm->getMap(curEvent->_mapTime) == _vm->_dungeonMan->_currMapIndex) &&
			(curEventType > k29_TMEventTypeGroupReactionDangerOnSquare - 1) && (curEventType < k41_TMEventTypeUpdateBehaviour_3 + 1) &&
			(curEvent->_Bu._location._mapX == mapX) && (curEvent->_Bu._location._mapY == mapY)) {
			_vm->_timeline->deleteEvent(eventIndex);
		}
		curEvent++;
	}
}

uint16 GroupMan::getGroupValueUpdatedWithCreatureValue(uint16 groupVal, uint16 creatureIndex, uint16 creatureVal) {
	creatureVal &= 0x0003;
	creatureIndex <<= 1;
	creatureVal <<= creatureIndex;
	return creatureVal | (groupVal & ~(3 << creatureVal));
}

int16 GroupMan::getDamageAllCreaturesOutcome(Group *group, int16 mapX, int16 mapY, int16 attack, bool notMoving) {
	bool killedSomeCreatures = false;
	bool killedAllCreatures = true;
	_dropMovingCreatureFixedPossCellCount = 0;
	if (attack > 0) {
		int16 creatureIndex = group->getCount();
		uint16 randomAttackSeed = (attack >> 3) + 1;
		attack -= randomAttackSeed;
		randomAttackSeed <<= 1;
		do {
			int16 outcomeVal = groupGetDamageCreatureOutcome(group, creatureIndex, mapX, mapY, attack + _vm->getRandomNumber(randomAttackSeed), notMoving);
			killedAllCreatures = outcomeVal && killedAllCreatures;
			killedSomeCreatures = killedSomeCreatures || outcomeVal;
		} while (creatureIndex--);
		if (killedAllCreatures)
			return k2_outcomeKilledAllCreaturesInGroup;

		if (killedSomeCreatures)
			return k1_outcomeKilledSomeCreaturesInGroup;
	}

	return k0_outcomeKilledNoCreaturesInGroup;
}

int16 GroupMan::groupGetResistanceAdjustedPoisonAttack(uint16 creatreType, int16 poisonAttack) {
	int16 poisonResistance = _vm->_dungeonMan->_creatureInfos[creatreType].getPoisonResistance();

	if (!poisonAttack || (poisonResistance == kDMImmuneToPoison))
		return 0;

	return ((poisonAttack + _vm->getRandomNumber(4)) << 3) / (poisonResistance + 1);
}

void GroupMan::processEvents29to41(int16 eventMapX, int16 eventMapY, int16 eventType, uint16 ticks) {
	int16 L0446_i_Multiple = 0;
#define AL0446_i_Direction           L0446_i_Multiple
#define AL0446_i_Ticks               L0446_i_Multiple
#define AL0446_i_Behavior2Or3        L0446_i_Multiple
#define AL0446_i_CreatureAspectIndex L0446_i_Multiple
#define AL0446_i_Range               L0446_i_Multiple
#define AL0446_i_CreatureAttributes  L0446_i_Multiple
#define AL0446_i_Cell                L0446_i_Multiple
#define AL0446_i_GroupCellsCriteria  L0446_i_Multiple
	int16 L0447_i_Multiple;
#define AL0447_i_Behavior           L0447_i_Multiple
#define AL0447_i_CreatureIndex      L0447_i_Multiple
#define AL0447_i_ReferenceDirection L0447_i_Multiple
#define AL0447_i_Ticks              L0447_i_Multiple
	int16 L0450_i_Multiple;
#define AL0450_i_DestinationMapX  L0450_i_Multiple
#define AL0450_i_DistanceXToParty L0450_i_Multiple
#define AL0450_i_TargetMapX       L0450_i_Multiple
	int16 L0451_i_Multiple;
#define AL0451_i_DestinationMapY  L0451_i_Multiple
#define AL0451_i_DistanceYToParty L0451_i_Multiple
#define AL0451_i_TargetMapY       L0451_i_Multiple

	/* If the party is not on the map specified in the event and the event type is not one of 32, 33, 37, 38 then the event is ignored */
	if ((_vm->_dungeonMan->_currMapIndex != _vm->_dungeonMan->_partyMapIndex)
	 && (eventType != k37_TMEventTypeUpdateBehaviourGroup) && (eventType != k32_TMEventTypeUpdateAspectGroup)
	 && (eventType != k38_TMEventTypeUpdateBehaviour_0) && (eventType != k33_TMEventTypeUpdateAspectCreature_0))
		return;

	Thing groupThing = groupGetThing(eventMapX, eventMapY);
	/* If there is no creature at the location specified in the event then the event is ignored */
	if (groupThing == Thing::_endOfList)
		return;

	Group *curGroup = (Group *)_vm->_dungeonMan->getThingData(groupThing);
	CreatureInfo creatureInfo = _vm->_dungeonMan->_creatureInfos[curGroup->_type];
	/* Update the event */
	TimelineEvent nextEvent;
	_vm->setMapAndTime(nextEvent._mapTime, _vm->_dungeonMan->_currMapIndex, _vm->_gameTime);
	nextEvent._priority = 255 - creatureInfo._movementTicks; /* The fastest creatures (with small MovementTicks value) get higher event priority */
	nextEvent._Bu._location._mapX = eventMapX;
	nextEvent._Bu._location._mapY = eventMapY;
	/* If the creature is not on the party map then try and move the creature in a random direction and place a new event 37 in the timeline for the next creature movement */
	if (_vm->_dungeonMan->_currMapIndex != _vm->_dungeonMan->_partyMapIndex) {
		if (isMovementPossible(&creatureInfo, eventMapX, eventMapY, AL0446_i_Direction = _vm->getRandomNumber(4), false)) { /* BUG0_67 A group that is not on the party map may wrongly move or not move into a teleporter. Normally, a creature type with Wariness >= 10 (Vexirk, Materializer / Zytaz, Demon, Lord Chaos, Red Dragon / Dragon) would only move into a teleporter if the creature type is allowed on the destination map. However, the variable G0380_T_CurrentGroupThing identifying the group is not set before being used by F0139_DUNGEON_IsCreatureAllowedOnMap called by f202_isMovementPossible so the check to see if the creature type is allowed may operate on another creature type and thus return an incorrect result, causing the creature to teleport while it should not, or not to teleport while it should */
			AL0450_i_DestinationMapX = eventMapX;
			AL0451_i_DestinationMapY = eventMapY;
			AL0450_i_DestinationMapX += _vm->_dirIntoStepCountEast[AL0446_i_Direction];
			AL0451_i_DestinationMapY += _vm->_dirIntoStepCountNorth[AL0446_i_Direction];
			if (_vm->_moveSens->getMoveResult(groupThing, eventMapX, eventMapY, AL0450_i_DestinationMapX, AL0451_i_DestinationMapY))
				return;
			nextEvent._Bu._location._mapX = _vm->_moveSens->_moveResultMapX;
			nextEvent._Bu._location._mapY = _vm->_moveSens->_moveResultMapY;
		}
		nextEvent._type = k37_TMEventTypeUpdateBehaviourGroup;
		AL0446_i_Ticks = MAX(ABS(_vm->_dungeonMan->_currMapIndex - _vm->_dungeonMan->_partyMapIndex) << 4, creatureInfo._movementTicks << 1);
		/* BUG0_68 A group moves or acts with a wrong timing. Event is added below but L0465_s_NextEvent.C.Ticks has not been initialized. No consequence while the group is not on the party map. When the party enters the group map the first group event may have a wrong timing */
T0209005_AddEventAndReturn:
		nextEvent._mapTime += AL0446_i_Ticks;
		_vm->_timeline->addEventGetEventIndex(&nextEvent);
		return;
	}
	/* If the creature is Lord Chaos then ignore the event if the game is won. Initialize data to analyze Fluxcages */
	bool isArchEnemy = getFlag(creatureInfo._attributes, k0x2000_MaskCreatureInfo_archenemy);
	if (isArchEnemy) {
		if (_vm->_gameWon)
			return;

		_fluxCageCount = 0;
		_fluxCages[0] = 0;
	}
	ActiveGroup *activeGroup = &_activeGroups[curGroup->getActiveGroupIndex()];

	// CHECKME: Terrible mix of types
	int16 ticksSinceLastMove = (unsigned char)_vm->_gameTime - activeGroup->_lastMoveTime;
	if (ticksSinceLastMove < 0)
		ticksSinceLastMove += 256;

	int16 movementTicks = creatureInfo._movementTicks;
	if (movementTicks == k255_immobile)
		movementTicks = 100;

	if (_vm->_championMan->_party._freezeLifeTicks && !isArchEnemy) { /* If life is frozen and the creature is not Lord Chaos (Lord Chaos is immune to Freeze Life) then reschedule the event later (except for reactions which are ignored when life if frozen) */
		if (eventType < 0)
			return;
		nextEvent._type = eventType;
		nextEvent._Cu._ticks = ticks;
		AL0446_i_Ticks = 4; /* Retry in 4 ticks */
		goto T0209005_AddEventAndReturn;
	}
	/* If the specified event type is a 'reaction' instead of a real event from the timeline then create the corresponding reaction event with a delay:
	For event kM1_TMEventTypeCreateReactionEvent31ParyIsAdjacent, the reaction time is 1 tick
	For event kM2_TMEventTypeCreateReactionEvent30HitByProjectile and kM3_TMEventTypeCreateReactionEvent29DangerOnSquare, the reaction time may be 1 tick or slower: slow moving creatures react more slowly. The more recent is the last creature move, the slower the reaction */
	if (eventType < 0) {
		nextEvent._type = eventType + k32_TMEventTypeUpdateAspectGroup;
		if (eventType == kM1_TMEventTypeCreateReactionEvent31ParyIsAdjacent) {
			AL0446_i_Ticks = 1; /* Retry in 1 tick */
		} else {
			 AL0446_i_Ticks = ((movementTicks + 2) >> 2) - ticksSinceLastMove;
			 if (AL0446_i_Ticks < 1) /* AL0446_i_Ticks is the reaction time */
				 AL0446_i_Ticks = 1; /* Retry in 1 tick */
		}
		goto T0209005_AddEventAndReturn; /* BUG0_68 A group moves or acts with a wrong timing. Event is added but L0465_s_NextEvent.C.Ticks has not been initialized */
	}
	AL0447_i_Behavior = curGroup->getBehaviour();
	uint16 creatureCount = curGroup->getCount();
	int16 creatureSize = getFlag(creatureInfo._attributes, k0x0003_MaskCreatureInfo_size);
	AL0450_i_DistanceXToParty = ABS(eventMapX - _vm->_dungeonMan->_partyMapX);
	AL0451_i_DistanceYToParty = ABS(eventMapY - _vm->_dungeonMan->_partyMapY);
	_currentGroupMapX = eventMapX;
	_currentGroupMapY = eventMapY;
	_currGroupThing = groupThing;
	_groupMovementTestedDirections[0] = 0;
	_currGroupDistanceToParty = getDistanceBetweenSquares(eventMapX, eventMapY, _vm->_dungeonMan->_partyMapX, _vm->_dungeonMan->_partyMapY);
	_currGroupPrimaryDirToParty = getDirsWhereDestIsVisibleFromSource(eventMapX, eventMapY, _vm->_dungeonMan->_partyMapX, _vm->_dungeonMan->_partyMapY);
	_currGroupSecondaryDirToParty = _vm->_projexpl->_secondaryDirToOrFromParty;
	int32 nextAspectUpdateTime = 0;
	bool notUpdateBehaviorFl = true;
	bool newGroupDirectionFound;
	bool approachAfterReaction = false;
	bool moveToPriorLocation = false;
	int16 distanceToVisibleParty = 0;

	if (eventType <= k31_TMEventTypeGroupReactionPartyIsAdjecent) { /* Process Reaction events 29 to 31 */
		switch (eventType = eventType - k32_TMEventTypeUpdateAspectGroup) {
		case kM1_TMEventTypeCreateReactionEvent31ParyIsAdjacent: /* This event is used when the party bumps into a group or attacks a group physically (not with a spell). It causes the creature behavior to change to attack if it is not already attacking the party or fleeing from target */
			if ((AL0447_i_Behavior != k6_behavior_ATTACK) && (AL0447_i_Behavior != k5_behavior_FLEE)) {
				groupDeleteEvents(eventMapX, eventMapY);
				goto T0209044_SetBehavior6_Attack;
			}
			activeGroup->_targetMapX = _vm->_dungeonMan->_partyMapX;
			activeGroup->_targetMapY = _vm->_dungeonMan->_partyMapY;
			return;
		case kM2_TMEventTypeCreateReactionEvent30HitByProjectile: /* This event is used for the reaction of a group after a projectile impacted with one creature in the group (some creatures may have been killed) */
			if ((AL0447_i_Behavior == k6_behavior_ATTACK) || (AL0447_i_Behavior == k5_behavior_FLEE)) /* If the creature is attacking the party or fleeing from the target then there is no reaction */
				return;
			AL0446_i_Behavior2Or3 = ((AL0447_i_Behavior == k3_behavior_USELESS) || (AL0447_i_Behavior == k2_behavior_USELESS));
			if (AL0446_i_Behavior2Or3 || (_vm->getRandomNumber(4))) { /* BUG0_00 Useless code. Behavior cannot be 2 nor 3 because these values are never used. The actual condition is thus: if 3/4 chances */
				if (!groupGetDistanceToVisibleParty(curGroup, kM1_wholeCreatureGroup, eventMapX, eventMapY)) { /* If the group cannot see the party then look in a random direction to try and search for the party */
					approachAfterReaction = newGroupDirectionFound = false;
					goto T0209073_SetDirectionGroup;
				}
				if (AL0446_i_Behavior2Or3 || (_vm->getRandomNumber(4))) /* BUG0_00 Useless code. Behavior cannot be 2 nor 3 because these values are never used. The actual condition is thus: if 3/4 chances then no reaction */
					return;
			} /* No 'break': proceed to instruction after the next 'case' below. Reaction is to move in a random direction to try and avoid other projectiles */
		case kM3_TMEventTypeCreateReactionEvent29DangerOnSquare: /* This event is used when some creatures in the group were killed by a Poison Cloud or by a closing door or if Lord Chaos is surrounded by 3 Fluxcages. It causes the creature to move in a random direction to avoid the danger */
			approachAfterReaction = (AL0447_i_Behavior == k6_behavior_ATTACK); /* If the creature behavior is 'Attack' and it has to move to avoid danger then it will change its behavior to 'Approach' after the movement */
			newGroupDirectionFound = false;
			goto T0209058_MoveInRandomDirection;
		}
	}
	if (eventType < k37_TMEventTypeUpdateBehaviourGroup) { /* Process Update Aspect events 32 to 36 */
		nextEvent._type = eventType + 5;
		if (groupGetDistanceToVisibleParty(curGroup, kM1_wholeCreatureGroup, eventMapX, eventMapY)) {
			if ((AL0447_i_Behavior != k6_behavior_ATTACK) && (AL0447_i_Behavior != k5_behavior_FLEE)) {
				if (_vm->getDistance(_vm->_dungeonMan->_partyMapX, _vm->_dungeonMan->_partyMapY, eventMapX, eventMapY) <= 1)
					goto T0209044_SetBehavior6_Attack;
				if (((AL0447_i_Behavior == k0_behavior_WANDER) || (AL0447_i_Behavior == k3_behavior_USELESS)) && (AL0447_i_Behavior != k7_behavior_APPROACH)) /* BUG0_00 Useless code. Behavior cannot be 3 because this value is never used. Moreover, the second condition in the && is redundant (if the value is 0 or 3, it cannot be 7). The actual condition is: if (AL0447_i_Behavior == k0_behavior_WANDER) */
					goto T0209054_SetBehavior7_Approach;
			}
			activeGroup->_targetMapX = _vm->_dungeonMan->_partyMapX;
			activeGroup->_targetMapY = _vm->_dungeonMan->_partyMapY;
		}
		if (AL0447_i_Behavior == k6_behavior_ATTACK) {
			AL0446_i_CreatureAspectIndex = eventType - k33_TMEventTypeUpdateAspectCreature_0; /* Value -1 for event 32, meaning aspect will be updated for all creatures in the group */
			nextAspectUpdateTime = getCreatureAspectUpdateTime(activeGroup, AL0446_i_CreatureAspectIndex, getFlag(activeGroup->_aspect[AL0446_i_CreatureAspectIndex], k0x0080_MaskActiveGroupIsAttacking));
			goto T0209136;
		}
		if ((AL0450_i_DistanceXToParty > 3) || (AL0451_i_DistanceYToParty > 3)) {
			nextAspectUpdateTime = _vm->_gameTime + ((creatureInfo._animationTicks >> 4) & 0xF);
			goto T0209136;
		}
	} else { /* Process Update Behavior events 37 to 41 */
		int16 primaryDirectionToOrFromParty;
		notUpdateBehaviorFl = false;
		if (ticks)
			nextAspectUpdateTime = _vm->_gameTime;

		if (eventType == k37_TMEventTypeUpdateBehaviourGroup) { /* Process event 37, Update Group Behavior */
			bool allowMovementOverFakePitsAndFakeWalls;
			if ((AL0447_i_Behavior == k0_behavior_WANDER) || (AL0447_i_Behavior == k2_behavior_USELESS) || (AL0447_i_Behavior == k3_behavior_USELESS)) { /* BUG0_00 Useless code. Behavior cannot be 2 nor 3 because these values are never used. The actual condition is: if (AL0447_i_Behavior == k0_behavior_WANDER) */
				distanceToVisibleParty = groupGetDistanceToVisibleParty(curGroup, kM1_wholeCreatureGroup, eventMapX, eventMapY);
				if (distanceToVisibleParty) {
					if ((distanceToVisibleParty <= (creatureInfo.getAttackRange())) && ((!AL0450_i_DistanceXToParty) || (!AL0451_i_DistanceYToParty))) { /* If the creature is in range for attack and on the same row or column as the party on the map */
T0209044_SetBehavior6_Attack:
						if (eventType == kM2_TMEventTypeCreateReactionEvent30HitByProjectile) {
							groupDeleteEvents(eventMapX, eventMapY);
						}
						activeGroup->_targetMapX = _vm->_dungeonMan->_partyMapX;
						activeGroup->_targetMapY = _vm->_dungeonMan->_partyMapY;
						curGroup->setBehaviour(k6_behavior_ATTACK);
						AL0446_i_Direction = _currGroupPrimaryDirToParty;
						for (AL0447_i_CreatureIndex = creatureCount; AL0447_i_CreatureIndex >= 0; AL0447_i_CreatureIndex--) {
							if ((getCreatureValue(activeGroup->_directions, AL0447_i_CreatureIndex) != AL0446_i_Direction) &&
								((!AL0447_i_CreatureIndex) || (!_vm->getRandomNumber(2)))) {
								setGroupDirection(activeGroup, AL0446_i_Direction, AL0447_i_CreatureIndex, creatureCount && (creatureSize == k1_MaskCreatureSizeHalf));
								M32_setTime(nextEvent._mapTime, _vm->_gameTime + _vm->getRandomNumber(4) + 2); /* Random delay represents the time for the creature to turn */
							} else {
								M32_setTime(nextEvent._mapTime, _vm->_gameTime + 1);
							}
							if (notUpdateBehaviorFl) {
								nextEvent._mapTime += MIN((uint16)((creatureInfo._attackTicks >> 1) + _vm->getRandomNumber(4)), ticks);
							}
							nextEvent._type = k38_TMEventTypeUpdateBehaviour_0 + AL0447_i_CreatureIndex;
							addGroupEvent(&nextEvent, getCreatureAspectUpdateTime(activeGroup, AL0447_i_CreatureIndex, false));
						}
						return;
					}
					if (AL0447_i_Behavior != k2_behavior_USELESS) { /* BUG0_00 Useless code. Behavior cannot be 2 because this value is never used */
T0209054_SetBehavior7_Approach:
						curGroup->setBehaviour(k7_behavior_APPROACH);
						activeGroup->_targetMapX = _vm->_dungeonMan->_partyMapX;
						activeGroup->_targetMapY = _vm->_dungeonMan->_partyMapY;
						nextEvent._mapTime += 1;
						goto T0209134_SetEvent37;
					}
				} else {
					if (AL0447_i_Behavior == k0_behavior_WANDER) {
						primaryDirectionToOrFromParty = getSmelledPartyPrimaryDirOrdinal(&creatureInfo, eventMapX, eventMapY);
						if (primaryDirectionToOrFromParty) {
							primaryDirectionToOrFromParty--;
							allowMovementOverFakePitsAndFakeWalls = false;
							goto T0209085_SingleSquareMove;
						}
						newGroupDirectionFound = false;
						if (_vm->getRandomNumber(2)) {
T0209058_MoveInRandomDirection:
							AL0446_i_Direction = _vm->getRandomNumber(4);
							AL0447_i_ReferenceDirection = AL0446_i_Direction;
							do {
								AL0450_i_DestinationMapX = eventMapX;
								AL0451_i_DestinationMapY = eventMapY;
								AL0450_i_DestinationMapX += _vm->_dirIntoStepCountEast[AL0446_i_Direction], AL0451_i_DestinationMapY += _vm->_dirIntoStepCountNorth[AL0446_i_Direction];
								if (((activeGroup->_priorMapX != AL0450_i_DestinationMapX) ||
									(activeGroup->_priorMapY != AL0451_i_DestinationMapY) ||
									 (moveToPriorLocation = !_vm->getRandomNumber(4))) /* 1/4 chance of moving back to the square that the creature comes from */
									&& isMovementPossible(&creatureInfo, eventMapX, eventMapY, AL0446_i_Direction, false)) {
T0209061_MoveGroup:
									AL0447_i_Ticks = (movementTicks >> 1) - ticksSinceLastMove;
									newGroupDirectionFound = (AL0447_i_Ticks <= 0);
									if (newGroupDirectionFound) {
										if (_vm->_moveSens->getMoveResult(groupThing, eventMapX, eventMapY, AL0450_i_DestinationMapX, AL0451_i_DestinationMapY))
											return;
										nextEvent._Bu._location._mapX = _vm->_moveSens->_moveResultMapX;
										nextEvent._Bu._location._mapY = _vm->_moveSens->_moveResultMapY;;
										activeGroup->_priorMapX = eventMapX;
										activeGroup->_priorMapY = eventMapY;
										activeGroup->_lastMoveTime = _vm->_gameTime;
									} else {
										movementTicks = AL0447_i_Ticks;
										ticksSinceLastMove = -1;
									}
									break;
								}
								if (_groupMovementBlockedByParty) {
									if ((eventType != kM3_TMEventTypeCreateReactionEvent29DangerOnSquare) &&
										((curGroup->getBehaviour() != k5_behavior_FLEE) ||
										 !getFirstPossibleMovementDirOrdinal(&creatureInfo, eventMapX, eventMapY, false) ||
										 _vm->getRandomNumber(2)))
										goto T0209044_SetBehavior6_Attack;
									activeGroup->_targetMapX = _vm->_dungeonMan->_partyMapX;
									activeGroup->_targetMapY = _vm->_dungeonMan->_partyMapY;
								}
								AL0446_i_Direction = _vm->turnDirRight(AL0446_i_Direction);
							} while (AL0446_i_Direction != AL0447_i_ReferenceDirection);
						}
						if (!newGroupDirectionFound &&
							(ticksSinceLastMove != -1) &&
							isArchEnemy &&
							((eventType == kM3_TMEventTypeCreateReactionEvent29DangerOnSquare) || !_vm->getRandomNumber(4))) { /* BUG0_15 The game hangs when you close a door on Lord Chaos. A condition is missing in the code to manage creatures and this may create an infinite loop between two parts in the code */
							_vm->_projexpl->_secondaryDirToOrFromParty = _vm->turnDirRight(primaryDirectionToOrFromParty = _vm->getRandomNumber(4));
							goto T0209089_DoubleSquareMove; /* BUG0_69 Memory corruption when you close a door on Lord Chaos. The local variable (L0454_i_PrimaryDirectionToOrFromParty) containing the direction where Lord Chaos tries to move may be used as an array index without being initialized and cause memory corruption */
						}
						if (newGroupDirectionFound || ((!_vm->getRandomNumber(4) || (distanceToVisibleParty <= creatureInfo.getSmellRange())) && (eventType != kM3_TMEventTypeCreateReactionEvent29DangerOnSquare))) {
T0209073_SetDirectionGroup:
							if (!newGroupDirectionFound && (ticksSinceLastMove >= 0)) { /* If direction is not found yet then look around in a random direction */
								AL0446_i_Direction = _vm->getRandomNumber(4);
							}
							setDirGroup(activeGroup, AL0446_i_Direction, creatureCount, creatureSize);
						}
						/* If event is kM3_TMEventTypeCreateReactionEvent29DangerOnSquare or kM2_TMEventTypeCreateReactionEvent30HitByProjectile */
						if (eventType < kM1_TMEventTypeCreateReactionEvent31ParyIsAdjacent) {
							if (!newGroupDirectionFound)
								return;
							if (approachAfterReaction)
								curGroup->setBehaviour(k7_behavior_APPROACH);

							stopAttacking(activeGroup, eventMapX, eventMapY);
						}
					}
				}
			} else {
				if (AL0447_i_Behavior == k7_behavior_APPROACH) {
					distanceToVisibleParty = groupGetDistanceToVisibleParty(curGroup, kM1_wholeCreatureGroup, eventMapX, eventMapY);
					if (distanceToVisibleParty) {
						if ((distanceToVisibleParty <= creatureInfo.getAttackRange()) && ((!AL0450_i_DistanceXToParty) || (!AL0451_i_DistanceYToParty))) /* If the creature is in range for attack and on the same row or column as the party on the map */
							goto T0209044_SetBehavior6_Attack;
T0209081_RunTowardParty:
						movementTicks++;
						movementTicks = movementTicks >> 1; /* Running speed is half the movement ticks */
						AL0450_i_TargetMapX = (activeGroup->_targetMapX = _vm->_dungeonMan->_partyMapX);
						AL0451_i_TargetMapY = (activeGroup->_targetMapY = _vm->_dungeonMan->_partyMapY);
					} else {
T0209082_WalkTowardTarget:
						AL0450_i_TargetMapX = activeGroup->_targetMapX;
						AL0451_i_TargetMapY = activeGroup->_targetMapY;
						/* If the creature reached its target but the party is not there anymore */
						if ((eventMapX == AL0450_i_TargetMapX) && (eventMapY == AL0451_i_TargetMapY)) {
							newGroupDirectionFound = false;
							curGroup->setBehaviour(k0_behavior_WANDER);
							goto T0209073_SetDirectionGroup;
						}
					}
					allowMovementOverFakePitsAndFakeWalls = true;
T0209084_SingleSquareMoveTowardParty:
					primaryDirectionToOrFromParty = getDirsWhereDestIsVisibleFromSource(eventMapX, eventMapY, AL0450_i_TargetMapX, AL0451_i_TargetMapY);
T0209085_SingleSquareMove:
					if (isMovementPossible(&creatureInfo, eventMapX, eventMapY, AL0446_i_Direction = primaryDirectionToOrFromParty, allowMovementOverFakePitsAndFakeWalls) ||
						isMovementPossible(&creatureInfo, eventMapX, eventMapY, AL0446_i_Direction = _vm->_projexpl->_secondaryDirToOrFromParty, allowMovementOverFakePitsAndFakeWalls && _vm->getRandomNumber(2)) ||
						isMovementPossible(&creatureInfo, eventMapX, eventMapY, AL0446_i_Direction = _vm->returnOppositeDir((Direction)AL0446_i_Direction), false) ||
						(!_vm->getRandomNumber(4) && isMovementPossible(&creatureInfo, eventMapX, eventMapY, AL0446_i_Direction = _vm->returnOppositeDir((Direction)primaryDirectionToOrFromParty), false))) {
						AL0450_i_DestinationMapX = eventMapX;
						AL0451_i_DestinationMapY = eventMapY;
						AL0450_i_DestinationMapX += _vm->_dirIntoStepCountEast[AL0446_i_Direction], AL0451_i_DestinationMapY += _vm->_dirIntoStepCountNorth[AL0446_i_Direction];
						goto T0209061_MoveGroup;
					}
					if (isArchEnemy) {
T0209089_DoubleSquareMove:
						getFirstPossibleMovementDirOrdinal(&creatureInfo, eventMapX, eventMapY, false); /* BUG0_00 Useless code. Returned value is ignored. When Lord Chaos teleports two squares away the ability to move to the first square is ignored which means Lord Chaos can teleport through walls or any other obstacle */
						if (isArchenemyDoubleMovementPossible(&creatureInfo, eventMapX, eventMapY, AL0446_i_Direction = primaryDirectionToOrFromParty) ||
							isArchenemyDoubleMovementPossible(&creatureInfo, eventMapX, eventMapY, AL0446_i_Direction = _vm->_projexpl->_secondaryDirToOrFromParty) ||
							(_fluxCageCount && isArchenemyDoubleMovementPossible(&creatureInfo, eventMapX, eventMapY, AL0446_i_Direction = _vm->returnOppositeDir((Direction)AL0446_i_Direction))) ||
							((_fluxCageCount >= 2) && isArchenemyDoubleMovementPossible(&creatureInfo, eventMapX, eventMapY, AL0446_i_Direction = _vm->returnOppositeDir((Direction)primaryDirectionToOrFromParty)))) {
							AL0450_i_DestinationMapX = eventMapX;
							AL0451_i_DestinationMapY = eventMapY;
							AL0450_i_DestinationMapX += _vm->_dirIntoStepCountEast[AL0446_i_Direction] * 2, AL0451_i_DestinationMapY += _vm->_dirIntoStepCountNorth[AL0446_i_Direction] * 2;
							_vm->_sound->requestPlay(k17_soundBUZZ, AL0450_i_DestinationMapX, AL0451_i_DestinationMapY, kDMSoundModePlayIfPrioritized);
							goto T0209061_MoveGroup;
						}
					}
					setDirGroup(activeGroup, primaryDirectionToOrFromParty, creatureCount, creatureSize);
				} else {
					if (AL0447_i_Behavior == k5_behavior_FLEE) {
T0209094_FleeFromTarget:
						allowMovementOverFakePitsAndFakeWalls = true;
						/* If the creature can see the party then update target coordinates */
						distanceToVisibleParty = groupGetDistanceToVisibleParty(curGroup, kM1_wholeCreatureGroup, eventMapX, eventMapY);
						if (distanceToVisibleParty) {
							AL0450_i_TargetMapX = (activeGroup->_targetMapX = _vm->_dungeonMan->_partyMapX);
							AL0451_i_TargetMapY = (activeGroup->_targetMapY = _vm->_dungeonMan->_partyMapY);
						} else {
							if (!(--(activeGroup->_delayFleeingFromTarget))) { /* If the creature is not afraid anymore then stop fleeing from target */
T0209096_SetBehavior0_Wander:
								newGroupDirectionFound = false;
								curGroup->setBehaviour(k0_behavior_WANDER);
								goto T0209073_SetDirectionGroup;
							}
							if (_vm->getRandomNumber(2)) {
								/* If the creature cannot move and the party is adjacent then stop fleeing */
								if (!getFirstPossibleMovementDirOrdinal(&creatureInfo, eventMapX, eventMapY, false)) {
									if (_vm->getDistance(eventMapX, eventMapY, _vm->_dungeonMan->_partyMapX, _vm->_dungeonMan->_partyMapY) <= 1)
										goto T0209096_SetBehavior0_Wander;
								}
								/* Set creature target to the home square where the creature was located when the party entered the map */
								AL0450_i_TargetMapX = activeGroup->_homeMapX;
								AL0451_i_TargetMapY = activeGroup->_homeMapY;
								goto T0209084_SingleSquareMoveTowardParty;
							}
							AL0450_i_TargetMapX = activeGroup->_targetMapX;
							AL0451_i_TargetMapY = activeGroup->_targetMapY;
						}
						/* Try and flee from the party (opposite direction) */
						primaryDirectionToOrFromParty = _vm->returnOppositeDir((Direction)getDirsWhereDestIsVisibleFromSource(eventMapX, eventMapY, AL0450_i_TargetMapX, AL0451_i_TargetMapY));
						_vm->_projexpl->_secondaryDirToOrFromParty = _vm->returnOppositeDir((Direction)_vm->_projexpl->_secondaryDirToOrFromParty);
						movementTicks -= (movementTicks >> 2);
						goto T0209085_SingleSquareMove;
					}
				}
			}
		} else { /* Process events 38 to 41, Update Creature Behavior */
			if (AL0447_i_Behavior == k5_behavior_FLEE) {
				if (creatureCount) {
					stopAttacking(activeGroup, eventMapX, eventMapY);
				}
				goto T0209094_FleeFromTarget;
			}
			/* If the creature is attacking, then compute the next aspect update time and the next attack time */
			if (getFlag(activeGroup->_aspect[AL0447_i_CreatureIndex = eventType - k38_TMEventTypeUpdateBehaviour_0], k0x0080_MaskActiveGroupIsAttacking)) {
				nextAspectUpdateTime = getCreatureAspectUpdateTime(activeGroup, AL0447_i_CreatureIndex, false);
				nextEvent._mapTime += ((AL0447_i_Ticks = creatureInfo._attackTicks) + _vm->getRandomNumber(4) - 1);
				if (AL0447_i_Ticks > 15)
					nextEvent._mapTime += _vm->getRandomNumber(8) - 2;
			} else { /* If the creature is not attacking, then try attacking if possible */
				if (AL0447_i_CreatureIndex > creatureCount) /* Ignore event if it is for a creature that is not in the group */
					return;

				primaryDirectionToOrFromParty = _currGroupPrimaryDirToParty;
				distanceToVisibleParty = groupGetDistanceToVisibleParty(curGroup, AL0447_i_CreatureIndex, eventMapX, eventMapY);
				/* If the party is visible, update the target coordinates */
				if (distanceToVisibleParty) {
					activeGroup->_targetMapX = _vm->_dungeonMan->_partyMapX;
					activeGroup->_targetMapY = _vm->_dungeonMan->_partyMapY;
				}
				/* If there is a single creature in the group that is not full square sized and 1/4 chance */
				if (!creatureCount && (creatureSize != k2_MaskCreatureSizeFull) && !((AL0446_i_GroupCellsCriteria = _vm->getRandomNumber(65536)) & 0x00C0)) {
					if (activeGroup->_cells != k255_CreatureTypeSingleCenteredCreature) {
						/* If the creature is not already on the center of the square then change its cell */
						if (AL0446_i_GroupCellsCriteria & 0x0038) /* 7/8 chances of changing cell to the center of the square */
							activeGroup->_cells = k255_CreatureTypeSingleCenteredCreature;
						else /* 1/8 chance of changing cell to the next or previous cell on the square */
							AL0446_i_GroupCellsCriteria = _vm->normalizeModulo4(_vm->normalizeModulo4(activeGroup->_cells) + ((AL0446_i_GroupCellsCriteria & 0x0001) ? 1 : -1));
					}
					/* If 1/8 chance and the creature is not adjacent to the party and is a quarter square sized creature then process projectile impacts and update the creature cell if still alive. When the creature is not in front of the party, it has 7/8 chances of dodging a projectile by moving to another cell or staying in the center of the square */
					if (!(AL0446_i_GroupCellsCriteria & 0x0038) && (distanceToVisibleParty != 1) && (creatureSize == k0_MaskCreatureSizeQuarter)) {
						if (_vm->_projexpl->projectileGetImpactCount(kDMElementTypeCreature, eventMapX, eventMapY, activeGroup->_cells) && (_vm->_projexpl->_creatureDamageOutcome == k2_outcomeKilledAllCreaturesInGroup)) /* This call to F0218_PROJECTILE_GetImpactCount works fine because there is a single creature in the group so L0445_ps_ActiveGroup->Cells contains only one cell index */
							return;
						activeGroup->_cells = _vm->normalizeModulo4(AL0446_i_GroupCellsCriteria);
					}
				}
				/* If the creature can see the party and is looking in the party direction or can attack in all direction */
				if (distanceToVisibleParty &&
					(getFlag(creatureInfo._attributes, k0x0004_MaskCreatureInfo_sideAttack) ||
					 getCreatureValue(activeGroup->_directions, AL0447_i_CreatureIndex) == primaryDirectionToOrFromParty)) {
					/* If the creature is in range to attack the party and random test succeeds */
					if ((distanceToVisibleParty <= (AL0446_i_Range = creatureInfo.getAttackRange())) &&
						(!AL0450_i_DistanceXToParty || !AL0451_i_DistanceYToParty) &&
						(AL0446_i_Range <= (_vm->getRandomNumber(16) + 1))) {
						if ((AL0446_i_Range == 1) &&
							(!getFlag(AL0446_i_CreatureAttributes = creatureInfo._attributes, k0x0008_MaskCreatureInfo_preferBackRow) || !_vm->getRandomNumber(4) || !getFlag(AL0446_i_CreatureAttributes, k0x0010_MaskCreatureInfo_attackAnyChamp)) &&
							(creatureSize == k0_MaskCreatureSizeQuarter) &&
							(activeGroup->_cells != k255_CreatureTypeSingleCenteredCreature) &&
							((AL0446_i_Cell = getCreatureValue(activeGroup->_cells, AL0447_i_CreatureIndex)) != primaryDirectionToOrFromParty) &&
							(AL0446_i_Cell != _vm->turnDirRight(primaryDirectionToOrFromParty))) { /* If the creature cannot cast spells (range = 1) and is not on a cell where it can attack the party directly and is a quarter square sized creature not in the center of the square then the creature moves to another cell and attack does not occur immediately */
							if (!creatureCount && _vm->getRandomNumber(2)) {
								activeGroup->_cells = k255_CreatureTypeSingleCenteredCreature;
							} else {
								if ((primaryDirectionToOrFromParty & 0x0001) == (AL0446_i_Cell & 0x0001))
									AL0446_i_Cell--;
								else
									AL0446_i_Cell++;

								AL0446_i_Cell = _vm->normalizeModulo4(AL0446_i_Cell);
								if (!getCreatureOrdinalInCell(curGroup, AL0446_i_Cell) ||
									(_vm->getRandomNumber(2) && !getCreatureOrdinalInCell(curGroup, AL0446_i_Cell = _vm->returnOppositeDir((Direction)AL0446_i_Cell)))) { /* If the selected cell (or the opposite cell) is not already occupied by a creature */
									if (_vm->_projexpl->projectileGetImpactCount(kDMElementTypeCreature, eventMapX, eventMapY, activeGroup->_cells) && (_vm->_projexpl->_creatureDamageOutcome == k2_outcomeKilledAllCreaturesInGroup)) /* BUG0_70 A projectile impact on a creature may be ignored. The function F0218_PROJECTILE_GetImpactCount to detect projectile impacts when a quarter square sized creature moves inside a group (to another cell on the same square) may fail if there are several creatures in the group because the function expects a single cell index for its last parameter. The function should be called once for each cell where there is a creature */
										return;
									if (_vm->_projexpl->_creatureDamageOutcome != k1_outcomeKilledSomeCreaturesInGroup) {
										activeGroup->_cells = getGroupValueUpdatedWithCreatureValue(activeGroup->_cells, AL0447_i_CreatureIndex, AL0446_i_Cell);
									}
								}
							}
							nextEvent._mapTime += MAX(1, (creatureInfo._movementTicks >> 1) + _vm->getRandomNumber(2)); /* Time for the creature to change cell */
							nextEvent._type = eventType;
							goto T0209135;
						}
						nextAspectUpdateTime = getCreatureAspectUpdateTime(activeGroup, AL0447_i_CreatureIndex, isCreatureAttacking(curGroup, eventMapX, eventMapY, AL0447_i_CreatureIndex));
						nextEvent._mapTime += (creatureInfo._animationTicks & 0xF) + _vm->getRandomNumber(2);
					} else {
						curGroup->setBehaviour(k7_behavior_APPROACH);
						if (creatureCount) {
							stopAttacking(activeGroup, eventMapX, eventMapY);
						}
						goto T0209081_RunTowardParty;
					}
				} else {
					/* If the party is visible, update target coordinates */
					if (groupGetDistanceToVisibleParty(curGroup, kM1_wholeCreatureGroup, eventMapX, eventMapY)) {
						activeGroup->_targetMapX = _vm->_dungeonMan->_partyMapX;
						activeGroup->_targetMapY = _vm->_dungeonMan->_partyMapY;
						setGroupDirection(activeGroup, primaryDirectionToOrFromParty, AL0447_i_CreatureIndex, creatureCount && (creatureSize == k1_MaskCreatureSizeHalf));
						nextEvent._mapTime += 2;
						nextAspectUpdateTime = _vm->filterTime(nextEvent._mapTime);
					} else { /* If the party is not visible, move to the target (last known party location) */
						curGroup->setBehaviour(k7_behavior_APPROACH);
						if (creatureCount) {
							stopAttacking(activeGroup, eventMapX, eventMapY);
						}
						goto T0209082_WalkTowardTarget;
					}
				}
			}
			nextEvent._type = eventType;
			goto T0209136;
		}
		nextEvent._mapTime += MAX(1, _vm->getRandomNumber(4) + movementTicks - 1);
T0209134_SetEvent37:
		nextEvent._type = k37_TMEventTypeUpdateBehaviourGroup;
	}
T0209135:
	if (!nextAspectUpdateTime) {
		nextAspectUpdateTime = getCreatureAspectUpdateTime(activeGroup, kM1_wholeCreatureGroup, false);
	}
T0209136:
	if (notUpdateBehaviorFl) {
		nextEvent._mapTime += ticks;
	} else {
		nextAspectUpdateTime += ticks;
	}
	addGroupEvent(&nextEvent, nextAspectUpdateTime);
}

bool GroupMan::isMovementPossible(CreatureInfo *creatureInfo, int16 mapX, int16 mapY, uint16 dir, bool allowMovementOverImaginaryPitsAndFakeWalls) {
	_groupMovementTestedDirections[dir] = true;
	_groupMovementBlockedByGroupThing = Thing::_endOfList;
	_groupMovementBlockedByDoor = false;
	_groupMovementBlockedByParty = false;
	if (creatureInfo->_movementTicks == k255_immobile)
		return false;

	_vm->_dungeonMan->mapCoordsAfterRelMovement((Direction)dir, 1, 0, mapX, mapY);
	uint16 curSquare = _vm->_dungeonMan->_currMapData[mapX][mapY];
	int16 curSquareType = Square(curSquare).getType();
	_groupMovBlockedByWallStairsPitFakeWalFluxCageTeleporter =
		!(((mapX >= 0) && (mapX < _vm->_dungeonMan->_currMapWidth)) &&
		 ((mapY >= 0) && (mapY < _vm->_dungeonMan->_currMapHeight)) &&
		  (curSquareType != kDMElementTypeWall) &&
		  (curSquareType != kDMElementTypeStairs) &&
		 ((curSquareType != kDMElementTypePit) || (getFlag(curSquare, kDMSquareMaskPitImaginary) && allowMovementOverImaginaryPitsAndFakeWalls) || !getFlag(curSquare, kDMSquareMaskPitOpen) || getFlag(creatureInfo->_attributes, k0x0020_MaskCreatureInfo_levitation)) &&
		 ((curSquareType != kDMElementTypeFakeWall) || getFlag(curSquare, kDMSquareMaskFakeWallOpen) || (getFlag(curSquare, kDMSquareMaskFakeWallImaginary) && allowMovementOverImaginaryPitsAndFakeWalls)));

	if (_groupMovBlockedByWallStairsPitFakeWalFluxCageTeleporter)
		return false;

	if (getFlag(creatureInfo->_attributes, k0x2000_MaskCreatureInfo_archenemy)) {
		Thing curThing = _vm->_dungeonMan->getSquareFirstThing(mapX, mapY);
		while (curThing != Thing::_endOfList) {
			if ((curThing).getType() == kDMThingTypeExplosion) {
				Teleporter *curTeleporter = (Teleporter *)_vm->_dungeonMan->getThingData(curThing);
				if (((Explosion *)curTeleporter)->setType(kDMExplosionTypeFluxcage)) {
					_fluxCages[dir] = true;
					_fluxCageCount++;
					_groupMovBlockedByWallStairsPitFakeWalFluxCageTeleporter = true;
					return false;
				}
			}
			curThing = _vm->_dungeonMan->getNextThing(curThing);
		}
	}
	if ((curSquareType == kDMElementTypeTeleporter) && getFlag(curSquare, kDMSquareMaskTeleporterOpen) && (creatureInfo->getWariness() >= 10)) {
		Teleporter *curTeleporter = (Teleporter *)_vm->_dungeonMan->getSquareFirstThingData(mapX, mapY);
		if (getFlag(curTeleporter->getScope(), kDMTeleporterScopeCreatures) && !_vm->_dungeonMan->isCreatureAllowedOnMap(_currGroupThing, curTeleporter->getTargetMapIndex())) {
			_groupMovBlockedByWallStairsPitFakeWalFluxCageTeleporter = true;
			return false;
		}
	}

	_groupMovementBlockedByParty = (_vm->_dungeonMan->_currMapIndex == _vm->_dungeonMan->_partyMapIndex) && (mapX == _vm->_dungeonMan->_partyMapX) && (mapY == _vm->_dungeonMan->_partyMapY);
	if (_groupMovementBlockedByParty)
		return false;

	if (curSquareType == kDMElementTypeDoor) {
		Teleporter *curTeleporter = (Teleporter *)_vm->_dungeonMan->getSquareFirstThingData(mapX, mapY);
		if (((Square(curSquare).getDoorState()) > (((Door *)curTeleporter)->opensVertically() ? CreatureInfo::getHeight(creatureInfo->_attributes) : 1)) && ((Square(curSquare).getDoorState()) != k5_doorState_DESTROYED) && !getFlag(creatureInfo->_attributes, k0x0040_MaskCreatureInfo_nonMaterial)) {
			_groupMovementBlockedByDoor = true;
			return false;
		}
	}

	_groupMovementBlockedByGroupThing = groupGetThing(mapX, mapY);
	return (_groupMovementBlockedByGroupThing == Thing::_endOfList);
}

int16 GroupMan::getDistanceBetweenSquares(int16 srcMapX, int16 srcMapY, int16 destMapX, int16 destMapY) {
	return ABS(srcMapX - destMapX) + ABS(srcMapY - destMapY);
}

int16 GroupMan::groupGetDistanceToVisibleParty(Group *group, int16 creatureIndex, int16 mapX, int16 mapY) {
	uint16 groupDirections;
	CreatureInfo *groupCreatureInfo = &_vm->_dungeonMan->_creatureInfos[group->_type];
	if (_vm->_championMan->_party._event71Count_Invisibility && !getFlag(groupCreatureInfo->_attributes, k0x0800_MaskCreatureInfo_seeInvisible))
		return 0;

	bool alwaysSee = false;
	int16 checkDirectionsCount; /* Count of directions to test in L0425_ai_CreatureViewDirections */
	int16 creatureViewDirections[4]; /* List of directions to test */
	if (getFlag(groupCreatureInfo->_attributes, k0x0004_MaskCreatureInfo_sideAttack)) { /* If creature can see in all directions */
		alwaysSee = true;
		checkDirectionsCount = 1;
		creatureViewDirections[0] = kDMDirNorth;
	} else {
		groupDirections = _activeGroups[group->getActiveGroupIndex()]._directions;
		if (creatureIndex < 0) { /* Negative index means test if each creature in the group can see the party in their respective direction */
			checkDirectionsCount = 0;
			for (creatureIndex = group->getCount(); creatureIndex >= 0; creatureIndex--) {
				int16 creatureDirection = _vm->normalizeModulo4(groupDirections >> (creatureIndex << 1));
				int16 counter = checkDirectionsCount;
				bool skipSet = false;
				while (counter--) {
					if (creatureViewDirections[counter] == creatureDirection) { /* If the creature looks in the same direction as another one in the group */
						skipSet = true;
						break;
					}
				}
				if (!skipSet)
					creatureViewDirections[checkDirectionsCount++] = creatureDirection;
			}
		} else { /* Positive index means test only if the specified creature in the group can see the party in its direction */
			creatureViewDirections[0] = getCreatureValue(groupDirections, creatureIndex);
			checkDirectionsCount = 1;
		}
	}

	while (checkDirectionsCount--) {
		if (alwaysSee || isDestVisibleFromSource(creatureViewDirections[checkDirectionsCount], mapX, mapY, _vm->_dungeonMan->_partyMapX, _vm->_dungeonMan->_partyMapY)) {
			int16 sightRange = groupCreatureInfo->getSightRange();
			if (!getFlag(groupCreatureInfo->_attributes, k0x1000_MaskCreatureInfo_nightVision))
				sightRange -= _vm->_displayMan->_dungeonViewPaletteIndex >> 1;

			if (_currGroupDistanceToParty > MAX<int16>(1, sightRange))
				return 0;

			return getDistanceBetweenUnblockedSquares(mapX, mapY, _vm->_dungeonMan->_partyMapX, _vm->_dungeonMan->_partyMapY, &GroupMan::isViewPartyBlocked);
		}
	}
	return 0;
}

int16 GroupMan::getDistanceBetweenUnblockedSquares(int16 srcMapX, int16 srcMapY,
													int16 destMapX, int16 destMapY, bool (GroupMan::*isBlocked)(uint16, uint16)) {

	if (_vm->getDistance(srcMapX, srcMapY, destMapX, destMapY) <= 1)
		return 1;

	int16 distanceX = ABS(destMapX - srcMapX);
	int16 distanceY = ABS(destMapY - srcMapY);
	bool isDistanceXSmallerThanDistanceY = (distanceX < distanceY);
	bool isDistanceXEqualsDistanceY = (distanceX == distanceY);
	int16 pathMapX = destMapX;
	int16 pathMapY = destMapY;
	int16 axisStepX = ((pathMapX - srcMapX) > 0) ? -1 : 1;
	int16 axisStepY = ((pathMapY - srcMapY) > 0) ? -1 : 1;
	int16 largestAxisDistance;

	int16 valueA;
	int16 valueB;
	int16 valueC;

	if (isDistanceXSmallerThanDistanceY) {
		largestAxisDistance = pathMapY - srcMapY;
		valueC = (largestAxisDistance ? ((pathMapX - srcMapX) << 6) / largestAxisDistance : 128);
	} else {
		largestAxisDistance = pathMapX - srcMapX;
		valueC = (largestAxisDistance ? ((pathMapY - srcMapY) << 6) / largestAxisDistance : 128);
	}

	/* 128 when the creature is on the same row or column as the party */
	do {
		if (isDistanceXEqualsDistanceY) {
			if ((   (CALL_MEMBER_FN(*_vm->_groupMan, isBlocked))(pathMapX + axisStepX, pathMapY)
				 && (CALL_MEMBER_FN(*_vm->_groupMan, isBlocked))(pathMapX, pathMapY + axisStepY))
			 || (CALL_MEMBER_FN(*_vm->_groupMan, isBlocked))(pathMapX = pathMapX + axisStepX, pathMapY = pathMapY + axisStepY))
				return 0;
		} else {
			if (isDistanceXSmallerThanDistanceY) {
				valueA = ABS(((pathMapY - srcMapY) ? ((pathMapX + axisStepX - srcMapX) << 6) / largestAxisDistance : 128) - valueC);
				valueB = ABS(((pathMapY + axisStepY - srcMapY) ? ((pathMapX - srcMapX) << 6) / largestAxisDistance : 128) - valueC);
			} else {
				valueA = ABS(((pathMapX + axisStepX - srcMapX) ? ((pathMapY - srcMapY) << 6) / largestAxisDistance : 128) - valueC);
				valueB = ABS(((pathMapX - srcMapX) ? ((pathMapY + axisStepY - srcMapY) << 6) / largestAxisDistance : 128) - valueC);
			}

			if (valueA < valueB)
				pathMapX += axisStepX;
			else
				pathMapY += axisStepY;

			if ((CALL_MEMBER_FN(*_vm->_groupMan, isBlocked))(pathMapX, pathMapY)) {
				pathMapX += axisStepX;
				pathMapY -= axisStepY;
				if (((valueA != valueB) || (CALL_MEMBER_FN(*_vm->_groupMan, isBlocked))(pathMapX, pathMapY)))
					return 0;
			}
		}
	} while (_vm->getDistance(pathMapX, pathMapY, srcMapX, srcMapY) > 1);
	return getDistanceBetweenSquares(srcMapX, srcMapY, destMapX, destMapY);
}

bool GroupMan::isViewPartyBlocked(uint16 mapX, uint16 mapY) {
	uint16 curSquare = _vm->_dungeonMan->_currMapData[mapX][mapY];
	int16 curSquareType = Square(curSquare).getType();
	if (curSquareType == kDMElementTypeDoor) {
		Door *curDoor = (Door *)_vm->_dungeonMan->getSquareFirstThingData(mapX, mapY);
		int16 curDoorState = Square(curSquare).getDoorState();
		return ((curDoorState == k3_doorState_FOURTH) || (curDoorState == k4_doorState_CLOSED)) && !getFlag(_vm->_dungeonMan->_currMapDoorInfo[curDoor->getType()]._attributes, k0x0001_MaskDoorInfo_CraturesCanSeeThrough);
	}
	return (curSquareType == kDMElementTypeWall) || ((curSquareType == kDMElementTypeFakeWall) && !getFlag(curSquare, kDMSquareMaskFakeWallOpen));
}

int32 GroupMan::getCreatureAspectUpdateTime(ActiveGroup *activeGroup, int16 creatureIndex, bool isAttacking) {
	Group *group = &(((Group *)_vm->_dungeonMan->_thingData[kDMThingTypeGroup])[activeGroup->_groupThingIndex]);
	uint16 creatureType = group->_type;
	uint16 creatureGraphicInfo = _vm->_dungeonMan->_creatureInfos[creatureType]._graphicInfo;
	bool processGroup = (creatureIndex < 0);
	if (processGroup) /* If the creature index is negative then all creatures in the group are processed */
		creatureIndex = group->getCount();

	do {
		uint16 aspect = activeGroup->_aspect[creatureIndex];
		aspect &= k0x0080_MaskActiveGroupIsAttacking | k0x0040_MaskActiveGroupFlipBitmap;
		int16 offset = ((creatureGraphicInfo >> 12) & 0x3);
		if (offset) {
			offset = _vm->getRandomNumber(offset);
			if (_vm->getRandomNumber(2))
				offset = (-offset) & 0x0007;

			aspect |= offset;
		}

		offset = ((creatureGraphicInfo >> 14) & 0x3);
		if (offset) {
			offset = _vm->getRandomNumber(offset);
			if (_vm->getRandomNumber(2))
				offset = (-offset) & 0x0007;

			aspect |= (offset << 3);
		}
		if (isAttacking) {
			if (getFlag(creatureGraphicInfo, k0x0200_CreatureInfoGraphicMaskFlipAttack)) {
				if (getFlag(aspect, k0x0080_MaskActiveGroupIsAttacking) && (creatureType == k18_CreatureTypeAnimatedArmourDethKnight)) {
					if (_vm->getRandomNumber(2)) {
						toggleFlag(aspect, k0x0040_MaskActiveGroupFlipBitmap);
						_vm->_sound->requestPlay(k16_soundCOMBAT_ATTACK_SKELETON_ANIMATED_ARMOUR_DETH_KNIGHT, _currentGroupMapX, _currentGroupMapY, kDMSoundModePlayIfPrioritized);
					}
				} else if (!getFlag(aspect, k0x0080_MaskActiveGroupIsAttacking) || !getFlag(creatureGraphicInfo, k0x0400_CreatureInfoGraphicMaskFlipDuringAttack)) {
					if (_vm->getRandomNumber(2))
						setFlag(aspect, k0x0040_MaskActiveGroupFlipBitmap);
					else
						clearFlag(aspect, k0x0040_MaskActiveGroupFlipBitmap);
				}
			} else
				clearFlag(aspect, k0x0040_MaskActiveGroupFlipBitmap);

			setFlag(aspect, k0x0080_MaskActiveGroupIsAttacking);
		} else {
			if (getFlag(creatureGraphicInfo, k0x0004_CreatureInfoGraphicMaskFlipNonAttack)) {
				if (creatureType == k13_CreatureTypeCouatl) {
					if (_vm->getRandomNumber(2)) {
						toggleFlag(aspect, k0x0040_MaskActiveGroupFlipBitmap);
						uint16 soundIndex = _vm->_moveSens->getSound(k13_CreatureTypeCouatl);
						if (soundIndex <= k34_D13_soundCount)
							_vm->_sound->requestPlay(soundIndex, _currentGroupMapX, _currentGroupMapY, kDMSoundModePlayIfPrioritized);
					}
				} else if (_vm->getRandomNumber(2))
					setFlag(aspect, k0x0040_MaskActiveGroupFlipBitmap);
				else
					clearFlag(aspect, k0x0040_MaskActiveGroupFlipBitmap);
			} else
				clearFlag(aspect, k0x0040_MaskActiveGroupFlipBitmap);

			clearFlag(aspect, k0x0080_MaskActiveGroupIsAttacking);
		}
		activeGroup->_aspect[creatureIndex] = aspect;
	} while (processGroup && (creatureIndex--));
	uint16 animationTicks = _vm->_dungeonMan->_creatureInfos[group->_type]._animationTicks;
	return _vm->_gameTime + (isAttacking ? ((animationTicks >> 8) & 0xF) : ((animationTicks >> 4) & 0xF)) + _vm->getRandomNumber(2);
}

void GroupMan::setGroupDirection(ActiveGroup *activeGroup, int16 dir, int16 creatureIndex, bool twoHalfSquareSizedCreatures) {
	static ActiveGroup *G0396_ps_TwoHalfSquareSizedCreaturesGroupLastDirectionSetActiveGroup;

	if (twoHalfSquareSizedCreatures
	 && (_vm->_gameTime == twoHalfSquareSizedCreaturesGroupLastDirectionSetTime)
	 && (activeGroup == G0396_ps_TwoHalfSquareSizedCreaturesGroupLastDirectionSetActiveGroup))
		return;

	uint16 groupDirections = activeGroup->_directions;
	if (_vm->normalizeModulo4(getCreatureValue(groupDirections, creatureIndex) - dir) == 2) { /* If current and new direction are opposites then change direction only one step at a time */
		dir = _vm->turnDirRight((_vm->getRandomNumber(65536) & 0x0002) + dir);
		groupDirections = getGroupValueUpdatedWithCreatureValue(groupDirections, creatureIndex, dir);
	} else
		groupDirections = getGroupValueUpdatedWithCreatureValue(groupDirections, creatureIndex, dir);

	if (twoHalfSquareSizedCreatures) {
		groupDirections = getGroupValueUpdatedWithCreatureValue(groupDirections, creatureIndex ^ 1, dir); /* Set direction of the second half square sized creature */
		twoHalfSquareSizedCreaturesGroupLastDirectionSetTime = _vm->_gameTime;
		G0396_ps_TwoHalfSquareSizedCreaturesGroupLastDirectionSetActiveGroup = activeGroup;
	}

	activeGroup->_directions = (Direction)groupDirections;
}

void GroupMan::addGroupEvent(TimelineEvent *event, uint32 time) {
	warning("potentially dangerous cast to uint32 below");
	if (time < (uint32)_vm->filterTime(event->_mapTime)) {
		event->_type -= 5;
		event->_Cu._ticks = _vm->filterTime(event->_mapTime) - time;
		M32_setTime(event->_mapTime, time);
	} else
		event->_Cu._ticks = time - _vm->filterTime(event->_mapTime);

	_vm->_timeline->addEventGetEventIndex(event);
}

int16 GroupMan::getSmelledPartyPrimaryDirOrdinal(CreatureInfo *creatureInfo, int16 mapY, int16 mapX) {
	uint16 smellRange = creatureInfo->getSmellRange();
	if (!smellRange)
		return 0;

	if ((((smellRange + 1) >> 1) >= _currGroupDistanceToParty) && getDistanceBetweenUnblockedSquares(mapY, mapX, _vm->_dungeonMan->_partyMapX, _vm->_dungeonMan->_partyMapY, &GroupMan::isSmellPartyBlocked)) {
		_vm->_projexpl->_secondaryDirToOrFromParty = _currGroupSecondaryDirToParty;
		return _vm->indexToOrdinal(_currGroupPrimaryDirToParty);
	}

	int16 scentOrdinal = _vm->_championMan->getScentOrdinal(mapY, mapX);
	if (scentOrdinal && ((_vm->_championMan->_party._scentStrengths[_vm->ordinalToIndex(scentOrdinal)] + _vm->getRandomNumber(4)) > (30 - (smellRange << 1)))) { /* If there is a fresh enough party scent on the group square */
		return _vm->indexToOrdinal(getDirsWhereDestIsVisibleFromSource(mapY, mapX, _vm->_championMan->_party._scents[scentOrdinal].getMapX(), _vm->_championMan->_party._scents[scentOrdinal].getMapY()));
	}
	return 0;
}

bool GroupMan::isSmellPartyBlocked(uint16 mapX, uint16 mapY) {
	uint16 square = _vm->_dungeonMan->_currMapData[mapX][mapY];
	int16 squareType = Square(square).getType();

	return ( (squareType) == kDMElementTypeWall) || ((squareType == kDMElementTypeFakeWall)
		  && !getFlag(square, kDMSquareMaskFakeWallOpen));
}

int16 GroupMan::getFirstPossibleMovementDirOrdinal(CreatureInfo *info, int16 mapX, int16 mapY, bool allowMovementOverImaginaryPitsAndFakeWalls) {
	for (int16 direction = kDMDirNorth; direction <= kDMDirWest; direction++) {
		if ((!_groupMovementTestedDirections[direction]) && isMovementPossible(info, mapX, mapY, direction, allowMovementOverImaginaryPitsAndFakeWalls)) {
			return _vm->indexToOrdinal(direction);
		}
	}
	return 0;
}

void GroupMan::setDirGroup(ActiveGroup *activeGroup, int16 dir, int16 creatureIndex, int16 creatureSize) {
	bool twoHalfSquareSizedCreatures = creatureIndex && (creatureSize == k1_MaskCreatureSizeHalf);

	if (twoHalfSquareSizedCreatures)
		creatureIndex--;

	do {
		if (!creatureIndex || _vm->getRandomNumber(2))
			setGroupDirection(activeGroup, dir, creatureIndex, twoHalfSquareSizedCreatures);
	} while (creatureIndex--);
}

void GroupMan::stopAttacking(ActiveGroup *group, int16 mapX, int16 mapY) {
	for (int16 creatureIndex = 0; creatureIndex < 4; creatureIndex++)
		clearFlag(group->_aspect[creatureIndex++], k0x0080_MaskActiveGroupIsAttacking);

	groupDeleteEvents(mapX, mapY);
}

bool GroupMan::isArchenemyDoubleMovementPossible(CreatureInfo *info, int16 mapX, int16 mapY, uint16 dir) {
	if (_fluxCages[dir])
		return false;

	mapX += _vm->_dirIntoStepCountEast[dir], mapY += _vm->_dirIntoStepCountNorth[dir];
	return isMovementPossible(info, mapX, mapY, dir, false);
}

bool GroupMan::isCreatureAttacking(Group *group, int16 mapX, int16 mapY, uint16 creatureIndex) {
	static const uint8 creatureAttackSounds[11] = { 3, 7, 14, 15, 19, 21, 29, 30, 31, 4, 16 }; /* Atari ST: { 3, 7, 14, 15, 19, 21, 4, 16 } */

	_vm->_projexpl->_lastCreatureAttackTime = _vm->_gameTime;
	ActiveGroup activeGroup = _activeGroups[group->getActiveGroupIndex()];
	uint16 creatureType = group->_type;
	CreatureInfo *creatureInfo = &_vm->_dungeonMan->_creatureInfos[creatureType];
	uint16 primaryDirectionToParty = _currGroupPrimaryDirToParty;

	int16 targetCell;
	byte groupCells = activeGroup._cells;
	if (groupCells == k255_CreatureTypeSingleCenteredCreature)
		targetCell = _vm->getRandomNumber(2);
	else
		targetCell = ((getCreatureValue(groupCells, creatureIndex) + 5 - primaryDirectionToParty) & 0x0002) >> 1;

	targetCell += primaryDirectionToParty;
	targetCell &= 0x0003;
	if ((creatureInfo->getAttackRange() > 1) && ((_currGroupDistanceToParty > 1) || _vm->getRandomNumber(2))) {
		Thing projectileThing = Thing::_none;

		switch (creatureType) {
		case k14_CreatureTypeVexirk:
		case k23_CreatureTypeLordChaos:
			if (_vm->getRandomNumber(2)) {
				projectileThing = Thing::_explFireBall;
			} else {
				switch (_vm->getRandomNumber(4)) {
				case 0:
					projectileThing = Thing::_explHarmNonMaterial;
					break;
				case 1:
					projectileThing = Thing::_explLightningBolt;
					break;
				case 2:
					projectileThing = Thing::_explPoisonCloud;
					break;
				case 3:
					projectileThing = Thing::_explOpenDoor;
				}
			}
			break;
		case k1_CreatureTypeSwampSlimeSlime:
			projectileThing = Thing::_explSlime;
			break;
		case k3_CreatureTypeWizardEyeFlyingEye:
			if (_vm->getRandomNumber(8)) {
				projectileThing = Thing::_explLightningBolt;
			} else {
				projectileThing = Thing::_explOpenDoor;
			}
			break;
		case k19_CreatureTypeMaterializerZytaz:
			if (_vm->getRandomNumber(2)) {
				projectileThing = Thing::_explPoisonCloud;
				break;
			}
		case k22_CreatureTypeDemon:
		case k24_CreatureTypeRedDragon:
			projectileThing = Thing::_explFireBall;
		} /* BUG0_13 The game may crash when 'Lord Order' or 'Grey Lord' cast spells. This cannot happen with the original dungeons as they do not contain any groups of these types. 'Lord Order' and 'Grey Lord' creatures can cast spells (attack range > 1) but no projectile type is defined for them in the code. If these creatures are present in a dungeon they will cast projectiles containing undefined things because the variable is not initialized */
		int16 kineticEnergy = (creatureInfo->_attack >> 2) + 1;
		kineticEnergy += _vm->getRandomNumber(kineticEnergy);
		kineticEnergy += _vm->getRandomNumber(kineticEnergy);
		_vm->_sound->requestPlay(k13_soundSPELL, mapX, mapY, kDMSoundModePlayImmediately);
		_vm->_projexpl->createProjectile(projectileThing, mapX, mapY, targetCell, (Direction)_currGroupPrimaryDirToParty, CLIP<byte>(20, kineticEnergy, 255), creatureInfo->_dexterity, 8);
	} else {
		int16 championIndex;
		if (getFlag(creatureInfo->_attributes, k0x0010_MaskCreatureInfo_attackAnyChamp)) {
			championIndex = _vm->getRandomNumber(4);
			int cpt;
			for (cpt = 0; (cpt < 4) && !_vm->_championMan->_champions[championIndex]._currHealth; cpt++)
				championIndex = _vm->turnDirRight(championIndex);

			if (cpt == 4)
				return false;
		} else {
			championIndex = _vm->_championMan->getTargetChampionIndex(mapX, mapY, targetCell);
			if (championIndex < 0)
				return false;
		}

		if (creatureType == k2_CreatureTypeGiggler)
			stealFromChampion(group, championIndex);
		else {
			int16 damage = getChampionDamage(group, championIndex) + 1;
			Champion *damagedChampion = &_vm->_championMan->_champions[championIndex];
			if (damage > damagedChampion->_maximumDamageReceived) {
				damagedChampion->_maximumDamageReceived = damage;
				damagedChampion->_directionMaximumDamageReceived = _vm->returnOppositeDir((Direction)primaryDirectionToParty);
			}
		}
	}
	int16 attackSoundOrdinal = creatureInfo->_attackSoundOrdinal;
	if (attackSoundOrdinal)
		_vm->_sound->requestPlay(creatureAttackSounds[--attackSoundOrdinal], mapX, mapY, kDMSoundModePlayIfPrioritized);

	return true;
}

void GroupMan::setOrderedCellsToAttack(signed char *orderedCellsToAttack, int16 targetMapX, int16 targetMapY, int16 attackerMapX, int16 attackerMapY, uint16 cellSource) {
	static signed char attackOrder[8][4] = { // @ G0023_aac_Graphic562_OrderedCellsToAttack
		{0, 1, 3, 2},   /* Attack South from position Northwest or Southwest */
		{1, 0, 2, 3},   /* Attack South from position Northeast or Southeast */
		{1, 2, 0, 3},   /* Attack West from position Northwest or Northeast */
		{2, 1, 3, 0},   /* Attack West from position Southeast or Southwest */
		{3, 2, 0, 1},   /* Attack North from position Northwest or Southwest */
		{2, 3, 1, 0},   /* Attack North from position Southeast or Northeast */
		{0, 3, 1, 2},   /* Attack East from position Northwest or Northeast */
		{3, 0, 2, 1}    /* Attack East from position Southeast or Southwest */
	};

	uint16 orderedCellsToAttackIndex = getDirsWhereDestIsVisibleFromSource(targetMapX, targetMapY, attackerMapX, attackerMapY) << 1;
	if (!(orderedCellsToAttackIndex & 0x0002))
		cellSource++;

	orderedCellsToAttackIndex += (cellSource >> 1) & 0x0001;
	for (uint16 i = 0; i < 4; ++i)
		orderedCellsToAttack[i] = attackOrder[orderedCellsToAttackIndex][i];
}

void GroupMan::stealFromChampion(Group *group, uint16 championIndex) {
	static unsigned char G0394_auc_StealFromSlotIndices[8]; /* Initialized with 0 bytes by C loader */

	bool objectStolen = false;
	Champion *champion = &_vm->_championMan->_champions[championIndex];
	int16 percentage = 100 - _vm->_championMan->getDexterity(champion);
	uint16 slotIdx = _vm->getRandomNumber(8);
	while ((percentage > 0) && !_vm->_championMan->isLucky(champion, percentage)) {
		uint16 stealFromSlotIndex = G0394_auc_StealFromSlotIndices[slotIdx];
		if (stealFromSlotIndex == kDMSlotBackpackLine1_1)
			stealFromSlotIndex += _vm->getRandomNumber(17); /* Select a random slot in the backpack */

		Thing slotThing = champion->_slots[stealFromSlotIndex];
		if ((slotThing != Thing::_none)) {
			objectStolen = true;
			slotThing = _vm->_championMan->getObjectRemovedFromSlot(championIndex, stealFromSlotIndex);
			if (group->_slot == Thing::_endOfList) {
				group->_slot = slotThing;
				/* BUG0_12 An object is cloned and appears at two different locations in the dungeon and/or inventory. The game may crash when interacting with this object. If a Giggler with no possessions steals an object that was previously in a chest and was not the last object in the chest then the objects that followed it are cloned. In the chest, the object is part of a linked list of objects that is not reset when the object is removed from the chest and placed in the inventory (but not in the dungeon), nor when it is stolen and added as the first Giggler possession. If the Giggler already has a possession before stealing the object then this does not create a cloned object.
				The following statement is missing: L0394_T_Thing->Next = Thing::_endOfList;
				This creates cloned things if L0394_T_Thing->Next is not Thing::_endOfList which is the case when the object comes from a chest in which it was not the last object */
			} else {
				_vm->_dungeonMan->linkThingToList(slotThing, group->_slot, kDMMapXNotOnASquare, 0);
			}
			_vm->_championMan->drawChampionState((ChampionIndex)championIndex);
		}
		++slotIdx;
		slotIdx &= 0x0007;
		percentage -= 20;
	}
	if (!_vm->getRandomNumber(8) || (objectStolen && _vm->getRandomNumber(2))) {
		_activeGroups[group->getActiveGroupIndex()]._delayFleeingFromTarget = _vm->getRandomNumber(64) + 20;
		group->setBehaviour(k5_behavior_FLEE);
	}
}

int16 GroupMan::getChampionDamage(Group *group, uint16 champIndex) {
	unsigned char allowedWoundMasks[4] = {32, 16, 8, 4}; // @ G0024_auc_Graphic562_WoundProbabilityIndexToWoundMask

	Champion *curChampion = &_vm->_championMan->_champions[champIndex];
	if (champIndex >= _vm->_championMan->_partyChampionCount)
		return 0;

	if (!curChampion->_currHealth)
		return 0;

	if (_vm->_championMan->_partyIsSleeping)
		_vm->_championMan->wakeUp();

	int16 doubledMapDifficulty = _vm->_dungeonMan->_currMap->_difficulty << 1;
	CreatureInfo creatureInfo = _vm->_dungeonMan->_creatureInfos[group->_type];
	_vm->_championMan->addSkillExperience(champIndex, kDMSkillParry, creatureInfo.getExperience());
	if (_vm->_championMan->_partyIsSleeping || (((_vm->_championMan->getDexterity(curChampion) < (_vm->getRandomNumber(32) + creatureInfo._dexterity + doubledMapDifficulty - 16)) || !_vm->getRandomNumber(4)) && !_vm->_championMan->isLucky(curChampion, 60))) {
		uint16 allowedWound;
		uint16 woundTest = _vm->getRandomNumber(65536);
		if (woundTest & 0x0070) {
			woundTest &= 0x000F;
			uint16 woundProbabilities = creatureInfo._woundProbabilities;
			uint16 woundProbabilityIndex;
			for (woundProbabilityIndex = 0; woundTest > (woundProbabilities & 0x000F); woundProbabilityIndex++) {
				woundProbabilities >>= 4;
			}
			allowedWound = allowedWoundMasks[woundProbabilityIndex];
		} else
			allowedWound = woundTest & 0x0001; /* 0 (Ready hand) or 1 (action hand) */

		int16 attack = (_vm->getRandomNumber(16) + creatureInfo._attack + doubledMapDifficulty) - (_vm->_championMan->getSkillLevel(champIndex, kDMSkillParry) << 1);
		if (attack <= 1) {
			if (_vm->getRandomNumber(2))
				return 0;

			attack = _vm->getRandomNumber(4) + 2;
		}
		attack >>= 1;
		attack += _vm->getRandomNumber(attack) + _vm->getRandomNumber(4);
		attack += _vm->getRandomNumber(attack);
		attack >>= 2;
		attack += _vm->getRandomNumber(4) + 1;
		if (_vm->getRandomNumber(2))
			attack -= _vm->getRandomNumber((attack >> 1) + 1) - 1;

		int16 damage = _vm->_championMan->addPendingDamageAndWounds_getDamage(champIndex, attack, allowedWound, creatureInfo._attackType);
		if (damage) {
			_vm->_sound->requestPlay(k09_soundCHAMPION_0_DAMAGED + champIndex, _vm->_dungeonMan->_partyMapX, _vm->_dungeonMan->_partyMapY, kDMSoundModePlayOneTickLater);

			uint16 poisonAttack = creatureInfo._poisonAttack;
			if (poisonAttack && _vm->getRandomNumber(2)) {
				poisonAttack = _vm->_championMan->getStatisticAdjustedAttack(curChampion, kDMStatVitality, poisonAttack);
				if (poisonAttack >= 0)
					_vm->_championMan->championPoison(champIndex, poisonAttack);
			}
			return damage;
		}
	}

	return 0;
}

void GroupMan::dropMovingCreatureFixedPossession(Thing thing, int16 mapX, int16 mapY) {
	if (_dropMovingCreatureFixedPossCellCount) {
		Group *group = (Group *)_vm->_dungeonMan->getThingData(thing);
		int16 creatureType = group->_type;
		while (_dropMovingCreatureFixedPossCellCount) {
			dropCreatureFixedPossessions(creatureType, mapX, mapY, _dropMovingCreatureFixedPossessionsCell[--_dropMovingCreatureFixedPossCellCount], kDMSoundModePlayOneTickLater);
		}
	}
}

void GroupMan::startWandering(int16 mapX, int16 mapY) {
	Group *L0332_ps_Group = (Group *)_vm->_dungeonMan->getThingData(groupGetThing(mapX, mapY));
	if (L0332_ps_Group->getBehaviour() >= k4_behavior_USELESS) {
		L0332_ps_Group->setBehaviour(k0_behavior_WANDER);
	}
	TimelineEvent nextEvent;
	_vm->setMapAndTime(nextEvent._mapTime, _vm->_dungeonMan->_currMapIndex, (_vm->_gameTime + 1));
	nextEvent._type = k37_TMEventTypeUpdateBehaviourGroup;
	nextEvent._priority = 255 - _vm->_dungeonMan->_creatureInfos[L0332_ps_Group->_type]._movementTicks; /* The fastest creatures (with small MovementTicks value) get higher event priority */
	nextEvent._Cu._ticks = 0;
	nextEvent._Bu._location._mapX = mapX;
	nextEvent._Bu._location._mapY = mapY;
	_vm->_timeline->addEventGetEventIndex(&nextEvent);
}

void GroupMan::addActiveGroup(Thing thing, int16 mapX, int16 mapY) {
	ActiveGroup *activeGroup = _activeGroups;
	int16 activeGroupIndex = 0;
	while (activeGroup->_groupThingIndex >= 0) {
		if (++activeGroupIndex >= _maxActiveGroupCount)
			return;

		activeGroup++;
	}
	_currActiveGroupCount++;

	activeGroup->_groupThingIndex = (thing).getIndex();
	Group *curGroup = (Group *)(_vm->_dungeonMan->_thingData[kDMThingTypeGroup] +
		_vm->_dungeonMan->_thingDataWordCount[kDMThingTypeGroup] * activeGroup->_groupThingIndex);

	activeGroup->_cells = curGroup->_cells;
	curGroup->getActiveGroupIndex() = activeGroupIndex;
	activeGroup->_priorMapX = activeGroup->_homeMapX = mapX;
	activeGroup->_priorMapY = activeGroup->_homeMapY = mapY;
	activeGroup->_lastMoveTime = _vm->_gameTime - 127;
	uint16 creatureIndex = curGroup->getCount();
	do {
		activeGroup->_directions = (Direction)getGroupValueUpdatedWithCreatureValue(activeGroup->_directions, creatureIndex, curGroup->getDir());
		activeGroup->_aspect[creatureIndex] = 0;
	} while (creatureIndex--);
	getCreatureAspectUpdateTime(activeGroup, kM1_wholeCreatureGroup, false);
}

void GroupMan::removeActiveGroup(uint16 activeGroupIndex) {
	if ((activeGroupIndex > _maxActiveGroupCount) || (_activeGroups[activeGroupIndex]._groupThingIndex < 0))
		return;

	ActiveGroup *activeGroup = &_activeGroups[activeGroupIndex];
	Group *group = &((Group *)_vm->_dungeonMan->_thingData[kDMThingTypeGroup])[activeGroup->_groupThingIndex];
	_currActiveGroupCount--;
	group->_cells = activeGroup->_cells;
	group->setDir(_vm->normalizeModulo4(activeGroup->_directions));
	if (group->getBehaviour() >= k4_behavior_USELESS) {
		group->setBehaviour(k0_behavior_WANDER);
	}
	activeGroup->_groupThingIndex = -1;
}

void GroupMan::removeAllActiveGroups() {
	for (int16 idx = 0; _currActiveGroupCount > 0; idx++) {
		if (_activeGroups[idx]._groupThingIndex >= 0) {
			removeActiveGroup(idx);
		}
	}
}

void GroupMan::addAllActiveGroups() {
	byte *curSquare = _vm->_dungeonMan->_currMapData[0];
	Thing *squareCurThing = &_vm->_dungeonMan->_squareFirstThings[_vm->_dungeonMan->_currMapColCumulativeSquareFirstThingCount[0]];
	for (uint16 mapX = 0; mapX < _vm->_dungeonMan->_currMapWidth; mapX++) {
		for (uint16 mapY = 0; mapY < _vm->_dungeonMan->_currMapHeight; mapY++) {
			if (getFlag(*curSquare++, kDMSquareMaskThingListPresent)) {
				Thing curThing = *squareCurThing++;
				do {
					if (curThing.getType() == kDMThingTypeGroup) {
						groupDeleteEvents(mapX, mapY);
						addActiveGroup(curThing, mapX, mapY);
						startWandering(mapX, mapY);
						break;
					}
					curThing = _vm->_dungeonMan->getNextThing(curThing);
				} while (curThing != Thing::_endOfList);
			}
		}
	}
}

Thing GroupMan::groupGetGenerated(int16 creatureType, int16 healthMultiplier, uint16 creatureCount, Direction dir, int16 mapX, int16 mapY) {
	Thing groupThing = _vm->_dungeonMan->getUnusedThing(kDMThingTypeGroup);
	if (((_currActiveGroupCount >= (_maxActiveGroupCount - 5)) && (_vm->_dungeonMan->_currMapIndex == _vm->_dungeonMan->_partyMapIndex))
		|| (groupThing == Thing::_none)) {
		return Thing::_none;
	}
	Group *group = (Group *)_vm->_dungeonMan->getThingData(groupThing);
	group->_slot = Thing::_endOfList;
	group->setDoNotDiscard(false);
	group->setDir(dir);
	group->setCount(creatureCount);
	bool severalCreaturesInGroup = creatureCount;
	uint16 cell = 0;
	uint16 groupCells = 0;
	if (severalCreaturesInGroup)
		cell = _vm->getRandomNumber(4);
	else
		groupCells = k255_CreatureTypeSingleCenteredCreature;

	CreatureInfo *creatureInfo = &_vm->_dungeonMan->_creatureInfos[group->_type = creatureType];
	uint16 baseHealth = creatureInfo->_baseHealth;
	do {
		group->_health[creatureCount] = (baseHealth * healthMultiplier) + _vm->getRandomNumber((baseHealth >> 2) + 1);
		if (severalCreaturesInGroup) {
			groupCells = getGroupValueUpdatedWithCreatureValue(groupCells, creatureCount, cell++);
			if (getFlag(creatureInfo->_attributes, k0x0003_MaskCreatureInfo_size) == k1_MaskCreatureSizeHalf)
				cell++;

			cell &= 0x0003;
		}
	} while (creatureCount--);
	group->_cells = groupCells;
	if (_vm->_moveSens->getMoveResult(groupThing, kDMMapXNotOnASquare, 0, mapX, mapY)) {
		/* If F0267_MOVE_GetMoveResult_CPSCE returns true then the group was either killed by a projectile
		   impact (in which case the thing data was marked as unused) or the party is on the destination
		   square and an event is created to move the creature into the dungeon later
		   (in which case the thing is referenced in the event) */
		return Thing::_none;
	}
	_vm->_sound->requestPlay(k17_soundBUZZ, mapX, mapY, kDMSoundModePlayIfPrioritized);
	return groupThing;
}

bool GroupMan::isSquareACorridorTeleporterPitOrDoor(int16 mapX, int16 mapY) {
	int16 squareType = Square(_vm->_dungeonMan->getSquare(mapX, mapY)).getType();

	return ((squareType == kDMElementTypeCorridor) || (squareType == kDMElementTypeTeleporter)
		 || (squareType == kDMElementTypePit) || (squareType == kDMElementTypeDoor));
}

int16 GroupMan::getMeleeTargetCreatureOrdinal(int16 groupX, int16 groupY, int16 partyX, int16 partyY, uint16 champCell) {
	Thing groupThing = groupGetThing(groupX, groupY);
	if (groupThing == Thing::_endOfList)
		return 0;

	Group *group = (Group *)_vm->_dungeonMan->getThingData(groupThing);
	signed char orderedCellsToAttack[4];
	setOrderedCellsToAttack(orderedCellsToAttack, groupX, groupY, partyX, partyY, champCell);
	uint16 counter = 0;
	for (;;) { /*_Infinite loop_*/
		int16 creatureOrdinal = getCreatureOrdinalInCell(group, orderedCellsToAttack[counter]);
		if (creatureOrdinal)
			return creatureOrdinal;

		counter++;
	}
}

int16 GroupMan::getMeleeActionDamage(Champion *champ, int16 champIndex, Group *group, int16 creatureIndex, int16 mapX, int16 mapY, uint16 actionHitProbability, uint16 actionDamageFactor, int16 skillIndex) {
	int16 L0565_i_Damage = 0;
	int16 L0566_i_Damage = 0;
	int16 L0568_i_Defense;
	int16 L0569_i_Outcome;

	if (champIndex >= _vm->_championMan->_partyChampionCount)
		return 0;

	if (!champ->_currHealth)
		return 0;

	int16 doubledMapDifficulty = _vm->_dungeonMan->_currMap->_difficulty << 1;
	CreatureInfo *creatureInfo = &_vm->_dungeonMan->_creatureInfos[group->_type];
	int16 actionHandObjectIconIndex = _vm->_objectMan->getIconIndex(champ->_slots[kDMSlotActionHand]);
	bool actionHitsNonMaterialCreatures = getFlag(actionHitProbability, k0x8000_hitNonMaterialCreatures);
	if (actionHitsNonMaterialCreatures)
		clearFlag(actionHitProbability, k0x8000_hitNonMaterialCreatures);

	if ((!getFlag(creatureInfo->_attributes, k0x0040_MaskCreatureInfo_nonMaterial) || actionHitsNonMaterialCreatures) &&
		((_vm->_championMan->getDexterity(champ) > (_vm->getRandomNumber(32) + creatureInfo->_dexterity + doubledMapDifficulty - 16)) ||
		(!_vm->getRandomNumber(4)) ||
		 (_vm->_championMan->isLucky(champ, 75 - actionHitProbability)))) {

		L0565_i_Damage = _vm->_championMan->getStrength(champIndex, kDMSlotActionHand);
		if (!(L0565_i_Damage))
			goto T0231009;

		L0565_i_Damage += _vm->getRandomNumber((L0565_i_Damage >> 1) + 1);
		L0565_i_Damage = ((long)L0565_i_Damage * (long)actionDamageFactor) >> 5;
		L0568_i_Defense = _vm->getRandomNumber(32) + creatureInfo->_defense + doubledMapDifficulty;
		if (actionHandObjectIconIndex == kDMIconIndiceWeaponDiamondEdge)
			L0568_i_Defense -= L0568_i_Defense >> 2;
		else if (actionHandObjectIconIndex == kDMIconIndiceWeaponHardcleaveExecutioner)
			L0568_i_Defense -= L0568_i_Defense >> 3;

		L0565_i_Damage += _vm->getRandomNumber(32) - L0568_i_Defense;
		L0566_i_Damage = L0565_i_Damage;
		if (L0566_i_Damage <= 1) {
T0231009:
			L0565_i_Damage = _vm->getRandomNumber(4);
			if (!L0565_i_Damage)
				goto T0231015;

			L0565_i_Damage++;
			L0566_i_Damage += _vm->getRandomNumber(16);

			if ((L0566_i_Damage > 0) || (_vm->getRandomNumber(2))) {
				L0565_i_Damage += _vm->getRandomNumber(4);
				if (!_vm->getRandomNumber(4))
					L0565_i_Damage += MAX(0, L0566_i_Damage + _vm->getRandomNumber(16));
			}
		}
		L0565_i_Damage >>= 1;
		L0565_i_Damage += _vm->getRandomNumber(L0565_i_Damage) + _vm->getRandomNumber(4);
		L0565_i_Damage += _vm->getRandomNumber(L0565_i_Damage);
		L0565_i_Damage >>= 2;
		L0565_i_Damage += _vm->getRandomNumber(4) + 1;
		if ((actionHandObjectIconIndex == kDMIconIndiceWeaponVorpalBlade)
			&& !getFlag(creatureInfo->_attributes, k0x0040_MaskCreatureInfo_nonMaterial)
			&& !(L0565_i_Damage >>= 1))
			goto T0231015;

		if (_vm->getRandomNumber(64) < _vm->_championMan->getSkillLevel(champIndex, skillIndex))
			L0565_i_Damage += L0565_i_Damage + 10;

		L0569_i_Outcome = groupGetDamageCreatureOutcome(group, creatureIndex, mapX, mapY, L0565_i_Damage, true);
		_vm->_championMan->addSkillExperience(champIndex, skillIndex, (L0565_i_Damage * creatureInfo->getExperience() >> 4) + 3);
		_vm->_championMan->decrementStamina(champIndex, _vm->getRandomNumber(4) + 4);
		goto T0231016;
	}
T0231015:
	L0565_i_Damage = 0;
	L0569_i_Outcome = k0_outcomeKilledNoCreaturesInGroup;
	_vm->_championMan->decrementStamina(champIndex, _vm->getRandomNumber(2) + 2);
T0231016:
	_vm->_championMan->drawChampionState((ChampionIndex)champIndex);
	if (L0569_i_Outcome != k2_outcomeKilledAllCreaturesInGroup) {
		processEvents29to41(mapX, mapY, kM1_TMEventTypeCreateReactionEvent31ParyIsAdjacent, 0);
	}
	return L0565_i_Damage;
}

void GroupMan::fluxCageAction(int16 mapX, int16 mapY) {
	ElementType squareType = _vm->_dungeonMan->getSquare(mapX, mapY).getType();
	if ((squareType == kDMElementTypeWall) || (squareType == kDMElementTypeStairs))
		return;

	Thing unusedThing = _vm->_dungeonMan->getUnusedThing(kDMThingTypeExplosion);
	if (unusedThing == Thing::_none)
		return;

	_vm->_dungeonMan->linkThingToList(unusedThing, Thing(0), mapX, mapY);
	(((Explosion *)_vm->_dungeonMan->_thingData[kDMThingTypeExplosion])[unusedThing.getIndex()]).setType(kDMExplosionTypeFluxcage);
	TimelineEvent newEvent;
	_vm->setMapAndTime(newEvent._mapTime, _vm->_dungeonMan->_currMapIndex, _vm->_gameTime + 100);
	newEvent._type = k24_TMEventTypeRemoveFluxcage;
	newEvent._priority = 0;
	newEvent._Cu._slot = unusedThing.toUint16();
	newEvent._Bu._location._mapX = mapX;
	newEvent._Bu._location._mapY = mapY;
	newEvent._Bu._location._mapY = mapY;
	_vm->_timeline->addEventGetEventIndex(&newEvent);
	int16 fluxcageCount;
	if (isLordChaosOnSquare(mapX, mapY - 1)) {
		mapY--;
		fluxcageCount = isFluxcageOnSquare(mapX + 1, mapY);
		fluxcageCount += isFluxcageOnSquare(mapX, mapY - 1) + isFluxcageOnSquare(mapX - 1, mapY);
	} else if (isLordChaosOnSquare(mapX - 1, mapY)) {
		mapX--;
		fluxcageCount = isFluxcageOnSquare(mapX, mapY + 1);
		fluxcageCount += isFluxcageOnSquare(mapX, mapY - 1) + isFluxcageOnSquare(mapX - 1, mapY);
	} else if (isLordChaosOnSquare(mapX + 1, mapY)) {
		mapX++;
		fluxcageCount = isFluxcageOnSquare(mapX, mapY - 1);
		fluxcageCount += isFluxcageOnSquare(mapX, mapY + 1) + isFluxcageOnSquare(mapX + 1, mapY);
	} else if (isLordChaosOnSquare(mapX, mapY + 1)) {
		mapY++;
		fluxcageCount = isFluxcageOnSquare(mapX - 1, mapY);
		fluxcageCount += isFluxcageOnSquare(mapX, mapY + 1) + isFluxcageOnSquare(mapX + 1, mapY);
	} else
		fluxcageCount = 0;

	if (fluxcageCount == 2)
		processEvents29to41(mapX, mapY, kM3_TMEventTypeCreateReactionEvent29DangerOnSquare, 0);
}

uint16 GroupMan::isLordChaosOnSquare(int16 mapX, int16 mapY) {
	Thing thing = groupGetThing(mapX, mapY);
	if (thing == Thing::_endOfList)
		return 0;

	Group *group = (Group *)_vm->_dungeonMan->getThingData(thing);
	if (group->_type == k23_CreatureTypeLordChaos)
		return thing.toUint16();

	return 0;
}

bool GroupMan::isFluxcageOnSquare(int16 mapX, int16 mapY) {
	ElementType squareType = _vm->_dungeonMan->getSquare(mapX, mapY).getType();
	if ((squareType == kDMElementTypeWall) || (squareType == kDMElementTypeStairs))
		return false;

	Thing thing = _vm->_dungeonMan->getSquareFirstThing(mapX, mapY);
	while (thing != Thing::_endOfList) {
		if ((thing.getType() == kDMThingTypeExplosion) && (((Explosion *)_vm->_dungeonMan->_thingData[kDMThingTypeExplosion])[thing.getIndex()].getType() == kDMExplosionTypeFluxcage))
			return true;

		thing = _vm->_dungeonMan->getNextThing(thing);
	}
	return false;
}

void GroupMan::fuseAction(uint16 mapX, uint16 mapY) {
	if ((mapX < 0) || (mapX >= _vm->_dungeonMan->_currMapWidth) || (mapY < 0) || (mapY >= _vm->_dungeonMan->_currMapHeight))
		return;

	_vm->_projexpl->createExplosion(Thing::_explHarmNonMaterial, 255, mapX, mapY, k255_CreatureTypeSingleCenteredCreature); /* BUG0_17 The game crashes after the Fuse action is performed while looking at a wall on a map boundary. An explosion thing is created on the square in front of the party but there is no check to ensure the square coordinates are in the map bounds. This corrupts a memory location and leads to a game crash */
	Thing lordChaosThing = Thing(isLordChaosOnSquare(mapX, mapY));
	if (lordChaosThing.toUint16()) {
		bool isFluxcages[4];
		isFluxcages[0] = isFluxcageOnSquare(mapX - 1, mapY);
		isFluxcages[1] = isFluxcageOnSquare(mapX + 1, mapY);
		isFluxcages[2] = isFluxcageOnSquare(mapX, mapY - 1);
		isFluxcages[3] = isFluxcageOnSquare(mapX, mapY + 1);

		uint16 fluxcageCount = 0;
		for (int i = 0; i < 4; i++) {
			if (isFluxcages[i])
				fluxcageCount++;
		}

		while (fluxcageCount++ < 4) {
			int16 destMapX = mapX;
			int16 destMapY = mapY;
			uint16 fluxcageIndex = _vm->getRandomNumber(4);
			for (uint16 i = 5; --i; fluxcageIndex = _vm->turnDirRight(fluxcageIndex)) {
				if (!isFluxcages[fluxcageIndex]) {
					isFluxcages[fluxcageIndex] = true;
					switch (fluxcageIndex) {
					case 0:
						destMapX--;
						break;
					case 1:
						destMapX++;
						break;
					case 2:
						destMapY--;
						break;
					case 3:
						destMapY++;
					}
					break;
				}
			}
			if (isSquareACorridorTeleporterPitOrDoor(destMapX, destMapY)) {
				if (!_vm->_moveSens->getMoveResult(lordChaosThing, mapX, mapY, destMapX, destMapY))
					startWandering(destMapX, destMapY);

				return;
			}
		}
		_vm->fuseSequence();
	}
}

void GroupMan::saveActiveGroupPart(Common::OutSaveFile *file) {
	for (uint16 i = 0; i < _maxActiveGroupCount; ++i) {
		ActiveGroup *group = &_activeGroups[i];
		file->writeUint16BE(group->_groupThingIndex);
		file->writeUint16BE(group->_directions);
		file->writeByte(group->_cells);
		file->writeByte(group->_lastMoveTime);
		file->writeByte(group->_delayFleeingFromTarget);
		file->writeByte(group->_targetMapX);
		file->writeByte(group->_targetMapY);
		file->writeByte(group->_priorMapX);
		file->writeByte(group->_priorMapY);
		file->writeByte(group->_homeMapX);
		file->writeByte(group->_homeMapY);
		for (uint16 j = 0; j < 4; ++j)
			file->writeByte(group->_aspect[j]);
	}
}

void GroupMan::loadActiveGroupPart(Common::InSaveFile *file) {
	for (uint16 i = 0; i < _maxActiveGroupCount; ++i) {
		ActiveGroup *group = &_activeGroups[i];
		group->_groupThingIndex = file->readUint16BE();
		group->_directions = (Direction)file->readUint16BE();
		group->_cells = file->readByte();
		group->_lastMoveTime = file->readByte();
		group->_delayFleeingFromTarget = file->readByte();
		group->_targetMapX = file->readByte();
		group->_targetMapY = file->readByte();
		group->_priorMapX = file->readByte();
		group->_priorMapY = file->readByte();
		group->_homeMapX = file->readByte();
		group->_homeMapY = file->readByte();
		for (uint16 j = 0; j < 4; ++j)
			group->_aspect[j] = file->readByte();
	}
}
}