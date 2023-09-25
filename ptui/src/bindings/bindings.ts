import type { Point3 } from '../PTTypes';


export interface AABB { x: number, y: number, z: number, }

export type AbilityID = string;

export interface AbilityStatus { ability_id: AbilityID, cooldown: number, }

export interface AppliedCondition { remaining: Duration, condition: Condition, }

export interface AttributeCheck { reliable: boolean, attr: AttrID, target: SkillLevel, }

export type AttrID = string;

export interface Class { id: ClassID, name: string, abilities: Array<AbilityID>, conditions: Array<Condition>, color: string, }

export type ClassID = string;

export interface Combat { scene: SceneID, creatures: { cursor: number; data: Array<[CreatureID, number]> }, movement_used: number, }

export type Condition = { RecurringEffect: CreatureEffect } | "Dead" | "Incapacitated" | { AddDamageBuff: HP } | "DoubleMaxMovement" | { ActivateAbility: AbilityID };

export type ConditionID = string;

export interface CreatureCreation { name: string, class: ClassID, portrait_url: string, icon_url: string, note: string, bio: string, initiative: Dice, size: AABB, }

export type CreatureEffect = { ApplyCondition: [Duration, Condition] } | { Heal: Dice } | { Damage: Dice } | { MultiEffect: Array<CreatureEffect> } | { GenerateEnergy: Energy };

export type CreatureID = string;

export type Dice = { Expr: { num: number, size: number, } } | { Plus: [Dice, Dice] } | { Flat: { value: number, } } | { BestOf: [number, Dice] };

export type Duration = "Interminate" | { Rounds: number };

export type Energy = number;

export type HP = number;

export interface Item { id: ItemID, name: string, }

export type ItemID = string;

export interface Note { name: string, content: string, }

export interface Player { player_id: PlayerID, scene: SceneID | null, creatures: Array<CreatureID>, }

export type PlayerID = string;

export type SceneID = string;

export type SkillLevel = "Inept" | "Unskilled" | "Skilled" | "Expert" | "Supernatural";

export type Volume = { Sphere: number } | { Line: { vector: Point3, } } | { VerticalCylinder: { radius: number, height: number, } } | { AABB: AABB };

