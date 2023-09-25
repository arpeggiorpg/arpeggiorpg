import type { Point3 } from '../PTTypes';


export type Volume = { Sphere: number } | { Line: { vector: Point3, } } | { VerticalCylinder: { radius: number, height: number, } } | { AABB: AABB };

export interface AABB { x: number, y: number, z: number, }

export type AbilityID = string;

export type ClassID = string;

export type CreatureID = string;

export type PlayerID = string;

export type SceneID = string;

export type ItemID = string;

export type AttrID = string;

export type ConditionID = string;

export type HP = number;

export type Energy = number;

export interface Note { name: string, content: string, }

export type Dice = { Expr: { num: number, size: number, } } | { Plus: [Dice, Dice] } | { Flat: { value: number, } } | { BestOf: [number, Dice] };

export interface AABB { x: number, y: number, z: number, }

export interface AttributeCheck { reliable: boolean, attr: AttrID, target: SkillLevel, }

export type Duration = "Interminate" | { Rounds: number };

export type Condition = { RecurringEffect: CreatureEffect } | "Dead" | "Incapacitated" | { AddDamageBuff: HP } | "DoubleMaxMovement" | { ActivateAbility: AbilityID };

export type CreatureEffect = { ApplyCondition: [Duration, Condition] } | { Heal: Dice } | { Damage: Dice } | { MultiEffect: Array<CreatureEffect> } | { GenerateEnergy: Energy };

export type SkillLevel = "Inept" | "Unskilled" | "Skilled" | "Expert" | "Supernatural";

export interface Combat { scene: SceneID, creatures: { cursor: number; data: Array<[CreatureID, number]> }, movement_used: number, }

