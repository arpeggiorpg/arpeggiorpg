use ts_rs::TS;
use std::fs::File;
use std::io::prelude::*;


use pandt::types as T;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut file = File::create("bindings/bindings.ts")?;
    let decls = vec![
        <T::Volume as TS>::decl(),
        <T::AABB as TS>::decl(),
        <T::AbilityID as TS>::decl(),
        <T::ClassID as TS>::decl(),
        <T::CreatureID as TS>::decl(),
        <T::PlayerID as TS>::decl(),
        <T::SceneID as TS>::decl(),
        <T::ItemID as TS>::decl(),
        <T::AttrID as TS>::decl(),
        <T::ConditionID as TS>::decl(),
        <T::HP as TS>::decl(),
        <T::Energy as TS>::decl(),
        <T::Note as TS>::decl(),
        <T::Dice as TS>::decl(),
        <T::AABB as TS>::decl(),
        <T::AttributeCheck as TS>::decl(),
        <T::Duration as TS>::decl(),
        <T::Condition as TS>::decl(),
        <T::CreatureEffect as TS>::decl(),
        <T::SkillLevel as TS>::decl(),
        <T::Combat as TS>::decl(),

    ];
    file.write_all(b"import type { Point3 } from '../PTTypes';\n\n\n")?;
    for decl in decls.iter() {
        file.write_all(b"export ")?;
        file.write_all(decl.as_bytes())?;
        file.write_all(b"\n\n")?;
    }
    return Ok(())
}
