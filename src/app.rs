use std::collections::VecDeque;
use std::collections::HashMap;

use types::*;
use creature::*;
use combat::*;

/// A data structure maintaining state for the whole app. It keeps track of the history of the
/// whole game, and exposes the top-level methods that run simulations on the game.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App {
    // more state might need to go into the history... not sure
    combat_history: VecDeque<Combat>,
    current_combat: Option<Combat>,
    abilities: HashMap<AbilityID, Ability>,
    creatures: HashMap<CreatureID, Creature>,
}

// Generic methods for any kind of App regardless of the CreatureState.
impl App {
    pub fn get_ability(&self, ability_id: &AbilityID) -> Result<Ability, GameError> {
        Ok(self.abilities.get(ability_id).ok_or(GameError::NoAbility(ability_id.clone()))?.clone())
    }

    /// Return an AppCapability according to the current state of the app.
    pub fn capability(self) -> AppCapability {
        enum X {
            Able,
            Incap,
            NoCombat,
        }

        let x = match self.current_combat {
            Some(ref combat) => {
                match combat.capability() {
                    CombatCapability::Able(_) => X::Able,
                    CombatCapability::Incap(_) => X::Incap,
                }
            }
            None => X::NoCombat,
        };
        match x {
            X::Able => AppCapability::Able(AppAble { app: self }),
            X::Incap => AppCapability::Incap(AppIncap { app: self }),
            X::NoCombat => AppCapability::NoCombat(AppNoCombat { app: self }),
        }
    }

    fn stop_combat(mut self) -> App {
        // TODO: Either copy all the creatures out of current_combat and back into self.creatures,
        // or ... something else.
        if let Some(c) = self.current_combat {
            self.combat_history.push_back(c.clone());
        }
        App {
            current_combat: None,
            creatures: self.creatures,
            abilities: self.abilities,
            combat_history: self.combat_history,
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct AppIncap {
    app: App,
}
impl AppIncap {
    pub fn skip(mut self) -> App {
        let newcombat = match self.app.current_combat.unwrap().capability() {
            CombatCapability::Incap(incap) => incap.skip(),
            _ => panic!("AppIncap contained something other than CombatIncap"),
        };
        self.app.current_combat = Some(newcombat);
        self.app
    }
    pub fn done(self) -> App {
        self.app
    }
    pub fn stop_combat(self) -> App {
        self.app.stop_combat()
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct AppAble {
    app: App,
}

impl AppAble {
    fn perform_able_op<F>(mut self, op: F) -> Result<App, GameError>
        where F: FnOnce(&CombatAble) -> Result<Combat, GameError>
    {
        let g = match self.app.current_combat.unwrap().capability() {
            CombatCapability::Able(able) => op(&able)?,
            _ => panic!("AppAble contained something other than CombatAble"),
        };
        // FIXME bring back history
        // if self.app.combat_history.len() >= 1000 {
        //     let _ = self.app.combat_history.pop_front();
        // }
        // self.app.combat_history.push_back((&self.app.current_combat).clone());
        self.app.current_combat = Some(g);
        Ok(self.app)
    }

    pub fn act(self, ability_id: AbilityID, target: DecidedTarget) -> Result<App, GameError> {
        let ability = self.app.get_ability(&ability_id)?;
        self.perform_able_op(move |able| {
            if able.combat.current_creature().has_ability(&ability_id) {
                able.act(&ability, target)
            } else {
                Err(GameError::CreatureLacksAbility(ability_id.clone()))
            }
        })
    }
    pub fn done(self) -> App {
        self.app
    }
    pub fn stop_combat(self) -> App {
        self.app.stop_combat()
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct AppNoCombat {
    pub app: App,
}
impl AppNoCombat {
    /// Create a Combat and return a new App with it. Returns None if there aren't enough
    /// combatants to start a combat, or if any combatants can't be found.
    pub fn start_combat(&mut self, combatants: Vec<CreatureID>) -> Result<(), GameError> {
        // for cid in &combatants {
        //     let creature = self.app
        //         .creatures
        //         .get(cid)
        //         .ok_or_else(|| GameError::CreatureNotFound(cid.clone()))?;
        // }
        let combatant_objs: Vec<Creature> =
            combatants.iter().flat_map(|cid| self.app.creatures.get(cid)).cloned().collect();
        if combatant_objs.len() != combatants.len() {
            Err(GameError::BuggyProgram(":(".to_string()))
        } else {
            self.app.current_combat = Combat::new(combatant_objs);
            Ok(())
        }
    }
    pub fn done(self) -> App {
        self.app
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum AppCapability {
    Incap(AppIncap),
    Able(AppAble),
    NoCombat(AppNoCombat),
}

#[cfg(test)]
pub fn t_able_app<'a>(app: App) -> AppAble {
    match app.capability() {
        AppCapability::Able(a) => a,
        _ => panic!("Not an Able App"),
    }
}

#[cfg(test)]
pub fn t_start_combat<'a>(app: App, combatants: Vec<CreatureID>) -> App {
    match app.capability() {
        AppCapability::NoCombat(mut a) => {
            a.start_combat(combatants).unwrap();
            a.done()
        }
        _ => panic!("Tried to start combat on already-Combative app"),
    }
}

#[test]
fn workflow() {
    let mut creatures = HashMap::new();
    let punch = t_punch();
    let punch_id = abid("punch");
    let bob_id = cid("bob");
    let creature = Creature::build("bob")
        .abilities(vec![punch_id.clone()])
        .build()
        .unwrap();
    creatures.insert(bob_id.clone(), creature);
    let mut abilities = HashMap::new();
    abilities.insert(punch_id.clone(), punch);
    let app = App {
        combat_history: VecDeque::new(),
        abilities: abilities,
        current_combat: None,
        creatures: creatures,
    };
    let app = t_start_combat(app, vec![bob_id.clone()]);
    let next = t_able_app(app).act(punch_id.clone(), DecidedTarget::Melee(bob_id.clone()));
    let next: App = next.expect("punch did not succeed");
    let next = t_able_app(next);
    let _: App = next.stop_combat();
}
