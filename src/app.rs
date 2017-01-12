use std::collections::HashMap;

use types::*;
use creature::*;
use combat::*;

/// A data structure maintaining state for the whole app. It keeps track of the history of the
/// whole game, and exposes the top-level methods that run simulations on the game.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App {
    current_combat: Option<Combat>,
    abilities: HashMap<AbilityID, Ability>,
    creatures: HashMap<CreatureID, Creature>,
}

// Generic methods for any kind of App regardless of the CreatureState.
impl App {
    pub fn new() -> Self {
        App {
            abilities: HashMap::new(),
            current_combat: None,
            creatures: HashMap::new(),
        }
    }

    pub fn get_ability(&self, ability_id: &AbilityID) -> Result<Ability, GameError> {
        Ok(self.abilities.get(ability_id).ok_or(GameError::NoAbility(ability_id.clone()))?.clone())
    }

    /// Perform an AppCommand on the current app.
    pub fn perform_unchecked(&self, cmd: AppCommand) -> Result<App, GameError> {
        fn disallowed<T>(cmd: AppCommand) -> Result<T, GameError> {
            Err(GameError::InvalidCommand(cmd))
        }
        // Is there a way I can get rid of this clone()
        match self.current_combat.clone() {
            None => {
                match cmd {
                    AppCommand::StartCombat(cids) => self.start_combat(cids),
                    _ => disallowed(cmd),
                }
            }
            Some(com) => {
                match com.capability() {
                    CombatCap::Incap(_) => {
                        match cmd {
                            AppCommand::Done => Ok(self.done()),
                            _ => disallowed(cmd),
                        }
                    }
                    CombatCap::Able(able) => {
                        match cmd {
                            AppCommand::Done => Ok(self.done()),
                            AppCommand::Act(abid, dtarget) => self.act(&able, abid, dtarget),
                            AppCommand::Move(pt) => self.move_creature(&able, pt),
                            _ => disallowed(cmd),
                        }
                    }
                }
            }
        }
    }

    fn move_creature(&self, able: &CombatAble, pt: Point3) -> Result<App, GameError> {
        Ok(App { current_combat: Some(able.move_creature(pt)?), ..self.clone() })
    }

    fn act(&self,
           able: &CombatAble,
           abid: AbilityID,
           target: DecidedTarget)
           -> Result<App, GameError> {
        let ability = self.get_ability(&abid)?;
        // checking if the creature has this AbilityID is dumb here, it should probably be in
        // Creature, but Creature::act just takes &Ability not &AbilityID
        if able.combat.current_creature().has_ability(&abid) {
            Ok(App { current_combat: Some(able.act(&ability, target)?), ..self.clone() })
        } else {
            Err(GameError::CreatureLacksAbility(able.combat.current_creature().id(), abid.clone()))
        }
    }

    fn stop_combat(&self) -> App {
        // TODO: Either copy all the creatures out of current_combat and back into self.creatures,
        // or ... something else.
        let mut newapp = self.clone();
        newapp.current_combat = None;
        newapp
    }

    pub fn get_creature(&self, cid: &CreatureID) -> Result<&Creature, GameError> {
        self.creatures.get(cid).ok_or(GameError::CreatureNotFound(cid.clone()))
    }

    /// this panics if not in combat
    fn done(&self) -> App {
        let newcombat = match self.current_combat {
            Some(ref combat) => {
                match combat.capability() {
                    CombatCap::Incap(ref incap) => incap.done(),
                    CombatCap::Able(ref able) => able.done(),
                }
            }
            None => panic!("No current combat when running done"),
        };
        App { current_combat: Some(newcombat), ..self.clone() }
    }

    fn start_combat(&self, cids: Vec<CreatureID>) -> Result<App, GameError> {
        let combatants: Vec<Creature> = cids.iter()
            .map(|cid| self.get_creature(cid).map(Clone::clone))
            .collect::<Result<_, GameError>>()?;
        Ok(App { current_combat: Some(Combat::new(combatants)?), ..self.clone() })
    }
}


#[cfg(test)]
pub mod test {
    extern crate test;
    use app::*;
    use types::test::*;
    use creature::test::*;
    use self::test::Bencher;

    pub fn t_start_combat<'a>(app: App, combatants: Vec<CreatureID>) -> App {
        app.start_combat(combatants).unwrap()
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
            abilities: abilities,
            current_combat: None,
            creatures: creatures,
        };
        let app = t_start_combat(app, vec![bob_id.clone()]);
        let next = app.perform_unchecked(AppCommand::Act(punch_id.clone(),
                                                         DecidedTarget::Melee(bob_id.clone())));
        let next: App = next.expect("punch did not succeed");
        let _: App = next.stop_combat();
    }


    #[test]
    fn start_combat_not_found() {
        let app = App::new();
        let non = cid("nonexistent");
        assert_eq!(app.start_combat(vec![non.clone()]),
                   Err(GameError::CreatureNotFound(non)));
    }

    #[test]
    fn combat_must_have_creatures() {
        let app = App::new();
        assert_eq!(app.start_combat(vec![]),
                   Err(GameError::CombatMustHaveCreatures));
    }

    #[bench]
    fn three_char_infinite_combat(bencher: &mut Bencher) {
        let punch = t_punch();
        let heal = t_heal();
        let mut app = App::new();
        app.creatures.insert(cid("rogue"), t_rogue("rogue"));
        app.creatures.insert(cid("ranger"), t_ranger("ranger"));
        app.creatures.insert(cid("cleric"), t_cleric("cleric"));
        app.abilities.insert(abid("punch"), punch);
        app.abilities.insert(abid("heal"), heal);
        let mut app = app.perform_unchecked(AppCommand::StartCombat(vec![cid("rogue"),
                                                            cid("ranger"),
                                                            cid("cleric")]))
            .unwrap();
        let iter = |app: &App| -> Result<App, GameError> {
            let app = app.perform_unchecked(AppCommand::Act(abid("punch"),
                                                   DecidedTarget::Melee(cid("ranger"))))?;
            let app = app.perform_unchecked(AppCommand::Done)?;
            let app = app.perform_unchecked(AppCommand::Act(abid("heal"),
                                                   DecidedTarget::Range(cid("ranger"))))?;
            Ok(app)
        };
        bencher.iter(|| {
            for _ in 0..1000 {
                app = iter(&app).unwrap();
            }
            app.clone()
        });
    }

}
