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
    pub fn perform_unchecked(&self, cmd: AppCommand) -> Result<(App, Vec<AppLog>), GameError> {
        fn disallowed<T>(cmd: AppCommand) -> Result<T, GameError> {
            Err(GameError::InvalidCommand(cmd))
        }
        // Is there a way I can get rid of this clone()
        let (newapp, logs) = match self.current_combat.clone() {
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
                            AppCommand::StopCombat => Ok(self.stop_combat()),
                            AppCommand::Done => self.next_turn(&com),
                            _ => disallowed(cmd),
                        }
                    }
                    CombatCap::Able(able) => {
                        match cmd {
                            AppCommand::Done => self.next_turn(&com),
                            AppCommand::Act(abid, dtarget) => self.act(&able, abid, dtarget),
                            AppCommand::Move(pt) => self.move_creature(&able, pt),
                            _ => disallowed(cmd),
                        }
                    }
                }
            }
        }?;
        // assert_eq!(newapp, self.apply_logs(logs.clone())?);
        Ok((newapp, logs))
    }

    fn apply_logs(&self, logs: Vec<AppLog>) -> Result<App, GameError> {
        let mut newapp = self.clone();
        for log in logs {
            newapp = newapp.apply_log(&log)?;
        }
        Ok(newapp)
    }

    pub fn apply_log(&self, log: &AppLog) -> Result<App, GameError> {
        match *log {
            AppLog::CombatLog(ref cl) => {
                Ok(App {
                    current_combat: Some(self.current_combat
                        .clone()
                        .ok_or(GameError::NotInCombat)?
                        .apply_log(cl)?),
                    ..self.clone()
                })
            }
            AppLog::StartCombat(ref cids) => Ok(self.start_combat(cids.clone())?.0),
            AppLog::StopCombat => Ok(self.stop_combat().0),
        }
    }

    fn move_creature(&self,
                     able: &CombatAble,
                     pt: Point3)
                     -> Result<(App, Vec<AppLog>), GameError> {
        let (next, logs) = able.move_creature(pt)?;
        Ok((App { current_combat: Some(next), ..self.clone() }, combat_logs_into_app_logs(logs)))
    }

    fn act(&self,
           able: &CombatAble,
           abid: AbilityID,
           target: DecidedTarget)
           -> Result<(App, Vec<AppLog>), GameError> {
        let ability = self.get_ability(&abid)?;
        // checking if the creature has this AbilityID is dumb here, it should probably be in
        // Creature, but Creature::act just takes &Ability not &AbilityID
        if able.combat.current_creature().has_ability(&abid) {
            let (next, logs) = able.act(&ability, target)?;
            Ok((App { current_combat: Some(next), ..self.clone() },
                combat_logs_into_app_logs(logs)))
        } else {
            Err(GameError::CreatureLacksAbility(able.combat.current_creature().id(), abid.clone()))
        }
    }

    fn stop_combat(&self) -> (App, Vec<AppLog>) {
        // TODO: Either copy all the creatures out of current_combat and back into self.creatures,
        // or ... something else.
        let mut newapp = self.clone();
        newapp.current_combat = None;
        (newapp, vec![AppLog::StopCombat])
    }

    pub fn get_creature(&self, cid: &CreatureID) -> Result<&Creature, GameError> {
        self.creatures.get(cid).ok_or(GameError::CreatureNotFound(cid.clone()))
    }

    fn next_turn(&self, combat: &Combat) -> Result<(App, Vec<AppLog>), GameError> {
        // I don't know why I need to current_combat.clone()
        let (newcombat, logs) = combat.next_turn()?;
        Ok((App { current_combat: Some(newcombat), ..self.clone() },
            combat_logs_into_app_logs(logs)))
    }

    fn start_combat(&self, cids: Vec<CreatureID>) -> Result<(App, Vec<AppLog>), GameError> {
        let combatants: Vec<Creature> = cids.iter()
            .map(|cid| self.get_creature(cid).map(Clone::clone))
            .collect::<Result<_, GameError>>()?;
        Ok((App { current_combat: Some(Combat::new(combatants)?), ..self.clone() },
            vec![AppLog::StartCombat(cids)]))
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
        app.start_combat(combatants).unwrap().0
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
        let next: App = next.expect("punch did not succeed").0;
        let _: App = next.stop_combat().0;
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
            .unwrap()
            .0;
        let iter = |app: &App| -> Result<App, GameError> {
            let app = app.perform_unchecked(AppCommand::Act(abid("punch"),
                                                   DecidedTarget::Melee(cid("ranger"))))?
                .0;
            let app = app.perform_unchecked(AppCommand::Done)?.0;
            let app = app.perform_unchecked(AppCommand::Done)?.0;
            let app = app.perform_unchecked(AppCommand::Act(abid("heal"),
                                                   DecidedTarget::Range(cid("ranger"))))?
                .0;
            let app = app.perform_unchecked(AppCommand::Done)?.0;
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
