use std::collections::HashMap;
use std::collections::VecDeque;

use types::*;
use creature::*;
use combat::*;

/// A data structure maintaining state for the whole app. It keeps track of the history of the
/// whole game, and exposes the top-level methods that run simulations on the game.
#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct App {
    history: VecDeque<AppLog>,
    current_combat: Option<Combat>,
    abilities: HashMap<AbilityID, Ability>,
    creatures: HashMap<CreatureID, Creature>,
}

// Generic methods for any kind of App regardless of the CreatureState.
impl App {
    pub fn new() -> Self {
        App {
            history: VecDeque::with_capacity(1000),
            abilities: HashMap::new(),
            current_combat: None,
            creatures: HashMap::new(),
        }
    }

    pub fn get_ability(&self, ability_id: &AbilityID) -> Result<Ability, GameError> {
        // maybe this should just return &Ability?
        Ok(self.abilities.get(ability_id).ok_or(GameError::NoAbility(ability_id.clone()))?.clone())
    }

    /// Perform an AppCommand on the current app.
    pub fn perform_unchecked(&self, cmd: AppCommand) -> Result<(App, Vec<AppLog>), GameError> {
        fn disallowed<T>(cmd: AppCommand) -> Result<T, GameError> {
            Err(GameError::InvalidCommand(cmd))
        }

        let (mut newapp, logs) = match self.current_combat.as_ref() {
            None => {
                match cmd {
                    AppCommand::StartCombat(cids) => self.start_combat(cids),
                    _ => disallowed(cmd),
                }
            }
            Some(com) => {
                match cmd {
                    AppCommand::StopCombat => Ok(self.stop_combat(&com)),
                    AppCommand::Move(pt) => self.move_creature(&com, pt),
                    AppCommand::Done => self.next_turn(&com),
                    AppCommand::Act(abid, dtarget) => self.act(&com, abid, dtarget),
                    _ => disallowed(cmd),
                }
            }
        }?;
        // Design challenge: figure out a way to make this assertion unnecessary or at least less
        // necessary.
        debug_assert!(newapp == self.apply_logs(logs.clone())?,
                      "newapp = {:?}, logapp = {:?}",
                      newapp,
                      self.apply_logs(logs.clone())?);
        newapp.history.extend(logs.clone());
        while newapp.history.len() >= 1000 {
            newapp.history.pop_front();
        }
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
                        .as_ref()
                        .ok_or(GameError::NotInCombat)?
                        .apply_log(cl)?),
                    ..self.clone()
                })
            }
            AppLog::StartCombat(ref cids) => Ok(self.start_combat(cids.clone())?.0),
            AppLog::StopCombat => {
                self.current_combat
                    .as_ref()
                    .map(|c| self.stop_combat(&c).0)
                    .ok_or(GameError::NotInCombat)
            }
        }
    }

    fn move_creature(&self, combat: &Combat, pt: Point3) -> Result<(App, Vec<AppLog>), GameError> {
        let movement = combat.get_movement()?;
        let (next, logs) = movement.move_creature(pt)?;
        Ok((App { current_combat: Some(next), ..self.clone() }, combat_logs_into_app_logs(logs)))
    }

    fn act(&self,
           combat: &Combat,
           abid: AbilityID,
           target: DecidedTarget)
           -> Result<(App, Vec<AppLog>), GameError> {
        let able = combat.get_able()?;
        let ability = self.get_ability(&abid)?;
        // checking if the creature has this AbilityID is dumb here, it should probably be in
        // Creature, but Creature::act just takes &Ability not &AbilityID
        if able.combat.current_creature().has_ability(&abid) {
            let (next, logs) = able.act(&ability, target)?;
            Ok((App { current_combat: Some(next), ..self.clone() },
                combat_logs_into_app_logs(logs)))
        } else {
            Err(GameError::CreatureLacksAbility(able.combat.current_creature().id(), abid))
        }
    }

    fn stop_combat(&self, combat: &Combat) -> (App, Vec<AppLog>) {
        let mut newapp = self.clone();
        for creature in combat.get_creatures() {
            newapp.creatures.insert(creature.id(), creature.clone());
        }
        newapp.current_combat = None;
        (newapp, vec![AppLog::StopCombat])
    }

    pub fn get_creature(&self, cid: CreatureID) -> Result<&Creature, GameError> {
        self.creatures.get(&cid).ok_or(GameError::CreatureNotFound(cid))
    }

    fn next_turn(&self, combat: &Combat) -> Result<(App, Vec<AppLog>), GameError> {
        let (newcombat, logs) = combat.next_turn()?;
        Ok((App { current_combat: Some(newcombat), ..self.clone() },
            combat_logs_into_app_logs(logs)))
    }

    fn start_combat(&self, cids: Vec<CreatureID>) -> Result<(App, Vec<AppLog>), GameError> {
        let combatants: Vec<Creature> = cids.iter()
            .map(|cid| self.get_creature(*cid).map(Clone::clone))
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

    pub fn t_start_combat(app: &App, combatants: Vec<CreatureID>) -> App {
        app.perform_unchecked(AppCommand::StartCombat(combatants)).unwrap().0
    }

    pub fn t_app_act(app: &App, ability_id: AbilityID, target: DecidedTarget) -> App {
        app.perform_unchecked(AppCommand::Act(ability_id, target)).unwrap().0
    }

    pub fn t_app() -> App {
        let punch = t_punch();
        let heal = t_heal();
        let mut app = App::new();
        app.creatures.insert(cid("rogue"), t_rogue("rogue"));
        app.creatures.insert(cid("ranger"), t_ranger("ranger"));
        app.creatures.insert(cid("cleric"), t_cleric("cleric"));
        app.abilities.insert(abid("punch"), punch);
        app.abilities.insert(abid("heal"), heal);
        app
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
        creatures.insert(bob_id, creature);
        let mut abilities = HashMap::new();
        abilities.insert(punch_id.clone(), punch);
        let app = App {
            history: VecDeque::new(),
            abilities: abilities,
            current_combat: None,
            creatures: creatures,
        };
        let app = t_start_combat(&app, vec![bob_id]);
        let next = app.perform_unchecked(AppCommand::Act(punch_id, DecidedTarget::Melee(bob_id)));
        let next: App = next.expect("punch did not succeed").0;
        let _: App = next.stop_combat(&next.current_combat.as_ref().unwrap()).0;
    }


    #[test]
    fn start_combat_not_found() {
        let app = App::new();
        let non = cid("nonexistent");
        assert_eq!(app.perform_unchecked(AppCommand::StartCombat(vec![non])),
                   Err(GameError::CreatureNotFound(non)));
    }

    #[test]
    fn combat_must_have_creatures() {
        let app = App::new();
        assert_eq!(app.perform_unchecked(AppCommand::StartCombat(vec![])),
                   Err(GameError::CombatMustHaveCreatures));
    }

    #[test]
    fn stop_combat() {
        let app = t_app();
        let app = t_start_combat(&app, vec![cid("rogue"), cid("ranger"), cid("cleric")]);
        let app =
            app.perform_unchecked(AppCommand::Act(abid("punch"),
                                                   DecidedTarget::Melee(cid("ranger"))))
                .unwrap()
                .0;
        assert_eq!(app.current_combat
                       .as_ref()
                       .unwrap()
                       .get_creature(cid("ranger"))
                       .unwrap()
                       .cur_health(),
                   HP(7));
        let app = app.perform_unchecked(AppCommand::StopCombat).unwrap().0;
        assert_eq!(app.get_creature(cid("ranger")).unwrap().cur_health(), HP(7));
    }

    #[test]
    fn movement() {
        let app = t_app();
        let app = t_start_combat(&app, vec![cid("rogue"), cid("ranger"), cid("cleric")]);
        app.perform_unchecked(AppCommand::Move((1, 0, 0))).unwrap();
    }

    #[bench]
    fn three_char_infinite_combat(bencher: &mut Bencher) {
        let app = t_app();
        let mut app = app.perform_unchecked(AppCommand::StartCombat(vec![cid("rogue"),
                                                            cid("ranger"),
                                                            cid("cleric")]))
            .unwrap()
            .0;
        let iter = |app: &App| -> Result<App, GameError> {
            let app = t_app_act(app, abid("punch"), DecidedTarget::Melee(cid("ranger")));
            let app = app.perform_unchecked(AppCommand::Done)?.0;
            let app = app.perform_unchecked(AppCommand::Done)?.0;
            let app = t_app_act(&app, abid("heal"), DecidedTarget::Range(cid("ranger")));
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
