use std::collections::HashMap;

use types::*;
use creature::*;
use combat::*;


#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Game {
    current_combat: Option<Combat>,
    abilities: HashMap<AbilityID, Ability>,
    creatures: HashMap<CreatureID, Creature>,
}

// Generic methods for any kind of Game regardless of the CreatureState.
impl Game {
    pub fn new() -> Self {
        Game {
            abilities: HashMap::new(),
            current_combat: None,
            creatures: HashMap::new(),
        }
    }

    pub fn get_ability(&self, ability_id: &AbilityID) -> Result<Ability, GameError> {
        // maybe this should just return &Ability?
        Ok(self.abilities.get(ability_id).ok_or(GameError::NoAbility(ability_id.clone()))?.clone())
    }

    /// Perform a GameCommand on the current Game.
    pub fn perform_unchecked(&self, cmd: GameCommand) -> Result<(Game, Vec<GameLog>), GameError> {
        fn disallowed<T>(cmd: GameCommand) -> Result<T, GameError> {
            Err(GameError::InvalidCommand(cmd))
        }

        use self::GameCommand::*;
        let (newgame, logs) = match (cmd.clone(), self.current_combat.as_ref()) {
            (GameCommand::CreateCreature(c), _) => self.add_creature(c),
            (StartCombat(cids), None) => self.start_combat(cids),
            (StopCombat, Some(com)) => Ok(self.stop_combat(&com)),
            (Move(pt), Some(com)) => self.move_creature(&com, pt),
            (Done, Some(com)) => self.next_turn(&com),
            (Act(abid, dtarget), Some(com)) => self.act(&com, abid, dtarget),
            _ => disallowed(cmd),
        }?;

        // Design challenge: figure out a way to make this assertion unnecessary or at least less
        // necessary.
        debug_assert!(newgame == self.apply_logs(logs.clone())?,
                      "newgame = {:?}, lgame = {:?}",
                      newgame,
                      self.apply_logs(logs.clone())?);
        Ok((newgame, logs))
    }

    fn add_creature(&self, creature: Creature) -> Result<(Game, Vec<GameLog>), GameError> {
        let mut newgame = self.clone();
        if newgame.creatures.contains_key(&creature.id()) {
            Err(GameError::CreatureAlreadyExists(creature.id()))
        } else {
            newgame.creatures.insert(creature.id(), creature.clone());
            Ok((newgame, vec![GameLog::AddCreature(creature)]))
        }
    }


    fn apply_logs(&self, logs: Vec<GameLog>) -> Result<Game, GameError> {
        let mut newgame = self.clone();
        for log in logs {
            newgame = newgame.apply_log(&log)?;
        }
        Ok(newgame)
    }

    pub fn apply_log(&self, log: &GameLog) -> Result<Game, GameError> {
        match *log {
            GameLog::AddCreature(ref c) => Ok(self.add_creature(c.clone())?.0),
            GameLog::CombatLog(ref cl) => {
                Ok(Game {
                    current_combat: Some(self.current_combat
                        .as_ref()
                        .ok_or(GameError::NotInCombat)?
                        .apply_log(cl)?),
                    ..self.clone()
                })
            }
            GameLog::StartCombat(ref cids) => Ok(self.start_combat(cids.clone())?.0),
            GameLog::StopCombat => {
                self.current_combat
                    .as_ref()
                    .map(|c| self.stop_combat(&c).0)
                    .ok_or(GameError::NotInCombat)
            }
        }
    }

    fn move_creature(&self,
                     combat: &Combat,
                     pt: Point3)
                     -> Result<(Game, Vec<GameLog>), GameError> {
        let movement = combat.get_movement()?;
        let (next, logs) = movement.move_creature(pt)?;
        Ok((Game { current_combat: Some(next), ..self.clone() }, combat_logs_into_game_logs(logs)))
    }

    fn act(&self,
           combat: &Combat,
           abid: AbilityID,
           target: DecidedTarget)
           -> Result<(Game, Vec<GameLog>), GameError> {
        let able = combat.get_able()?;
        let ability = self.get_ability(&abid)?;
        // checking if the creature has this AbilityID is dumb here, it should probably be in
        // Creature, but Creature::act just takes &Ability not &AbilityID
        if able.combat.current_creature().has_ability(&abid) {
            let (next, logs) = able.act(&ability, target)?;
            Ok((Game { current_combat: Some(next), ..self.clone() },
                combat_logs_into_game_logs(logs)))
        } else {
            Err(GameError::CreatureLacksAbility(able.combat.current_creature().id(), abid))
        }
    }

    fn stop_combat(&self, combat: &Combat) -> (Game, Vec<GameLog>) {
        let mut newgame = self.clone();
        for creature in combat.get_creatures() {
            newgame.creatures.insert(creature.id(), creature.clone());
        }
        newgame.current_combat = None;
        (newgame, vec![GameLog::StopCombat])
    }

    pub fn get_creature(&self, cid: CreatureID) -> Result<&Creature, GameError> {
        self.creatures.get(&cid).ok_or(GameError::CreatureNotFound(cid))
    }

    fn next_turn(&self, combat: &Combat) -> Result<(Game, Vec<GameLog>), GameError> {
        let (newcombat, logs) = combat.next_turn()?;
        Ok((Game { current_combat: Some(newcombat), ..self.clone() },
            combat_logs_into_game_logs(logs)))
    }

    fn start_combat(&self, cids: Vec<CreatureID>) -> Result<(Game, Vec<GameLog>), GameError> {
        let combatants: Vec<Creature> = cids.iter()
            .map(|cid| self.get_creature(*cid).map(Clone::clone))
            .collect::<Result<_, GameError>>()?;
        Ok((Game { current_combat: Some(Combat::new(combatants)?), ..self.clone() },
            vec![GameLog::StartCombat(cids)]))
    }
}


#[cfg(test)]
pub mod test {
    use game::*;
    use types::test::*;
    use creature::test::*;
    use test::Bencher;

    pub fn t_start_combat(game: &Game, combatants: Vec<CreatureID>) -> Game {
        game.perform_unchecked(GameCommand::StartCombat(combatants)).unwrap().0
    }

    pub fn t_game_act(game: &Game, ability_id: AbilityID, target: DecidedTarget) -> Game {
        game.perform_unchecked(GameCommand::Act(ability_id, target)).unwrap().0
    }

    pub fn t_game() -> Game {
        let punch = t_punch();
        let heal = t_heal();
        let mut game = Game::new();
        game.abilities.insert(abid("punch"), punch);
        game.abilities.insert(abid("heal"), heal);

        let game = game.perform_unchecked(GameCommand::CreateCreature(t_rogue("rogue"))).unwrap().0;
        let game =
            game.perform_unchecked(GameCommand::CreateCreature(t_ranger("ranger"))).unwrap().0;
        let game =
            game.perform_unchecked(GameCommand::CreateCreature(t_cleric("cleric"))).unwrap().0;
        game
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
        let game = Game {
            abilities: abilities,
            current_combat: None,
            creatures: creatures,
        };
        let game = t_start_combat(&game, vec![bob_id]);
        let next = game.perform_unchecked(GameCommand::Act(punch_id, DecidedTarget::Melee(bob_id)));
        let next: Game = next.expect("punch did not succeed").0;
        let _: Game = next.stop_combat(&next.current_combat.as_ref().unwrap()).0;
    }


    #[test]
    fn start_combat_not_found() {
        let game = Game::new();
        let non = cid("nonexistent");
        assert_eq!(game.perform_unchecked(GameCommand::StartCombat(vec![non])),
                   Err(GameError::CreatureNotFound(non)));
    }

    #[test]
    fn combat_must_have_creatures() {
        let game = Game::new();
        assert_eq!(game.perform_unchecked(GameCommand::StartCombat(vec![])),
                   Err(GameError::CombatMustHaveCreatures));
    }

    #[test]
    fn stop_combat() {
        let game = t_game();
        let game = t_start_combat(&game, vec![cid("rogue"), cid("ranger"), cid("cleric")]);
        let game = game.perform_unchecked(GameCommand::Act(abid("punch"),
                                                DecidedTarget::Melee(cid("ranger"))))
            .unwrap()
            .0;
        assert_eq!(game.current_combat
                       .as_ref()
                       .unwrap()
                       .get_creature(cid("ranger"))
                       .unwrap()
                       .cur_health(),
                   HP(7));
        let game = game.perform_unchecked(GameCommand::StopCombat).unwrap().0;
        assert_eq!(game.get_creature(cid("ranger")).unwrap().cur_health(),
                   HP(7));
    }

    #[test]
    fn movement() {
        let game = t_game();
        let game = t_start_combat(&game, vec![cid("rogue"), cid("ranger"), cid("cleric")]);
        game.perform_unchecked(GameCommand::Move((1, 0, 0))).unwrap();
    }

    #[bench]
    fn three_char_infinite_combat(bencher: &mut Bencher) {
        let game = t_game();
        let mut game = game.perform_unchecked(GameCommand::StartCombat(vec![cid("rogue"),
                                                             cid("ranger"),
                                                             cid("cleric")]))
            .unwrap()
            .0;
        let iter = |game: &Game| -> Result<Game, GameError> {
            let game = t_game_act(game, abid("punch"), DecidedTarget::Melee(cid("ranger")));
            let game = game.perform_unchecked(GameCommand::Done)?.0;
            let game = game.perform_unchecked(GameCommand::Done)?.0;
            let game = t_game_act(&game, abid("heal"), DecidedTarget::Range(cid("ranger")));
            let game = game.perform_unchecked(GameCommand::Done)?.0;
            Ok(game)
        };
        bencher.iter(|| {
            for _ in 0..1000 {
                game = iter(&game).unwrap();
            }
            game.clone()
        });
    }

}
