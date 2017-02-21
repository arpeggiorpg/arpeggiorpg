use std::collections::HashMap;

use types::*;
use combat::*;
use creature::ChangedCreature;
use grid::{get_all_accessible, creature_within_distance};


lazy_static! {
    static ref BORING_MAP: Vec<Point3> = vec![(0,0,0)];
}

// Generic methods for any kind of Game regardless of the CreatureState.
impl Game {
    pub fn new(classes: HashMap<String, Class>, abilities: HashMap<AbilityID, Ability>) -> Self {
        Game {
            abilities: abilities,
            current_combat: None,
            creatures: HashMap::new(),
            maps: HashMap::new(),
            current_map: None,
            classes: classes,
        }
    }

    pub fn current_map(&self) -> &Map {
        // TODO: this should return &Map instead of Map
        match self.current_map.as_ref() {
            Some(x) => self.maps.get(x).unwrap_or(&BORING_MAP),
            None => &BORING_MAP,
        }
    }

    pub fn get_ability(&self, ability_id: &AbilityID) -> Result<Ability, GameError> {
        // maybe this should just return &Ability?
        Ok(self.abilities.get(ability_id).ok_or(GameError::NoAbility(ability_id.clone()))?.clone())
    }

    /// Perform a GameCommand on the current Game.
    pub fn perform_unchecked(&self, cmd: GameCommand) -> Result<ChangedGame, GameError> {
        fn disallowed<T>(cmd: GameCommand) -> Result<T, GameError> {
            Err(GameError::InvalidCommand(cmd))
        }

        use self::GameCommand::*;
        let change = match (cmd.clone(), self.current_combat.as_ref()) {
            (CreateCreature(c), _) => self.create_creature(c),
            (PathCreature(cid, pt), _) => self.path_creature(cid, pt),
            (SetCreaturePos(cid, pt), _) => self.change().apply_creature(cid, |c| c.set_pos(pt)),
            (PathCurrentCombatCreature(pt), Some(_)) => {
                self.change().apply_combat(|c| c.combat.get_movement(self)?.move_current(self, pt))
            }
            (CombatAct(abid, dtarget), Some(_)) => self.act(abid, dtarget),
            (ActCreature(cid, abid, dtarget), _) => self.act_ooc(cid, abid, dtarget),
            (SelectMap(ref name), _) => self.change_with(GameLog::SelectMap(name.clone())),
            (EditMap(ref name, ref terrain), _) => {
                self.change_with(GameLog::EditMap(name.clone(), terrain.clone()))
            }
            (RemoveCreature(cid), _) => self.change_with(GameLog::RemoveCreature(cid)),
            (StartCombat(cids), None) => self.change_with(GameLog::StartCombat(cids)),
            (StopCombat, Some(_)) => self.change_with(GameLog::StopCombat),
            (AddCreatureToCombat(cid), Some(_)) => {
                self.change_with(GameLog::AddCreatureToCombat(cid))
            }
            (RemoveCreatureFromCombat(cid), Some(_)) => {
                self.change_with(GameLog::RemoveCreatureFromCombat(cid))
            }
            (ChangeCreatureInitiative(cid, new_pos), Some(_)) => {
                self.change().apply_combat(|c| c.combat.change_creature_initiative(self, cid, new_pos))
            }
            (Done, Some(_)) => self.change().apply_combat(|c| c.next_turn()),
            _ => disallowed(cmd),
        }?;
        Ok(change)
    }

    fn create_creature(&self, spec: CreatureCreation) -> Result<ChangedGame, GameError> {
        let creature = Creature::build(&spec.id.to_string(), &spec.class).pos(spec.pos)
            .name(&spec.name)
            .build()?;
        self.change_with(GameLog::CreateCreature(creature))
    }

    pub fn apply_log(&self, log: &GameLog) -> Result<Game, GameError> {
        use self::GameLog::*;
        let mut newgame = self.clone();
        match *log {
            SelectMap(ref name) => {
                let _ = newgame.maps.get(name).ok_or_else(|| GameError::MapNotFound(name.clone()))?;
                newgame.current_map = Some(name.clone());
            }
            EditMap(ref name, ref terrain) => {
                newgame.maps.insert(name.clone(), terrain.clone());
            }
            CreateCreature(ref c) => {
                if newgame.creatures.contains_key(&c.id()) {
                    return Err(GameError::CreatureAlreadyExists(c.id()));
                } else {
                    newgame.creatures.insert(c.id(), c.clone());
                }
            }
            RemoveCreature(cid) => {
                newgame.creatures.remove(&cid).ok_or_else(|| GameError::CreatureNotFound(cid))?;
            }
            AddCreatureToCombat(cid) => {
                let mut combat = newgame.current_combat.clone().ok_or(GameError::NotInCombat)?;
                let creature =
                    newgame.creatures.remove(&cid).ok_or_else(|| GameError::CreatureNotFound(cid))?;
                combat.creatures.push(creature);
                newgame.current_combat = Some(combat);
            }
            RemoveCreatureFromCombat(cid) => {
                let (combat, creature) = {
                    let combat = newgame.get_combat()?;
                    combat.remove_from_combat(cid)?
                };
                newgame.current_combat = combat;
                newgame.creatures.insert(creature.id(), creature);
            }
            CombatLog(ref cl) => {
                return Ok(Game {
                    current_combat: Some(self.get_combat()?.apply_log(cl)?),
                    ..self.clone()
                });
            }
            CreatureLog(cid, ref cl) => {
                let mut newgame = self.clone();
                let creature = self.get_creature(cid)?.apply_log(cl)?;
                *newgame.get_creature_mut(cid)? = creature;
                return Ok(newgame);
            }
            StartCombat(ref cids) => {
                let mut creatures = vec![];
                for cid in cids {
                    let creature =
                        newgame.creatures.remove(cid).ok_or(GameError::CreatureNotFound(*cid))?;
                    creatures.push(creature);
                }
                newgame.current_combat = Some(Combat::new(creatures)?);
            }
            StopCombat => {
                // we shouldn't need to collect into a Vec here.
                let creatures: Vec<Creature> = newgame.current_combat
                    .take()
                    .ok_or(GameError::NotInCombat)?
                    .creatures
                    .into_iter()
                    .collect();
                for creature in creatures {
                    newgame.creatures.insert(creature.id(), creature.clone());
                }
            }
            // Ignore Rollback, since it's something that's handled at the App level.
            Rollback(..) => {}
        }
        Ok(newgame)
    }

    fn path_creature(&self, cid: CreatureID, pt: Point3) -> Result<ChangedGame, GameError> {
        // TODO: this should use a GameLog::PathCreature instead of just creature.set_pos to the destination.
        // One reason is so that we check each step in the path to make sure it's valid.
        match self.change().apply_creature(cid, |c| c.set_pos(pt)) {
            Ok(r) => Ok(r),
            Err(_) => {
                self.change()
                    .apply_combat(|com| com.combat.change().apply_creature(cid, |c| c.set_pos(pt)))
            }
        }
    }

    // self.change().apply_creature(cid, move |c| c.set_pos_path(pts, distance))

    fn act(&self, abid: AbilityID, target: DecidedTarget) -> Result<ChangedGame, GameError> {
        self.change().apply_combat(move |c| {
            let ability = self.get_ability(&abid)?;
            let able = c.combat.get_able()?;
            if self.creature_has_ability(&c.combat.current_creature(), &abid)? {
                able.act(&ability, target)
            } else {
                Err(GameError::CreatureLacksAbility(c.combat.current_creature().id(), abid))
            }
        })
    }

    fn act_ooc(&self,
               cid: CreatureID,
               abid: AbilityID,
               target: DecidedTarget)
               -> Result<ChangedGame, GameError> {
        self.get_creature(cid)?.act(|cid| self.get_creature(cid),
                                    &self.get_ability(&abid)?,
                                    target,
                                    self.change(),
                                    false)
    }

    fn creature_has_ability(&self,
                            creature: &Creature,
                            ability: &AbilityID)
                            -> Result<bool, GameError> {
        let class = creature.class();
        let abilities =
            &self.classes.get(&class).ok_or(GameError::ClassNotFound(class.clone()))?.abilities;
        Ok(abilities.contains(ability))
    }

    pub fn get_creature(&self, cid: CreatureID) -> Result<&Creature, GameError> {
        self.creatures.get(&cid).ok_or(GameError::CreatureNotFound(cid))
    }
    pub fn get_creature_mut(&mut self, cid: CreatureID) -> Result<&mut Creature, GameError> {
        self.creatures.get_mut(&cid).ok_or(GameError::CreatureNotFound(cid))
    }

    /// Get a reference to the named Creature, whether or not it's in Combat.
    pub fn find_creature(&self, cid: CreatureID) -> Result<&Creature, GameError> {
        self.get_creature(cid).or_else(|_| {
            self.current_combat
                .as_ref()
                .ok_or(GameError::CreatureNotFound(cid.clone()))
                .and_then(|combat| combat.get_creature(cid))
        })
    }

    pub fn get_combat(&self) -> Result<DynamicCombat, GameError> {
        self.current_combat
            .as_ref()
            .map(|com| {
                DynamicCombat {
                    combat: &com,
                    game: self,
                }
            })
            .ok_or(GameError::NotInCombat)
    }

    pub fn get_movement_options(&self, creature_id: CreatureID) -> Result<Vec<Point3>, GameError> {
        let creature = self.find_creature(creature_id)?;
        if creature.can_move {
            Ok(get_all_accessible(creature.pos(), self.current_map(), creature.speed(self)?))
        } else {
            Err(GameError::CannotAct(creature.id()))
        }
    }

    /// Get a list of possible targets for an ability being used by a creature.
    pub fn get_target_options(&self,
                              creature_id: CreatureID,
                              ability_id: AbilityID)
                              -> Result<Vec<PotentialTarget>, GameError> {
        let ability = self.get_ability(&ability_id)?;

        Ok(match ability.target {
            TargetSpec::Melee => self.creatures_in_range(creature_id, MELEE_RANGE)?,
            TargetSpec::Range(distance) => self.creatures_in_range(creature_id, distance)?,
            TargetSpec::Actor => vec![PotentialTarget::CreatureID(creature_id)],
        })
    }

    fn creatures_in_range(&self,
                          creature_id: CreatureID,
                          distance: Distance)
                          -> Result<Vec<PotentialTarget>, GameError> {
        let (creature, creatures) = match self.current_combat.as_ref() {
            Some(combat) => {
                // OOC creatures target OOC creatures; in-combat creatures target in-combat creatures
                match combat.get_creature(creature_id) {
                    Ok(creature) => (creature, combat.get_creatures()),
                    Err(_) => (self.get_creature(creature_id)?, self.creatures.values().collect()),
                }
            }
            None => (self.get_creature(creature_id)?, self.creatures.values().collect()),
        };
        let mut results = vec![];
        for ptarget in creatures {
            if creature_within_distance(creature, ptarget, distance) {
                results.push(PotentialTarget::CreatureID(ptarget.id()));
            }
        }
        Ok(results)
    }

    pub fn get_class(&self, class: &str) -> Result<&Class, GameError> {
        self.classes.get(class).ok_or_else(|| GameError::ClassNotFound(class.to_string()))
    }

    pub fn change(&self) -> ChangedGame {
        ChangedGame {
            game: self.clone(),
            logs: vec![],
        }
    }

    pub fn change_with(&self, log: GameLog) -> Result<ChangedGame, GameError> {
        let game = self.apply_log(&log)?;
        Ok(ChangedGame {
            game: game,
            logs: vec![log],
        })
    }
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ChangedGame {
    game: Game,
    logs: Vec<GameLog>,
}

impl ChangedGame {
    pub fn apply(&self, log: &GameLog) -> Result<ChangedGame, GameError> {
        let mut new = self.clone();
        new.game = self.game.apply_log(log)?;
        Ok(new)
    }

    pub fn apply_combat<F>(&self, f: F) -> Result<ChangedGame, GameError>
        where F: FnOnce(DynamicCombat) -> Result<ChangedCombat, GameError>
    {
        match self.game.current_combat.as_ref() {
            None => Err(GameError::NotInCombat),
            Some(combat) => {
                let change = f(DynamicCombat{combat: &combat, game: &self.game})?;
                let (combat, logs) = change.done();
                let mut new = self.clone();
                new.game.current_combat = Some(combat);
                new.logs.extend(combat_logs_into_game_logs(logs));
                Ok(new)
            }
        }
    }

    pub fn apply_creature<F>(&self, cid: CreatureID, f: F) -> Result<ChangedGame, GameError>
        where F: FnOnce(&Creature) -> Result<ChangedCreature, GameError>
    {
        let creature = self.game.get_creature(cid)?;
        let change = f(creature)?;
        let mut new = self.clone();
        let (creature, logs) = change.done();
        *new.game.get_creature_mut(cid)? = creature;
        new.logs.extend(creature_logs_into_game_logs(cid, logs));
        Ok(new)
    }

    pub fn done(self) -> (Game, Vec<GameLog>) {
        (self.game, self.logs)
    }
}

impl CreatureChanger for ChangedGame {
    fn apply_creature<F>(&self, cid: CreatureID, f: F) -> Result<ChangedGame, GameError>
        where F: FnOnce(&Creature) -> Result<ChangedCreature, GameError>
    {
        Ok(ChangedGame::apply_creature(self, cid, f)?)
    }
}



#[cfg(test)]
pub mod test {
    use game::*;
    use types::test::*;
    use grid::test::*;
    use std::iter::FromIterator;

    pub fn t_start_combat(game: &Game, combatants: Vec<CreatureID>) -> Game {
        game.perform_unchecked(GameCommand::StartCombat(combatants)).unwrap().game
    }

    pub fn t_game_act(game: &Game, ability_id: AbilityID, target: DecidedTarget) -> Game {
        game.perform_unchecked(GameCommand::CombatAct(ability_id, target)).unwrap().game
    }

    pub fn t_game() -> Game {
        let mut game = Game::new(t_classes(), t_abilities());
        game.maps.insert("huge".to_string(), huge_box());
        game.current_map = Some("huge".to_string());
        let game = game.perform_unchecked(GameCommand::CreateCreature(t_rogue_creation("rogue")))
            .unwrap()
            .game;
        let game = game.perform_unchecked(GameCommand::CreateCreature(t_ranger_creation("ranger")))
            .unwrap()
            .game;
        let game = game.perform_unchecked(GameCommand::CreateCreature(t_cleric_creation("cleric")))
            .unwrap()
            .game;
        game
    }

    pub fn t_classes() -> HashMap<String, Class> {
        let rogue_abs = vec![abid("punch")];
        let ranger_abs = vec![abid("shoot")];
        let cleric_abs = vec![abid("heal")];
        HashMap::from_iter(vec![("rogue".to_string(),
                                 Class {
                                     abilities: rogue_abs,
                                     conditions: vec![],
                                 }),
                                ("ranger".to_string(),
                                 Class {
                                     abilities: ranger_abs,
                                     conditions: vec![],
                                 }),
                                ("cleric".to_string(),
                                 Class {
                                     abilities: cleric_abs,
                                     conditions: vec![],
                                 })])
    }

    #[test]
    fn workflow() {
        let mut creatures = HashMap::new();
        let punch = t_punch();
        let punch_id = abid("punch");
        let bob_id = cid("bob");
        let creature = Creature::build("bob", "rogue")
            .abilities(vec![punch_id.clone()])
            .build()
            .unwrap();
        creatures.insert(bob_id, creature);
        let mut abilities = HashMap::new();
        abilities.insert(punch_id.clone(), punch);
        let game = Game {
            abilities: abilities,
            classes: t_classes(),
            current_combat: None,
            creatures: creatures,
            maps: HashMap::from_iter(vec![("huge".to_string(), huge_box())]),
            current_map: Some("huge".to_string()),
        };
        let game = t_start_combat(&game, vec![bob_id]);
        let next =
            game.perform_unchecked(GameCommand::CombatAct(punch_id, DecidedTarget::Melee(bob_id)));
        let next: Game = next.expect("punch did not succeed").game;
        let _: Game = next.perform_unchecked(GameCommand::StopCombat).unwrap().game;
    }

    #[test]
    fn start_combat_not_found() {
        let game = t_game();
        let non = cid("nonexistent");
        assert_eq!(game.perform_unchecked(GameCommand::StartCombat(vec![non])),
                   Err(GameError::CreatureNotFound(non)));
    }

    #[test]
    fn combat_must_have_creatures() {
        let game = t_game();
        assert_eq!(game.perform_unchecked(GameCommand::StartCombat(vec![])),
                   Err(GameError::CombatMustHaveCreatures));
    }

    #[test]
    fn start_combat() {
        let game = t_game();
        let game =
            game.perform_unchecked(GameCommand::StartCombat(vec![cid("rogue"), cid("ranger")]))
                .unwrap()
                .game;
        assert_eq!(game.creatures.get(&cid("rogue")), None);
        assert_eq!(game.creatures.get(&cid("ranger")), None);
        assert!(game.creatures.get(&cid("cleric")).is_some());
    }

    #[test]
    fn stop_combat() {
        let game = t_game();
        let game = t_start_combat(&game, vec![cid("rogue"), cid("ranger"), cid("cleric")]);
        let game = game.perform_unchecked(GameCommand::CombatAct(abid("punch"),
                                                      DecidedTarget::Melee(cid("ranger"))))
            .unwrap()
            .game;
        assert_eq!(game.current_combat
                       .as_ref()
                       .unwrap()
                       .get_creature(cid("ranger"))
                       .unwrap()
                       .cur_health(),
                   HP(7));
        let game = game.perform_unchecked(GameCommand::StopCombat).unwrap().game;
        assert_eq!(game.get_creature(cid("ranger")).unwrap().cur_health(),
                   HP(7));
    }

    #[test]
    fn movement() {
        let game = t_game();
        let game = t_start_combat(&game, vec![cid("rogue"), cid("ranger"), cid("cleric")]);
        game.perform_unchecked(GameCommand::PathCurrentCombatCreature((1, 0, 0))).unwrap();
    }

    #[test]
    fn three_char_infinite_combat() {
        let game = t_game();
        let game = game.perform_unchecked(GameCommand::StartCombat(vec![cid("rogue"),
                                                             cid("ranger"),
                                                             cid("cleric")]))
            .unwrap()
            .game;
        let iter = |game: &Game| -> Result<Game, GameError> {
            let game = t_game_act(game, abid("punch"), DecidedTarget::Melee(cid("ranger")));
            let game = game.perform_unchecked(GameCommand::Done)?.game;
            let game = game.perform_unchecked(GameCommand::Done)?.game;
            let game = t_game_act(&game, abid("heal"), DecidedTarget::Range(cid("ranger")));
            let game = game.perform_unchecked(GameCommand::Done)?.game;
            Ok(game)
        };
        iter(&game).unwrap();
    }

}
