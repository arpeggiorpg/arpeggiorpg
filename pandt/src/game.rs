use std::collections::{HashMap, HashSet};

use types::*;
use creature::*;
use combat::*;
use grid::{find_path, get_all_accessible};

#[derive(Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
pub struct Game {
    current_combat: Option<Combat>,
    abilities: HashMap<AbilityID, Ability>,
    ability_sets: HashMap<AbilitySetID, HashSet<AbilityID>>,
    creatures: HashMap<CreatureID, Creature>,
    maps: HashMap<MapName, Map>,
    current_map: Map, // maybe Option<MapName>?
}

// Generic methods for any kind of Game regardless of the CreatureState.
impl Game {
    pub fn new(ability_sets: HashMap<AbilitySetID, HashSet<AbilityID>>,
               abilities: HashMap<AbilityID, Ability>)
               -> Self {
        Game {
            abilities: abilities,
            ability_sets: ability_sets,
            current_combat: None,
            creatures: HashMap::new(),
            maps: HashMap::new(),
            current_map: vec![],
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
            (SelectMap(ref name), _) => self.select_map(name),
            (CreateCreature(c), _) => self.add_creature(c),
            (RemoveCreature(cid), _) => self.remove_creature(cid),
            (StartCombat(cids), None) => self.start_combat(cids),
            (StopCombat, Some(com)) => Ok(self.stop_combat(&com)),
            (AddCreatureToCombat(cid), Some(com)) => self.add_to_combat(&com, cid),
            (RemoveCreatureFromCombat(cid), Some(com)) => self.remove_from_combat(&com, cid),
            (Move(pt), Some(com)) => self.move_creature(&com, pt),
            (MoveOutOfCombat(cid, pt), None) => self.move_creature_ooc(cid, pt),
            (Done, Some(com)) => self.next_turn(&com),
            (Act(abid, dtarget), Some(com)) => self.act(&com, abid, dtarget),
            _ => disallowed(cmd),
        }?;
        // Design challenge: figure out a way to make this assertion unnecessary or at least less
        // necessary.
        debug_assert!(newgame == self.apply_logs(logs.clone())?,
                      "[ASSERT] log application != command performance!\nnewgame = {:?}\nlgame = \
                       {:?}\nlogs = {:?}",
                      newgame,
                      self.apply_logs(logs.clone())?,
                      logs.clone());
        Ok((newgame, logs))
    }

    fn select_map(&self, name: &MapName) -> Result<(Game, Vec<GameLog>), GameError> {
        let mut newgame = self.clone();
        let terrain = self.maps.get(name).ok_or_else(|| GameError::MapNotFound(name.clone()))?;
        newgame.current_combat = newgame.current_combat
            .map(|c| c.update_movement_options(&terrain));
        newgame.current_map = terrain.clone();
        Ok((newgame, vec![GameLog::SelectMap(name.clone())]))
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

    fn remove_creature(&self, cid: CreatureID) -> Result<(Game, Vec<GameLog>), GameError> {
        let mut newgame = self.clone();
        newgame.creatures.remove(&cid).ok_or_else(|| GameError::CreatureNotFound(cid))?;
        Ok((newgame, vec![GameLog::RemoveCreature(cid)]))
    }

    fn add_to_combat(&self,
                     combat: &Combat,
                     cid: CreatureID)
                     -> Result<(Game, Vec<GameLog>), GameError> {
        // TODO: this should probably take an initiative!
        let mut newgame = self.clone();
        let mut combat = combat.clone();
        let creature =
            newgame.creatures.remove(&cid).ok_or_else(|| GameError::CreatureNotFound(cid))?;
        combat.creatures.push(creature);
        newgame.current_combat = Some(combat);
        Ok((newgame, vec![GameLog::AddCreatureToCombat(cid)]))
    }

    /// Remove a creature from combat. If it's the last creature, the Combat will be ended.
    fn remove_from_combat(&self,
                          combat: &Combat,
                          cid: CreatureID)
                          -> Result<(Game, Vec<GameLog>), GameError> {
        let mut newgame = self.clone();
        let (combat, creature) = combat.remove_from_combat(cid)?;
        newgame.current_combat = combat;
        newgame.creatures.insert(creature.id(), creature);
        Ok((newgame, vec![GameLog::RemoveCreatureFromCombat(cid)]))
    }

    fn apply_logs(&self, logs: Vec<GameLog>) -> Result<Game, GameError> {
        let mut newgame = self.clone();
        for log in logs {
            newgame = newgame.apply_log(&log)?;
        }
        Ok(newgame)
    }

    pub fn maybe_combat(&self) -> Result<&Combat, GameError> {
        self.current_combat.as_ref().ok_or(GameError::NotInCombat)
    }

    pub fn apply_log(&self, log: &GameLog) -> Result<Game, GameError> {
        use self::GameLog::*;
        match *log {
            SelectMap(ref name) => Ok(self.select_map(name)?.0),
            AddCreature(ref c) => Ok(self.add_creature(c.clone())?.0),
            RemoveCreature(cid) => Ok(self.remove_creature(cid)?.0),
            AddCreatureToCombat(cid) => Ok(self.add_to_combat(self.maybe_combat()?, cid)?.0),
            RemoveCreatureFromCombat(cid) => {
                Ok(self.remove_from_combat(self.maybe_combat()?, cid)?.0)
            }
            CombatLog(ref cl) => {
                Ok(Game {
                    current_combat: Some(self.current_combat
                        .as_ref()
                        .ok_or(GameError::NotInCombat)?
                        .apply_log(cl, &self.current_map)?),
                    ..self.clone()
                })
            }
            CreatureLog(cid, ref cl) => {
                let mut newgame = self.clone();
                let creature = self.get_creature(cid)?.apply_log(cl)?;
                *newgame.get_creature_mut(cid)? = creature;
                Ok(newgame)
            }
            StartCombat(ref cids) => Ok(self.start_combat(cids.clone())?.0),
            StopCombat => {
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
        let (next, logs) = movement.move_creature(&self.current_map, pt)?;
        Ok((Game { current_combat: Some(next), ..self.clone() }, combat_logs_into_game_logs(logs)))
    }

    fn move_creature_ooc(&self,
                         cid: CreatureID,
                         pt: Point3)
                         -> Result<(Game, Vec<GameLog>), GameError> {
        let creature = self.get_creature(cid)?;
        let (pts, distance) = find_path(creature.pos(),
                                        creature.speed(),
                                        &self.current_map,
                                        pt).ok_or(GameError::NoPathFound)?;
        let (creature, log) = creature.set_pos_path(pts, distance)?;
        let mut newgame = self.clone();
        *newgame.get_creature_mut(cid)? = creature;
        Ok((newgame, vec![GameLog::CreatureLog(cid, log)]))
    }

    fn act(&self,
           combat: &Combat,
           abid: AbilityID,
           target: DecidedTarget)
           -> Result<(Game, Vec<GameLog>), GameError> {
        let able = combat.get_able()?;
        let ability = self.get_ability(&abid)?;
        if self.creature_has_ability(&able.combat.current_creature(), &abid)? {
            let (next, logs) = able.act(&ability, target)?;
            Ok((Game { current_combat: Some(next), ..self.clone() },
                combat_logs_into_game_logs(logs)))
        } else {
            Err(GameError::CreatureLacksAbility(able.combat.current_creature().id(), abid))
        }
    }

    fn creature_has_ability(&self,
                            creature: &Creature,
                            ability: &AbilityID)
                            -> Result<bool, GameError> {
        let abset = creature.ability_set();
        let abilities = self.ability_sets.get(&abset).ok_or(GameError::AbilitySetNotFound(abset))?;
        Ok(abilities.contains(ability))
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
    pub fn get_creature_mut(&mut self, cid: CreatureID) -> Result<&mut Creature, GameError> {
        self.creatures.get_mut(&cid).ok_or(GameError::CreatureNotFound(cid))
    }

    fn next_turn(&self, combat: &Combat) -> Result<(Game, Vec<GameLog>), GameError> {
        let (newcombat, logs) = combat.next_turn(&self.current_map)?;
        Ok((Game { current_combat: Some(newcombat), ..self.clone() },
            combat_logs_into_game_logs(logs)))
    }

    fn start_combat(&self, cids: Vec<CreatureID>) -> Result<(Game, Vec<GameLog>), GameError> {
        let mut newgame = self.clone();
        let mut creatures = vec![];
        for cid in &cids {
            let creature = newgame.creatures.remove(cid).ok_or(GameError::CreatureNotFound(*cid))?;
            creatures.push(creature);
        }
        newgame.current_combat = Some(Combat::new(creatures, &self.current_map)?);
        Ok((newgame, vec![GameLog::StartCombat(cids)]))
    }

    pub fn get_movement_options(&self, creature_id: CreatureID) -> Result<Vec<Point3>, GameError> {
        let creature = self.get_creature(creature_id)?;
        Ok(get_all_accessible(creature.pos(), &self.current_map, creature.speed()))
    }
}


#[cfg(test)]
pub mod test {
    use game::*;
    use types::test::*;
    use creature::test::*;
    use test::Bencher;
    use std::iter::FromIterator;


    pub fn t_start_combat(game: &Game, combatants: Vec<CreatureID>) -> Game {
        game.perform_unchecked(GameCommand::StartCombat(combatants)).unwrap().0
    }

    pub fn t_game_act(game: &Game, ability_id: AbilityID, target: DecidedTarget) -> Game {
        game.perform_unchecked(GameCommand::Act(ability_id, target)).unwrap().0
    }

    pub fn t_game() -> Game {
        let punch = t_punch();
        let heal = t_heal();
        let shoot = t_shoot();
        let game = Game::new(t_classes(),
                             HashMap::from_iter(vec![(abid("punch"), punch),
                                                     (abid("shoot"), shoot),
                                                     (abid("heal"), heal)]));

        let game = game.perform_unchecked(GameCommand::CreateCreature(t_rogue("rogue"))).unwrap().0;
        let game =
            game.perform_unchecked(GameCommand::CreateCreature(t_ranger("ranger"))).unwrap().0;
        let game =
            game.perform_unchecked(GameCommand::CreateCreature(t_cleric("cleric"))).unwrap().0;
        game
    }

    pub fn t_classes() -> HashMap<AbilitySetID, HashSet<AbilityID>> {
        let rogue = HashSet::from_iter(vec![abid("punch")]);
        let ranger = HashSet::from_iter(vec![abid("shoot")]);
        let cleric = HashSet::from_iter(vec![abid("heal")]);
        HashMap::from_iter(vec![(AbilitySetID::new("rogue").unwrap(), rogue),
                                (AbilitySetID::new("ranger").unwrap(), ranger),
                                (AbilitySetID::new("cleric").unwrap(), cleric)])
    }

    #[test]
    fn workflow() {
        let mut creatures = HashMap::new();
        let punch = t_punch();
        let punch_id = abid("punch");
        let bob_id = cid("bob");
        let creature = Creature::build("bob")
            .abilities(vec![punch_id.clone()])
            .ability_set(AbilitySetID::new("rogue").unwrap())
            .build()
            .unwrap();
        creatures.insert(bob_id, creature);
        let mut abilities = HashMap::new();
        abilities.insert(punch_id.clone(), punch);
        let game = Game {
            abilities: abilities,
            ability_sets: t_classes(),
            current_combat: None,
            creatures: creatures,
            maps: HashMap::new(),
            current_map: vec![],
        };
        let game = t_start_combat(&game, vec![bob_id]);
        let next = game.perform_unchecked(GameCommand::Act(punch_id, DecidedTarget::Melee(bob_id)));
        let next: Game = next.expect("punch did not succeed").0;
        let _: Game = next.stop_combat(&next.current_combat.as_ref().unwrap()).0;
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
                .0;
        assert_eq!(game.creatures.get(&cid("rogue")), None);
        assert_eq!(game.creatures.get(&cid("ranger")), None);
        assert!(game.creatures.get(&cid("cleric")).is_some());
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
            game = iter(&game).unwrap();
            game.clone()
        });
    }

}
