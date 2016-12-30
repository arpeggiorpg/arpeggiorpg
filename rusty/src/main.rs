extern crate pandt;

use std::rc::Rc;

fn main() {
    let creat = pandt::Creature {
        name: "Bob".to_string(),
        energy: 10,
        abilities: vec![],
    };
    println!("Creature: {:?}", creat);

    let mut games = vec![];

    let creatures = vec![Rc::new(creat)];
    games.push(pandt::Game::new(creatures));
    println!("Game: {:?}", games[0]);
    let anothergame = {
        let newgame = games[0].start();
        println!("Game2: {:?}", newgame);
        newgame
    };
    games.push(anothergame);
    println!("And we can still print the old game? {:?}", games[0])
}
