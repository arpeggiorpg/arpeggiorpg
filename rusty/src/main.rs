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
    games.push(pandt::GameWithState::GS(pandt::Game::new(creatures)));
    println!("Game: {:?}", games[0]);
    let anothergame = {
        if let pandt::GameWithState::GS(ref game0) = games[0] {
            let newgame = pandt::GameWithState::PCA(game0.start());
            println!("Game2: {:?}", newgame);
            newgame
        } else { panic!();}
    };
    games.push(anothergame);
    println!("And we can still print the old game? {:?}", games[0])
}
