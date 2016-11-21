extern crate pandt;

fn main() {
    let creat = pandt::Creature {
        name: "Bob".to_string(),
        energy: 10,
        abilities: vec![],
    };
    println!("Creature: {:?}", creat);
    let creatures = vec![&creat];
    let game = pandt::Game::new(&creatures);
    println!("Game: {:?}", game);
    let game2 = game.start();
    println!("Game2: {:?}", game2);
    println!("Current creature name: {:?}", game2.current_creature());
}
