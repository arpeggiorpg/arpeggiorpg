extern crate pandt;

fn main()  {
    let creat = pandt::Creature { name: "Bob".to_string(), energy: 10, abilities: vec![]};
    println!("Creature: {:?}", creat);
    let game = pandt::Game::new(vec![creat]);
    println!("Game: {:?}", game);
    let game2 = game.start();
    println!("Game2: {:?}", game2);
    println!("Current creature name: {:?}", game2.current_creature_name());


    // GameR
    let creatr = pandt::Creature { name: "BobR".to_string(), energy: 10, abilities: vec![]};
    let creatures = vec![&creatr];
    let gamer = pandt::GameR::new(&creatures);
    println!("GameR: {:?}", gamer);
    println!("GameR current creature: {:?}", gamer.current_creature());
}
