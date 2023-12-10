use std::collections::HashMap;

use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Player {
    position: usize,
    score: usize,
}

impl Player {
    fn new(position: usize, score: usize) -> Self {
        Self { position, score }
    }
    fn roll(&mut self, v: usize) {
        self.position = (self.position + v) % 10;
        // Because our board is 0-9, the score is one greater.
        self.score += self.position + 1;
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Game {
    players: Vec<Player>,
}

impl Game {
    fn new(players: Vec<Player>) -> Self {
        Self { players }
    }
}

fn main() {
    // Not reading from input because there are only two values.
    let mut game = Game::new(vec![Player::new(4 - 1, 0), Player::new(1 - 1, 0)]);

    // Use itertools to chunk over the cycle.
    let chunker = (1..=100).cycle().chunks(3);
    let mut deterministic_dice = chunker.into_iter();
    let mut rolls = 0;
    let mut turn = 0;
    while game.players.iter().map(|p| p.score).max().unwrap() < 1000 {
        rolls += 3;
        let score = deterministic_dice.next().unwrap().sum::<usize>();
        game.players[turn].roll(score);

        // next players turn.
        turn = (turn + 1) % game.players.len();
    }

    println!(
        "p1: {}",
        game.players.iter().map(|p| p.score).min().unwrap() * rolls
    );

    // Reset the game for part 2.
    let game = Game::new(vec![Player::new(4 - 1, 0), Player::new(1 - 1, 0)]);

    // There are 3 dice, each 1-3 for each turn. We can pre-compute
    // these.
    let rolls = (0..27)
        .map(|i| i / 9 + (i / 3) % 3 + i % 3 + 3)
        .collect::<Vec<usize>>();

    // We can keep track of known results based on the positions and
    // scores so we don't have to compute them again.
    let mut known = HashMap::new();
    println!("p2: {:?}", play_games(&rolls, &mut known, &game))
}

// Recursively play games keeping track of who wins.
fn play_games(
    rolls: &[usize],
    known: &mut HashMap<Game, (usize, usize)>,
    game: &Game,
) -> (usize, usize) {
    // If someone has won, we are done.
    if game.players[0].score >= 21 {
        return (0, 1);
    }
    if game.players[1].score >= 21 {
        return (1, 0);
    }

    // If we've seen this state, we can return the results.
    if let Some(v) = known.get(game) {
        return *v;
    }

    // Otherwise, we need to go through the rolls and track the wins.
    let mut wins = (0, 0);
    for r in rolls.iter() {
        // We are swapping the first and second player here for the
        // "turn taking". We assume the first player is going now and
        // then we'll simulate in reverse.
        let mut p0 = game.players[0].clone();
        p0.roll(*r);
        let results = play_games(rolls, known, &Game::new(vec![game.players[1].clone(), p0]));

        // Increment wins, again in reverse because of the turn
        // switching.
        wins.0 += results.1;
        wins.1 += results.0;
    }

    // Save our state
    known.insert(game.clone(), wins);

    wins
}
