#[derive(Debug)]
struct Ingredient {
    name: String,
    capacity: i32,
    durability: i32,
    flavor: i32,
    texture: i32,
    calories: i32,
}

impl From<&str> for Ingredient {
    fn from(s: &str) -> Self {
        let parts = s.split_whitespace().collect::<Vec<_>>();
        let name = parts[0].trim_end_matches(':').to_string();
        let capacity = parts[2].trim_end_matches(',').parse().unwrap();
        let durability = parts[4].trim_end_matches(',').parse().unwrap();
        let flavor = parts[6].trim_end_matches(',').parse().unwrap();
        let texture = parts[8].trim_end_matches(',').parse().unwrap();
        let calories = parts[10].parse().unwrap();
        Self {
            name,
            capacity,
            durability,
            flavor,
            texture,
            calories,
        }
    }
}

fn score(ingredients: &[Ingredient], amounts: &[i32]) -> (i32, i32) {
    let capacity = ingredients
        .iter()
        .zip(amounts)
        .map(|(i, a)| i.capacity * a)
        .sum::<i32>();
    let durability = ingredients
        .iter()
        .zip(amounts)
        .map(|(i, a)| i.durability * a)
        .sum::<i32>();
    let flavor = ingredients
        .iter()
        .zip(amounts)
        .map(|(i, a)| i.flavor * a)
        .sum::<i32>();
    let texture = ingredients
        .iter()
        .zip(amounts)
        .map(|(i, a)| i.texture * a)
        .sum::<i32>();
    let calories = ingredients
        .iter()
        .zip(amounts)
        .map(|(i, a)| i.calories * a)
        .sum::<i32>();
    (
        capacity.max(0) * durability.max(0) * flavor.max(0) * texture.max(0),
        calories,
    )
}

fn main() {
    let input = std::fs::read_to_string("input").unwrap();
    let ingredients: Vec<_> = input.lines().map(Ingredient::from).collect();

    let mut max_score = 0;
    let mut max_score_500_calories = 0;
    for a in 0..=100 {
        for b in 0..=(100 - a) {
            for c in 0..=(100 - a - b) {
                let d = 100 - a - b - c;
                let (score, calories) = score(&ingredients, &[a, b, c, d]);
                max_score = max_score.max(score);
                if calories == 500 {
                    max_score_500_calories = max_score_500_calories.max(score);
                }
            }
        }
    }
    println!("Part 1: {}", max_score);
    println!("Part 2: {}", max_score_500_calories);
}
