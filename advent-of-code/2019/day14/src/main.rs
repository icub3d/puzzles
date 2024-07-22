use std::collections::{HashMap, VecDeque};

const INPUT: &str = include_str!("../input");

#[derive(Hash, Eq, PartialEq, Debug)]
struct Ingredient<'a> {
    name: &'a str,
    amount: usize,
}

impl<'a> From<&'a str> for Ingredient<'a> {
    fn from(s: &'a str) -> Self {
        let mut parts = s.split(' ');
        let amount = parts.next().unwrap().parse().unwrap();
        let name = parts.next().unwrap();
        Ingredient { name, amount }
    }
}

struct Recipe<'a> {
    ingredients: Vec<Ingredient<'a>>,
    result: Ingredient<'a>,
}

impl<'a> From<&'a str> for Recipe<'a> {
    fn from(s: &'a str) -> Self {
        let mut parts = s.split(" => ");
        let ingredients = parts
            .next()
            .unwrap()
            .split(", ")
            .map(Ingredient::from)
            .collect();
        let result = parts.next().unwrap();
        let mut parts = result.split(' ');
        let amount = parts.next().unwrap().parse().unwrap();
        let name = parts.next().unwrap();
        Recipe {
            ingredients,
            result: Ingredient { name, amount },
        }
    }
}

fn calculate_ore_needed(recipes: &HashMap<&str, (usize, Vec<Ingredient>)>, fuel: usize) -> usize {
    let mut needed = VecDeque::new();
    needed.push_back(Ingredient {
        name: "FUEL",
        amount: fuel,
    });
    let mut excess: HashMap<&str, usize> = HashMap::new();
    let mut ore = 0;

    while !needed.is_empty() {
        let mut ingredient = needed.pop_front().unwrap();
        let (amount, recipe) = recipes.get(&ingredient.name).unwrap();
        if let Some(excess_amount) = excess.get_mut(&ingredient.name) {
            if *excess_amount >= ingredient.amount {
                *excess_amount -= ingredient.amount;
                continue;
            } else {
                ingredient.amount -= *excess_amount;
                *excess_amount = 0;
            }
        }
        let runs = ingredient.amount.div_ceil(*amount);
        if runs * amount > ingredient.amount {
            *excess.entry(ingredient.name).or_insert(0) += runs * amount - ingredient.amount;
        }
        for i in recipe {
            if i.name == "ORE" {
                ore += i.amount * runs;
                continue;
            }
            needed.push_back(Ingredient {
                name: i.name,
                amount: i.amount * runs,
            });
        }
    }

    ore
}

fn main() {
    let recipes: HashMap<&str, (usize, Vec<Ingredient>)> = INPUT
        .lines()
        .map(|line| line.into())
        .map(|i: Recipe| (i.result.name, (i.result.amount, i.ingredients)))
        .collect();

    println!("p1: {}", calculate_ore_needed(&recipes, 1));

    let mut low = 0;
    let mut high = 1_000_000_000_000;
    while low < high {
        let mid = (low + high + 1) / 2;
        let ore = calculate_ore_needed(&recipes, mid);
        if ore > 1_000_000_000_000 {
            high = mid - 1;
        } else {
            low = mid;
        }
    }

    println!("p2: {}", low);
}
