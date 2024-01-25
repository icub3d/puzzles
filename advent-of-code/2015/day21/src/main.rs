struct Character {
    health: i32,
    damage: i32,
    armor: i32,
}

struct Item {
    name: &'static str,
    cost: i32,
    damage: i32,
    armor: i32,
}

static WEAPONS: [Item; 5] = [
    Item {
        name: "Dagger",
        cost: 8,
        damage: 4,
        armor: 0,
    },
    Item {
        name: "Shortsword",
        cost: 10,
        damage: 5,
        armor: 0,
    },
    Item {
        name: "Warhammer",
        cost: 25,
        damage: 6,
        armor: 0,
    },
    Item {
        name: "Longsword",
        cost: 40,
        damage: 7,
        armor: 0,
    },
    Item {
        name: "Greataxe",
        cost: 74,
        damage: 8,
        armor: 0,
    },
];

static ARMORS: [Item; 6] = [
    Item {
        name: "Leather",
        cost: 13,
        damage: 0,
        armor: 1,
    },
    Item {
        name: "Chainmail",
        cost: 31,
        damage: 0,
        armor: 2,
    },
    Item {
        name: "Splintmail",
        cost: 53,
        damage: 0,
        armor: 3,
    },
    Item {
        name: "Bandedmail",
        cost: 75,
        damage: 0,
        armor: 4,
    },
    Item {
        name: "Platemail",
        cost: 102,
        damage: 0,
        armor: 5,
    },
    Item {
        name: "No Armor",
        cost: 0,
        damage: 0,
        armor: 0,
    },
];

static RINGS: [Item; 7] = [
    Item {
        name: "Damage +1",
        cost: 25,
        damage: 1,
        armor: 0,
    },
    Item {
        name: "Damage +2",
        cost: 50,
        damage: 2,
        armor: 0,
    },
    Item {
        name: "Damage +3",
        cost: 100,
        damage: 3,
        armor: 0,
    },
    Item {
        name: "Defense +1",
        cost: 20,
        damage: 0,
        armor: 1,
    },
    Item {
        name: "Defense +2",
        cost: 40,
        damage: 0,
        armor: 2,
    },
    Item {
        name: "Defense +3",
        cost: 80,
        damage: 0,
        armor: 3,
    },
    Item {
        name: "No Ring",
        cost: 0,
        damage: 0,
        armor: 0,
    },
];

fn main() {
    let mut player = Character {
        health: 100,
        damage: 0,
        armor: 0,
    };

    let boss = Character {
        health: 104,
        damage: 8,
        armor: 1,
    };

    let mut min = i32::MAX;
    for armor in ARMORS.iter() {
        for weapon in WEAPONS.iter() {
            for ring1 in RINGS.iter() {
                for ring2 in RINGS.iter() {
                    if ring1.name == ring2.name {
                        continue;
                    }

                    player.damage = weapon.damage + ring1.damage + ring2.damage;
                    player.armor = armor.armor + ring1.armor + ring2.armor;
                    let cost = weapon.cost + armor.cost + ring1.cost + ring2.cost;

                    if simulate(&player, &boss) {
                        if cost < min {
                            min = cost;
                        }
                    }
                }
            }
        }
    }
    println!("p1: {}", min);

    let mut max = i32::MIN;
    for armor in ARMORS.iter() {
        for weapon in WEAPONS.iter() {
            for ring1 in RINGS.iter() {
                for ring2 in RINGS.iter() {
                    if ring1.name == ring2.name {
                        continue;
                    }

                    player.damage = weapon.damage + ring1.damage + ring2.damage;
                    player.armor = armor.armor + ring1.armor + ring2.armor;
                    let cost = weapon.cost + armor.cost + ring1.cost + ring2.cost;

                    if !simulate(&player, &boss) {
                        if cost > max {
                            max = cost;
                        }
                    }
                }
            }
        }
    }
    println!("p2: {}", max);
}

fn simulate(player: &Character, boss: &Character) -> bool {
    let mut b = boss.health;
    let mut p = player.health;
    loop {
        b -= player.damage - boss.armor;
        if b <= 0 {
            return true;
        }

        p -= boss.damage - player.armor;
        if p <= 0 {
            return false;
        }
    }
}
