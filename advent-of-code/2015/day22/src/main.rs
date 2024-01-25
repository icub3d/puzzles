use std::collections::{hash_map::Entry, BinaryHeap, HashMap};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct State {
    player_hp: i32,
    player_mana: i32,
    enemy_hp: i32,
    enemy_dmg: i32,
    shield: i32,
    poison: i32,
    recharge: i32,
}

impl State {
    fn apply_effects(&mut self) {
        if self.poison > 0 {
            self.enemy_hp -= 3;
            self.poison -= 1;
        }
        if self.recharge > 0 {
            self.player_mana += 101;
            self.recharge -= 1;
        }
        if self.shield > 0 {
            self.shield -= 1;
        }
    }

    fn enemy_turn(&mut self) {
        self.apply_effects();
        if self.enemy_hp <= 0 {
            return;
        }
        if self.shield > 0 {
            self.player_hp -= std::cmp::max(1, self.enemy_dmg - 7);
        } else {
            self.player_hp -= std::cmp::max(1, self.enemy_dmg);
        }
    }

    fn magic_missile(&self) -> Self {
        let mut state = self.clone();
        state.player_mana -= 53;
        state.enemy_hp -= 4;
        state.enemy_turn();
        state
    }

    fn drain(&self) -> Self {
        let mut state = self.clone();
        state.player_mana -= 73;
        state.enemy_hp -= 2;
        state.player_hp += 2;
        state.enemy_turn();
        state
    }

    fn shield(&self) -> Self {
        let mut state = self.clone();
        state.player_mana -= 113;
        state.shield = 6;
        state.enemy_turn();
        state
    }

    fn poison(&self) -> Self {
        let mut state = self.clone();
        state.player_mana -= 173;
        state.poison = 6;
        state.enemy_turn();
        state
    }

    fn recharge(&self) -> Self {
        let mut state = self.clone();
        state.player_mana -= 229;
        state.recharge = 5;
        state.enemy_turn();
        state
    }

    fn neighbors(&self, hard: bool) -> Vec<(usize, State)> {
        let mut states = Vec::new();

        // Apply any effects that may be happening.
        let mut cur = self.clone();
        // Hard mode.
        if hard {
            cur.player_hp -= 1;
            if cur.player_hp <= 0 {
                return states;
            }
        }
        cur.apply_effects();
        if cur.player_mana >= 53 {
            states.push((53, cur.magic_missile()));
        }
        if cur.player_mana >= 73 {
            states.push((73, cur.drain()));
        }
        if cur.player_mana >= 113 && cur.shield == 0 {
            states.push((113, cur.shield()));
        }
        if cur.player_mana >= 173 && cur.poison == 0 {
            states.push((173, cur.poison()));
        }
        if cur.player_mana >= 229 && cur.recharge == 0 {
            states.push((229, cur.recharge()));
        }
        states
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct PrioritizedState {
    state: State,
    cost: usize,
}

impl PartialOrd for PrioritizedState {
    fn partial_cmp(&self, other: &PrioritizedState) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PrioritizedState {
    fn cmp(&self, other: &PrioritizedState) -> std::cmp::Ordering {
        // Reverse order so that the BinaryHeap is a min-heap.
        // self.enemy_hp.cmp(&other.enemy_hp).reverse()
        self.cost.cmp(&other.cost).reverse()
    }
}

fn p1(init: &State, hard: bool) -> usize {
    let mut queue = BinaryHeap::new();
    let mut visited = HashMap::new();
    queue.push(PrioritizedState {
        state: init.clone(),
        cost: 0,
    });
    visited.insert(init.clone(), 0);

    while let Some(PrioritizedState { state, .. }) = queue.pop() {
        let dist = visited[&state];
        if state.player_hp <= 0 {
            continue;
        }
        if state.enemy_hp <= 0 {
            return dist;
        }
        for (cost, neighbor) in state.neighbors(hard) {
            if neighbor.player_hp <= 0 && neighbor.enemy_hp > 0 {
                continue;
            }
            match visited.entry(neighbor.clone()) {
                Entry::Occupied(mut entry) => {
                    if *entry.get() > dist + cost {
                        entry.insert(dist + cost);
                        queue.push(PrioritizedState {
                            state: neighbor.clone(),
                            cost: dist + cost,
                        });
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(dist + cost);
                    queue.push(PrioritizedState {
                        state: neighbor.clone(),
                        cost: dist + cost,
                    });
                }
            }
        }
    }
    panic!("No solution found");
}

fn main() {
    let init = State {
        player_hp: 50,
        player_mana: 500,
        enemy_hp: 58,
        enemy_dmg: 9,
        shield: 0,
        poison: 0,
        recharge: 0,
    };

    println!("p1: {}", p1(&init, false));
    println!("p2: {}", p1(&init, true));
}
