#![allow(dead_code)]
#![feature(test)]
extern crate test;

use std::collections::HashSet;
use std::ops::BitXor;

use rayon::prelude::*;

fn p1_simple(input: &str) -> usize {
    input
        .lines()
        .filter(|line| {
            // Inserting into a HashSet returns false if the value
            // already exists. We can use this feature to check if a
            // word has already been seen. If it has, all() will
            // return false and the line will be filtered out.
            let mut seen = HashSet::new();
            line.split_whitespace().all(|word| seen.insert(word))
        })
        .count()
}

fn p2_simple(input: &str) -> usize {
    input
        .lines()
        .filter(|line| {
            // We do the same as above, but we sort the characters in
            // each word before inserting.
            let mut seen = HashSet::new();
            line.split_whitespace().all(|word| {
                let mut word = word.chars().collect::<Vec<_>>();
                word.sort();
                seen.insert(word)
            })
        })
        .count()
}

fn p1_self_iter(input: &str) -> usize {
    // At some point every rustacean questions whether they can do
    // better than the iters(). Let's see if a C like solution is
    // faster.
    let mut total = 0;
    let mut seen = HashSet::new();
    let input = input.as_bytes();

    let mut i = 0;
    while i < input.len() {
        let start = i;

        // Skip to next word boundary.
        while input[i] != b'\n' && input[i] != b' ' {
            i += 1;
        }

        // Check to see if we've seen this word before.
        if !seen.insert(&input[start..i]) {
            // If we have, this line is done, move to the next.
            while input[i] != b'\n' {
                i += 1;
            }
            i += 1;
            seen = HashSet::new();
            continue;
        }

        // Otherwise, if we are at the end, there were no repeats so
        // we increment our count.
        if input[i] == b'\n' {
            total += 1;
            seen = HashSet::new();
        }
        i += 1;
    }

    total
}

fn p1_contains(input: &str) -> usize {
    // Giving a capacity to the vector took me from about 60ms to 48ms.
    // Moving the vector outside the loop took me from 42ms to 32ms.
    //
    // These improvements can be thought of as memory managment
    // improvements. The fewer allocs/frees that occur, the faster the
    // program will run.
    let mut seen = Vec::with_capacity(16);

    input.lines().fold(0, |acc, line| {
        // Using an iter and vec took me from 140ms to 60ms. Again,
        // this is a memory management improvement. We aren't
        // collect()'ing and can stop early.
        for word in line.split_ascii_whitespace() {
            if seen.contains(&word) {
                seen.clear();
                return acc;
            }
            seen.push(word);
        }
        seen.clear();
        acc + 1
    })
    // Using folding instead of filter + count took me from 48ms to
    // 42ms. I get similar gains using sum.
}

fn p2_contains(input: &str) -> usize {
    // Similar to above, but we sort the characters in each word
    // before checking and pushing.
    let mut seen = Vec::with_capacity(16);
    input.lines().fold(0, |acc, line| {
        let words = line.split_ascii_whitespace();
        for word in words {
            let mut chars = word.chars().collect::<Vec<_>>();
            chars.sort();

            if seen.contains(&chars) {
                return acc;
            }
            seen.push(chars);
        }
        seen.clear();
        acc + 1
    })
}

fn p1_rayon(input: &str) -> usize {
    // Can we use rayon for performan improvements? This is basically
    // the same as p1_contains but using par_lines() instead..
    input
        .par_lines()
        .fold(
            || 0,
            |acc, line| {
                let mut seen = Vec::with_capacity(16);
                for word in line.split_ascii_whitespace() {
                    if seen.contains(&word) {
                        return acc;
                    }
                    seen.push(word);
                }
                acc + 1
            },
        )
        .sum()
}

fn p2_rayon(input: &str) -> usize {
    // Can we use rayon for performan improvements? This is basically
    // the same as p1_contains but using par_lines() instead.
    input
        .par_lines()
        .fold(
            || 0,
            |acc, line| {
                let mut seen = Vec::with_capacity(16);
                let words = line.split_ascii_whitespace();
                for word in words {
                    let mut chars = word.chars().collect::<Vec<_>>();
                    chars.sort();

                    if seen.contains(&chars) {
                        return acc;
                    }
                    seen.push(chars);
                }
                acc + 1
            },
        )
        .sum()
}

fn p1_maneatingape(input: &str) -> usize {
    // Maneatingape's solution is using a hashset but with a custom
    // hasher that is substantially faster than the default hasher at
    // the cost of security.
    let mut seen = HashSet::with_capacity_and_hasher(100, BuildFxHasher);
    input
        .lines()
        .filter(|line| {
            seen.clear();
            line.split_ascii_whitespace()
                .all(|token| seen.insert(token.as_bytes()))
        })
        .count()
}

fn p2_maneatingape(input: &str) -> usize {
    // Same as above but use a fequency count to find anagrams.
    fn convert(token: &str) -> [u8; 26] {
        let mut freq = [0; 26];
        for b in token.bytes() {
            freq[(b - b'a') as usize] += 1;
        }
        freq
    }

    let mut seen = HashSet::with_capacity_and_hasher(100, BuildFxHasher);
    input
        .lines()
        .filter(|line| {
            seen.clear();
            line.split_ascii_whitespace()
                .all(|token| seen.insert(convert(token)))
        })
        .count()
}

fn p1_gordin508(lines: &str) -> Option<i64> {
    // Gordin508's solution uses a bloom filter. See wikipedia for
    // more information: https://en.wikipedia.org/wiki/Bloom_filter
    let mut seen = WordBloomFilter::new();
    Some(
        lines
            .lines()
            .map(|line| password_valid(&mut seen, line))
            .filter(|b| *b)
            .count() as i64,
    )
}

fn p2_gordin508(lines: &str) -> Option<i64> {
    let mut seen = SimpleUnorderedBloomFilter::new();
    Some(
        lines
            .lines()
            .map(|line| password_valid(&mut seen, line))
            .filter(|b| *b)
            .count() as i64,
    )
}

fn main() {
    let input = include_str!("../input");

    // I did some analysis here.
    let (longest, words, lines) = input
        .lines()
        .fold((0, 0, 0), |(longest, words, lines), line| {
            let ww = line.split_ascii_whitespace().collect::<Vec<_>>();
            let max = ww.iter().map(|w| w.len()).max().unwrap_or(0);
            (max.max(longest), ww.len().max(words), lines + 1)
        });
    println!("longest: {}, words: {}, lines: {}", longest, words, lines);

    let now = std::time::Instant::now();
    println!("p1: {} ({:?})", p1_simple(input), now.elapsed());

    let now = std::time::Instant::now();
    println!("p2: {} ({:?})", p2_simple(input), now.elapsed());
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    static INPUT: &str = include_str!("../input");

    #[test]
    fn test_all_p1() {
        // Verify that all solutions produce the same result.
        assert_eq!(p1_simple(INPUT), 386);
        assert_eq!(p1_contains(INPUT), 386);
        assert_eq!(p1_maneatingape(INPUT), 386);
        assert_eq!(p1_gordin508(INPUT), Some(386));
        assert_eq!(p1_rayon(INPUT), 386);
        assert_eq!(p1_self_iter(INPUT), 386);
    }

    #[test]
    fn test_all_p2() {
        // Verify that all solutions produce the same result.
        assert_eq!(p2_simple(INPUT), 208);
        assert_eq!(p2_contains(INPUT), 208);
        assert_eq!(p2_maneatingape(INPUT), 208);
        assert_eq!(p2_gordin508(INPUT), Some(208));
        assert_eq!(p2_rayon(INPUT), 208);
    }

    // The rest runs benchmarks on the solutions.

    #[bench]
    fn bench_p1_simple(b: &mut Bencher) {
        b.iter(|| p1_simple(INPUT));
    }

    #[bench]
    fn bench_p1_contains(b: &mut Bencher) {
        b.iter(|| p1_contains(INPUT));
    }

    #[bench]
    fn bench_p1_self_iter(b: &mut Bencher) {
        b.iter(|| p1_self_iter(INPUT));
    }

    #[bench]
    fn bench_p1_rayon(b: &mut Bencher) {
        b.iter(|| p1_rayon(INPUT));
    }

    #[bench]
    fn bench_p1_maneatingape(b: &mut Bencher) {
        b.iter(|| p1_maneatingape(INPUT));
    }

    #[bench]
    fn bench_p1_gordin508(b: &mut Bencher) {
        b.iter(|| p1_gordin508(INPUT));
    }

    #[bench]
    fn bench_p2_simple(b: &mut Bencher) {
        b.iter(|| p2_simple(INPUT));
    }

    #[bench]
    fn bench_p2_contains(b: &mut Bencher) {
        b.iter(|| p2_contains(INPUT));
    }

    #[bench]
    fn bench_p2_rayon(b: &mut Bencher) {
        b.iter(|| p2_rayon(INPUT));
    }

    #[bench]
    fn bench_p2_maneatingape(b: &mut Bencher) {
        b.iter(|| p2_maneatingape(INPUT));
    }

    #[bench]
    fn bench_p2_gordin508(b: &mut Bencher) {
        b.iter(|| p2_gordin508(INPUT));
    }
}

// The following code comes from maneatingape's solution:
// https://github.com/maneatingape/advent-of-code-rust/blob/main/src/util/hash.rs

#[derive(Clone, Copy, Default)]
pub struct BuildFxHasher;

impl std::hash::BuildHasher for BuildFxHasher {
    type Hasher = FxHasher;

    #[inline]
    fn build_hasher(&self) -> Self::Hasher {
        FxHasher { hash: 0 }
    }
}

const K: u64 = 0x517cc1b727220a95;

pub struct FxHasher {
    hash: u64,
}

impl FxHasher {
    #[inline]
    fn add(&mut self, i: u64) {
        self.hash = self.hash.rotate_left(5).bitxor(i).wrapping_mul(K);
    }
}

impl std::hash::Hasher for FxHasher {
    #[inline]
    fn write(&mut self, mut bytes: &[u8]) {
        while bytes.len() >= 8 {
            self.add(u64::from_ne_bytes(bytes[..8].try_into().unwrap()));
            bytes = &bytes[8..];
        }
        if bytes.len() >= 4 {
            self.add(u32::from_ne_bytes(bytes[..4].try_into().unwrap()) as u64);
            bytes = &bytes[4..];
        }
        if bytes.len() >= 2 {
            self.add(u16::from_ne_bytes(bytes[..2].try_into().unwrap()) as u64);
            bytes = &bytes[2..];
        }
        if !bytes.is_empty() {
            self.add(bytes[0] as u64);
        }
    }

    #[inline]
    fn write_u8(&mut self, i: u8) {
        self.add(i as u64);
    }

    #[inline]
    fn write_u16(&mut self, i: u16) {
        self.add(i as u64);
    }

    #[inline]
    fn write_u32(&mut self, i: u32) {
        self.add(i as u64);
    }

    #[inline]
    fn write_u64(&mut self, i: u64) {
        self.add(i);
    }

    #[inline]
    fn write_usize(&mut self, i: usize) {
        self.add(i as u64);
    }

    #[inline]
    fn finish(&self) -> u64 {
        self.hash
    }
}

// The following code comes from gordin508's solution:
// https://gist.github.com/Gordin508/fba1aa154f92b8bfd136c742c695a683
// drop-in replacement for HashSets
trait BloomFilter<'a> {
    fn new() -> Self;
    fn insert(&mut self, word: &'a str) -> bool;
    fn clear(&mut self);
}

// DS to store words as references to strings
struct WordBloomFilter<'a> {
    bitmap: u32,
    words: Vec<&'a str>,
}

// DS to store words as unordered vec of chars
// (uses u8 internally as all inputs or 'a'..='z'
struct SimpleUnorderedBloomFilter {
    bitmap: u64,
    words: Vec<[u8; 26]>,
}

impl<'a> BloomFilter<'a> for WordBloomFilter<'a> {
    fn new() -> WordBloomFilter<'a> {
        WordBloomFilter {
            bitmap: 0,
            words: Vec::new(),
        }
    }

    fn insert(&mut self, word: &'a str) -> bool {
        // RESOLUTION is how many characters we consider for our filter
        // instead of using RESOLUTION hash functions, we simply consider the first RESOLUTION chars
        // so hash_1(word) == word[0], hash_2(word) == word[1] ...
        // The value 2 is experimentally chosen
        const RESOLUTION: usize = 2;
        let mut nums = [0u32; RESOLUTION];
        for (i, n) in word.bytes().take(RESOLUTION).enumerate() {
            nums[i] = 1 << (n - 0x61); // 0x61 == 'a'
        }
        if nums.iter().all(|n| self.bitmap & *n != 0 || *n == 0)
            && self.words.iter().any(|stored| *stored == word)
        {
            return false;
        }
        for n in nums {
            self.bitmap |= n;
        }

        self.words.push(word);
        true
    }

    fn clear(&mut self) {
        self.bitmap = 0;
        self.words.clear();
    }
}

impl<'a> BloomFilter<'a> for SimpleUnorderedBloomFilter {
    fn new() -> SimpleUnorderedBloomFilter {
        SimpleUnorderedBloomFilter {
            bitmap: 0,
            words: Vec::new(),
        }
    }

    fn insert(&mut self, word: &str) -> bool {
        // well, this once was like a bloom filter, but all my 'optimizations'
        // made the code slower
        // My guess is that my original ideas would have been
        // faster if the passphrases were much longer

        // convert word into an array of character counts
        let mut charcounts = [0u8; 26];

        for b in word.bytes().map(|c| c - 0x61) {
            charcounts[b as usize] += 1;
        }
        // we trust that no word is longer than 63 characters
        // if we had such long words, we could simply omit this check
        // as it saves almost no time anyway
        let wordlen_bit = 1 << (word.len() - 1);
        if wordlen_bit & self.bitmap > 0 && self.words.iter().any(|stored| *stored == charcounts) {
            return false;
        }
        self.bitmap |= wordlen_bit;
        self.words.push(charcounts);
        true
    }

    fn clear(&mut self) {
        self.bitmap = 0;
        self.words.clear();
    }
}

// passphrase policy of part 1
fn password_valid_no_repeats(passphrase: &str) -> bool {
    let mut seen = WordBloomFilter::new();
    password_valid(&mut seen, passphrase)
}

// passphrase policy of part 2
fn password_valid_no_anagrams(passphrase: &str) -> bool {
    let mut seen = SimpleUnorderedBloomFilter::new();
    password_valid(&mut seen, passphrase)
}

// passphrase policy is based on first argument
// This function allows to reuse the 'seen' argument across invocations.
// This increases speed, as the filter is internally backed by a vector,
// which is costly if rapidly reinitialized.
fn password_valid<'a, T: BloomFilter<'a>>(seen: &mut T, passphrase: &'a str) -> bool {
    let result = passphrase.split(' ').all(|word| seen.insert(word));
    seen.clear();
    result
}
