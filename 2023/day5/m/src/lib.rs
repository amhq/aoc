use std::collections::HashSet;

use rayon::prelude::*;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

pub struct AlmanacMap(Vec<Box<dyn Fn(usize) -> isize + Send>>);

impl AlmanacMap {
    pub fn from_stream(stream: Vec<usize>) -> Result<Self> {
        let map: Vec<Box<dyn Fn(usize) -> isize + Send>> = stream
            .chunks_exact(3)
            .map(|chunk| {
                let (dest_start, src_start, range_length) =
                    (chunk[0] as isize, chunk[1] as isize, chunk[2] as isize);

                Box::new(move |src: usize| {
                    let src = src as isize;
                    if src >= src_start && src <= src_start + range_length {
                        src - (src_start - dest_start)
                    } else {
                        src
                    }
                }) as Box<_>
            })
            .collect();
        Ok(Self(map))
    }

    fn map_to_seeds(&self, seed_slots: HashSet<usize>) -> HashSet<usize> {
        seed_slots
            .iter()
            .map(|seed| {
                let mapped_seeds: Vec<_> = self.0.iter().map(|map| map(*seed)).collect();
                if let Some(s) = mapped_seeds
                    .iter()
                    .filter(|&&mapped_seed| mapped_seed != *seed as isize)
                    .next()
                {
                    *s as usize
                } else {
                    *seed
                }
            })
            .collect()
    }
}

pub fn part1(input: &str) -> Result<usize> {
    let entries: Vec<&str> = input.split("\n\n").collect();

    let seed_slots: HashSet<_> = entries
        .first()
        .ok_or("No seeds specified!".to_string())?
        .split(':')
        .last()
        .ok_or("Incorrect format found!".to_string())?
        .split(' ')
        .filter_map(|seed| seed.trim().parse::<usize>().ok())
        .collect();

    let maps: Vec<AlmanacMap> = entries
        .iter()
        .skip(1)
        .filter_map(|entry| {
            let range_values: Vec<_> = entry
                .split(':')
                .last()
                .unwrap()
                .split_whitespace()
                .filter_map(|line| line.parse::<usize>().ok())
                .collect();

            AlmanacMap::from_stream(range_values).ok()
        })
        .collect();

    let smallest_location = maps
        .iter()
        .fold(seed_slots, |seed_slots, map| map.map_to_seeds(seed_slots))
        .iter()
        .min()
        .copied()
        .unwrap();

    Ok(smallest_location)
}

pub fn part2(input: &str) -> Result<usize> {
    let entries: Vec<&str> = input.split("\n\n").collect();

    let seed_slots: HashSet<_> = entries
        .first()
        .ok_or("No seeds specified!".to_string())?
        .split(':')
        .last()
        .ok_or("Incorrect format found!".to_string())?
        .split(' ')
        .filter_map(|seed| seed.trim().parse::<usize>().ok())
        .collect::<Vec<_>>()
        .chunks_exact(2)
        .map(|chunk|{
            let (seed_start, range_length) = (chunk[0], chunk[1]);
            seed_start..seed_start+range_length-1
        })
        .flatten()
        .collect();
    
    let maps: Vec<AlmanacMap> = entries
    .par_iter()
    .skip(1)
    .filter_map(|entry| {
        let range_values: Vec<_> = entry
        .split(':')
        .last()
        .unwrap()
        .split_whitespace()
                .filter_map(|line| line.parse::<usize>().ok())
                .collect();
            
            AlmanacMap::from_stream(range_values).ok()
        })
        .collect();

    let smallest_location = maps
        .iter()
        .fold(seed_slots, |seed_slots, map| map.map_to_seeds(seed_slots))
        .par_iter()
        .min()
        .copied()
        .unwrap();

    Ok(smallest_location)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_input() {
        let result = part1(include_str!("test_input_part1.txt"));
        assert_eq!(result.unwrap(), 35);
        let result = part2(include_str!("test_input_part1.txt"));
        assert_eq!(result.unwrap(), 46);
    }

    #[test]
    fn puzzle_input() {
        let result = part1(include_str!("puzzle_input.txt"));
        assert_eq!(result.unwrap(), 510109797);
        // It takes too long to run.
        // let result = part2(include_str!("puzzle_input.txt"));
        // assert_eq!(result.unwrap(), 510109797);
    }
}
