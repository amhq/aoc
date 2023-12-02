use std::collections::HashMap;

pub fn part1(input: &str) -> usize {
    input
        .lines()
        .map(|line| {
            let digits: Vec<char> = line.chars().filter(|c| c.is_numeric()).collect();

            let first_digit = digits.first().unwrap();
            let last_digit = digits.last().unwrap();

            format!("{first_digit}{last_digit}")
                .parse::<usize>()
                .unwrap()
        })
        .sum()
}

pub fn part2(input: &str) -> usize {
    // We aren't directly replacing the word with the number, because of some edge cases:
    // sevenine: If we were to directly replace with the number, we'd get 7ine or seve9
    // depending on the order of replacement. However, what we actually want is 79. The
    // simplest fix to this is to keep the first and last character in the word after
    // adding the digit, as it doesn't affect our solution to part 1.
    // So here, sevenine would become s7n9n, which would reduce to 79, which is what we want.
    let word_map = HashMap::from([
        ("one", "o1e"),
        ("two", "t2o"),
        ("three", "t3e"),
        ("four", "f4r"),
        ("five", "f5e"),
        ("six", "s6x"),
        ("seven", "s7n"),
        ("eight", "e8t"),
        ("nine", "n9e"),
    ]);

    let processed_input = input
        .lines()
        .map(|line| {
            word_map
                .iter()
                .fold(line.to_string(), |line, (&word, &replacement)| {
                    line.replace(word, replacement)
                })
        })
        .collect::<Vec<_>>()
        .join("\n");

    part1(&processed_input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_input() {
        let result = part1(include_str!("test_input_part1.txt"));
        assert_eq!(result, 142);
        let result = part2(include_str!("test_input_part2.txt"));
        assert_eq!(result, 281);
    }

    #[test]
    fn puzzle_input() {
        let result = part1(include_str!("puzzle_input.txt"));
        assert_eq!(result, 54605);
        let result = part2(include_str!("puzzle_input.txt"));
        assert_eq!(result, 55429);
    }
}
