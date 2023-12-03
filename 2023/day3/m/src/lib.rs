use itertools::Itertools;

pub fn part1(input: &str) -> usize {
    let schematic: Vec<Vec<char>> = input
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect();

    let sum = schematic
        .iter()
        .enumerate()
        .map(|(row, line)| {
            line.iter()
                .enumerate()
                .filter_map(|(col, &cell)| {
                    (!cell.is_numeric() && cell != '.').then(|| {
                        part_codes_around((row, col), &schematic)
                            .iter()
                            .sum::<usize>()
                    })
                })
                .sum::<usize>()
        })
        .sum::<usize>();
    sum
}

/// Given a position and a grid schematic, return a list of all unique part codes that are adjacent to that position.
fn part_codes_around((row, col): (usize, usize), schematic: &Vec<Vec<char>>) -> Vec<usize> {
    get_neighbour_positions((row, col))
        .iter()
        .filter_map(|(x, y)| extract_part_code(&schematic[*x].iter().collect::<String>(), *y))
        .unique()
        .collect()
}

/// Given an origin point, return a list containing every adjacent neighbour to that origin, including diagonals.
fn get_neighbour_positions((x, y): (usize, usize)) -> Vec<(usize, usize)> {
    (0..3)
        .cartesian_product(0..3)
        .filter_map(|(i, j)| (i != 1 || j != 1).then_some((x + i - 1, y + j - 1)))
        .collect()
}

/// Given a line from a schematic and a position to start on, find the nearest part code, stopping on any
/// periods, and escaping if the original position happens to also be a period.
///
/// Given a line such as: "467..114..", and the position 7, it will first check if that position contains
/// any numbers. If it doesn't, it will exit. In this case it contains the number 4.
///
/// Now it will look at every character including and after that position, to receive "4..".
/// It will only take the numerical part to get "4". This is the right side.
/// Next it will reverse the line to get "..411..764". Then it will take every character from and including
/// the total length of the line - position, which is 10 - 7 = 3, and the characters are "11..764".
/// It will then stop at the first period, to get the numerical part of "11". It will then reverse it again to
/// get back the correct part code. This is "11" and is the left side.
///
/// Then it will combine the left and right sides in that order to get the correct part code.
fn extract_part_code(line: &str, position: usize) -> Option<usize> {
    let chars: Vec<char> = line.chars().collect();
    let length = chars.len();
    if !chars[position].is_numeric() {
        return None;
    }

    let right_side: String = chars[position..]
        .iter()
        .take_while(|c| c.is_numeric())
        .collect();

    let left_side: String = chars.into_iter().rev().collect::<Vec<char>>()[length - position..]
        .iter()
        .take_while(|c| c.is_numeric())
        .collect::<Vec<&char>>()
        .iter()
        .rev()
        .copied()
        .collect();
    format!("{left_side}{right_side}").parse().ok()
}

pub fn part2(input: &str) -> usize {
    let schematic: Vec<Vec<char>> = input
        .lines()
        .map(|line| line.chars().collect::<Vec<char>>())
        .collect();

    let sum = schematic
        .iter()
        .enumerate()
        .map(|(row, line)| {
            line.iter()
                .enumerate()
                .filter_map(|(col, &cell)| {
                    (!cell.is_numeric() && cell != '.').then(|| {
                        let part_codes = part_codes_around((row, col), &schematic);
                        if part_codes.len() == 2 {
                            part_codes.iter().product()
                        } else {
                            0
                        }
                    })
                })
                .sum::<usize>()
        })
        .sum::<usize>();
    sum
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_input() {
        let result = part1(include_str!("test_input_part1.txt"));
        assert_eq!(result, 4361);
        let result = part2(include_str!("test_input_part1.txt"));
        assert_eq!(result, 467835);
    }

    #[test]
    fn puzzle_input() {
        let result = part1(include_str!("puzzle_input.txt"));
        assert_eq!(result, 550934);
        let result = part2(include_str!("puzzle_input.txt"));
        assert_eq!(result, 81997870);
    }
}
