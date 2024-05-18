
fn main() {
    let input = include_str!("../../resources/aoc2017/input1.txt");
    println!("part1 is: {:?}", part1(input));
    println!("part2 is: {:?}", part2(input));
}

fn part1(input: &str) -> u32 {
    let part1 = input.chars().zip(input.chars().cycle().skip(1))
        .filter(|(a, b)| a == b)
        .map(|(a, _)| a.to_digit(10).unwrap());
    part1.sum::<u32>()
}

fn part2(input: &str) -> u32 {
    let part2 = input.chars().zip(input.chars().cycle().skip(input.len()/2))
        .filter(|(a, b)| a == b)
        .map(|(a, _)| a.to_digit(10).unwrap());
    part2.sum::<u32>()
}

#[cfg(test)]
mod tests {
    use super::*;

    static INPUT: &str = include_str!("../../resources/aoc2017/input1.txt");

    #[test]
    fn solve_part1() {
        assert_eq!(part1("1122"), 3);
        assert_eq!(part1(INPUT), 1136);
    }

    #[test]
    fn solve_part2() {
        assert_eq!(part2("1122"), 0);
        assert_eq!(part2(INPUT), 1092);
    }
}