
fn main() {
    let input = include_str!("../../resources/aoc2017/input1.txt");
    //println!("input is: {:?}", input);
    let part1 = input.chars().zip(input.chars().cycle().skip(1))
        .filter(|(a, b)| a == b)
        .map(|(a, _)| a.to_digit(10).unwrap());
    println!("part1 is: {:?}", part1.sum::<u32>());

    let part2 = input.chars().zip(input.chars().cycle().skip(input.len()/2))
        .filter(|(a, b)| a == b)
        .map(|(a, _)| a.to_digit(10).unwrap());
    println!("part2 is: {:?}", part2.sum::<u32>());
}