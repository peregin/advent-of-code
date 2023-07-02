

fn main() {
    let input = include_str!("../../resources/aoc2022/input1.txt");
    let back: Vec<&str> = input.lines().collect();
    let mut caps: Vec<i32> = back
        .split(|s| s.is_empty())
        .map(|s| s.iter().map(|i| i.trim().parse::<i32>().unwrap()).sum())
        .collect();
    println!("caps {:?}", caps);

    let res1 = caps.iter().max().unwrap();
    println!("res1 {}", res1);

    caps.sort();
    let res2: i32 = caps.iter().rev().take(3).sum();
    println!("res2 {:?}", res2);
}
