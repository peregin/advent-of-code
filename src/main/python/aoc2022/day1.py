def sol():
    # read file
    with open("../../resources/aoc2022/input1.txt") as f:
        elves = f.read().split("\n\n")
        calories = []
        for elf in elves:
            cals = map(int, elf.split("\n"))
            calories.append(sum(cals))
        print(f"Part 1: {max(calories)}")
        print(f"Part 2: {sum(sorted(calories, reverse=True)[:3:1])}")


if __name__ == '__main__':
    sol()
