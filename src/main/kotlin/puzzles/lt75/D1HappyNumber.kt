package puzzles.lt75

fun isHappy(n: Int): Boolean {
    val seen = mutableSetOf<Int>()
    fun next(n: Int): Int = n.toString().map { Character.getNumericValue(it) }.map { it * it }.sum()
    val seq = generateSequence(n) { next(it) }
    val last = seq.takeWhile {
        val go = !seen.contains(it) && it != 1
        seen.add(it)
        go
    }.lastOrNull()
    return last?.let { next(it) == 1 } ?: true
}

fun main() {
    val n = 1
    println("isHappy $n -> ${isHappy(n)}")
}
