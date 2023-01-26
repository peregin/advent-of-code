package puzzles

fun main() {
    val nums = listOf(6, 32, 31).map { Integer.toBinaryString(it).padStart(16, '0') }.fold(""){ a, b -> a + b}
    println("res: $nums")
}