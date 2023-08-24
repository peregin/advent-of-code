package leetcode.daily

class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}
object P2AddTwoNumbers extends App {

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    def nodes(l: ListNode): List[Int] = if l == null then Nil else nodes(l.next) :+ l.x
    def num(l: ListNode): BigDecimal = nodes(l).map(BigDecimal.apply).reverse.zipWithIndex.map{case (n, p) => n * BigDecimal(10).pow(p)}.sum
    val a = num(l1)
    val b = num(l2)
    val n = a + b
    println(a)
    println(b)
    println(n)
    val res = n.toString.map(_.asDigit).reverse.map(ListNode(_)).toArray
    (0 until res.length-1).foreach(ix => res(ix).next = res(ix+1))
    res.head
  }

  println(addTwoNumbers(
//    ListNode(2, ListNode(4, ListNode(3))),
//    ListNode(5, ListNode(6, ListNode(4)))
    ListNode(2, ListNode(4, ListNode(9))),
    ListNode(5, ListNode(6, ListNode(4, ListNode(9))))
  ))
}
