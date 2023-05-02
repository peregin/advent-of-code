package leetcode.lt75

object D4MiddleList extends App {

  def middleNode(head: ListNode): ListNode = {
    var p1 = head
    var p2 = head.next // double speed pointer
    while (p2 != null) {
      p1 = p1.next
      p2 = p2.next
      if (p2 != null) p2 = p2.next
    }
    p1
  }

  val node = middleNode(ListNode.generate(1, 2, 3, 4, 5))
  println(node.x)
}
