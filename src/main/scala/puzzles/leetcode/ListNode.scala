package puzzles.leetcode

/**
 * Definition for singly-linked list.
 */
class ListNode(_x: Int = 0, _next: ListNode = null) {
  var next: ListNode = _next
  var x: Int = _x
}

object ListNode {

  def console(n: ListNode): Unit = {
    var node = n
    while (node != null) {
      Console.println(node.x)
      node = node.next
    }
  }

  def generate(items:Int *): ListNode = {
    items.foldLeft(List.empty[ListNode]){(list, item) =>
      val on = ListNode(item)
      list.lastOption.foreach(_.next = on)
      list :+ on
    }.head
  }
}