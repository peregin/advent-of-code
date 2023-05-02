package leetcode.lt75

object D3ReverseList extends App {

  def reverseList(head: ListNode): ListNode = {
    var prev: ListNode = null
    var curr: ListNode = head
    var next: ListNode = null
    while (curr != null) {
      next = curr.next
      curr.next = prev
      prev = curr
      curr = next
    }
    prev
  }

  val node = reverseList(ListNode.generate(1, 2, 4))
  ListNode.console(node)
}
