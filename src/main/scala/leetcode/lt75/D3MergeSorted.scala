package leetcode.lt75

object D3MergeSorted extends App {

  def mergeTwoLists(list1: ListNode, list2: ListNode): ListNode = {
    var l1  = list1
    var l2  = list2
    var head: ListNode = null
    var curr: ListNode = null
    while (l1 != null || l2 != null) {
      val n = if (l1 == null) {
        val a = l2
        l2 = null
        a
      }
      else if (l2 == null) {
        val a = l1
        l1 = null
        a
      }
      else {
        if (l1.x < l2.x) {
          val a = l1
          l1 = l1.next
          a
        } else {
          val a = l2
          l2 = l2.next
          a
        }
      }
      if (head == null) head = n
      if (curr != null) curr.next = n
      curr = n
    }
    head
  }

  val node = mergeTwoLists(ListNode(1, ListNode(2, ListNode(4))), ListNode(1, ListNode(3, ListNode(4))))
  ListNode.console(node)
}
