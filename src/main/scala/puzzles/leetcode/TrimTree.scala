package puzzles.leetcode

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object TrimTree extends App {

  def trimBST(root: TreeNode, low: Int, high: Int): TreeNode = {
    if (root == null) return null

    // If current node's value is outside the range, trim the subtree
    if (root.value < low) return trimBST(root.right, low, high)
    if (root.value > high) return trimBST(root.left, low, high)

    // Recursively trim left and right subtrees
    root.left = trimBST(root.left, low, high)
    root.right = trimBST(root.right, low, high)

    root
  }

  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
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

    var carry = 0
    var p1 = reverseList(l1)
    var p2 = reverseList(l2)
    val dummyHead = new ListNode()
    var current = dummyHead

    while (p1 != null || p2 != null) {
      val x1 = if (p1 != null) p1.x else 0
      val x2 = if (p2 != null) p2.x else 0
      val sum = x1 + x2 + carry
      carry = sum / 10
      current.next = new ListNode(sum % 10)
      current = current.next

      if (p1 != null) p1 = p1.next
      if (p2 != null) p2 = p2.next
    }

    if (carry > 0) {
      current.next = new ListNode(carry)
    }

    reverseList(dummyHead.next)
  }

  val res = addTwoNumbers(ListNode.generate(7, 2, 4, 3), ListNode.generate(5, 6, 4))
  ListNode.print(res)
}


