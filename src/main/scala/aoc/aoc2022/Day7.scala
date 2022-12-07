package aoc.aoc2022

import aoc.Aoc

import scala.collection
import scala.collection.mutable

object Day7 extends aoc.Aoc("aoc2022/input7.txt", identity):

  case class Node(
    name: String,
    parent: Option[Node],
    files: mutable.HashMap[String, Long] = mutable.HashMap.empty,
    children: mutable.ListBuffer[Node] = mutable.ListBuffer.empty,
    var total: Long = 0L
  ) {
    override def toString: String = s"<$name, dirs[${children.size}], files[${files.keys.mkString(",")}]>"
  }

  require(input.head == "$ cd /", "parsing should start from root")
  val root = Node(name = "/", parent = None)
  def build(curr: Node, remaining: List[String]): Unit = remaining match {
    case Nil => () // end of parsing
    case line :: rest =>
      line match {
        case "$ cd /" =>
          build(root, rest)
        case "$ ls" =>
          build(curr, rest)
        case "$ cd .." =>
          build(curr.parent.get, rest)
        case s"$$ cd $name" =>
          build(curr.children.find(_.name == name).get, rest)
        case s"dir $name" =>
          curr.children += Node(name, Some(curr))
          build(curr, rest)
        case s"$size $name" =>
          curr.files += name -> size.toLong
          build(curr, rest)
        case other =>
          sys.error(s"unable to parse [$other]")
      }
  }

  build(root, input.tail)

  def postOrder(curr: Node, accu: List[Node]): List[Node] = {
    val all = if (curr.children.nonEmpty) curr.children.map(postOrder(_, accu)).reduce(_ ++ _) else Nil
    val total = curr.files.values.sum + curr.children.map(_.total).sum
    curr.total = total
    all :+ curr
  }
  val list = postOrder(root, Nil)


  // just debugging
  def printPreOrder(c: Node, depth: Int): Unit = {
    val tab = List.fill(depth)("  ").mkString
    println(tab + " " + c.name + " " + c.total + " files[" + c.files.mkString(",") + "]")
    c.children.foreach(printPreOrder(_, depth + 1))
  }
  //printPreOrder(root, 1)

  // 1428881
  // 10475598

  val res1 = list.map(_.total).filter{_ < 100000}.sum
  println(s"res1: $res1")

  val free = 70000000 - root.total
  val needed = 30000000 - free
  println(s"free = $free, needed = $needed")
  val res2 = list.map(_.total).sorted.filter(_ > needed).head
  println(s"res2: $res2")


