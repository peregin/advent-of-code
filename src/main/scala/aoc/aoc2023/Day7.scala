package aoc.aoc2023

import aoc.Aoc

import scala.collection.parallel.CollectionConverters.*

object Day7 extends aoc.Aoc("aoc2023/input7_1.txt", identity):

  enum HandType(val rank: Int) extends Ordered[HandType]:
    case Five      extends HandType(6)
    case Four      extends HandType(5)
    case FullHouse extends HandType(4)
    case Three     extends HandType(3)
    case TwoPair   extends HandType(2)
    case OnePair   extends HandType(1)
    case HighCard  extends HandType(0)

    def compare(that: HandType): Int = this.rank - that.rank

  def detectStraight(card: String): HandType =
    card.groupMapReduce(identity)(_ => 1)(_ + _).values.toList.sorted match {
      case List(1, 1, 1, 1, 1) => HandType.HighCard
      case List(1, 1, 1, 2)    => HandType.OnePair
      case List(1, 2, 2)       => HandType.TwoPair
      case List(1, 1, 3)       => HandType.Three
      case List(2, 3)          => HandType.FullHouse
      case List(1, 4)          => HandType.Four
      case List(5)             => HandType.Five
      case other               => sys.error(s"unexpected $other")
    }

  def detectWithJokers(card: String): HandType =
    if (card.count(_ == 'J') > 0) card.replace("J", "").groupMapReduce(identity)(_ => 1)(_ + _).values.toList.sorted match {
      case Nil              => HandType.Five      // all jokers
      case List(1, 1, 1, 1) => HandType.OnePair   // 1 joker, all different
      case List(1, 1, 2)    => HandType.Three     // 1 joker, 1 pair
      case List(2, 2)       => HandType.FullHouse // 1 joker, 2 pairs
      case List(1, 3)       => HandType.Four      // 1 joker, 3 same
      case List(4)          => HandType.Five      // 1 joker 4 same
      case other            => sys.error(s"unexpected $other")
    }
    else detectStraight(card) // no jokers

  case class Hand(cards: String, bid: Int, handType: HandType) extends Ordered[Hand]:
    // to sort the same type of hands
    lazy val numeric = cards.map {
      case 'T' => "10"
      case 'J' => "11"
      case 'Q' => "12"
      case 'K' => "13"
      case 'A' => "14"
      case d   => s"0$d"
    }

    override def compare(that: Hand): Int =
      this.handType.compare(that.handType) match {
        case 0 => this.numeric.mkString.toInt - that.numeric.mkString.toInt
        case r => r
      }

    def strongestWithJoker(): Hand = {
      val labels = cards.toSet
      if (labels.contains('J')) {
        val max = labels.map(c => cards.replace('J', c)).map(detectStraight).max
        this.copy(handType = max)
      } else this
    }

  def score(on: List[Hand]): Int = on.zipWithIndex.map((h, ix) => h.bid * (ix + 1)).sum

  val hands = input
    .map(_.split(' ').map(_.trim) match {
      case Array(cards, bid) => Hand(cards, bid.toInt, detectStraight(cards))
    })
    .sorted

  val res1 = score(hands)
  println(s"res1: $res1") // 251106089

  val res2 = score(hands.map(_.strongestWithJoker()).sorted) // 249722254 too high
  println(s"res2: $res2")
