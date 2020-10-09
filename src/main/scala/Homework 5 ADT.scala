package com.evolutiongaming.bootcamp.adt

import com.evolutiongaming.bootcamp.adt.AlgebraicDataTypes._

object AlgebraicDataTypes {
sealed trait HandType

  case object Holdem extends HandType

  case object Omaha extends HandType

  final case class Card(rank: Rank, suit: Suit)

  sealed abstract case class Board(cards: List[Card])
  object Board {
    def create(cards: List[Card]): Option[Board] =
      cards.size match {
        case 5 => Some(new Board(cards) {})
        case _ => None
      }
  }

  sealed abstract case class Hand(cards: List[Card])

  object Hand {
    def create(pockets: List[Card], gameType: HandType): Option[Hand] =
      gameType match {
        case Holdem if pockets.size == 2 => Some(new Hand(pockets) {})
        case Omaha if pockets.size == 4  => Some(new Hand(pockets) {})
        case _                           => None
      }
  }

  sealed abstract case class TestCase(board: Board, hands: List[Hand], handType: HandType)
  object TestCase {
    def create(board: Option[Board], hands: Option[List[Hand]], handType: HandType): Option[TestCase] =
      (board, hands) match {
        case (Some(board), Some(hands)) if hands.size > 0 => Some(new TestCase(board, hands, handType) {})
        case _                             => None
      }
  }
  case class TestResult(result: List[(Set[Hand], Int)])
  
    //suits
  sealed abstract class Suit(val value: Byte)

  final case object Club extends Suit(1)

  final case object Heart extends Suit(2)

  final case object Diamond extends Suit(4)

  final case object Spade extends Suit(8)

// ranks
  sealed abstract class Rank(val rank: Byte)

  case object Ace extends Rank(41)

  case object King extends Rank(37)

  case object Queen extends Rank(31)

  case object Jack extends Rank(29)

  case object Ten extends Rank(23)

  case object Nine extends Rank(19)

  case object Eight extends Rank(17)

  case object Seven extends Rank(13)

  case object Six extends Rank(11)

  case object Five extends Rank(7)

  case object Four extends Rank(5)

  case object Three extends Rank(3)

  case object Two extends Rank(2)
}
