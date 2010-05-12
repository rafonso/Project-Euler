package eulerProject.solved

package cards {

  abstract sealed case class ValueCard(symbol: String, value: Int) extends Ordered[ValueCard] {
    
    def compare(other: ValueCard): Int = this.value - other.value 
    
  } 
  case object two   extends ValueCard("2",  2)
  case object three extends ValueCard("3",  3)
  case object four  extends ValueCard("4",  4)
  case object five  extends ValueCard("5",  5)
  case object six   extends ValueCard("6",  6)
  case object seven extends ValueCard("7",  7)
  case object eight extends ValueCard("8",  8)
  case object nine  extends ValueCard("9",  9)
  case object ten   extends ValueCard("T", 10)
  case object joker extends ValueCard("J", 11)
  case object queen extends ValueCard("Q", 12)
  case object king  extends ValueCard("K", 13)
  case object ace   extends ValueCard("A", 14)
  
  abstract sealed case class SuiteCard(symbol: String)
  case object diamonds extends SuiteCard("D")
  case object hearts   extends SuiteCard("H")
  case object clubs    extends SuiteCard("C")
  case object spades   extends SuiteCard("S")
  
  case class Card(suite: SuiteCard, value: ValueCard) {
    
    override def toString = value.symbol + suite.symbol
    
  }
  
  object Card {
    
    import scala.collection.immutable.Map
    
    private val values = Map(
      (two.symbol,   two), 
      (three.symbol, three), 
      (four.symbol,  four), 
      (five.symbol,  five), 
      (six.symbol,   six), 
      (seven.symbol, seven), 
      (eight.symbol, eight), 
      (nine.symbol,  nine), 
      (ten.symbol,   ten),
      (joker.symbol, joker),
      (queen.symbol, queen),
      (king.symbol,  king),
      (ace.symbol,   ace)
    )
    
    private val suites = Map(
      (diamonds.symbol, diamonds),
      (hearts.symbol,   hearts),
      (clubs.symbol,    clubs),
      (spades.symbol,   spades)    
    )
    
    private val regex = """^(.)([DHCS])$""".r
    
    @throws(classOf[IllegalArgumentException])
    def apply(string: String): Card = {
      val optMatch = regex.findFirstMatchIn(string)
      
      if(optMatch.isEmpty) {
        throw new IllegalArgumentException(string)
      }
      
      try {
        val value = optMatch.get.group(1)
        val suite = optMatch.get.group(2)
        Card(suites(suite), values(value))
      } catch {
        case e: NoSuchElementException => throw new IllegalArgumentException("Unknown Card: " + string)
      }
    }
    
  }

}

package pocker {
  
  import _root_.eulerProject.solved.cards._
  
  trait HandsMatcher {
    
    import scala.collection.mutable.Map
    import scala.collection.mutable.LinkedHashMap
    
    private def getMap: Map[ValueCard, Int] = 
      LinkedHashMap(
        (two, 0),
        (three, 0),
        (four, 0),
        (five, 0),
        (six, 0),
        (seven, 0),
        (eight, 0),
        (nine, 0),
        (ten, 0),
        (joker, 0),
        (queen, 0),
        (king, 0),
        (ace, 0)
      )
    
    protected def contabilize(hand: List[Card]): Iterable[(ValueCard, Int)] = {
      val valuesQuantity = getMap
      hand.foreach(card => valuesQuantity(card.value) = valuesQuantity(card.value) + 1)
      valuesQuantity.filter(valQty => valQty._2 > 0)
    }
    
    def code: String
    
    def evaluate(hand: List[Card]): Int
    
    def apply(hand1: List[Card], hand2: List[Card]): (Int, Int) = 
      (evaluate(hand1), evaluate(hand2))
    
  }
  
  /**
   * High Card: Highest value card.
   */
  object HighCard extends HandsMatcher {
    
    private def getValues(hand: List[Card]): List[ValueCard] = 
      super.contabilize(hand).toList.reverse.map(_._1) 
    
    private def eval(values12: List[(ValueCard, ValueCard)]) : (Int, Int) = values12 match {
      case Nil => (0, 0)
      case first :: others if(first._1 != first._2) => (first._1.value, first._2.value)
      case first :: others => eval(others)
    }
    
    def code = "HC"
    
    def evaluate(hand: List[Card]): Int = 0
    
    override def apply(hand1: List[Card], hand2: List[Card]): (Int, Int) = {
      val values1 = getValues(hand1)
      val values2 = getValues(hand2)
      val values12 = values1.zip(values2)
      
      eval(values12)
    }
    
  } 
  
  /**
   * One Pair: Two cards of the same value.
   */
  object OnePair extends HandsMatcher {
    
    def code = "OP"
    
    def evaluate(hand: List[Card]): Int = {
      val values = super
        .contabilize(hand)
        .filter(valQty => valQty._2 == 2)
        .toList
      
      if(values.size == 1) values.last._1.value else 0 
    }
    
  }

  /**
   * Two Pairs: Two different pairs.
   */
  object TwoPairs extends HandsMatcher {
    
    def code = "TP"
    
    def evaluate(hand: List[Card]): Int = {
      val values = super
        .contabilize(hand)
        .filter(valQty => valQty._2 == 2)
        .toList
      
      if(values.size == 2) values.last._1.value else 0 
    }
    
  }
  
  /**
   * Three of a Kind: Three cards of the same value.
   */
  object ThreeOfAKind extends HandsMatcher {
    
    def code = "TK"
    
    def evaluate(hand: List[Card]): Int = {
      val values = super
        .contabilize(hand)
        .filter(valQty => valQty._2 == 3)
        .toList
      
      if(values.size == 1) values.last._1.value else 0 
    }
  }
  
  /**
   * Straight: All cards are consecutive values.
   */
  object Straight extends HandsMatcher {
    
    private def eval(earlyValue: Int, cards: List[Card]): Int = cards match {
      case Nil => earlyValue
      case first :: others if(first.value.value == earlyValue + 1) => eval(first.value.value, others)
      case _ => 0
    } 
    
    def code = "ST"
    
    def evaluate(hand: List[Card]): Int = hand match {
      case first :: others => eval(first.value.value, others)
      case _ => error("Irregulas Hand: " + hand)
    }
  }

  /**
   * Flush: All cards of the same suit.
   */
  object Flush extends HandsMatcher {
    
    private def eval(orignalSuite: SuiteCard, cards: List[Card]): Int = cards match {
      case List(card) if(card.suite == orignalSuite) => card.value.value
      case first :: others if(first.suite == orignalSuite) => eval(orignalSuite, others)
      case _ => 0
    }
    
    def code = "FL"
    
    def evaluate(hand: List[Card]): Int = hand match {
      case first :: others => eval(first.suite, others)
      case _ => error("Irregular Hand: " + hand)
    }
    
  }

  /**
   * Full House: Three of a kind and a pair.
   */
  object FullHouse extends HandsMatcher {
    
    def getTripletDuple(hand: List[Card]): Option[(ValueCard, ValueCard)] = {
      val values = super.contabilize(hand)
      val values2 = values.filter(valQty => valQty._2 == 2).toList
      val values3 = values.filter(valQty => valQty._2 == 3).toList

      if(!values2.isEmpty && !values3.isEmpty) Some((values3.last._1, values2.last._1)) else None
    }

    def code = "FH"
    
    def evaluate(hand: List[Card]): Int = 0
    
    override def apply(hand1: List[Card], hand2: List[Card]): (Int, Int) = {
      val optCard1 = getTripletDuple(hand1)
      val optCard2 = getTripletDuple(hand2)
      
      if(optCard1.isEmpty && optCard2.isEmpty) (0,0)
      else if(optCard1.isDefined && optCard2.isEmpty) (optCard1.get._1.value, 0)
      else if(optCard1.isEmpty && optCard2.isDefined) (0, optCard2.get._1.value)
      else if(optCard1.get._1 != optCard1.get._2) (optCard1.get._1.value, optCard2.get._1.value)
      else (optCard1.get._2.value, optCard2.get._2.value)
    }
    
  }
  
  /**
   * Four of a Kind: Four cards of the same value.
   */
  object FourOfAKind extends HandsMatcher {

    def code = "FK"
    
    def evaluate(hand: List[Card]): Int = {
      val values = super.contabilize(hand)
      val values4 = values.filter(valQty => valQty._2 == 4).toList
      
      if(!values4.isEmpty) values4.last._1.value else 0
    }
    
  }
  
  /**
   * Straight Flush: All cards are consecutive values of same suit.
   */
  object StraightFlush extends HandsMatcher {
    
    private def eval(priorCard: Card, cards: List[Card]): Int = cards match {
      case Nil => priorCard.value.value
      case first :: others  => {
        val consecutiveValues = (first.value.value == priorCard.value.value + 1) 
        val sameSuite = (first.suite == priorCard.suite)
        
        if(consecutiveValues && sameSuite) eval(first, others) else 0 
      }
    }
    
    def code = "SF"
    
    def evaluate(hand: List[Card]): Int = hand match {
      case first :: others => eval(first, others)
      case _ => error("Irregular Hand: " + hand)
    }
    
  }
  
  /**
   * Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
   */
  object RoyalFlush extends HandsMatcher  {
    
    private def isAce(value: Int) = if(value == ace.value) 1 else 0 
    
    def code = "RF"
    
    def evaluate(hand: List[Card]): Int = 0
    
    override def apply(hand1: List[Card], hand2: List[Card]): (Int, Int) = {
      val (result1, result2) = StraightFlush(hand1, hand2)
      (isAce(result1), isAce(result2))
    }
    
  }
  
  
  object PockerMatch {
    
    val evaluators = List(RoyalFlush, StraightFlush, FourOfAKind, FullHouse, Flush, Straight, ThreeOfAKind, TwoPairs, OnePair, HighCard)
    
    private def runEvaluation(disponibleEvaluators: List[HandsMatcher], 
                              hand1: List[Card], 
                              hand2: List[Card]): (Int, Int) = disponibleEvaluators match {
      case Nil => error("This game has no winner: " + hand1 + " x " + hand2)
      case evaluator :: others => {
        val result = evaluator(hand1, hand2)
        print(" %s(%02d x %02d)".format(evaluator.code, result._1, result._2))
        if(result._1 == result._2) runEvaluation(others, hand1, hand2)
        else result
      }
    } 
    
    def apply(hands: (List[Card], List[Card])): (Int, Int) = {
      print("\n" + hands + ":")
      runEvaluation(evaluators, hands._1, hands._2)
    }
    
  }
}