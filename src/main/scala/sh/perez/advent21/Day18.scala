package sh.perez.advent21

import scala.util.parsing.combinator.JavaTokenParsers


object Day18 extends Day {
  val day = 18

  trait Node {
    var parent: Option[Pair] = None
    def setParent(parent: Pair): Unit = this.parent = Some(parent)
    def firstLeaf(isLeft: Boolean): Option[Leaf]
    def handleSplits(): Boolean
    def handleExplosions(currentHeight: Int): Boolean
    def magnitude: Int
    def duplicate: Node
  }

  object Pair {
    def apply(left: Node, right: Node): Pair = {
      val pair = new Pair(left, right)
      left.setParent(pair)
      right.setParent(pair)
      pair
    }
  }

  class Pair(var left: Node, var right: Node) extends Node {
    def isRoot(): Boolean = parent.isEmpty

    def getNode(isLeft: Boolean): Node = if (isLeft) left else right

    def firstLeaf(isLeft: Boolean): Option[Leaf] = getNode(isLeft).firstLeaf(isLeft)

    def firstValue(isLeft: Boolean): Option[Leaf] = {
      parent.flatMap(p => {
        if (p.getNode(isLeft) == this) p.firstValue(isLeft)
        else p.getNode(isLeft).firstLeaf(!isLeft)
      })
    }

    def firstLeftValue(): Option[Leaf] = firstValue(true)

    def firstRightValue(): Option[Leaf] = firstValue(false)

    def replace(old: Node, newNode: Node): Unit = {
      if (left == old) left = newNode
      else if (right == old) right = newNode
      else throw new IllegalArgumentException("Node not found")
      newNode.setParent(this)
    }

    def explode(): Unit = {
      (left, right) match {
        case (l: Leaf, r: Leaf) => {
          firstLeftValue().foreach(_.value += l.value)
          firstRightValue().foreach(_.value += r.value)
          parent.foreach(_.replace(this, new Leaf(0)))
        }
        case _ => throw new RuntimeException("can only explode node with two leaves")
      }
    }

    def reduce(): Pair = {
      if (handleExplosions(0) || handleSplits()) reduce()
      this
    }

    def handleSplits(): Boolean = left.handleSplits() || right.handleSplits()

    def handleExplosions(currentHeight: Int = 0): Boolean = {
      if (currentHeight >= 4) {
        explode()
        true
      } else {
        left.handleExplosions(currentHeight + 1) || right.handleExplosions(currentHeight + 1)
      }
    }

    def magnitude: Int = 3 * left.magnitude + 2 * right.magnitude

    def add(other: Pair): Pair = Pair(this, other)

    def duplicate: Pair = Pair(left.duplicate, right.duplicate)

    override def toString() = s"[$left, $right]"
  }

  object Leaf {
    def apply(value: Int): Leaf = new Leaf(value)
  }

  class Leaf(var value: Int) extends Node {
    override def toString() = value.toString

    def firstLeaf(isLeft: Boolean): Option[Leaf] = Some(this)

    def handleExplosions(currentHeight: Int): Boolean = false

    def handleSplits(): Boolean = {
      val shouldSplit = value >= 10
      if (shouldSplit) split()
      shouldSplit
    }

    def magnitude: Int = value

    def duplicate: Leaf = Leaf(value)

    def split(): Unit = {
      parent.foreach(_.replace(this, Pair(Leaf(value / 2), Leaf((value + 1) / 2))))
    }
  }

  object NodeParser extends JavaTokenParsers {
    def node(): Parser[Node] = leaf() | pair()
    def pair(): Parser[Pair] = ("[" ~> node() <~ ",") ~ (node() <~ "]") ^^ { case x ~ y => Pair(x, y) }
    def leaf(): Parser[Leaf] = wholeNumber ^^ { case x => Leaf(x.toInt) }

    def parseLine(line: String): Pair = parseAll(pair(), line) match {
      case Success(result, _) => result
      case failure: NoSuccess => throw new RuntimeException(failure.msg)
    }
  }

  def inputNumbers = inputLines().map(NodeParser.parseLine).toList

  def solveFirst(): Int = inputNumbers.reduce(_.add(_).reduce()).magnitude

  def solveSecond(): Int = {
    val allNumbers = for (x <- inputNumbers; y <- inputNumbers) yield x.duplicate.add(y.duplicate).reduce()
    allNumbers.map(_.magnitude).max
  }
}
