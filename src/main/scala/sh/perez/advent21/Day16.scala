package sh.perez.advent21

object Day16 extends Day {
  val day = 16

  object BitStream {
    def empty: BitStream = new BitStream("")
    def apply(input: String): BitStream = {
      val isBinary = input.forall(v => v == '0' || v == '1')
      val bits = if (isBinary) input else
        input.map(v => "%04d".format(BigInt(v.toString, 16).toString(2).toInt)).mkString
      new BitStream(bits)
    }
  }
  class BitStream(var bits: String) {
    def apply(n: Int): Char = bits(n)

    def +(other: BitStream): BitStream = new BitStream(bits + other.bits)

    def size: Int = bits.length

    def take(n: Int) = {
      val (left, right) = bits.splitAt(n)
      bits = right
      new BitStream(left)
    }

    def toInt: Int = Integer.parseInt(bits, 2)

    def toBigInt: BigInt = BigInt(bits, 2)

    override def toString(): String = bits.toString
  }

  trait Packet {
    val version: Int
    def computeVersionSum(): Long
    def eval(): BigInt
  }
  case class Literal(val version: Int, val value: BigInt) extends Packet {
    def computeVersionSum(): Long = version

    def eval(): BigInt = value
  }
  case class Operator(val version: Int, val packetType: Int, val subpackets: List[Packet]) extends Packet {
    def computeVersionSum(): Long = version + subpackets.map(_.computeVersionSum()).sum

    def eval(): BigInt = packetType match {
      case 0 => subpackets.map(_.eval()).sum
      case 1 => subpackets.map(_.eval()).product
      case 2 => subpackets.map(_.eval()).min
      case 3 => subpackets.map(_.eval()).max
      case 5 => if (subpackets(0).eval() > subpackets(1).eval()) 1 else 0
      case 6 => if (subpackets(0).eval() < subpackets(1).eval()) 1 else 0
      case 7 => if (subpackets(0).eval() == subpackets(1).eval()) 1 else 0
    }
  }

  val literalPacketId = 4

  lazy val packet = parsePacket(inputFirstLine())

  def parseLiteral(version: Int, bitstream: BitStream): Literal = {
    def parse(current: BitStream): BigInt = {
      val isEnd = bitstream.take(1)(0) == '0'
      val value = bitstream.take(4)
      val next = current + value
      if (isEnd) next.toBigInt else parse(next)
    }
    Literal(version, parse(BitStream.empty))
  }

  def parseOperator(version: Int, typeId: Int, bitstream: BitStream): Operator = {
    val isPacketCount = bitstream.take(1)(0) == '1'
    val limitSize = if (isPacketCount) 11 else 15
    var bitsLeft = bitstream.take(limitSize).toInt
    var subpackets = List[Packet]()
    while (bitsLeft > 0) {
      val streamSize = bitstream.size
      val subpacket = parsePacket(bitstream)
      val consumed = streamSize - bitstream.size
      subpackets = subpackets :+ subpacket
      bitsLeft -= (if (isPacketCount) 1 else consumed)
    }
    Operator(version, typeId, subpackets)
  }

  def parsePacket(bitstream: BitStream): Packet = {
    val version = bitstream.take(3).toInt
    val packetId = bitstream.take(3).toInt
    if (packetId == literalPacketId) parseLiteral(version, bitstream)
    else parseOperator(version, packetId, bitstream)
  }

  def parsePacket(rawPacket: String): Packet = {
    parsePacket(BitStream(rawPacket))
  }

  def solveFirst(): Long = packet.computeVersionSum()

  def solveSecond(): BigInt = packet.eval()
}
