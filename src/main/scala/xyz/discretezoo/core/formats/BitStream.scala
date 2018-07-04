package xyz.discretezoo.core.formats

class BitStream(bytes: Array[Byte], ignoredBits: Int) {

  val topBit: Int = 7 - ignoredBits

  var currentBit: Int = topBit
  var currentByte: Int = 0

  private def ones(n: Int): Int = (1 << n) - 1

  def remainingBits: Int = (8 - ignoredBits) * (bytes.length - currentByte - 1) + currentBit + 1

  def getNextBits(n: Int): Long = {

    var partialResult = 0
    var result = 0
    var remainingBits = n

    while (remainingBits > 0) {

      val bits = math.min(currentBit + 1, remainingBits)
      if (bits > currentBit) {
        partialResult = bytes(currentByte) & ones(currentBit + 1)
        currentByte += 1
        currentBit = topBit
      }
      else {
        partialResult = bytes(currentByte) & (ones(currentBit + 1) ^ ones(currentBit - bits))
        partialResult = partialResult >>> (currentBit - bits + 1)
        currentBit -= bits
      }
      remainingBits -= bits
      result = (result << bits) | partialResult
    }

    result

  }

}
