package BloomFilter

import scala.util.hashing.{MurmurHash3 => MH}

trait HashesMixin {
  def hashes(value: String, i: Int)(upperBound: Int = 1000): List[Int] = {
    val hashValues = for (i <- 0 until i) yield Math.abs((murmurhash(value) + i * basichash(value)) % upperBound)
    hashValues.toList
  }

  def murmurhash(value: String): Int = MH.stringHash(value)

  def basichash(value: String): Int = value.hashCode
}
