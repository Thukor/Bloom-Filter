package BloomFilter

import scala.collection.immutable.BitSet

class BloomFilter(val k: Int, val size: Int)(private val bits: BitSet = BitSet(size)) extends HashesMixin {

  def insert(element: String): BloomFilter = {
    def insertHelper(hashes: List[Int], acc: BitSet): BitSet = hashes match {
      case hash :: rest => insertHelper(rest, acc + hash)
      case Nil => acc
    }

    BloomFilter(k, size, insertHelper(hashes(element, k)(size), bits))
  }

  def contains(element: String): Boolean = hashes(element, k)(size).forall(hash => bits(hash))
}


object BloomFilter {
  def apply(k: Int, size: Int, bits: BitSet = BitSet()): BloomFilter = new BloomFilter(k, size)(bits)
}


