package com.thisisfranklin.crdt
package collection

import com.thisisfranklin.crdt.numeric.MaximumLattice

object LMap {
  def bottom[K, L <: Lattice[_, L]] = LMap[K, L](Map.empty)
}

case class LMap[K, L <: Lattice[_, L]](value: Map[K, L]) extends Lattice[Map[K, L], LMap[K, L]] {

  def merge(other: LMap[K, L]): LMap[K, L] = {
    val myKeys = value.keySet
    val otherKeys = other.value.keySet
    val newValues = (myKeys ++ otherKeys) map {
      case k if myKeys.contains(k) && otherKeys.contains(k) =>
        k -> (value(k) merge other.value(k))
      case k if myKeys.contains(k) =>
        k -> value(k)
      case k if otherKeys.contains(k) =>
        k -> other.value(k)
    }
    LMap(newValues.toMap)
  }

  def tryCompareTo(other: LMap[K, L]): Option[Int] = {
    /*
     * m1 is strictly less than m2 if:
     *   1) for all keys `k` in m1, `k` is a key in m2 AND
     *   2) m1[k] <= m2[k]
     *
     * return true if m1 is strictly less than m2, false otherwise
     */
    def lt(m1: Map[K, L], m2: Map[K, L]): Boolean = {
      m1 forall { case (k, v1) =>
        m2.get(k) match {
          case Some(v2) => v1 <= v2
          case _ => false
        }
      }
    }

    if (value equals other.value) {
      Some(0)
    } else if (lt(value, other.value)) {
      Some(-1)
    } else if (lt(other.value, value)) {
      Some(1)
    } else {
      None
    }
  }

  @Morphism
  def intersect(other: LMap[K, L]): LMap[K, L] = {
    throw new Exception("UNIMPLEMENTED")
  }

  @Morphism
  def map[U, W <: Lattice[U, W]](f: ((K, L)) => (K, W)): LMap[K, W] = LMap(value.map(f))

  @Morphism
  def keySet(): SetLattice[K] = SetLattice(value.keySet)

  //@Morphism
  //def at(u: V): K

  @Morphism
  def contains(k: K): BooleanLattice = BooleanLattice(value.contains(k))

  @Monotone
  def size: MaximumLattice = MaximumLattice(value.size)
}
