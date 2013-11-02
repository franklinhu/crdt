package com.thisisfranklin.crdt
package collection

import com.thisisfranklin.crdt.numeric.MaximumLattice

object LSet {
  def bottom[A] = LSet(Set.empty[A])
}

case class LSet[A](value: Set[A]) extends Lattice[Set[A], LSet[A]] {
  def merge(other: LSet[A]) = copy(value = value ++ other.value)

  def tryCompareTo(other: LSet[A]): Option[Int] = {
    value.size - other.value.size match {
      case x if x > 0 && (other.value subsetOf value) => Some(1)
      case x if x > 0                                 => None
      case x if x == 0 && (value equals other.value)  => Some(0)
      case x if x == 0                                => None
      case x if x < 0 && (value subsetOf other.value) => Some(-1)
      case x if x < 0                                 => None
    }
  }

  @Morphism
  def intersect(other: LSet[A]) = copy(value = value intersect other.value)

  @Morphism
  def map[B](f: A => B): LSet[B] = LSet(value.map(f))

  @Monotone
  def size: MaximumLattice = MaximumLattice(value.size)
}

object PositiveLSet {
  def bottom = PositiveLSet(Set.empty)
}

case class PositiveLSet(value: Set[Int]) extends Lattice[Set[Int], PositiveLSet] {
  def merge(other: PositiveLSet) = copy(value = value ++ other.value)

  def tryCompareTo(other: PositiveLSet): Option[Int] = {
    value.size - other.value.size match {
      case x if x > 0 && (other.value subsetOf value) => Some(1)
      case x if x > 0                                 => None
      case x if x == 0 && (value equals other.value)  => Some(0)
      case x if x == 0                                => None
      case x if x < 0 && (value subsetOf other.value) => Some(-1)
      case x if x < 0                                 => None
    }
  }
}
