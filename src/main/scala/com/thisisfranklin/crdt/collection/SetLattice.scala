package com.thisisfranklin.crdt
package collection

import com.thisisfranklin.crdt.numeric.MaximumLattice

object SetLattice {
  def bottom[A] = SetLattice(Set.empty[A])
}

case class SetLattice[A](value: Set[A]) extends Lattice[Set[A], SetLattice[A]] {
  def merge(other: SetLattice[A]) = copy(value = value ++ other.value)

  @Morphism
  def intersect(other: SetLattice[A]) = copy(value = value intersect other.value)

  @Morphism
  def map[B](f: A => B): SetLattice[B] = SetLattice(value.map(f))

  @Monotone
  def size: MaximumLattice = MaximumLattice(value.size)
}

object PositiveSetLattice {
  def bottom = PositiveSetLattice(Set.empty)
}

case class PositiveSetLattice(value: Set[Int]) extends Lattice[Set[Int], PositiveSetLattice] {
  def merge(other: PositiveSetLattice) = copy(value = value ++ other.value)
}
