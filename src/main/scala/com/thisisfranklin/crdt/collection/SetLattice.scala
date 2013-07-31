package com.thisisfranklin.crdt
package collection

import com.thisisfranklin.crdt.numeric.MaximumLattice

case class SetLattice[A](value: Set[A] = Set.empty) extends Lattice[Set[A], SetLattice[A]] {
  def merge(other: SetLattice[A]) = copy(value = value ++ other.value)

  @Morphism
  def intersect(other: SetLattice[A]) = copy(value = value intersect other.value)

  @Morphism
  def map[B](f: A => B): SetLattice[B] = SetLattice(value.map(f))

  @Monotone
  def size: MaximumLattice = MaximumLattice(value.size)
}

case class PositiveSetLattice(value: Set[Int] = Set.empty) extends Lattice[Set[Int], PositiveSetLattice] {
  def merge(other: PositiveSetLattice) = copy(value = value ++ other.value)
}
