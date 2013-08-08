package com.thisisfranklin.crdt
package numeric

object MaximumLattice {
  def bottom = MaximumLattice(Int.MinValue)
}

case class MaximumLattice(value: Int) extends Lattice[Int, MaximumLattice] {
  def merge(other: MaximumLattice) = copy(value = value max other.value)

  def tryCompareTo(other: MaximumLattice): Option[Int] = Some(value compareTo other.value)


  @Morphism
  def >(n: Int): BooleanLattice = BooleanLattice(value > n)

  @Morphism
  def >=(n: Int): BooleanLattice = BooleanLattice(value >= n)

  @Morphism
  def +(n: Int) = copy(value = value + n)

  @Morphism
  def -(n: Int) = copy(value = value - n)
}
