package com.thisisfranklin.crdt
package numeric

case class MaximumLattice(value: Int = Int.MinValue) extends Lattice[Int, MaximumLattice] {
  def merge(other: MaximumLattice) = copy(value = value max other.value)

  @Morphism
  def >(n: Int): BooleanLattice = BooleanLattice(value > n)

  @Morphism
  def >=(n: Int): BooleanLattice = BooleanLattice(value >= n)

  @Morphism
  def +(n: Int) = copy(value = value + n)

  @Morphism
  def -(n: Int) = copy(value = value - n)
}
