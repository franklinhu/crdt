package com.thisisfranklin.crdt
package numeric

import com.thisisfranklin.crdt.BooleanLattice

case class MinimumLattice(value: Int = Int.MaxValue) extends Lattice[Int, MinimumLattice] {
  def merge(other: MinimumLattice) = copy(value = value min other.value)

  @Morphism
  def <(n: Int): BooleanLattice = BooleanLattice(value < n)

  @Morphism
  def <=(n: Int): BooleanLattice = BooleanLattice(value <= n)

  @Morphism
  def +(n: Int) = copy(value = value + n)

  @Morphism
  def -(n: Int) = copy(value = value - n)
}
