package com.thisisfranklin.crdt

object BooleanLattice {
  def bottom = BooleanLattice(false)
}

case class BooleanLattice(value: Boolean) extends Lattice[Boolean, BooleanLattice] {
  def merge(other: BooleanLattice): BooleanLattice = copy(value = value || other.value)
}
