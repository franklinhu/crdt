package com.thisisfranklin.crdt

case class BooleanLattice(value: Boolean = false) extends Lattice[Boolean, BooleanLattice] {
  def merge(other: BooleanLattice): BooleanLattice = copy(value = value || other.value)
}
