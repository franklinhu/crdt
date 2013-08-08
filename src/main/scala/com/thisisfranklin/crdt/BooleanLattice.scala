package com.thisisfranklin.crdt

object BooleanLattice {
  def bottom = BooleanLattice(false)
}

case class BooleanLattice(value: Boolean) extends Lattice[Boolean, BooleanLattice] {
  def merge(other: BooleanLattice): BooleanLattice = copy(value = value || other.value)

  def tryCompareTo(other: BooleanLattice): Option[Int] = {
    if (value ^ other.value) {
      if (value)
        Some(1)
      else
        Some(-1)
    } else {
      Some(0)
    }
  }
}
