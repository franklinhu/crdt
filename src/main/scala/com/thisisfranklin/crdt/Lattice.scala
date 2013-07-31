package com.thisisfranklin.crdt

trait Lattice[+T, S] { self: S =>
  val value: T
  def merge(other: S): S
}
