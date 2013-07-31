package com.thisisfranklin.crdt
package collection

import com.thisisfranklin.crdt.numeric.MaximumLattice

case class MapLattice[K, V, L <: Lattice[V, L]](value: Map[K, L] = Map.empty) extends Lattice[Map[K, L], MapLattice[K, V, L]] {
  def merge(other: MapLattice[K, V, L]): MapLattice[K, V, L] = {
    val myKeys = value.keySet
    val otherKeys = other.value.keySet
    val newValues = (myKeys ++ otherKeys) map {
      case k if myKeys.contains(k) && otherKeys.contains(k) =>
        k -> (value(k) merge other.value(k))
      case k if myKeys.contains(k) =>
        k -> value(k)
      case k if otherKeys.contains(k) =>
        k -> other.value(k)
    }
    MapLattice(newValues.toMap)
  }

  @Morphism
  def intersect(other: MapLattice[K, V, L]): MapLattice[K, V, L] = {
    throw new Exception("UNIMPLEMENTED")
  }

  @Morphism
  def map[U, W <: Lattice[U, W]](f: ((K, L)) => (K, W)): MapLattice[K, U, W] = MapLattice(value.map(f))

  @Morphism
  def keySet(): SetLattice[K] = SetLattice(value.keySet)

  //@Morphism
  //def at(u: V): K

  @Morphism
  def contains(k: K): BooleanLattice = BooleanLattice(value.contains(k))

  @Monotone
  def size: MaximumLattice = MaximumLattice(value.size)
}
