package com.thisisfranklin.crdt

trait Lattice[+T, S] { self: S =>
  val value: T
  def merge(other: S): S

  def tryCompareTo(other: S): Option[Int]

  def < (that: S): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x < 0 => true
      case _ => false
    }
  def > (that: S): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x > 0 => true
      case _ => false
    }
  def <= (that: S): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x <= 0 => true
      case _ => false
    }
  def >= (that: S): Boolean =
    (this tryCompareTo that) match {
      case Some(x) if x >= 0 => true
      case _ => false
    }
}

case class LatticePair[A, B <: Lattice[A, B], C, D <: Lattice[C, D]](
  value: (B, D)
) extends Lattice[(B, D), LatticePair[A, B, C, D]] {

  def this(b: B, d: D) = this((b, d))

  def merge(other: LatticePair[A, B, C, D]): LatticePair[A, B, C, D] = {
    value._1 tryCompareTo other.value._1 match {
      case Some(x) if x < 0 => other
      case Some(x) if x > 0 => this
      case _ => LatticePair(value._1 merge other.value._1, value._2 merge other.value._2)
    }
  }

  def tryCompareTo(other: LatticePair[A, B, C, D]): Option[Int] = {
    value._1 tryCompareTo other.value._1
  }
}
