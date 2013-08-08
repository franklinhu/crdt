package com.thisisfranklin.crdt
package collection

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec

class MapLatticeSpec extends WordSpec with MustMatchers {
  "MapLattice" should {
    "merge: base case" in {
      val a = MapLattice.bottom[Int, BooleanLattice]
      val b = MapLattice(
        Map(1 -> BooleanLattice(false),
            2 -> BooleanLattice(true), 
            3 -> BooleanLattice(false)))
      
      (a merge a) must equal (a)
      (b merge b) must equal (b)
      (a merge b) must equal (b)
      (b merge a) must equal (b)
    }

    "merge value lattices" in {
      val a = MapLattice(Map(1 -> BooleanLattice(false),
                             2 -> BooleanLattice(true),
                             3 -> BooleanLattice(false)))

      val b = MapLattice(Map(1 -> BooleanLattice(true),
                             2 -> BooleanLattice(false),
                             3 -> BooleanLattice(false),
                             4 -> BooleanLattice(false)))

      val expected = MapLattice(Map(1 -> BooleanLattice(true),
                                    2 -> BooleanLattice(true),
                                    3 -> BooleanLattice(false),
                                    4 -> BooleanLattice(false)))

      (a merge b) must equal (expected)
      (b merge a) must equal (expected)
    }

    "tryCompareTo" in {
      val a = MapLattice.bottom[Int, BooleanLattice]

      val b = MapLattice(Map(1 -> BooleanLattice(false),
                             2 -> BooleanLattice(true)))

      val c = MapLattice(Map(1 -> BooleanLattice(false),
                             2 -> BooleanLattice(true),
                             3 -> BooleanLattice(true)))

      val d = MapLattice(Map(1 -> BooleanLattice(false),
                             2 -> BooleanLattice(true),
                             3 -> BooleanLattice(false)))

      val e = MapLattice(Map(1 -> BooleanLattice(true),
                             2 -> BooleanLattice(false),
                             3 -> BooleanLattice(true)))

      (a tryCompareTo a) must equal (Some(0))
      (b tryCompareTo b) must equal (Some(0))

      (a tryCompareTo b) must equal (Some(-1))
      (b tryCompareTo a) must equal (Some(1))

      (b tryCompareTo c) must equal (Some(-1))
      (c tryCompareTo b) must equal (Some(1))

      (c tryCompareTo d) must equal (Some(1))
      (d tryCompareTo c) must equal (Some(-1))

      (d tryCompareTo e) must equal (None)
      (e tryCompareTo d) must equal (None)
    }
  }
}

