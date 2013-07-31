package com.thisisfranklin.crdt.numeric

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec

class MaximumLatticeSpec extends WordSpec with MustMatchers {
  "MaximumLattice" should {
    val bottom = MaximumLattice()
    val a = MaximumLattice(0)
    val b = MaximumLattice(100)
    val c = MaximumLattice(1000)

    "merge with identity" in {
      (bottom merge bottom) must equal (bottom)
      (a merge a) must equal (a)
      (b merge b) must equal (b)
      (c merge c) must equal (c)
    }

    "merge and converge" in {
      (bottom merge a) must equal (a)
      (a merge bottom) must equal (a)
      (bottom merge b merge c merge a) must equal (c)
    }
  }
}
