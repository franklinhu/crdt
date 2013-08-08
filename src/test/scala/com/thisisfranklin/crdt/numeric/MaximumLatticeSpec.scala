package com.thisisfranklin.crdt.numeric

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec

class MaximumLatticeSpec extends WordSpec with MustMatchers {
  "MaximumLattice" should {
    val bottom = MaximumLattice.bottom
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

    "tryCompareTo" in {
      (bottom tryCompareTo bottom) must equal (Some(0))
      (a tryCompareTo a) must equal (Some(0))
      (c tryCompareTo c) must equal (Some(0))

      (bottom tryCompareTo a).get must be < (0)
      (bottom tryCompareTo c).get must be < (0)
      (a tryCompareTo c).get must be < (0)

      (c tryCompareTo a).get must be > (0)
      (c tryCompareTo bottom).get must be > (0)
    }
  }
}
