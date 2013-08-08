package com.thisisfranklin.crdt

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec

class BooleanLatticeSpec extends WordSpec with MustMatchers {
  "BooleanLattice" should {
    val f = BooleanLattice(false)
    val t = BooleanLattice(true)

    "merge with identity" in {
      (f merge f) must equal (f)
      (t merge t) must equal (t)
    }

    "merge commutatively" in {
      (f merge t) must equal (t)
      (t merge f) must equal (t)
    }

    "tryCompareTo" in {
      (f tryCompareTo f) must equal (Some(0))
      (t tryCompareTo t) must equal (Some(0))

      (f tryCompareTo t) must equal (Some(-1))
      (t tryCompareTo f) must equal (Some(1))
    }
  }
}
