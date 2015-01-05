1.8.0
-----
* Support stepped computations
* Introduce Data.Type.Nat, which provides some Nat operations not found in
  GHC.TypeLits
* Some cleanup

1.7.0
-----
* Ensure things get inlined by removing recursive dependencies between 'Dim'
  and 'Bounded'
* Remove value-level :.
* Add .: for safe construction of indices

1.6.3
-----
Added 'Bounded' constraint to 'Dim'

1.6.2
-----
Added this changelog.

1.6.1
-----
* Remove 'reflect' entirely.
* Relax dependencies

1.6.0
-----
* Added QuasiQuoters for Unroll and Roll
* New 'Mode' type to specify whether you want unrolled or normal loops
* Removed "s" prefixed functions
* Fix Ix instance
* Reduce duplication

1.5.0
-----
Initial Hackage release.
