## Resubmission

This is a resubmission. In response to the CRAN review (Benjamin Altmann),
the following changes were made:

* The `Title` and `Description` fields are now written in English, and
  references describing the methods were added to the `Description` in the
  requested format (Lumley (2010, ISBN:9780470284308) and the Chilean Social
  Observatory methodology with a `<https:...>` link). The function
  documentation and vignettes remain in Spanish (`Language: es`), the
  language of the target user base.
* Examples are no longer wrapped in `\donttest{}`. All examples now run and
  are tested automatically; they use a small region of the bundled CASEN
  data so that each runs in well under 5 seconds.

## R CMD check results

0 errors | 0 warnings | 2 notes

* checking CRAN incoming feasibility ... NOTE
  New submission.

* checking installed package size ... NOTE
  installed size is 5.0Mb; sub-directories of 1Mb or more: data 2.7Mb.
  The package ships two real subsets of the Chilean CASEN household survey
  (`casen_2022`, `casen_2024`, xz-compressed) that are required to make all
  examples and vignettes reproducible with realistic complex-survey designs
  (the strata/PSU structure cannot be meaningfully simulated with toy data).

## Test environments

* macOS aarch64 (Apple Silicon), R 4.4.3 (local) — 0 errors, 0 warnings,
  2 notes (see above; a third local-only NOTE "unable to verify current
  time" is an artifact of the offline check environment).
* win-builder (R-devel) and Debian (r-devel) pretest of 0.3.3 — 1 note
  ("New submission") only.

## Reverse dependencies

None (new submission).
