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

## Notes for the reviewers

* Functions never write to the user's filespace by default: the output
  directory `dir` has no default value and must be supplied explicitly when
  an Excel report is requested. All examples, tests and vignettes either
  write to `tempdir()` or skip writing (`save_xlsx = FALSE`).
* All examples run in a few seconds; the heavier ones (which build a survey
  design on the bundled CASEN data) are additionally wrapped in \donttest{}.

## Test environments

* macOS aarch64 (Apple Silicon), R 4.4.3 (local) — 0 errors, 0 warnings,
  2 notes (see above; a third local-only NOTE "unable to verify current
  time" is an artifact of the offline check environment).
* win-builder (R-devel) — to be re-run for 0.3.3 before submission.

## Reverse dependencies

None (new submission).
