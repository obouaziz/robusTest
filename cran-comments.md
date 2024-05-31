## R CMD check results
There were no ERRORs, WARNINGs or NOTEs. 

## Downstream dependencies
The package depends on the Rcpp library.

This is a re-submission following the initial CRAN submission: commit 371eecc

## Resubmission (2022-12-14)
This resubmission fixes very minor patches. In this version I have:

- added the arXiv link to the scientific paper.

- changed minor details in the Description and Readme files.

## Fix minor issues (2024-06-01)
Package was removed from CRAN. The package was tested again, a test from testthat 
failed for the indeptest function. This is now fixed, the test was too strict 
and has been changed to a more flexible condition.




