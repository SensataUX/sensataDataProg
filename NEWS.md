# sensataDataProg 1.1.8
specialSkipValue should now turn the underlying data to that value and work with SPSS, Stata, RDS and CSV files

# sensataDataProg 1.1.7
fix bug for makeFactors for specialSkipValue that was not working, and create labelled spss for multiple choice

# sensataDataProg 1.1.6
change default to structuredResponses, since that is how the platform now works

# sensataDataProg 1.1.5
add particularVal2 and particularVal3 to scrubRows to scrub different particular values of a variable

# sensataDataProg 1.1.4
add geo.accuracy to colsToKeep argument on cleanCols and kept it on selectCols if dropGeo = F

# sensataDataProg 1.1.3
fix documentation, and fix bugs on delExtraCar, add trim whitespace options dict generator

# sensataDataProg 1.1.2
Include missing questions feature to scrubRows

# sensataDataProg 1.1.1
Including conjoint functions, delExtraCar and conjoint2Tasks

# sensataDataProg 1.0.2
Fix several bugs, including translateFactors screens problem

# sensataDataProg 1.0.1

* Adding util.R with functions for selecting columns with all NA and any NA
* Using this selecter function on cleanCols so empty columns are erased on interim data
* makeFactors now includes specialSkipValue argument to create SPSS labelled
* also includes function from0to100 to rescale any vector to a 0-100

# sensataDataProg 1.0.0

* Added a `NEWS.md` file to track changes to the package.
* Created first feature complete version of the package.
* For now it is a MVP, all suggestions welcome
