# googleSuggestQueriesR 0.1.0
Download suggested queries based on your input generated by Google Suggest API

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/googleSuggestQueriesR)](https://CRAN.R-project.org/package=googleSuggestQueriesR)
 [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Build Status](https://travis-ci.org/Leszek-Sieminski/googleSuggestQueriesR.svg?branch=master)](https://travis-ci.org/Leszek-Sieminski/googleSuggestQueriesR)
[![codecov](https://codecov.io/gh/Leszek-Sieminski/googleSuggestQueriesR/branch/master/graph/badge.svg)](https://codecov.io/gh/Leszek-Sieminski/googleSuggestQueriesR)
[![](https://cranlogs.r-pkg.org/badges/googleSuggestQueriesR)](https://cran.r-project.org/package=googleSuggestQueriesR)
<!-- badges: end -->

* [What is Google Suggest Query API?](#what-is-google-suggest-query-api)
* [News](#news)
* [Features](#features)
* [Installation](#installation)
* [Usage](#usage)
* [Bugs, issues](#bugs-issues)

## What is Google Suggest Query API?
Google Suggest Query API is a tool for extracting keyword suggestions from Google.

## News
For more, see the [NEWS.md](https://github.com/Leszek-Sieminski/googleSuggestQueriesR/blob/master/NEWS.md)

## Features
* downloads keyword suggestions
* returns vector of character strings
* extracts more suggestions by automatic creation of additional variations of keywords
* you can also create your own keyword variations on-the-fly
* has enabled interval options to not exceed API limits

## Installation
```r
# CRAN version is is not yet available - to be announced

# GitHub version:
install.packages("devtools")
devtools::install_github("Leszek-Sieminski/googleSuggestQueriesR")
```
## Usage
```r
library(googleSuggestQueriesR)

# this can take some time due to the number of variations, please be patient
keyword_suggestions <- googleSuggestQueriesR::suggest_keywords(
  queries = "mtcars",
  lang = "en",
  interval = 1)

head(keyword_suggestions)
# [1] "mtcars c(1 2)" "mtcars am" "mtcars analysis" "mtcars analysis in r" "mtcars anova" "mtcars analysis using r"

str(keyword_suggestions)
# chr [1:196] "mtcars c(1 2)" "mtcars am" "mtcars analysis" "mtcars analysis in r" "mtcars anova" "mtcars analysis using r" ...
```

## Bugs, issues
Please report them [here.](https://github.com/Leszek-Sieminski/googleSuggestQueriesR/issues)
