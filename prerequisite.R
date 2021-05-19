  ## Need the knitr package to set chunk options
  library(knitr)

  ## Set knitr options for knitting code into the report:
  ## Print out code (echo)
  ## Save results so that code blocks aren't re-run unless code changes (cache),
  ## or a relevant earlier code block changed (autodep), but don't re-run if the
  ## only thing that changed was the comments (cache.comments)
  ## Align plots center (fig.align)
  ## Don't clutter R output with messages or warnings (message, warning)
  ## This will leave error messages showing up in the knitted report
  opts_chunk$set(echo=TRUE,
                 cache=TRUE, autodep=TRUE, cache.comments=FALSE,
                 fig.align="center",
                 fig.width=12, fig.height=9,
                 message=FALSE, warning=FALSE)

require(glmnet)
require(ALEPlot)
## require(data.table)
## require(readr)
require(caret)
require(xgboost)
require(dplyr)
require(Matrix)
require(catboost)
require(caret)
require(stringr)
require(MLmetrics)
## require(fastICA)
require(nnet)
## require(plyr)
require(ggplot2)
require(WeightedROC)
require(ranger)

## Don't convert text strings to factors with base read functions
options(stringsAsFactors = FALSE)
## Dont' omit NA rows
options(na.action = "na.pass")
