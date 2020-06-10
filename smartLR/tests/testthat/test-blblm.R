if(!require(kernlab)) install.packages("kernlab",repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse",repos = "http://cran.us.r-project.org")
if(!require(parallel)) install.packages("parallel",repos = "http://cran.us.r-project.org")
if(!require(smartLR)) install.packages("smartLR",repos = "http://cran.us.r-project.org")
if(!require(rbenchmark)) install.packages("rbenchmark",repos = "http://cran.us.r-project.org")
if(!require(rsample)) install.packages("rsample",repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools",repos = "http://cran.us.r-project.org")
if(!require(blblm)) devtools::install_github("ucdavis-sta141c-sq-2020/blblm")

test_that("testCase #1", {
  # In Such A small dataset Arma Method will be picked
  #  It spends ~ 1min to loop this test
  library(rbenchmark)
  frm <- formula(log(Volume) ~ log(Girth))

  # Non Parallel Method
  oj2 <- smartLR::blblm(frm, data=trees, m = 10, B = 100,parallel = FALSE)

  # Check bootstrap length
  expect_equivalent(length(oj2$estimates$`10`), 100)

  # Check coefficient value is Null or not
  expect_equivalent(length(oj2$estimates$`10`[[1]]$coef[[1]]), ncol(trees) - 1) # -1 since no intercept

  # Check sigma value is Null or not
  expect_equivalent(length(oj2$estimates$`10`[[1]]$sigma[[1]]), 1)
})


test_that("testCase #2", {
  # In Such A large dataset lm.wfit Method will be picked
  #  It spends ~ 10s to loop this test
  library(rsample)
  library(kernlab)
  library(tidyverse)
  library(RcppArmadillo)
  data(spam)
  oj  <- spam %>% mutate(type = if_else(spam$type == "spam",1,0))

  # Non Parallel Method
  oj2 <- smartLR::blblm(type~., data=oj, m=10, B=100, parallel = FALSE)

  # Check bootstrap length
  expect_equivalent(length(oj2$estimates$`10`), 100)

  # Check coefficient value is Null or not
  expect_equivalent(length(oj2$estimates$`10`[[1]]$coef[[1]]), ncol(oj)) # no -1 because lm.wfit count intercept

  # Check sigma value is Null or not
  expect_equivalent(length(oj2$estimates$`10`[[1]]$sigma[[1]]), 1)
})
