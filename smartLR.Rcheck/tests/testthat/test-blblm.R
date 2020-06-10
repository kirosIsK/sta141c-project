if(!require(kernlab)) install.packages("kernlab",repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse",repos = "http://cran.us.r-project.org")
if(!require(parallel)) install.packages("parallel",repos = "http://cran.us.r-project.org")
if(!require(smartLR)) install.packages("smartLR",repos = "http://cran.us.r-project.org")
if(!require(rbenchmark)) install.packages("rbenchmark",repos = "http://cran.us.r-project.org")
if(!require(rsample)) install.packages("rsample",repos = "http://cran.us.r-project.org")
if(!require(RcppArmadillo)) install.packages("rsample",repos = "http://cran.us.r-project.org")


test_that("testCase #1", {
  # In Such A small dataset Arma Method will be picked
  #  It spends ~ 1min to loop this test
  frm <- formula(log(Volume) ~ log(Girth))

  # Parallel Method
  oj1 <- blblm(frm, data=trees, m = 10, B = 100, parallel = TRUE)

  # Non Parallel Method
  oj2 <- blblm(frm, data=trees, m = 10, B = 100,parallel = FALSE)

  # Check bootstrap length
  expect_equivalent(length(oj1$estimates$`10`), 100)
  expect_equivalent(length(oj2$estimates$`10`), 100)

  # Check coefficient value is Null or not
  expect_equivalent(length(oj1$estimates$`10`[[1]]$coef[[1]]), ncol(trees) - 1) # -1 since no intercept
  expect_equivalent(length(oj2$estimates$`10`[[1]]$coef[[1]]), ncol(trees) - 1) # -1 since no intercept

  # Check sigma value is Null or not
  expect_equivalent(length(oj1$estimates$`10`[[1]]$sigma[[1]]), 1)
  expect_equivalent(length(oj2$estimates$`10`[[1]]$sigma[[1]]), 1)
})


test_that("testCase #2", {
  # In Such A large dataset lm.wfit Method will be picked
  #  It spends ~ 10s to loop this test
  data(spam)
  oj  <- spam %>% mutate(type = if_else(spam$type == "spam",1,0))

  # Parallel Method
  oj1 <- blblm(type~., data=oj, m=10, B=100, parallel = TRUE)

  # Non Parallel Method
  oj2 <- blblm(type~., data=oj, m=10, B=100, parallel = FALSE)

  # Check bootstrap length
  expect_equivalent(length(oj1$estimates$`10`), 100)
  expect_equivalent(length(oj2$estimates$`10`), 100)

  # Check coefficient value is Null or not
  expect_equivalent(length(oj1$estimates$`10`[[1]]$coef[[1]]), ncol(oj)) # no -1 because lm.wfit count intercept
  expect_equivalent(length(oj2$estimates$`10`[[1]]$coef[[1]]), ncol(oj)) # no -1 because lm.wfit count intercept

  # Check sigma value is Null or not
  expect_equivalent(length(oj1$estimates$`10`[[1]]$sigma[[1]]), 1)
  expect_equivalent(length(oj2$estimates$`10`[[1]]$sigma[[1]]), 1)
})
