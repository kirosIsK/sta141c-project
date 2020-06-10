test_that("testCase #1", {
  # In Such A small dataset Arma Method will be picked
  #  It spends ~ 1min to loop this test
  library(rbenchmark)
  frm <- formula(log(Volume) ~ log(Girth))

  # Parallel Method
  oj1 <- smartLR::blblm(frm, data=trees, m = 10, B = 5000,parallel = TRUE)

  # Non Parallel Method
  oj2 <- smartLR::blblm(frm, data=trees, m = 10, B = 5000,parallel = FALSE)

  # Check bootstrap length
  expect_equivalent(length(oj2$estimates$`10`), 5000)

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
  library(rsample)
  library(kernlab)
  library(tidyverse)
  library(rbenchmark)
  library(RcppArmadillo)
  data(spam)
  oj  <- spam %>% mutate(type = if_else(spam$type == "spam",1,0))

  # Parallel Method
  oj1 <- smartLR::blblm(type~., data=oj, m=10, B=100, parallel = TRUE)

  # Non Parallel Method
  oj2 <- smartLR::blblm(type~., data=oj, m=10, B=100, parallel = FALSE)

  # Check split data lenth
  expect_equivalent(length(oj1$estimates), 10)

  # Check bootstrap length
  expect_equivalent(length(oj2$estimates$`10`), 100)

  # Check coefficient value is Null or not
  expect_equivalent(length(oj1$estimates$`10`[[1]]$coef[[1]]), ncol(oj)) # no -1 because lm.wfit count intercept
  expect_equivalent(length(oj2$estimates$`10`[[1]]$coef[[1]]), ncol(oj)) # no -1 because lm.wfit count intercept

  # Check sigma value is Null or not
  expect_equivalent(length(oj1$estimates$`10`[[1]]$sigma[[1]]), 1)
  expect_equivalent(length(oj2$estimates$`10`[[1]]$sigma[[1]]), 1)
})
