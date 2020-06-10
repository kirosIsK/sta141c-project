install.packages("kernlab",repos = "http://cran.us.r-project.org")
install.packages("tidyverse",repos = "http://cran.us.r-project.org")
install.packages("parallel",repos = "http://cran.us.r-project.org")
install.packages("smartLR",repos = "http://cran.us.r-project.org")
install.packages("rbenchmark",repos = "http://cran.us.r-project.org")
install.packages("devtools",repos = "http://cran.us.r-project.org")
devtools::install_github("ucdavis-sta141c-sq-2020/blblm")


test_that("Test Case #1", {
  library(RcppArmadillo)
  x <- c(44.4,45.9,41.9,53.3,44.7,44.1,50.7,45.2,60.1)
  y <- c(2.6,3.1,2.5,5.0,3.6,4.0,5.2,2.8,3.8)
  z <- replicate(length(y), 1)

  expect_identical(round(fastLmX(x,y,z)$coefficients, 7),  round(fastLm(x,y)$coefficients, 7))
})


test_that("Test Case #2", {
  library(kernlab)
  library(tidyverse)
  library(RcppArmadillo)
  data(spam)
  oj      <- spam %>% mutate(type = if_else(spam$type == "spam",1,0))
  y_data  <- spam %>% select(type = if_else(spam$type == "spam",1,0)) %>% unlist() %>% as.numeric()
  x_data  <- spam %>% select(-type) %>% as.matrix()
  Z       <- replicate(length(y_data), 1)

  # Check intercept are equivalent when pass by Matrix and Vector
  expect_equivalent(round(fastLmX(x_data,y_data,Z)$coefficients[1], 7),      round(fastLm(x_data,y_data)$coefficients[1], 7))

  # Check intercept are equivalent when pass by Formula and Object
  expect_equivalent(round(fastLmX(type~., data = oj,Z)$coefficients[1], 7),  round(fastLm(type~., data = oj)$coefficients[1], 7))
})
