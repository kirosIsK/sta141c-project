# Use data from HW#6 to try
library(rsample)
library(kernlab)
library(tidyverse)
data(spam)
set.seed(141)
nspam <- 1813
nham <- 2788
test <- c(sample(1:nspam, floor(nspam * 0.1)), sample(nspam + 1:nham, floor(nham * 0.1)))
spam_train <- spam[-test, ]
spam_test <- spam[test, ]
y=ifelse(spam_train$type == "spam", 1, 0)
x_data <- spam_train %>% select(-type) %>% as.matrix()

test_that("try", {
  expect_equal(fastLmX(x_data,y ,y)$coefficient[1] %>% round(6), -0.058283)
})
