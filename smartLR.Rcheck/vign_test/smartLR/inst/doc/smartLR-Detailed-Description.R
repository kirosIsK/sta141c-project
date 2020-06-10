## ---- echo=FALSE--------------------------------------------------------------
install.packages("kernlab",repos = "http://cran.us.r-project.org")
install.packages("tidyverse",repos = "http://cran.us.r-project.org")
install.packages("parallel",repos = "http://cran.us.r-project.org")
install.packages("smartLR",repos = "http://cran.us.r-project.org")
install.packages("rbenchmark",repos = "http://cran.us.r-project.org")
devtools::install_github("ucdavis-sta141c-sq-2020/blblm")

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(kernlab)
library(tidyverse)
library(parallel)
library(blblm)
library(smartLR)
data(spam)
oj <- spam %>% mutate(type = if_else(spam$type == "spam",1,0))
frm    <- formula(log(Volume) ~ log(Girth))

cat(sprintf("Number of observations: %d \nNumber of variables   : %d", nrow(oj), ncol(oj)))

## It seems that Knit somthing can't detect variables inside my function
##  But it doesn't affect the speed of our function

## I am able to run it in RMD without knit
##  Delete tthe below until system.time if you
##  want to try run it by yourfels in RMD
m=1;  B=1; para = TRUE; CL = "CL"
##
## ADD UNtil this  
    
## Acutally Knit stops when they are not aware of the variables
##  Can still execute the function using arguments
##  that being passed to the function

    

system.time({
  # SmartLR -- non parallel method
  smartLR::blblm(type~., data=oj, m=10, B=100, parallel = FALSE)
})

system.time({
  # SmartLR -- parallel method
  smartLR::blblm(type~., data=oj, m=10, B=100, parallel = TRUE)
})

system.time({
  # blblm -- self define method
  blblm::blblm(type~.,   data=oj, m = 10, B = 100)
})


## -----------------------------------------------------------------------------
library(rbenchmark)
frm <- formula(log(Volume) ~ log(Girth))

cat(sprintf("Number of observations: %d '\n'Number of variables   : %d", nrow(trees), ncol(trees)))


## It seems that Knit somthing can't detect variables inside my function
##  But it doesn't affect the speed of our function

## I am able to run it in RMD without knit
##  Delete tthe below until system.time if you
##  want to try run it by yourfels in RMD
m=1;  B=1; para = TRUE; CL = "CL"
##
## ADD UNtil this  
    
## Acutally Knit stops when they are not aware of the variables
##  Can still execute the function using arguments
##  that being passed to the function


    
# SmartLR -- non parallel method
system.time({
  smartLR::blblm(frm, data=trees, m=10, B=5000, parallel = FALSE)
})

# SmartLR -- parallel method
system.time({
  smartLR::blblm(frm, data=trees, m=10, B=5000, parallel = TRUE)
})

# blblm -- self define method
system.time({
  blblm::blblm(frm,   data=trees, m = 10, B = 5000)
})

## -----------------------------------------------------------------------------
library(rbenchmark)
library(RcppArmadillo)

cat(sprintf("Number of observations: %d \nNumber of variables   : %d", nrow(trees), ncol(trees)))
y      <- log(trees$Volume)
X      <- as.matrix(log(trees$Girth))
weight <- replicate(length(y), 1)
frm    <- formula(log(Volume) ~ log(Girth))




benchmark(
  fastLm(frm, data=trees),
  fastLmPure(X, y),
  fastLmX(frm, data=trees, weight),
  fastLmXPure(X, y, weight),
  lm(frm, data=trees, weights = weight)
)

## -----------------------------------------------------------------------------
library(rsample)
library(kernlab)
library(tidyverse)
library(rbenchmark)
library(RcppArmadillo)
data(spam)

cat(sprintf("Number of observations: %d \nNumber of variables   : %d", nrow(oj), ncol(oj)))
oj      <- spam %>% mutate(type = if_else(spam$type == "spam",1,0))
y_data  <- spam %>% select(type = if_else(spam$type == "spam",1,0)) %>% unlist() %>% as.numeric()
x_data  <- spam %>% select(-type) %>% as.matrix()
Z       <- replicate(length(y_data), 1)


## fast Lm Pure from RcppArmadillo
system.time({
  fastLmPure(x_data, y_data)
})

## Rewritten weighted linear regression using Armadillo
system.time({
  fastLmXPure(x_data, y_data, Z)
})

## lm() in r-base
system.time({
  lm(type~., data=oj)
})

## -----------------------------------------------------------------------------
library(rbenchmark)
library(RcppArmadillo)

cat(sprintf("Number of observations: %d \nNumber of variables   : %d", nrow(trees), ncol(trees)))
y      <- log(trees$Volume)
X      <- as.matrix(log(trees$Girth))
weight <- replicate(length(y), 1)
frm    <- formula(log(Volume) ~ log(Girth))



## fast Lm Pure from RcppArmadillo
system.time({
  fastLmPure(X, y)
})

## lm() in r-base
system.time({
  lm(log(trees$Volume)~log(trees$Girth), data=trees, weights = weight)
})

## fastLmX being selected
system.time({
  smart_Pure(X, y, weight)
})

## -----------------------------------------------------------------------------
library(rsample)
library(kernlab)
library(tidyverse)
library(rbenchmark)
library(RcppArmadillo)
data(spam)

cat(sprintf("Number of observations: %d \nNumber of variables   : %d", nrow(oj), ncol(oj)))
oj      <- spam %>% mutate(type = if_else(spam$type == "spam",1,0))
weight  <- replicate(nrow(oj), 1)



## lm
system.time({
  lm(type~., data=oj, weights = weight)
})

## lm.wfit being selected by smart_lr
system.time({
  smart_lr(type~., data=oj, weight = weight)
})

