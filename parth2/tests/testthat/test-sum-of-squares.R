library(testthat)
library(hw10parth2)

test_check("hw10parth2")


#install.packages(c("devtools", "testthat", "roxygen2"))
usethis::use_testthat()

# Set a seed for reproducibility
set.seed(5055)
# Number of Observations
n = 10000
# Generate x
x = seq(0, 1, length.out = n)

# Generate random y
# Set seed for reproducibility
set.seed(114)
y = runif(n)

# Calculate mean
y_bar = mean(y)

# Obtain y_hat
y_hat = lm(y~x)$fitted.values


context("Testing sum of squares")
test_that("Check coding", {
  expect_equal(
    shared_relation(y, y_hat, y_bar),
    TSS(y, y_bar)
  )
})
