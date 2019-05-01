#' @title Calculating TSS
#'
#' @description With this function we will be able to calculate the TSS of
#' a sample, given the y and y_bar
#'
#' @param y A vector
#'
#' @param y_bar A value that denotes the mean of a sample
#'
#' @return A integer that represents the TSS
#'
#'
#'
#' @examples
#' y = c(1, 3, 5, 7, 10)
#' y_bar = 5.2
#' TSS(y, y_bar)

TSS <- function(y, y_bar) {
  s  = 0
  for(i in y) {
    s = s + (i-y_bar)^2
  }
  return(s)
}

#' @title Calculating FSS
#'
#' @description With this function we will be able to calculate the FSS of
#' a sample, given the y_hat and y_bar
#'
#' @param y_hat A vector
#'
#' @param y_bar A value that denotes the mean of a sample
#'
#' @return A integer that represents the FSS
#'
#' @export
#'
#' @examples
#' y_hat = c(1, 3, 5, 7, 10)
#' y_bar = 5.2
#' FSS(y_hat, y_bar)
FSS <- function(y_hat, y_bar) {
  s  = 0
  for(i in y_hat) {
    s = s + (i-y_bar)^2
  }
  return(s)
}

#' @title Calculating RSS
#'
#' @description With this function we will be able to calculate the RSS of
#' a sample, given the y and y_hat
#'
#' @param y A vector
#'
#' @param y_hat A vector
#'
#' @return A integer that represents the RSS
#'
#'
#'
#' @examples
#' y = c(1, 3, 5, 7, 10)
#' y_hat = c(3, 4, 2, 10, 7)
#' TSS(y, y_hat)
RSS <- function(y, y_hat) {
  s = sum((y - y_hat)^2)
  return(s)
}



#' @title Calculating TSS by the relationship of TSS, RSS, and FSS
#'
#' @description With this function we will be able to calculate the TSS using the
#' relationship between TSS, RSS, and FSS
#'
#' @param y A vector
#'
#' @param y_hat A vector
#'
#' @param y_bar A integer
#'
#' @return A integer that represents the TSS
#'
#' @export
#'
#' @examples
#' y = c(1, 3, 5, 7, 10)
#' y_hat = c(3, 4, 2, 10, 7)
#' y_bar = 5.2
#' shared_relation(y, y_hat, y_bar)
shared_relation <- function(y, y_hat, y_bar) {
  return(RSS(y, y_hat)+ FSS(y_hat, y_bar))
}


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

#shared_relation(y, y_hat, y_bar)
print(TSS(y, y_bar))


