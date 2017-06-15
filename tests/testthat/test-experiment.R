library(testthat)
context("experiment")

## method
method_lm_ridge <- function(lambda = 1e-4) {
  args <- as.list(match.call())[-1]
  res <- do.call(ExpRmethod, args)
  class(res) <- c("method_lm_ridge", class(res))
  res
}

ExpRmouline.method_lm_ridge <- function(m, dat) {
  A <- dat$Y
  X <- dat$X
  D <- diag(1, ncol(X), ncol(X))
  m$B <- t(solve((crossprod(X,X) + m$lambda * D), crossprod(X, A)))
  m
}

## sampler
sampler_gaussian <- function(n, p, K) {
  args <- as.list(match.call())[-1]
  res <- do.call(ExpRmethod, args)
  class(res) <- c("sampler_gaussian", class(res))
  res
}

ExpRmouline.sampler_gaussian <- function(s) {
  U <- MASS::mvrnorm(s$n, mu = rep(0.0,s$K), Sigma = 1.0 * diag(s$K))
  V <- MASS::mvrnorm(s$p, mu = rep(0.0,s$K), Sigma = 1.0 * diag(s$K))
  X <- matrix(rnorm(s$n), s$n, 1)
  ExpRdata(Y = U %*% t(V),
           U = U,
           V = V,
           X = X,
           K = s$K)
}

extract_B <- function(dat, m) {
  tibble::tibble(i = dat$i, K = dat$K, lambda = m$lambda, B = as.numeric(m$B[,1]))
}

test_that("expr", {

  ## samplers
  dat <- ExpRmouline(sampler_gaussian(n = 10, p = 100, K = 3))
  expect_equal(names(dat), c('Y', "U", "V", "X", "K"))
  samplers <- sampler_gaussian(n = 10, p = 100, K = NULL) * param(K = 1:3)
  expect_equal(length(samplers), 3)

  ## methods
  m.res <- ExpRmouline(m = method_lm_ridge(lambda = 1e-4), dat)
  expect_equal(names(m.res), c('lambda', "B"))
  methods <- method_lm_ridge(lambda = 1e-5) * param(lambda = c(1e-1, 1e-5))
  expect_equal(length(methods), 2)

  ## expr
  expr <- ExpR(rep.nb = 5,
               samplers = samplers,
               methods = methods,
               preprocessors = NULL,
               extractor = extract_B)


  expr <- ExpRmouline(expr)

})

