library(testthat)
context("operators")


test_that("+ and * for list", {

  ## +
  list1 <- list(a = 1)
  class(list1) <- "ExpRlistOperand"
  list2 <- list(b = 2, c = 1)
  class(list2) <- "ExpRlistOperand"
  list3 <- list1 + list2
  list3
  expect_equal(length(list3), 3)


  ## *
  list1 <- list(a = 1)
  class(list1) <- "ExpRoperand"
  list2 <- list(b = 2:3, c = 3, K = 1:3)
  class(list2) <- "ExpRoperand"
  list3 <- list1 * list2
  list3

})

test_that("with method", {

  ms <- ExpRmethod() * param(K = 1, a = 2:4)
  expect_equal(length(ms), 3)

  ms <- ExpRmethod(K = 1) * param()
  expect_equal(length(ms), 1)

  ms <- ExpRmethod(K = 1) * param(c = 1)
  expect_equal(length(ms), 1)
  
  ms <- ExpRmethod(K = 1) * param(c = 1:2, b =1:3) +
    ExpRmethod() * param()
  expect_equal(length(ms), 7)
  
  ms <- ms + ms + ms
  expect_equal(length(ms), 21)
  
}
