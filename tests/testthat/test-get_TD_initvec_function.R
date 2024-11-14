listA_test <- list(matrix(c(0, 0.5, 0, 0, 0, 0.6, 2, 0, 0.6), nrow = 3, ncol = 3),
                   matrix(c(0, 0.4, 0, 0, 0, 0.5, 2, 0, 0.6), nrow = 3, ncol = 3))

test_that("function crashes if listA has length 0", {
  expect_error(get_TD_vector(c(0, 0, 1), list()))
})

test_that("function works if listA has length 1", {
  expect_equal(get_TD_vector(c(0, 0, 1), listA_test[1]), list(c(0, 0, 1)))
})

test_that("function works if listA has length > 1", {
  expect_equal(lapply(get_TD_vector(c(0, 0, 1), listA_test), round, digits = 2),
               lapply(list(c(0, 0, 1), c(0.769230769230769, 0, 0.230769230769231)), round, digits = 2))
})
