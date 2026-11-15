test_that("exact draw probabilities handle boundaries and hypergeometric draws", {
  expect_equal(draw_probability(4,52),1/13)
  expect_equal(draw_probability(4,52,2),1-(48*47)/(52*51))
  expect_equal(draw_probability(0,0,0),0)
  expect_equal(draw_probability(4,4,1),1)
  expect_equal(draw_probability(4,52,0),0)
  expect_error(draw_probability(53,52),"successes")
})

test_that("statistical reports distinguish exact samples and uncertainty", {
  summary <- estimate_summary(c(0,0),binary=TRUE)
  expect_gt(summary$upper,0)
  expect_equal(estimate_summary(c(0,1),exact=TRUE)$standard_error,0)
  expect_lt(estimate_summary(c(1,1),bounds=c(0,1))$lower,1)
  expect_true(is.na(estimate_summary(1)$standard_error))
  expect_true(is.na(estimate_summary(numeric())$estimate))
  expect_error(estimate_hand(list()),"public game observation")
})
