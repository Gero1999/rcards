# test-table.R: Tests for table S3 class and methods

test_that("table constructor creates correct object", {
  t1 <- table()
  expect_s3_class(t1, "table")
  expect_type(t1$decks, "list")
  expect_equal(t1$points, 0)
})

test_that("print.table outputs expected string", {
  t2 <- table(decks = list(deck()), points = 10)
  expect_output(print(t2), "Table (")
})

# Edge case: table with multiple decks and points

test_that("table handles multiple decks and points", {
  t3 <- table(decks = list(deck(), deck()), points = 50)
  expect_length(t3$decks, 2)
  expect_equal(t3$points, 50)
})
