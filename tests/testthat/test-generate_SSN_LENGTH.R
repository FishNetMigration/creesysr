test_that("example data doesn't cause error", {
  expect_no_error(generate_SSN_LENGTH(SC00$FN022, SC00$FN023, SC00$FN024, SC00$FN025))
})

test_that("returns a table", {
  SSN_LENGTH <- generate_SSN_LENGTH(SC00$FN022, SC00$FN023, SC00$FN024, SC00$FN025)
  expect_true(class(SSN_LENGTH) == "data.frame")
})

test_that("returns a table with exactly 1 row", {
  SSN_LENGTH <- generate_SSN_LENGTH(SC00$FN022, SC00$FN023, SC00$FN024, SC00$FN025)
  expect_true(nrow(SSN_LENGTH) == 1)
})
