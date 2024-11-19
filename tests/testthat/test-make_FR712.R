test_that("example data doesn't cause error", {
  expect_no_error(make_FR712(SC00$FN022, SC00$FN023, SC00$FN024, SC00$FN025, SC00$FN026, SC00$FN028, SC00$FN111))
})

test_that("returns a table", {
  FR712 <- make_FR712(SC00$FN022, SC00$FN023, SC00$FN024, SC00$FN025, SC00$FN026, SC00$FN028, SC00$FN111)
  expect_true(any(class(FR712) == "data.frame"))
})

test_that("returns a table with exactly 1 row", {
  FR712 <- make_FR712(SC00$FN022, SC00$FN023, SC00$FN024, SC00$FN025, SC00$FN026, SC00$FN028, SC00$FN111)
  expect_true(nrow(FR712) == 2)
})
