test_that("example data doesn't cause error", {
  expect_no_error(make_all_stratum(SC00$FN022, SC00$FN023, SC00$FN024, SC00$FN026, SC00$FN028))
})

test_that("returns a table", {
  strat <- make_all_stratum(SC00$FN022, SC00$FN023, SC00$FN024, SC00$FN026, SC00$FN028)
  expect_true(any(class(strat) == "data.frame"))
})

test_that("returns a table with exactly 1 row", {
  strat <- make_all_stratum(SC00$FN022, SC00$FN023, SC00$FN024, SC00$FN026, SC00$FN028)
  expect_true(nrow(strat) == 1)
})
