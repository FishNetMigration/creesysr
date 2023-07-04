dummydf <- data.frame(STRAT = c("01_13_01_02", "01_11_02_01", "03_22_04_01"))
faildf <- data.frame(X = 1:10)

test_that("parse_STRATA fails when STRAT not present", {
  expect_error(parse_STRAT(faildf))
})

test_that("parse_STRATA returns a data.frame", {
  expect_true(class(parse_STRAT(dummydf)) == "data.frame")
})

test_that("parse_STRATA SSN is correct", {
  expect_equal(parse_STRAT(dummydf)$SSN, as.factor(c("01", "01", "03")))
})

test_that("parse_STRATA DTP is correct", {
  expect_equal(parse_STRAT(dummydf)$DTP, as.factor(c("1", "1", "2")))
})

test_that("parse_STRATA PRD is correct", {
  expect_equal(parse_STRAT(dummydf)$PRD, as.factor(c("3", "1", "2")))
})

test_that("parse_STRATA SPACE is correct", {
  expect_equal(parse_STRAT(dummydf)$SPACE, as.factor(c("01", "02", "04")))
})

test_that("parse_STRATA MODE is correct", {
  expect_equal(parse_STRAT(dummydf)$MODE, as.factor(c("02", "01", "01")))
})


