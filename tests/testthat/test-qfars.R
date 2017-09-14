test_that("check the year from read data",{
  expect_that(make_filename(2017), is_identical_to("accident_2017.csv.bz2"))
})
