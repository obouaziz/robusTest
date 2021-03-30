x <- c(1,2,2,3,4,5,5,5,7)
xbreak=tiebreak(x)

y <- c(4,9,12,11,2,10)
xy_break=tiebreak(x,y)


test_that("tiebreak works well",{
  expect_equal(sum(duplicated(xbreak)),0)
  expect_equal(sum(xy_break$x%in%xy_break$y),0)
  expect_equal(length(tiebreak(x,y,nb_break=TRUE)),3)
  expect_equal(length(tiebreak(x,nb_break=TRUE)),2)
})
