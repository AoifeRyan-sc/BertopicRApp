test_that("multiplication works", {
  test_that("sliderInput01() creates expected HTML", {
    expect_snapshot(sliderInput01("x"))
  })
})
