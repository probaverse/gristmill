car <- data.frame(
  hp = c(
    110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123,
    180, 180, 180, 205, 215, 230, 66, 52, 65, 97, 150,
    150, 245, 175, 66, 91, 113, 264, 175, 335, 109
  )
)

test_that("next_discrete finite works", {
  expect_equal(
    52,
    next_discrete(dst_empirical(hp, data = car), 1)
  )
  expect_equal(
    97,
    next_discrete(dst_empirical(hp, data = car), 95)
  )
  expect_equal(
    numeric(),
    next_discrete(dst_empirical(hp, data = car), 335)
  )
  expect_equal(
    97,
    next_discrete(dst_empirical(hp, data = car), 95.4654)
  )
  expect_equal(
    1,
    next_discrete(dst_finite(1:5, rep(0.2, 5)), 0)
  )
  expect_equal(
    numeric(),
    next_discrete(dst_finite(1:5, rep(0.2, 5)), 5)
  )
  expect_equal(
    1,
    next_discrete(dst_finite(1:5, rep(0.2, 5)), 0.3235)
  )
  expect_equal(
    numeric(),
    next_discrete(dst_finite(1:5, rep(0.2, 5)), 65.265)
  )
})

rm("car")


test_that("next_discrete degenerate works", {
  expect_equal(numeric(), next_discrete(dst_degenerate(1), 1))
  expect_equal(-15, next_discrete(dst_degenerate(-15), -16))
  expect_equal(numeric(), next_discrete(dst_degenerate(56), 80))
  expect_equal(56, next_discrete(dst_degenerate(56), 16))
})
