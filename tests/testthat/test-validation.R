test_that("assert_required catches missing required args in public functions", {
  expect_error(
    as.jury.stats(sample_n = 100),
    "Missing required argument\\(s\\): sample_pg"
  )

  expect_error(
    compare.jury.stats(pg_actual = 0.7, n_actual = 100, n_hypo = 100),
    "Missing required argument\\(s\\): pg_hypo"
  )

  expect_error(
    transition.matrix(),
    "Missing required argument\\(s\\): jury_n"
  )
})

test_that("edge probabilities at 0 and 1 are well behaved", {
  p0 <- as.jury.point(sample_pg = 0, jury_n = 12)
  p1 <- as.jury.point(sample_pg = 1, jury_n = 12)

  expect_true(is.finite(p0))
  expect_true(is.finite(p1))
  expect_true(p0 >= 0 && p0 <= 1)
  expect_true(p1 >= 0 && p1 <= 1)
  expect_true(p0 <= p1)
})
