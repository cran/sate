test_that("select.with.strikes returns a proper probability distribution", {
  out <- select.with.strikes(p_g = 0.70, jury_n = 12, pstrikes = 4, dstrikes = 4, accuracy = 0.15)

  expect_length(out, 13)
  expect_true(all(is.finite(out)))
  expect_true(all(out >= 0))
  expect_true(all(out <= 1))
  expect_equal(sum(out), 1, tolerance = 1e-10)
})

test_that("transition.matrix is column-stochastic with absorbing endpoints", {
  P <- transition.matrix(12)

  expect_equal(dim(P), c(13, 13))
  expect_true(all(is.finite(P)))
  expect_true(all(P >= 0))
  expect_equal(colSums(P), rep(1, 13), tolerance = 1e-12)
  expect_equal(P[1, 1], 1)
  expect_equal(P[13, 13], 1)
})

test_that("get_pG_by_k has valid shape and bounds", {
  p <- get_pG_by_k(12)

  expect_length(p, 13)
  expect_true(all(is.finite(p)))
  expect_true(all(p >= 0))
  expect_true(all(p <= 1))
  expect_equal(p[1], 0, tolerance = 1e-12)
  expect_equal(p[13], 1, tolerance = 1e-12)
})

test_that("as.jury.point handles scalar and vector inputs", {
  scalar <- as.jury.point(sample_pg = 0.6, jury_n = 12)
  vec <- as.jury.point(sample_pg = c(0.2, 0.5, 0.8), jury_n = 12)

  expect_length(scalar, 1)
  expect_true(is.numeric(scalar))
  expect_true(scalar >= 0 && scalar <= 1)

  expect_length(vec, 3)
  expect_true(all(vec >= 0))
  expect_true(all(vec <= 1))
})

test_that("as.jury.stats reports coherent inferential quantities", {
  out <- as.jury.stats(sample_pg = 0.6, sample_n = 400, jury_n = 12, digits = 8)

  expect_true(is.data.frame(out))
  expect_true(all(c("SE", "MOE", "Lower 5%", "Upper 95%") %in% names(out)))
  expect_true(any(c("P(G)", "PG") %in% names(out)))
  pg <- if ("P(G)" %in% names(out)) out[["P(G)"]] else out[["PG"]]
  expect_true(out$SE >= 0)
  expect_true(out$MOE >= 0)
  expect_true(out$`Lower 5%` <= pg)
  expect_true(pg <= out$`Upper 95%`)
})

test_that("compare.jury.stats difference matches actual minus hypothetical", {
  out <- compare.jury.stats(
    pg_actual = 0.70, n_actual = 400,
    pg_hypo = 0.60, n_hypo = 400,
    jury_n = 12, digits = 8
  )

  expect_true(is.list(out))
  expect_true(all(c("actual_jury", "hypo_jury", "difference") %in% names(out)))

  lhs <- out$actual_jury$`P(G|actual)` - out$hypo_jury$`P(G|hypo)`
  rhs <- out$difference$`Difference in P(G)`
  expect_equal(lhs, rhs, tolerance = 1e-8)
})

test_that("deliberation functions return valid labels", {
  set.seed(123)
  crim <- replicate(20, deliberate(g_votes = 6, jury_n = 12))
  civ <- replicate(20, deliberate.civil(p_votes = 6, jury_n = 12))

  expect_true(all(crim %in% c("G", "NG")))
  expect_true(all(civ %in% c("P", "D")))
})
