test_that("transition.matrix.ordered is column-stochastic and has metadata", {
  P <- transition.matrix.ordered(3, c("NG", "M2", "M1"))
  meta <- attr(P, "meta")

  expect_true(is.matrix(P))
  expect_true(all(is.finite(P)))
  expect_true(all(P >= -1e-12))
  expect_equal(unname(colSums(P)), rep(1, ncol(P)), tolerance = 1e-12)

  expect_true(is.list(meta))
  expect_true(all(c("T", "S", "K", "n", "verdict_options") %in% names(meta)))
  expect_equal(meta$n, 3)
  expect_equal(meta$K, 3)
  expect_equal(meta$S, ncol(P))
})

test_that("prob.ordered.verdicts columns sum to one and labels match", {
  labs <- c("NG", "M2", "M1")
  B <- prob.ordered.verdicts(3, labs, digits = NULL, collab = TRUE)

  expect_true(is.matrix(B))
  expect_equal(rownames(B), labs)
  expect_true(all(is.finite(B)))
  expect_true(all(B >= -1e-12))
  expect_true(all(B <= 1 + 1e-12))
  expect_equal(unname(colSums(B)), rep(1, ncol(B)), tolerance = 1e-10)
})

test_that("prob_ord_from_pool returns named distribution", {
  out <- prob_ord_from_pool(
    jury_n = 6,
    verdict_options = c("NG", "M2", "M1"),
    verdict_props = c(0.25, 0.50, 0.25),
    digits = NULL
  )

  expect_type(out, "double")
  expect_equal(names(out), c("NG", "M2", "M1"))
  expect_true(all(is.finite(out)))
  expect_true(all(out >= -1e-12))
  expect_true(all(out <= 1 + 1e-12))
  expect_equal(sum(out), 1, tolerance = 1e-10)
})
