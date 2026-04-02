# test - solve_bipartite_match.R
library(testthat)

test_that("solve_bipartite_match solves an exactly-feasible instance and satisfies constraints", {
  # A supplies
  cA <- c(a1 = 1, a2 = 2, a3 = 3)

  # B targets (total mass matches: 6)
  tB <- c(b1 = 2, b2 = 4)

  # Edges allow exact match:
  # a1 -> b1
  # a2 -> b1 or b2
  # a3 -> b2
  edges <- data.frame(
    a = c("a1", "a2", "a2", "a3"),
    b = c("b1", "b1", "b2", "b2"),
    stringsAsFactors = FALSE
  )

  fit <- solve_bipartite_match(edges, cA, tB, lambda = 1e-10)

  # Basic structure
  expect_true(is.list(fit))
  expect_true(all(
    c("flows", "y", "target", "residual", "obj", "solver_info") %in% names(fit)
  ))

  # Nonnegativity of flows
  expect_true(all(fit$flows$x >= -1e-10))

  # Mass conservation on A: sum over outgoing per a == cA
  out_by_a <- tapply(fit$flows$x, fit$flows$a, sum)
  out_by_a <- out_by_a[names(cA)] # align
  expect_equal(as.numeric(out_by_a), as.numeric(cA), tolerance = 1e-6)

  # Totals on B: sum over incoming per b == y
  in_by_b <- tapply(fit$flows$x, fit$flows$b, sum)
  in_by_b <- in_by_b[names(tB)] # align
  expect_equal(as.numeric(in_by_b), as.numeric(fit$y), tolerance = 1e-6)

  # Total mass matches
  expect_equal(sum(fit$y), sum(cA), tolerance = 1e-6)
  expect_equal(sum(fit$y), sum(tB), tolerance = 1e-6)

  # This instance is exactly solvable -> y should match tB (nearly exactly)
  expect_equal(as.numeric(fit$y[names(tB)]), as.numeric(tB), tolerance = 1e-5)
  expect_lt(fit$obj, 1e-8)
})

test_that("solve_bipartite_match ignores duplicate edges (unique() is applied)", {
  cA <- c(a1 = 1, a2 = 1)
  tB <- c(b1 = 1, b2 = 1)

  edges <- data.frame(
    a = c("a1", "a1", "a2", "a2"),
    b = c("b1", "b1", "b2", "b2"), # duplicates
    stringsAsFactors = FALSE
  )

  fit <- solve_bipartite_match(edges, cA, tB, lambda = 1e-10)

  # Because duplicates are dropped, flows should have only 2 rows
  expect_equal(nrow(fit$flows), 2)

  # Still feasible and exact
  expect_equal(sum(fit$y), sum(tB), tolerance = 1e-6)
  expect_equal(as.numeric(fit$y), as.numeric(tB), tolerance = 1e-5)
})

test_that("solve_bipartite_match errors on total mass mismatch", {
  cA <- c(a1 = 1, a2 = 2) # total 3
  tB <- c(b1 = 1, b2 = 1) # total 2 (mismatch)

  edges <- data.frame(
    a = c("a1", "a2", "a2"),
    b = c("b1", "b1", "b2"),
    stringsAsFactors = FALSE
  )

  expect_error(
    solve_bipartite_match(edges, cA, tB),
    "Total mass mismatch"
  )
})

test_that("solve_bipartite_match errors when an A node has no outgoing edges", {
  cA <- c(a1 = 1, a2 = 1, a3 = 1)
  tB <- c(b1 = 2, b2 = 1)

  # a3 missing from edges -> should error
  edges <- data.frame(
    a = c("a1", "a2"),
    b = c("b1", "b2"),
    stringsAsFactors = FALSE
  )

  expect_error(
    solve_bipartite_match(edges, cA, tB),
    "no outgoing edges"
  )
})

test_that("solve_bipartite_match errors when a positive-target B node has no incoming edges", {
  cA <- c(a1 = 1, a2 = 1)
  tB <- c(b1 = 1, b2 = 1)

  # b2 has tB>0 but no incoming edges
  edges <- data.frame(
    a = c("a1", "a2"),
    b = c("b1", "b1"),
    stringsAsFactors = FALSE
  )

  expect_error(
    solve_bipartite_match(edges, cA, tB),
    "tB>0 have no incoming edges"
  )
})
