# solve_bipartite_match

library(Matrix)
library(osqp)

# edges: data.frame with columns a, b (character)
# cA: named numeric vector; names are A node ids; values are supplies (counts)
# tB: named numeric vector; names are B node ids; values are targets
# lambda: optional ridge on x to stabilize / discourage extreme splits
solve_bipartite_match <- function(edges, cA, tB, lambda = 0) {
  stopifnot(is.data.frame(edges), all(c("a", "b") %in% names(edges)))
  stopifnot(is.numeric(cA), !is.null(names(cA)))
  stopifnot(is.numeric(tB), !is.null(names(tB)))

  # Ensure character ids
  edges$a <- as.character(edges$a)
  edges$b <- as.character(edges$b)

  # Node sets (explicit from vectors)
  A_ids <- names(cA)
  B_ids <- names(tB)

  # Basic feasibility checks
  if (any(cA < 0)) {
    stop("cA must be nonnegative.")
  }
  if (any(tB < 0)) {
    stop("tB must be nonnegative (typical for counts).")
  }

  totalA <- sum(cA)
  totalB <- sum(tB)
  if (abs(totalA - totalB) > 1e-8 * max(1, totalA, totalB)) {
    stop(sprintf(
      "Total mass mismatch: sum(cA)=%.6g, sum(tB)=%.6g",
      totalA,
      totalB
    ))
  }

  # Filter edges to those with known endpoints
  edges <- edges[edges$a %in% A_ids & edges$b %in% B_ids, , drop = FALSE]
  if (nrow(edges) == 0) {
    stop("No edges remain after filtering to A_ids and B_ids.")
  }

  # Drop duplicate edges (important!)
  edges <- unique(edges)

  m <- length(A_ids)
  n <- length(B_ids)
  E <- nrow(edges)

  # Map ids to integer indices
  a_idx <- match(edges$a, A_ids) # 1..m
  b_idx <- match(edges$b, B_ids) # 1..n

  # Check each A has at least one outgoing edge
  outdeg <- tabulate(a_idx, nbins = m)
  if (any(outdeg == 0)) {
    missingA <- A_ids[outdeg == 0]
    stop(sprintf(
      "Some A nodes have no outgoing edges: %s",
      paste(head(missingA, 10), collapse = ", ")
    ))
  }

  # Check each B with positive target has at least one incoming edge (optional but helpful)
  indeg <- tabulate(b_idx, nbins = n)
  badB <- (tB > 0) & (indeg == 0)
  if (any(badB)) {
    missingB <- B_ids[badB]
    stop(sprintf(
      "Some B nodes with tB>0 have no incoming edges: %s",
      paste(head(missingB, 10), collapse = ", ")
    ))
  }

  # Build sparse constraint matrix Aeq (m x E): for each edge e=(i,j), contributes to row i
  Aeq <- sparseMatrix(
    i = a_idx,
    j = seq_len(E),
    x = 1,
    dims = c(m, E),
    dimnames = list(A_ids, NULL)
  )

  # Build sparse aggregation matrix Bag (n x E): y = Bag %*% x
  Bag <- sparseMatrix(
    i = b_idx,
    j = seq_len(E),
    x = 1,
    dims = c(n, E),
    dimnames = list(B_ids, NULL)
  )

  # Objective: minimize ||Bag x - t||^2 + lambda ||x||^2
  # Expand: x' (Bag'Bag + lambda I) x - 2 t' Bag x + const
  P <- crossprod(Bag) # E x E sparse
  if (lambda > 0) {
    P <- P + Diagonal(E, x = lambda)
  }
  q <- as.numeric(-2 * crossprod(Bag, tB)) # length E

  # OSQP uses 1/2 x' P x + q' x, so we pass P2 = 2*P and q2 = q
  P2 <- 2 * P
  q2 <- q

  # Constraints: Aeq x = cA, and x >= 0
  # OSQP form: l <= A x <= u
  # Variable bounds must be incorporated by augmenting constraints with identity
  A_osqp <- rbind(Aeq, Diagonal(E))
  l <- c(as.numeric(cA), rep(0, E))      # equality constraints + lower bounds (x >= 0)
  u <- c(as.numeric(cA), rep(Inf, E))    # equality constraints + upper bounds

  # Solve
  model <- osqp(
    P = P2,
    q = q2,
    A = A_osqp,
    l = l,
    u = u,
    pars = list(verbose = FALSE, eps_abs = 1e-8, eps_rel = 1e-8)
  )
  res <- model$Solve()

  if (res$info$status_val %in% c(1L, 2L)) {
    x <- res$x
  } else {
    stop(sprintf("OSQP did not solve optimally. Status: %s", res$info$status))
  }

  # Build outputs
  y <- as.numeric(Bag %*% x)
  names(y) <- B_ids

  # Edge-flow table
  flows <- data.frame(
    a = edges$a,
    b = edges$b,
    x = x,
    stringsAsFactors = FALSE
  )

  list(
    flows = flows,
    y = y,
    target = tB,
    residual = y - tB,
    obj = sum((y - tB)^2),
    solver_info = res$info
  )
}
