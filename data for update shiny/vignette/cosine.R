library(philentropy)

#' Compute cosine similarity between two vectors
#' @param a A numeric vector
#' @param b A numeric vector
#' @return Cosine similarity (0 to 1)
cosine <- function(a, b) {
  a_norm <- a / sum(a)
  b_norm <- b / sum(b)
  dist_mat <- rbind(a_norm, b_norm)
  suppressMessages(
    philentropy::distance(dist_mat, method = "cosine", test.na = FALSE)[1]
  )
}
