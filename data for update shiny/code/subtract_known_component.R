#' Subtract a known component from a mixture spectrum.
#'
#' Given a known component A and a mixed observation C, find the maximum
#' weight w of A's proportions that can be subtracted from C while allowing
#' at most \code{max_allowed_negative_proportion} negative proportion in any
#' channel, then return the clamped and normalized residual as the estimated
#' unknown component.
#'
#' @param A Known component (counts or proportions). Can be a vector or
#'   single-column data frame.
#' @param C Mixed observation (counts). Can be a vector or single-column
#'   data frame.
#' @param max_allowed_negative_proportion Maximum negative proportion allowed
#'   in any channel before clamping to zero.
#'
#' @return A list with:
#'   \describe{
#'     \item{weights}{List with known and unknown component weights}
#'     \item{residual}{Named numeric vector of estimated unknown component
#'       proportions}
#'   }
subtract_known_component <- function(
  A,
  C,
  max_allowed_negative_proportion = 0.01
) {
  # Coerce data frames to named numeric vectors
  if (is.data.frame(A)) {
    nms <- rownames(A)
    A <- as.numeric(A[[1]])
    names(A) <- nms
  }
  if (is.data.frame(C)) {
    nms <- rownames(C)
    C <- as.numeric(C[[1]])
    names(C) <- nms
  }

  # Known component proportions
  alpha <- A / sum(A)
  c_prop <- C / sum(C)

  # Find maximum mixing weight w such that
  # c_prop - w*alpha >= -max_allowed_negative_proportion
  positive_alpha <- alpha > 0
  w_max <- min(
    (c_prop[positive_alpha] + max_allowed_negative_proportion) /
      alpha[positive_alpha]
  )

  # Compute residual (unknown component)
  residual <- c_prop - w_max * alpha

  results <- list(
    weights = list(
      known = w_max,
      unknown = 1 - w_max
    ),
    residual = residual
  )

  return(results)
}
