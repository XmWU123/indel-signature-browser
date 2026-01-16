# Plotting parameters for vignette
# Changes to this file will trigger plot regeneration

plot476_base_size <- 25
plot476_label_size <- 3
plto476_simplify_labels <- FALSE

ppar <- list(
  w476 = 19,
  h476 = 6,

  # For type-89 plots
  w89 = 24,
  h89 = 7,
  hw89 = "80%",
  basesize89 = 30,
  textsize89 = 8,
  topbartextsize89 = 4,
  extra89y = 1.1,

  # For type-83 plots
  w83 = 19,
  h83 = 6,
  basesize83 = 25,
  textsize83 = 5,

  # Plot scaling for the html output
  hw = "100%", # Default

  hw83 = "80%",

  cosine_digits = 4
)

getp <- function(parname) {
  ret = ppar[[parname]]
  if (is.null(ret)) {
    stop("unknown parameter: ", parname)
  }
  return(ret)
}
