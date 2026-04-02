# collapse_476_to_83.R
#
# Collapse 476-type signatures to 83-type using bipartite matching.
# Uses solve_bipartite_match() to optimally redistribute each 476-type
# signature to match the corresponding 83-type signature as closely as possible.

# Use here::here() for robust path resolution when sourced from different directories
source(here::here("data for update shiny", "code", "solve_bipartite_match.R"))


#' Generate missing edges for R(9,) types
#'
#' The 476-type signatures use `R(9,)` format for 9+ repeats, but the
#' unique_pairs.tsv mapping uses specific repeat numbers (R9, R10, etc.).
#' This function generates the missing edges for R(9,) types.
#'
#' @return A data.frame with columns a (476-type) and b (83-type)
generate_r9plus_edges <- function() {
  # R(9,) types need to map to the 5+ repeat categories in 83-type
  # Pattern: X[Op(Base):R(9,)]Y -> OP:Base:1:5+

  # Del(C) -> DEL:C:1:5+
  del_c_bases <- c("A", "G", "T")
  del_c_flanks <- expand.grid(left = del_c_bases, right = del_c_bases)
  del_c_edges <- data.frame(
    a = paste0(del_c_flanks$left, "[Del(C):R(9,)]", del_c_flanks$right),
    b = "DEL:C:1:5+",
    stringsAsFactors = FALSE
  )

  # Del(T) -> DEL:T:1:5+
  del_t_bases <- c("A", "C", "G")
  del_t_flanks <- expand.grid(left = del_t_bases, right = del_t_bases)
  del_t_edges <- data.frame(
    a = paste0(del_t_flanks$left, "[Del(T):R(9,)]", del_t_flanks$right),
    b = "DEL:T:1:5+",
    stringsAsFactors = FALSE
  )

  # Ins(C) -> INS:C:1:5+
  ins_c_bases <- c("A", "G", "T")
  ins_c_flanks <- expand.grid(left = ins_c_bases, right = ins_c_bases)
  ins_c_edges <- data.frame(
    a = paste0(ins_c_flanks$left, "[Ins(C):R(9,)]", ins_c_flanks$right),
    b = "INS:C:1:5+",
    stringsAsFactors = FALSE
  )

  # Ins(T) -> INS:T:1:5+
  ins_t_bases <- c("A", "C", "G")
  ins_t_flanks <- expand.grid(left = ins_t_bases, right = ins_t_bases)
  ins_t_edges <- data.frame(
    a = paste0(ins_t_flanks$left, "[Ins(T):R(9,)]", ins_t_flanks$right),
    b = "INS:T:1:5+",
    stringsAsFactors = FALSE
  )

  rbind(del_c_edges, del_t_edges, ins_c_edges, ins_t_edges)
}

#' Collapse a 476-type signature to 83-type
#'
#' Uses bipartite matching to optimally redistribute a 476-type signature
#' to match a corresponding 83-type signature as closely as possible.
#'
#' @param sig_476_name Column name from 476-type signatures (e.g., "InsDel1a")
#' @param sig_83_name Column name from 83-type signatures (e.g., "C_ID1")
#' @param data_dir Directory containing the data files
#'
#' @return A list containing:
#'   \item{flows}{data.frame with columns a (476-type), b (83-type), x (flow)}
#'   \item{y}{Named numeric vector: the collapsed 83-type signature}
#'   \item{target}{Named numeric vector: the original 83-type target signature}
#'   \item{residual}{Named numeric vector: y - target (difference from target)}
#'   \item{obj}{Numeric: objective value (sum of squared errors)}
#'   \item{solver_info}{OSQP solver information}
#'
#' @examples
#' \dontrun{
#' result <- collapse_476_to_83("InsDel1a", "C_ID1")
#' sum(result$y)      # Should be ~1.0
#' result$obj         # Squared error (should be small)
#' head(result$flows) # Flow details
#' }
collapse_476_to_83 <- function(
  sig_476_name,
  sig_83_name,
  data_dir = "Manuscript_data",
  sig_dir = NULL
) {
  # sig_dir: directory containing signature files; defaults to data_dir
  if (is.null(sig_dir)) {
    sig_dir <- data_dir
  }

  # Load edges from unique_pairs.tsv
  # Column 1 (COSMIC_83) becomes b (target)
  # Column 2 (Koh_476) becomes a (source)
  edges_file <- file.path(data_dir, "unique_pairs.tsv")
  edges_raw <- read.delim(edges_file, stringsAsFactors = FALSE)
  edges <- data.frame(
    a = edges_raw$Koh_476, # 476-type (source)
    b = edges_raw$COSMIC_83, # 83-type (target)
    stringsAsFactors = FALSE
  )

  # Add missing edges for R(9,) types (9+ repeats)
  # The signature files use R(9,) format but unique_pairs.tsv uses specific
  # repeat numbers like R9, R10, etc. We need to add mappings for R(9,) to
  # the 5+ repeat categories.
  r9_edges <- generate_r9plus_edges()
  edges <- rbind(edges, r9_edges)
  message(sprintf("Added %d edges for R(9,) types", nrow(r9_edges)))

  # Load 476-type signatures (supplies: cA)
  sig_476_file <- file.path(sig_dir, "liu_et_al_476_signatures.tsv")
  sig_476_df <- read.delim(sig_476_file, row.names = 1, check.names = FALSE)

  if (!sig_476_name %in% colnames(sig_476_df)) {
    stop(sprintf(
      "Signature '%s' not found in 476-type signatures. Available: %s",
      sig_476_name,
      paste(head(colnames(sig_476_df), 10), collapse = ", ")
    ))
  }

  cA <- sig_476_df[[sig_476_name]]
  names(cA) <- rownames(sig_476_df)

  # Filter out Complex types which have no mapping to 83-type
  # (and typically have zero mass anyway)
  complex_types <- names(cA)[grepl("^Complex", names(cA))]
  if (length(complex_types) > 0) {
    complex_mass <- sum(cA[complex_types])
    if (complex_mass > 1e-10) {
      warning(sprintf(
        "Excluding %d Complex types with total mass %.6g",
        length(complex_types),
        complex_mass
      ))
    }
    cA <- cA[!names(cA) %in% complex_types]
    message(sprintf("Filtered out %d Complex types", length(complex_types)))
  }

  # Load 83-type signatures (targets: tB)
  sig_83_file <- file.path(sig_dir, "liu_et_al_83_signatures.tsv")
  sig_83_df <- read.delim(sig_83_file, row.names = 1, check.names = FALSE)

  if (!sig_83_name %in% colnames(sig_83_df)) {
    stop(sprintf(
      "Signature '%s' not found in 83-type signatures. Available: %s",
      sig_83_name,
      paste(head(colnames(sig_83_df), 10), collapse = ", ")
    ))
  }

  tB <- sig_83_df[[sig_83_name]]
  names(tB) <- rownames(sig_83_df)

  # Verify mass balance (both should sum to ~1.0 for normalized signatures)
  total_cA <- sum(cA)
  total_tB <- sum(tB)
  message(sprintf("Sum of 476-type signature (cA): %.10f", total_cA))
  message(sprintf("Sum of 83-type signature (tB): %.10f", total_tB))

  # Always normalize both to sum to exactly 1.0 to ensure mass balance
  # The solver requires exact mass balance
  cA <- cA / total_cA
  tB <- tB / total_tB
  message("Normalized both signatures to sum to exactly 1.0")

  # Solve the bipartite matching problem
  result <- solve_bipartite_match(edges, cA, tB)

  # Clean up numerical precision issues (tiny negative flows from QP solver)
  eps <- 1e-9
  neg_flows <- sum(result$flows$x < -eps)
  if (neg_flows > 0) {
    warning(sprintf(
      "%d flows have significant negative values (< -%.0e)",
      neg_flows,
      eps
    ))
  }
  result$flows$x <- pmax(result$flows$x, 0)

  message(sprintf("Objective (squared error): %.6g", result$obj))
  message(sprintf("Sum of collapsed signature (y): %.6f", sum(result$y)))

  result
}


#' Get 476→83 signature mapping from connection file
#'
#' Reads the 89type_to_83type_connection.tsv file and returns a data frame
#' mapping 476-type signature names to their corresponding 83-type signatures.
#'
#' @return A data.frame with columns:
#'   \item{sig_476}{476-type signature name (e.g., "InsDel1a")}
#'   \item{sig_83}{Corresponding 83-type signature name (e.g., "C_ID1")}
get_signature_mapping <- function() {
  conn <- read_finalized("connection_table", row.names = NULL)

  # The connection file maps InDel89 -> InDel83
  # InDel89 column has names like "InsDel1a" which are also the 476-type column names
  # InDel83 column has names like "C_ID1"
  data.frame(
    sig_476 = conn$InDel89,
    sig_83 = conn$InDel83,
    stringsAsFactors = FALSE
  )
}


#' Collapse and plot all matching 476-type signatures to 83-type
#'
#' Iterates through all signature pairs defined in the connection file,
#' collapses each 476-type signature to its corresponding 83-type, and
#' generates PDF plots for each pair.
#'
#' @param data_dir Directory containing the data files
#' @param out_dir Output directory for the PDFs
#' @param min_flow Minimum flow value to display in Sankey plots
#' @param all_sankey If TRUE, include all Sankey plots in each PDF
#'
#' @return A list with one element per successfully collapsed signature pair.
#'   Each element is named by the 476-type signature name and contains the
#'   result from collapse_476_to_83().
collapse_and_plot_all_signatures <- function(
  data_dir = "Manuscript_data",
  out_dir = "output",
  min_flow = 0.001,
  all_sankey = FALSE
) {
  mapping <- get_signature_mapping()

  # Load signature files to check which signatures exist
  sig_476_file <- file.path(data_dir, "Liu_et_al_final_476_type_signatures.tsv")
  sig_476_df <- read.delim(sig_476_file, row.names = 1, check.names = FALSE)
  available_476 <- colnames(sig_476_df)

  sig_83_file <- file.path(data_dir, "Liu_et_al_final_83_type_signatures.tsv")
  sig_83_df <- read.delim(sig_83_file, row.names = 1, check.names = FALSE)
  available_83 <- colnames(sig_83_df)

  results <- list()

  for (i in seq_len(nrow(mapping))) {
    sig_476 <- mapping$sig_476[i]
    sig_83 <- mapping$sig_83[i]

    if (sig_476 %in% available_476 && sig_83 %in% available_83) {
      message(sprintf("\n=== Collapsing %s -> %s ===", sig_476, sig_83))
      tryCatch(
        {
          results[[sig_476]] <- plot_collapse_476_to_83(
            sig_476,
            sig_83,
            data_dir = data_dir,
            out_dir = out_dir,
            min_flow = min_flow,
            all_sankey = all_sankey
          )
        },
        error = function(e) {
          message(sprintf(
            "Error collapsing %s -> %s: %s",
            sig_476,
            sig_83,
            e$message
          ))
        }
      )
    } else {
      if (!sig_476 %in% available_476) {
        message(sprintf(
          "Skipping %s: not found in 476-type signatures",
          sig_476
        ))
      }
      if (!sig_83 %in% available_83) {
        message(sprintf(
          "Skipping %s -> %s: %s not found in 83-type signatures",
          sig_476,
          sig_83,
          sig_83
        ))
      }
    }
  }

  results
}


#' Extract repeat number from 476-type labels
#'
#' Extracts the number following :R in patterns like A[Ins(T):R2]A or G[Del(C):R(9,)]G
#'
#' @param x Character vector of 476-type labels
#' @return Numeric vector of repeat numbers (9 for R(9,) patterns)
extract_repeat_number <- function(x) {
  sapply(
    x,
    function(s) {
      # Handle R(9,) pattern -> 9
      if (grepl(":R\\(9,\\)", s)) {
        return(9)
      }
      # Handle :Rn pattern where n is a number
      m <- regmatches(s, regexpr(":R(\\d+)", s))
      if (length(m) > 0 && nchar(m) > 0) {
        return(as.numeric(gsub(":R", "", m)))
      }
      return(NA_real_)
    },
    USE.NAMES = FALSE
  )
}


#' Extract base (C or T) from 476-type labels
#'
#' Extracts the base from patterns like A[Ins(T):R2]A or G[Del(C):R7]G
#'
#' @param x Character vector of 476-type labels
#' @return Character vector ("C" or "T")
extract_base <- function(x) {
  sapply(
    x,
    function(s) {
      if (grepl("\\(C\\)", s)) {
        return("C")
      }
      if (grepl("\\(T\\)", s)) {
        return("T")
      }
      return(NA_character_)
    },
    USE.NAMES = FALSE
  )
}


#' Extract final number from 83-type labels
#'
#' Extracts the final number from patterns like INS:T:1:2 or DEL:C:1:5+
#'
#' @param x Character vector of 83-type labels
#' @return Numeric vector (5 for 5+ patterns)
extract_83_final_number <- function(x) {
  sapply(
    x,
    function(s) {
      final_part <- sub(".*:", "", s)
      if (final_part == "5+") {
        return(5)
      }
      return(suppressWarnings(as.numeric(final_part)))
    },
    USE.NAMES = FALSE
  )
}


#' Extract first number from identifier
#'
#' Extracts the first number from identifiers like "Ins4:U4:R0", "Del(7,):M3",
#' "INS:repeats:4:0", "DEL:MH:5+:3"
#'
#' @param x Character vector of identifiers
#' @return Numeric vector
extract_first_number <- function(x) {
  sapply(
    x,
    function(s) {
      # Find first number (including 5+)
      m <- regmatches(s, regexpr("\\d+", s))
      if (length(m) > 0 && nchar(m) > 0) {
        return(as.numeric(m))
      }
      return(NA_real_)
    },
    USE.NAMES = FALSE
  )
}


#' Check if identifier contains M (microhomology)
#'
#' @param x Character vector of identifiers
#' @return Logical vector
has_microhomology <- function(x) {
  grepl("M", x)
}


#' Create a Sankey/alluvial plot from flow data
#'
#' Helper function to create a single ggalluvial diagram from flow data.
#'
#' @param flows data.frame with columns a, b, x (source, target, flow value)
#' @param title Plot title
#' @param min_flow Minimum flow value to include (filters out tiny flows)
#' @param order_by_repeat If TRUE, order left stratum by repeat number (:R) and
#'   right stratum by final number (0-5+). Use for single-base ins/del plots.
#' @param order_by_mh_then_number If TRUE, order strata with M identifiers first,
#'   then by first number. Use for "other deletions" plots.
#' @param order_by_first_number If TRUE, order strata by first number only.
#'   Use for "other insertions" plots.
#'
#' @return A ggplot2 alluvial diagram object
create_sankey_plot <- function(
  flows,
  title = "",
  min_flow = 1e-6,
  order_by_repeat = FALSE,
  order_by_mh_then_number = FALSE,
  order_by_first_number = FALSE
) {
  if (!requireNamespace("ggalluvial", quietly = TRUE)) {
    stop("Package 'ggalluvial' is required for Sankey plots")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for Sankey plots")
  }

  # Filter out tiny flows
  flows <- flows[flows$x >= min_flow, , drop = FALSE]

  if (nrow(flows) == 0) {
    message(sprintf("No flows above threshold %.2e for: %s", min_flow, title))
    return(NULL)
  }

  # Prepare data for ggalluvial (needs factor columns for axes)
  plot_data <- data.frame(
    source = flows$a,
    target = flows$b,
    flow = flows$x,
    stringsAsFactors = FALSE
  )

  # Order strata
  if (order_by_repeat) {
    # Order left stratum by repeat number, then by base (C before T)
    source_levels <- unique(plot_data$source)
    source_repeat <- extract_repeat_number(source_levels)
    source_base <- extract_base(source_levels)
    # C = 1, T = 2 for ordering
    source_base_num <- ifelse(source_base == "C", 1, 2)
    source_order <- order(source_repeat, source_base_num)
    plot_data$source <- factor(
      plot_data$source,
      levels = source_levels[source_order]
    )

    # Order right stratum by final number (0, 1, 2, 3, 4, 5+), then by base (C before T)
    target_levels <- unique(plot_data$target)
    target_final <- extract_83_final_number(target_levels)
    # Extract base from 83-type (e.g., INS:T:1:2 -> T, DEL:C:1:0 -> C)
    target_base <- sapply(
      target_levels,
      function(s) {
        if (grepl(":C:", s)) {
          return("C")
        }
        if (grepl(":T:", s)) {
          return("T")
        }
        return(NA_character_)
      },
      USE.NAMES = FALSE
    )
    target_base_num <- ifelse(target_base == "C", 1, 2)
    target_order <- order(target_final, target_base_num)
    plot_data$target <- factor(
      plot_data$target,
      levels = target_levels[target_order]
    )
  } else if (order_by_mh_then_number) {
    # Order strata with M identifiers first, then by first number
    source_levels <- unique(plot_data$source)
    source_has_m <- has_microhomology(source_levels)
    source_first_num <- extract_first_number(source_levels)
    # M = 0 (first), no M = 1 (second)
    source_m_order <- ifelse(source_has_m, 0, 1)
    source_order <- order(source_m_order, source_first_num)
    plot_data$source <- factor(
      plot_data$source,
      levels = source_levels[source_order]
    )

    target_levels <- unique(plot_data$target)
    target_has_m <- has_microhomology(target_levels)
    target_first_num <- extract_first_number(target_levels)
    target_m_order <- ifelse(target_has_m, 0, 1)
    target_order <- order(target_m_order, target_first_num)
    plot_data$target <- factor(
      plot_data$target,
      levels = target_levels[target_order]
    )
  } else if (order_by_first_number) {
    # Order strata by first number only
    source_levels <- unique(plot_data$source)
    source_first_num <- extract_first_number(source_levels)
    source_order <- order(source_first_num)
    plot_data$source <- factor(
      plot_data$source,
      levels = source_levels[source_order]
    )

    target_levels <- unique(plot_data$target)
    target_first_num <- extract_first_number(target_levels)
    target_order <- order(target_first_num)
    plot_data$target <- factor(
      plot_data$target,
      levels = target_levels[target_order]
    )
  } else {
    # Order by flow for better visual appearance
    plot_data <- plot_data[order(-plot_data$flow), ]
  }

  # Create alluvial plot
  # Use lode.guidance = "forward" to minimize unnecessary crossings
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(axis1 = source, axis2 = target, y = flow)
  ) +
    ggalluvial::geom_alluvium(
      ggplot2::aes(fill = source),
      width = 1 / 12,
      alpha = 0.7,
      lode.guidance = "zigzag"
    ) +
    ggalluvial::geom_stratum(
      width = 1 / 12,
      fill = "grey80",
      color = "grey30"
    ) +
    ggplot2::geom_text(
      stat = ggalluvial::StatStratum,
      ggplot2::aes(label = ggplot2::after_stat(stratum)),
      size = 2.5
    ) +
    ggplot2::scale_x_discrete(
      limits = c("476-type", "83-type"),
      expand = c(0.1, 0.1)
    ) +
    ggplot2::labs(title = title, y = "Flow") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 12)
    )

  p
}


#' Visualize collapse_476_to_83 output as Sankey/alluvial plots
#'
#' Creates three alluvial diagrams (using ggalluvial) showing the flow from
#' 476-type to 83-type:
#' \enumerate{
#'   \item Insertions of single T or C (A labels matching `Ins([CT])`)
#'   \item Deletions of single T or C (A labels matching `Del([TC])`)
#'   \item Everything else (repeats, microhomology, etc.)
#' }
#'
#' @param result Output from collapse_476_to_83()
#' @param min_flow Minimum flow value to display (filters out tiny flows)
#' @param title_prefix Prefix for plot titles
#'
#' @return A list with eight ggplot2 alluvial diagram objects:
#'   \item{insertions}{Alluvial plot for single-base insertions (T/C combined)}
#'   \item{insertions_c}{Alluvial plot for single-base insertions (C only)}
#'   \item{insertions_t}{Alluvial plot for single-base insertions (T only)}
#'   \item{deletions}{Alluvial plot for single-base deletions (T/C)}
#'   \item{other}{Alluvial plot for all other mutation types (repeats, MH)}
#'   \item{other_ins}{Alluvial plot for other insertions (repeats)}
#'   \item{other_del}{Alluvial plot for other deletions (repeats, MH)}
#'   \item{insertions_t_no5plus}{Alluvial plot for single T insertions, excluding INS:T:1:5+}
#'
#' @examples
#' \dontrun{
#' result <- collapse_476_to_83("InsDel1a", "C_ID1")
#' plots <- plot_collapse_sankey(result)
#' plots$insertions
#' plots$deletions
#' plots$other
#'
#' # Save to PDF
#' ggsave("sankey_insertions.pdf", plots$insertions, width = 12, height = 8)
#' }
plot_collapse_sankey <- function(result, min_flow = 0.001, title_prefix = "") {
  flows <- result$flows

  # Category 1a: Insertions of single C
  # Pattern: X[Ins(C):...]Y (with literal brackets)
  # e.g., "G[Ins(C):R0]T"
  ins_c_mask <- grepl("\\[Ins\\(C\\):", flows$a)
  flows_ins_c <- flows[ins_c_mask, , drop = FALSE]

  # Category 1b: Insertions of single T
  # Pattern: X[Ins(T):...]Y (with literal brackets)
  # e.g., "A[Ins(T):R2]A"
  ins_t_mask <- grepl("\\[Ins\\(T\\):", flows$a)
  flows_ins_t_all <- flows[ins_t_mask, , drop = FALSE]

  # Combined insertions (for backward compatibility)
  ins_mask <- ins_c_mask | ins_t_mask
  flows_ins <- flows[ins_mask, , drop = FALSE]

  # Category 2: Deletions of single T or C
  # Pattern: X[Del(T):...]Y or X[Del(C):...]Y (with literal brackets)
  # e.g., "A[Del(T):R1]G", "G[Del(C):R7]G"
  del_mask <- grepl("\\[Del\\([TC]\\):", flows$a)
  flows_del <- flows[del_mask, , drop = FALSE]

  # Category 3: Everything else (repeats, microhomology)
  other_mask <- !ins_mask & !del_mask
  flows_other <- flows[other_mask, , drop = FALSE]

  # Split "other" into insertions and deletions
  other_ins_mask <- grepl("^Ins", flows_other$a)
  flows_other_ins <- flows_other[other_ins_mask, , drop = FALSE]
  flows_other_del <- flows_other[!other_ins_mask, , drop = FALSE]

  # Create titles
  prefix <- if (nchar(title_prefix) > 0) paste0(title_prefix, ": ") else ""

  # Summary stats for titles
  ins_total <- sum(flows_ins$x)
  ins_c_total <- sum(flows_ins_c$x)
  ins_t_all_total <- sum(flows_ins_t_all$x)
  del_total <- sum(flows_del$x)
  other_total <- sum(flows_other$x)
  other_ins_total <- sum(flows_other_ins$x)
  other_del_total <- sum(flows_other_del$x)

  title_ins <- sprintf(
    "%sSingle-base Insertions (T/C) [%.1f%% of flow]",
    prefix,
    100 * ins_total
  )
  title_ins_c <- sprintf(
    "%sSingle-base Insertions (C only) [%.1f%% of flow]",
    prefix,
    100 * ins_c_total
  )
  title_ins_t_all <- sprintf(
    "%sSingle-base Insertions (T only) [%.1f%% of flow]",
    prefix,
    100 * ins_t_all_total
  )
  title_del <- sprintf(
    "%sSingle-base Deletions (T/C) [%.1f%% of flow]",
    prefix,
    100 * del_total
  )
  title_other <- sprintf(
    "%sOther (repeats, MH, etc.) [%.1f%% of flow]",
    prefix,
    100 * other_total
  )
  title_other_ins <- sprintf(
    "%sOther Insertions (repeats) [%.1f%% of flow]",
    prefix,
    100 * other_ins_total
  )
  title_other_del <- sprintf(
    "%sOther Deletions (repeats, MH) [%.1f%% of flow]",
    prefix,
    100 * other_del_total
  )

  # Category 4: Insertions of single T only, excluding INS:T:1:5+
  # Pattern: X[Ins(T):...]Y but not mapping to INS:T:1:5+
  ins_t_mask <- grepl("\\[Ins\\(T\\):", flows$a) & flows$b != "INS:T:1:5+"
  flows_ins_t <- flows[ins_t_mask, , drop = FALSE]
  ins_t_total <- sum(flows_ins_t$x)
  title_ins_t <- sprintf(
    "%sSingle-base Insertions (T only, excl. 5+) [%.1f%% of flow]",
    prefix,
    100 * ins_t_total
  )

  # Create plots (order by repeat number for single-base ins/del)
  plot_ins <- create_sankey_plot(
    flows_ins,
    title_ins,
    min_flow,
    order_by_repeat = TRUE
  )
  plot_ins_c <- create_sankey_plot(
    flows_ins_c,
    title_ins_c,
    min_flow,
    order_by_repeat = TRUE
  )
  plot_ins_t_all <- create_sankey_plot(
    flows_ins_t_all,
    title_ins_t_all,
    min_flow,
    order_by_repeat = TRUE
  )
  plot_del <- create_sankey_plot(
    flows_del,
    title_del,
    min_flow,
    order_by_repeat = TRUE
  )
  plot_other <- create_sankey_plot(
    flows_other,
    title_other,
    min_flow,
    order_by_mh_then_number = TRUE
  )
  plot_other_ins <- create_sankey_plot(
    flows_other_ins,
    title_other_ins,
    min_flow,
    order_by_first_number = TRUE
  )
  plot_other_del <- create_sankey_plot(
    flows_other_del,
    title_other_del,
    min_flow,
    order_by_mh_then_number = TRUE
  )
  plot_ins_t <- create_sankey_plot(
    flows_ins_t,
    title_ins_t,
    min_flow,
    order_by_repeat = TRUE
  )

  list(
    insertions = plot_ins,
    insertions_c = plot_ins_c,
    insertions_t = plot_ins_t_all,
    deletions = plot_del,
    other = plot_other,
    other_ins = plot_other_ins,
    other_del = plot_other_del,
    insertions_t_no5plus = plot_ins_t
  )
}


#' Plot 476-to-83 collapse with Sankey and signature comparison
#'
#' Wrapper function that collapses a 476-type signature to 83-type and creates
#' a PDF containing Sankey plot(s) and the collapsed 83-type signature
#' plotted with mSigPlot::plot_83.
#'
#' @param sig_476_name Column name from 476-type signatures (e.g., "InsDel1a")
#' @param sig_83_name Column name from 83-type signatures (e.g., "C_ID1")
#' @param data_dir Directory containing the data files
#' @param out_dir Output directory for the PDF
#' @param min_flow Minimum flow value to display in Sankey plot
#' @param all_sankey If TRUE, include all Sankey plots (insertions_c, insertions_t,
#'   deletions, other). If FALSE, only include the "other" plot.
#'
#' @return Invisibly returns the result from collapse_476_to_83()
#'
#' @examples
#' \dontrun{
#' plot_collapse_476_to_83("InsDel1a", "C_ID1")
#' plot_collapse_476_to_83("InsDel1a", "C_ID1", all_sankey = TRUE)
#' }
#' Collapse all 476-type signatures to 83-type using bipartite matching
#'
#' Creates a matrix of collapsed 83-type signatures from 476-type signatures,
#' compatible with the vignette's expected format. Column names are
#' `{sig_name}_converted` to match the convention used by `t476_to_89()`.
#'
#' @param type476_sigs Data frame of 476-type signatures (rows = mutation types,
#'   columns = signature names like "InsDel1a")
#' @param store_flows If TRUE, also stores the flow data for each signature pair
#'   in the returned object's "flows" attribute
#'
#' @return A data frame with:
#'   - Row names: 83-type mutation categories
#'   - Columns: `{sig_name}_converted` for each signature that was collapsed
#'   - Attribute "cosine_similarities": named vector of cosine similarities
#'   - Attribute "flows" (if store_flows=TRUE): list of flow data frames
#'
#' @examples
#' \dontrun{
#' ID83_mapped <- collapse_all_476_to_83_matrix()
#' }
collapse_all_476_to_83_matrix <- function(
  type476_sigs = read_finalized("476_signatures"),
  store_flows = FALSE
) {
  # Get the signature mapping (476/89-type name -> 83-type name)
  mapping <- get_signature_mapping()

  # Load 83-type signatures to get row names and check availability
  sig_83_df <- read_finalized("83_signatures")
  available_83 <- colnames(sig_83_df)
  row_names_83 <- rownames(sig_83_df)

  # Initialize output matrix
  result_matrix <- matrix(
    0,
    nrow = length(row_names_83),
    ncol = 0,
    dimnames = list(row_names_83, NULL)
  )
  result_cols <- character(0)
  cosine_similarities <- numeric(0)
  flows_list <- list()

  # Available 476-type signatures
  available_476 <- colnames(type476_sigs)

  for (i in seq_len(nrow(mapping))) {
    sig_476 <- mapping$sig_476[i]
    sig_83 <- mapping$sig_83[i]

    if (sig_476 %in% available_476 && sig_83 %in% available_83) {
      message(sprintf("Collapsing %s -> %s", sig_476, sig_83))
      tryCatch(
        {
          collapse_result <- collapse_476_to_83(
            sig_476,
            sig_83,
            data_dir = here::here("Manuscript_data"),
            sig_dir = finalized_dir
          )

          # Add collapsed signature to matrix
          col_name <- paste0(sig_476, "_converted")
          result_cols <- c(result_cols, col_name)

          # The y vector contains the collapsed signature
          y_col <- collapse_result$y[row_names_83]
          result_matrix <- cbind(result_matrix, y_col)

          # Store cosine similarity
          cos_sim <- lsa::cosine(collapse_result$y, collapse_result$target)[
            1,
            1
          ]
          cosine_similarities[col_name] <- cos_sim

          # Store flows if requested
          if (store_flows) {
            flows_list[[sig_476]] <- collapse_result$flows
          }

          message(sprintf("  Cosine similarity: %.4f", cos_sim))
        },
        error = function(e) {
          message(sprintf(
            "Error collapsing %s -> %s: %s",
            sig_476,
            sig_83,
            e$message
          ))
        }
      )
    } else {
      if (!sig_476 %in% available_476) {
        message(sprintf(
          "Skipping %s: not found in 476-type signatures",
          sig_476
        ))
      }
      if (!sig_83 %in% available_83) {
        message(sprintf(
          "Skipping %s: %s not found in 83-type signatures",
          sig_476,
          sig_83
        ))
      }
    }
  }

  # Set column names
  colnames(result_matrix) <- result_cols

  # Convert to data frame
  result_df <- as.data.frame(result_matrix)

  # Attach attributes
  attr(result_df, "cosine_similarities") <- cosine_similarities
  if (store_flows) {
    attr(result_df, "flows") <- flows_list
  }

  result_df
}


#' Generate Sankey plot PNG for a signature pair
#'
#' Creates a Sankey plot PNG file for use in vignettes, showing the flow
#' from 476-type to 83-type for the "other" category (non-single-base indels).
#'
#' @param sig_476_name Column name from 476-type signatures (e.g., "InsDel1a")
#' @param sig_83_name Column name from 83-type signatures (e.g., "C_ID1")
#' @param out_dir Output directory for the PNG file
#' @param data_dir Directory containing the data files
#' @param min_flow Minimum flow value to display
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Resolution in dots per inch
#'
#' @return List with paths to generated PNG files:
#'   \item{other_ins}{Path to "other insertions" Sankey PNG}
#'   \item{other_del}{Path to "other deletions" Sankey PNG}
#'   \item{cosine}{Cosine similarity between collapsed and target}
#'
#' @examples
#' \dontrun{
#' paths <- generate_sankey_png("InsDel1a", "C_ID1", "output")
#' }
generate_sankey_png <- function(
  sig_476_name,
  sig_83_name,
  out_dir,
  data_dir = "Manuscript_data",
  min_flow = 0.001,
  width = 12,
  height = 8,
  dpi = 150
) {
  # Collapse the signature
  result <- collapse_476_to_83(sig_476_name, sig_83_name, data_dir)

  # Calculate cosine similarity
  cos_sim <- lsa::cosine(result$y, result$target)[1, 1]

  # Generate Sankey plots
  title_prefix <- sprintf("%s -> %s", sig_476_name, sig_83_name)
  plots <- plot_collapse_sankey(
    result,
    min_flow = min_flow,
    title_prefix = title_prefix
  )

  # Create output directory
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Safe filename prefix
  safe_name <- gsub("[^a-zA-Z0-9_]", "_", sig_476_name)

  paths <- list(cosine = cos_sim)

  # Save "other insertions" plot
  if (!is.null(plots$other_ins)) {
    path_ins <- file.path(out_dir, paste0(safe_name, "_sankey_other_ins.png"))
    ggplot2::ggsave(
      path_ins,
      plots$other_ins,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )
    paths$other_ins <- path_ins
  }

  # Save "other deletions" plot
  if (!is.null(plots$other_del)) {
    path_del <- file.path(out_dir, paste0(safe_name, "_sankey_other_del.png"))
    ggplot2::ggsave(
      path_del,
      plots$other_del,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )
    paths$other_del <- path_del
  }

  paths
}


plot_collapse_476_to_83 <- function(
  sig_476_name,
  sig_83_name,
  data_dir = "Manuscript_data",
  out_dir = "output",
  min_flow = 0.001,
  all_sankey = FALSE
) {
  if (!requireNamespace("mSigPlot", quietly = TRUE)) {
    stop("Package 'mSigPlot' is required")
  }
  if (!requireNamespace("lsa", quietly = TRUE)) {
    stop("Package 'lsa' is required for cosine similarity")
  }

  # Create output directory if needed
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Collapse 476 to 83
  result <- collapse_476_to_83(sig_476_name, sig_83_name, data_dir)

  # Calculate cosine similarity between y and target
  cos_sim <- lsa::cosine(result$y, result$target)[1, 1]

  # Create the "other" Sankey plot
  title_prefix <- sprintf("%s -> %s", sig_476_name, sig_83_name)
  plots <- plot_collapse_sankey(
    result,
    min_flow = min_flow,
    title_prefix = title_prefix
  )

  # Create the 83-type signature plot using mSigPlot::plot_83
  # Convert y to a matrix format expected by plot_83
  y_matrix <- matrix(
    result$y,
    ncol = 1,
    dimnames = list(names(result$y), "collapsed")
  )

  plot_title <- sprintf(
    "%s collapsed to 83-type (cos sim = %.4f)",
    sig_476_name,
    cos_sim
  )
  sig_plot <- mSigPlot::plot_83(
    y_matrix,
    plot_title = plot_title,
    base_size = 20,
    text_size = 8
  )

  # Generate PDF filename
  pdf_file <- file.path(
    out_dir,
    sprintf("collapse_%s_to_%s.pdf", sig_476_name, sig_83_name)
  )

  # Save to PDF (multiple pages)
  pdf(pdf_file, width = 14, height = 10)

  if (all_sankey) {
    # All Sankey plots
    if (!is.null(plots$insertions_c)) {
      print(plots$insertions_c)
    }
    if (!is.null(plots$insertions_t)) {
      print(plots$insertions_t)
    }
    if (!is.null(plots$deletions)) {
      print(plots$deletions)
    }
  }

  # Other Sankey plots (split into insertions and deletions)
  if (!is.null(plots$other_ins)) {
    print(plots$other_ins)
  }
  if (!is.null(plots$other_del)) {
    print(plots$other_del)
  }

  # Collapsed 83-type signature
  print(sig_plot)

  dev.off()

  message(sprintf("Saved PDF to: %s", pdf_file))
  message(sprintf("Cosine similarity: %.4f", cos_sim))

  invisible(result)
}
