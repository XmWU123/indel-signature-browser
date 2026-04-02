# Script to export prot_table_1.csv to Excel with formatting
# - Bold header row
# - Numeric columns formatted to 4 decimal places
# - Merged cells in columns type83_sig_id through cosine_v_jin for rows with identical type83_sig_id

library(openxlsx)

source(here::here("vignette/table_1_col_name_mapping.R"))

# Read CSV
df <- read.csv(
  here::here("vignette/prot_table_1.csv"),
  stringsAsFactors = FALSE
)

# Replace underscores with spaces in column names
# names(df) <- gsub("_", " ", names(df))

polyT_rows <- which(df$`is_polyT_removed` == TRUE)
df$`type83_sig_id`[polyT_rows] <- paste0(df$`type83_sig_id`[polyT_rows], "**")

# Remove "jin" prefix from best_match_jin values
df$`best_match_jin` <- gsub("^jin", "", df$`best_match_jin`)

# Handle best_match_koh duplicates: add asterisk to best match, track non-best for graying
# For non-duplicates, also add dagger
best_koh_col <- which(names(df) == "best_match_koh")
cos_koh_col <- which(names(df) == "cos_v_koh")

# Get non-NA values and find duplicates
koh_values <- df$`best_match_koh`
non_na_idx <- which(!is.na(koh_values))

# Track rows to gray out (non-best duplicates)
rows_to_gray <- c()

# Process each unique non-NA value
unique_koh <- unique(koh_values[non_na_idx])
for (koh_val in unique_koh) {
  matching_rows <- which(koh_values == koh_val)
  if (length(matching_rows) > 1) {
    # Duplicate: find row with highest cos_v_koh
    cos_vals <- df$`cos_v_koh`[matching_rows]
    best_idx <- matching_rows[which.max(cos_vals)]
    non_best_idx <- setdiff(matching_rows, best_idx)
    # Add asterisk to best match (only for duplicates)
    df$`best_match_koh`[best_idx] <- paste0(df$`best_match_koh`[best_idx], "*")
    # Track non-best rows for graying
    rows_to_gray <- c(rows_to_gray, non_best_idx)
  }
}

# Handle best_match_jin duplicates: add asterisk to best match, track non-best for graying
best_jin_col <- which(names(df) == "best_match_jin")
cos_jin_col <- which(names(df) == "cosine_v_jin")

jin_values <- df$`best_match_jin`
jin_non_na_idx <- which(!is.na(jin_values))

jin_rows_to_gray <- c()

unique_jin <- unique(jin_values[jin_non_na_idx])
for (jin_val in unique_jin) {
  matching_rows <- which(jin_values == jin_val)
  if (length(matching_rows) > 1) {
    cos_vals <- df$`cosine_v_jin`[matching_rows]
    best_idx <- matching_rows[which.max(cos_vals)]
    non_best_idx <- setdiff(matching_rows, best_idx)
    df$`best_match_jin`[best_idx] <- paste0(df$`best_match_jin`[best_idx], "*")
    jin_rows_to_gray <- c(jin_rows_to_gray, non_best_idx)
  }
}

# Create workbook and worksheet
wb <- createWorkbook()
addWorksheet(wb, "Table 1")

# Remove is_polyT_removed column (no longer needed after marking rows with **)
df$is_polyT_removed <- NULL

# Write data starting at row 1
writeData(wb, 1, df, startRow = 1, startCol = 1)

# Style header row (bold, wrap text, centered)
headerStyle <- createStyle(
  textDecoration = "bold",
  wrapText = TRUE,
  halign = "center"
)
addStyle(wb, 1, headerStyle, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)

# Identify numeric columns
numeric_cols <- which(sapply(df, is.numeric))

# Style for numeric columns (4 decimal places, centered)
numericStyle <- createStyle(numFmt = "0.0000", halign = "center")

# Style for non-numeric columns (centered)
textStyle <- createStyle(halign = "center")

# Apply styles to data rows
for (col in 1:ncol(df)) {
  if (col %in% numeric_cols) {
    addStyle(
      wb,
      1,
      numericStyle,
      rows = 2:(nrow(df) + 1),
      cols = col,
      gridExpand = TRUE
    )
  } else {
    addStyle(
      wb,
      1,
      textStyle,
      rows = 2:(nrow(df) + 1),
      cols = col,
      gridExpand = TRUE
    )
  }
}

# Apply gray style to non-best duplicate rows for best_match_koh and cos_v_koh columns
if (length(rows_to_gray) > 0) {
  grayStyle <- createStyle(fontColour = "#B0B0B0", halign = "center")
  grayNumericStyle <- createStyle(
    fontColour = "#B0B0B0",
    numFmt = "0.0000",
    halign = "center"
  )
  for (row_idx in rows_to_gray) {
    excel_row <- row_idx + 1 # Add 1 for header row
    # Gray out best_match_koh (text column)
    addStyle(
      wb,
      1,
      grayStyle,
      rows = excel_row,
      cols = best_koh_col,
      stack = TRUE
    )
    # Gray out cos_v_koh (numeric column)
    addStyle(
      wb,
      1,
      grayNumericStyle,
      rows = excel_row,
      cols = cos_koh_col,
      stack = TRUE
    )
  }
}

# Apply gray style to non-best duplicate rows for best_match_jin and cosine_v_jin columns
if (length(jin_rows_to_gray) > 0) {
  grayStyle <- createStyle(fontColour = "#B0B0B0", halign = "center")
  grayNumericStyle <- createStyle(
    fontColour = "#B0B0B0",
    numFmt = "0.0000",
    halign = "center"
  )
  for (row_idx in jin_rows_to_gray) {
    excel_row <- row_idx + 1 # Add 1 for header row
    # Gray out best_match_jin (text column)
    addStyle(
      wb,
      1,
      grayStyle,
      rows = excel_row,
      cols = best_jin_col,
      stack = TRUE
    )
    # Gray out cosine_v_jin (numeric column)
    addStyle(
      wb,
      1,
      grayNumericStyle,
      rows = excel_row,
      cols = cos_jin_col,
      stack = TRUE
    )
  }
}

# Calculate column widths based on data only (rows 2-48), not headers
col_widths <- sapply(1:ncol(df), function(col) {
  if (col %in% numeric_cols) {
    # For numeric columns, format to 4 decimal places and find max width
    vals <- df[[col]]
    formatted <- ifelse(is.na(vals), "NA", sprintf("%.4f", vals))
  } else {
    # For text columns, use as-is
    formatted <- as.character(df[[col]])
    formatted[is.na(formatted)] <- "NA"
  }
  max(nchar(formatted), na.rm = TRUE) + 2 # Add padding
})

setColWidths(wb, 1, cols = 1:ncol(df), widths = col_widths)

# Find column indices for merge range
merge_start_col <- which(names(df) == "type83_sig_id")
merge_end_col <- which(names(df) == "cosine_v_jin")

# Find groups of consecutive rows with same type83 sig id
type83_col <- df$`type83_sig_id`

# Get run-length encoding of type83_sig_id
rle_result <- rle(type83_col)
lengths <- rle_result$lengths
values <- rle_result$values

# Compute starting positions for each group
start_pos <- cumsum(c(1, lengths[-length(lengths)]))

# Merge cells for groups with more than 1 row
for (i in seq_along(lengths)) {
  if (lengths[i] > 1) {
    # Row indices in Excel (add 1 for header row)
    row_start <- start_pos[i] + 1
    row_end <- row_start + lengths[i] - 1

    # Merge each column in the range
    for (col in merge_start_col:merge_end_col) {
      mergeCells(wb, 1, cols = col, rows = row_start:row_end)
    }
  }
}

# Add vertical and horizontal alignment to merged cells (center both)
mergeStyle <- createStyle(valign = "center", halign = "center")
for (i in seq_along(lengths)) {
  if (lengths[i] > 1) {
    row_start <- start_pos[i] + 1
    row_end <- row_start + lengths[i] - 1
    for (col in merge_start_col:merge_end_col) {
      addStyle(
        wb,
        1,
        mergeStyle,
        rows = row_start:row_end,
        cols = col,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  # Freeze header row and first column
  freezePane(wb, 1, firstRow = TRUE, firstCol = TRUE)
}

# Replace header row with user-friendly column names
mapped_names <- as.list(table_1_col_name_mapping(names(df)))
writeData(wb, 1, mapped_names, startRow = 1, startCol = 1, colNames = FALSE)

# Add footnotes
footnote <- "* This is the signature with the best match to the given type-89 signature."
writeData(wb, 1, footnote, startRow = 50, startCol = 1)
footnote <- "** indicates that in the linking tumor's spectrum, insertions of single Ts in long poly-T contexts were set to 0 before calculating cosine similarity to the 83-type signature."
writeData(wb, 1, footnote, startRow = 51, startCol = 1)

# Save workbook
saveWorkbook(wb, here::here("vignette/prot_table_1.xlsx"), overwrite = TRUE)

message("Saved ", here::here("vignette/prot_table_1.xlsx"))
