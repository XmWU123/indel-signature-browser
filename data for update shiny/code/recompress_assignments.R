# Recompress signature assignments by combining sub-signatures
#
# Combines rows from Liu_et_al_89_type_signature_assignments.tsv:
# - InsDel1a, InsDel1b, InsDel1c, InsDel1d -> C_ID1 (sum)
# - InsDel2a, InsDel2b, InsDel2c -> C_ID2 (sum)
# - InsDel3a, InsDel3b -> C_ID3 (sum)
# - InsDel5a, InsDel5b -> C_ID5 (sum)
# - InsDel19a, InsDel19b, InsDel19c -> C_ID19 (sum)
# - InsDel_A_alpha, InsDel_A_beta -> ID_A (sum)
# - InsDel_K_alpha, InsDel_K_beta -> ID_K (sum)
# - InsDel(\d+) -> C_ID\1 (rename)
# - InsDel_([A-Z]) -> ID_\1 (rename)

# Read input
df <- read.delim(
  "Manuscript_data/Liu_et_al_89_type_signature_assignments.tsv",
  row.names = 1,
  check.names = FALSE
)

# Create new names based on patterns
old_names <- rownames(df)
new_names <- old_names

# Pattern 1: InsDel(\d+)[abcd] -> ID\1
new_names <- gsub("^InsDel(\\d+)[abcd]$", "C_ID\\1", new_names)

# Pattern 2: InsDel_([AK])_(alpha|beta) -> ID_\1
new_names <- gsub("^InsDel_([AK])_(alpha|beta)$", "ID_\\1", new_names)

# Pattern 3: InsDel(\d+) -> ID\1
new_names <- gsub("^InsDel(\\d+)$", "C_ID\\1", new_names)

# Pattern 4: InsDel_([A-Z]) -> ID_\1
new_names <- gsub("^InsDel_([A-Z])$", "ID_\\1", new_names)

# Aggregate by new names (sum)
df$new_name <- new_names
result <- aggregate(. ~ new_name, data = df, FUN = sum)
rownames(result) <- result$new_name
result$new_name <- NULL

# Write output
write.table(
  result,
  "Manuscript_data/recompressed_assignments.tsv",
  sep = "\t",
  quote = FALSE,
  col.names = NA
)

cat("Input rows:", length(old_names), "\n")
cat("Output rows:", nrow(result), "\n")
