# explore_assignments
aa = read.csv("table_s2.csv", row.names = 1, check.names = FALSE)
aacn = colnames(aa)
sum(grepl("::SP", aacn))

c83 = aa[1:33, ]
k89 = aa[34:74, ]


s_c83 = read.csv("table_s8_cosmic83.csv", row.names = 1, check.names = FALSE)
sum(grepl("::SP", colnames(s_c83)))

s_k89 = read.csv("table_s9_koh89.csv", row.names = 1, check.names = FALSE)
sum(grepl("::SP", colnames(s_k89)))

common_cols = intersect(colnames(s_c83), colnames(s_k89))

xc83 = s_c83[, common_cols]
xk89 = s_k89[, common_cols]

cancertypes = stringi::stri_replace(
  colnames(s_c83),
  replacement = "",
  regex = "::.*"
)
