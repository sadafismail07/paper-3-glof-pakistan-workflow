# Ranking under alternative weightings

setwd("C:/Users/sadaf/Documents/PPR3")
library(dplyr)

cat("========== Weighting-sensitivity check ==========\n\n")

scores <- read.csv("data/processed/lake_priority_scores.csv", stringsAsFactors = FALSE)

stopifnot(all(c("lake_id", "hazard_score", "exposure_score", "final_score") %in% names(scores)))

scores <- scores |>
  dplyr::mutate(
    final_score_5050_check = 0.5 * hazard_score + 0.5 * exposure_score
  )

check_5050 <- max(abs(scores$final_score_5050_check - scores$final_score), na.rm = TRUE)
cat("Check: max|recomputed 50/50 score - final_score column| =", round(check_5050, 6), "\n")
if (check_5050 > 1e-6) {
  cat("*** WARNING: this does not match final_score exactly -- final_score may not be a\n")
  cat("simple 0.5/0.5 average of hazard_score and exposure_score as assumed. Check\n")
  cat("14_priority_scoring.R before trusting the alternate weightings below. ***\n\n")
} else {
  cat("Confirmed: final_score = 0.5*hazard_score + 0.5*exposure_score, as expected.\n\n")
}

# --- Two alternative weightings ---
scores <- scores |>
  dplyr::mutate(
    final_score_60h_40e = 0.6 * hazard_score + 0.4 * exposure_score,
    final_score_70h_30e = 0.7 * hazard_score + 0.3 * exposure_score
  )

rank_by <- function(df, score_col) {
  df |>
    dplyr::arrange(dplyr::desc(.data[[score_col]])) |>
    dplyr::mutate(rank_new = dplyr::row_number()) |>
    dplyr::select(lake_id, dplyr::all_of(score_col), rank_new)
}

r_5050 <- rank_by(scores, "final_score")         |> dplyr::rename(rank_5050 = rank_new, score_5050 = final_score)
r_6040 <- rank_by(scores, "final_score_60h_40e") |> dplyr::rename(rank_6040 = rank_new, score_6040 = final_score_60h_40e)
r_7030 <- rank_by(scores, "final_score_70h_30e") |> dplyr::rename(rank_7030 = rank_new, score_7030 = final_score_70h_30e)

combined <- r_5050 |>
  dplyr::left_join(r_6040, by = "lake_id") |>
  dplyr::left_join(r_7030, by = "lake_id") |>
  dplyr::arrange(rank_5050)

cat("--- Full ranking under all three weightings ---\n")
print(combined, row.names = FALSE)
cat("\n")

top2_5050 <- combined$lake_id[combined$rank_5050 <= 2]
top2_6040 <- combined$lake_id[combined$rank_6040 <= 2]
top2_7030 <- combined$lake_id[combined$rank_7030 <= 2]

cat("Top 2 under 50/50 (original):        ", paste(top2_5050, collapse = ", "), "\n")
cat("Top 2 under 60/40 (hazard-weighted): ", paste(top2_6040, collapse = ", "), "\n")
cat("Top 2 under 70/30 (hazard-weighted): ", paste(top2_7030, collapse = ", "), "\n\n")

stable_6040 <- setequal(top2_5050, top2_6040)
stable_7030 <- setequal(top2_5050, top2_7030)

if (stable_6040 && stable_7030) {
  cat("Top two unchanged under 60/40 and 70/30.\n")
} else {
  cat("Top two change under at least one weighting.\n")
}

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write.csv(combined, "data/processed/composite_weighting_sensitivity.csv", row.names = FALSE)
cat("\nSaved: data/processed/composite_weighting_sensitivity.csv\n")
cat("\n========== DONE ==========\n")
