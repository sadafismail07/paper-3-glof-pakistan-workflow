# Ranking stability

library(dplyr)
library(readr)

setwd("C:/Users/sadaf/Documents/PPR3")

S2 <- read_csv("data/processed/TableS2_ranking_stability.csv", show_col_types = FALSE)

tableS2 <- S2 |>
  transmute(
    `Lake ID` = lake_id,
    Lake = lake_name,
    `With tier`,
    `With tier, no surge`,
    `Adopted (min-max)`,
    `Adopted, no surge`,
    `Rank norm`,
    `Z-score`,
    `Robust 5-95`,
    `Rank range`
  ) |>
  arrange(`Adopted (min-max)`)

stopifnot(
  nrow(tableS2) == 17,
  tableS2$`Lake ID`[tableS2$`With tier` == 1] == "L29",
  tableS2$`Lake ID`[tableS2$`With tier` == 2] == "L27"
)
cat("Check passed.\n")

dir.create("figures", showWarnings = FALSE)
write.csv(tableS2, "figures/tableS2_ranking_stability.csv", row.names = FALSE)
cat("Saved: figures/tableS2_ranking_stability.csv (", nrow(tableS2), "rows )\n")
