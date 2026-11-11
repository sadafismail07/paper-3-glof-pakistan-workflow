# Weighting sensitivity

library(sf)
library(dplyr)

setwd("C:/Users/sadaf/Documents/PPR3")

weighting <- read.csv("data/processed/composite_weighting_sensitivity.csv",
                       stringsAsFactors = FALSE)

names_lu <- st_read("data/processed/lakes_polygons_verified.gpkg", quiet = TRUE) |>
  st_drop_geometry() |>
  select(lake_id, glacier_name)

strip_bualtar <- function(name) sub("\\s*\\(Bualtar\\)\\s*$", "", name)

fix_khurdupin_spelling <- function(name, lake_id) {
  ifelse(lake_id == "L15" & name == "Khurdopin", "Khurdupin", name)
}

tableS7 <- weighting |>
  left_join(names_lu, by = "lake_id") |>
  arrange(rank_5050) |>
  transmute(
    `Lake ID` = lake_id,
    Lake = fix_khurdupin_spelling(strip_bualtar(glacier_name), lake_id),
    `Final score (50/50)` = round(score_5050, 3),
    `Rank (50/50)` = rank_5050,
    `Final score (60/40)` = round(score_6040, 3),
    `Rank (60/40)` = rank_6040,
    `Final score (70/30)` = round(score_7030, 3),
    `Rank (70/30)` = rank_7030
  )

stopifnot(
  tableS7$Lake[tableS7$`Lake ID` == "L15"] == "Khurdupin",
  all(tableS7$`Lake ID`[tableS7$`Rank (50/50)` %in% 1:2] %in% c("L27", "L29")),
  all(tableS7$`Lake ID`[tableS7$`Rank (60/40)` %in% 1:2] %in% c("L27", "L29")),
  all(tableS7$`Lake ID`[tableS7$`Rank (70/30)` %in% 1:2] %in% c("L27", "L29"))
)
cat("Check passed.\n")

tableS7 <- tableS7 |>
  mutate(
    `Final score (50/50)` = sprintf("%.3f", `Final score (50/50)`),
    `Final score (60/40)` = sprintf("%.3f", `Final score (60/40)`),
    `Final score (70/30)` = sprintf("%.3f", `Final score (70/30)`)
  )
stopifnot(
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", tableS7$`Final score (50/50)`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", tableS7$`Final score (60/40)`)),
  all(grepl("^-?[0-9]+\\.[0-9]{3}$", tableS7$`Final score (70/30)`))
)
cat("Check passed.\n")

dir.create("figures", showWarnings = FALSE)
write.csv(tableS7, "figures/tableS7_weighting_sensitivity.csv", row.names = FALSE)
cat("Saved: figures/tableS7_weighting_sensitivity.csv (", nrow(tableS7), "rows )\n")
