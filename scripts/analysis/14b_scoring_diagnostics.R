# ══════════════════════════════════════════════════════════════
# Table S2 -- ranking stability
# Self-contained: rebuilds inputs, computes all variants, writes CSV
#
# FIX (2026-08-24): surge credit is now restricted to lakes that are
# themselves ice-dammed, matching 14_priority_scoring.R
# (R3 Comment 12/13) -- the earlier version of this script gave L29
# (moraine-dammed, fed by a surge-type glacier) undeserved surge
# credit, which pushed it above L27 in every surge-inclusive column.
# ══════════════════════════════════════════════════════════════
setwd("C:/Users/sadaf/Documents/PPR3")
sf::sf_use_s2(FALSE)
suppressPackageStartupMessages({library(sf); library(dplyr); library(tidyr)})
# ── 1. Rebuild the scoring inputs ─────────────────────────────
corridors <- st_read("data/processed/lake_corridors_with_exposure.gpkg", quiet = TRUE)
lakes_v   <- st_read("data/processed/lakes_polygons_verified.gpkg",      quiet = TRUE)
lookup <- data.frame(
  lake_id    = c("L01","L02","L03","L04","L05","L06","L15","L19","L20",
                 "L21","L22","L24","L25","L26","L27","L28","L29"),
  lake_name  = c("Reshun","Booni","Sonoghor","Brep","Yarkhun Lusht","Dir Gole",
                 "Khurdupin","Hinarchi","Badswat","Karamber","Ishkoman","Gulkin",
                 "Gulmit","Hundur","Shisper","Ultar","Passu"),
  surge_type = c(0,0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,1),
  tier_score = c(1,3,3,1,2,2,1,2,2,3,3,3,3,3,4,3,4),   # C=1 B=2 A=3 A++=4
  stringsAsFactors = FALSE)
sb <- st_drop_geometry(corridors) |>
  filter(lake_id != "L30") |>
  left_join(st_drop_geometry(lakes_v) |> select(lake_id, vulnerability, area_km2, lake_type),
            by = "lake_id") |>
  left_join(lookup, by = "lake_id") |>
  mutate(
    area_for_score = ifelse(is.na(area_km2), 0, area_km2),
    is_ice_dammed  = grepl("ice", lake_type, ignore.case = TRUE),
    surge_credit   = surge_type * as.integer(is_ice_dammed)
  )
stopifnot(nrow(sb) == 17, !any(is.na(sb$tier_score)))
cat("Inputs rebuilt:", nrow(sb), "lakes\n")
cat("Surge credit after ice-dammed restriction:\n")
print(sb[sb$surge_type == 1, c("lake_id", "lake_type", "surge_type", "surge_credit")])
# ── 2. Normalisations ─────────────────────────────────────────
minmax    <- function(x) { r <- range(x, na.rm=TRUE)
                           if (diff(r)==0) rep(.5,length(x)) else (x-r[1])/diff(r) }
norm_rank <- function(x) (rank(x, ties.method="average") - 1)/(length(x) - 1)
norm_z    <- function(x) if (sd(x)==0) rep(0,length(x)) else (x-mean(x))/sd(x)
norm_rob  <- function(x) { q <- quantile(x, c(.05,.95), na.rm=TRUE)
                           if (diff(q)==0) rep(.5,length(x)) else
                           pmin(pmax((x-q[1])/diff(q),0),1) }
log10p <- function(x) log10(x + 1)
# ── 3. Scoring ────────────────────────────────────────────────
score_it <- function(d, norm = minmax, use_tier = FALSE, use_surge = TRUE) {
  h <- list(norm(d$vulnerability), norm(log10p(d$area_for_score * 1e6)))
  if (use_surge) h <- c(h, list(d$surge_credit))
  if (use_tier)  h <- c(h, list(norm(d$tier_score)))
  e <- (norm(log10p(d$pop_mean))       + norm(log10p(d$building_count)) +
        norm(log10p(d$road_length_km)) + norm(log10p(d$bridge_count))   +
        norm(log10p(d$built_area_m2))) / 5
  data.frame(lake_id = d$lake_id,
             final_score = (Reduce(`+`, h)/length(h) + e)/2)
}
rank_of <- function(s, label) {
  s <- s[order(-s$final_score), ]
  out <- data.frame(lake_id = s$lake_id, r = seq_len(nrow(s)))
  names(out)[2] <- label
  out
}
# ── 4. Assemble ───────────────────────────────────────────────
S2 <- Reduce(function(x, y) left_join(x, y, by = "lake_id"), list(
  rank_of(score_it(sb, use_tier = TRUE),                    "With tier"),
  rank_of(score_it(sb, use_tier = TRUE, use_surge = FALSE), "With tier, no surge"),
  rank_of(score_it(sb),                                     "Adopted (min-max)"),
  rank_of(score_it(sb, use_surge = FALSE),                  "Adopted, no surge"),
  rank_of(score_it(sb, norm_rank),                          "Rank norm"),
  rank_of(score_it(sb, norm_z),                             "Z-score"),
  rank_of(score_it(sb, norm_rob),                           "Robust 5-95")
))
cols <- setdiff(names(S2), "lake_id")
S2$`Rank range` <- apply(S2[, cols], 1, function(r) paste0(min(r), "-", max(r)))
S2 <- S2 |> left_join(lookup[, c("lake_id","lake_name")], by = "lake_id") |>
  arrange(`Adopted (min-max)`) |>
  select(lake_id, lake_name, all_of(cols), `Rank range`)
print(as.data.frame(S2), row.names = FALSE)
write.csv(S2, "data/processed/TableS2_ranking_stability.csv", row.names = FALSE)
# ── 5. Summary lines for the caption ──────────────────────────
adopted <- c("Adopted (min-max)","Adopted, no surge","Rank norm","Z-score","Robust 5-95")
cat("\nL27 rank 1 in", sum(sapply(adopted, function(c) S2[[c]][S2$lake_id=="L27"]==1)),
    "of", length(adopted), "adopted-composite variants\n")
cat("Top-2 = {L29,L27} in",
    sum(sapply(adopted, function(c) setequal(S2$lake_id[S2[[c]] <= 2], c("L29","L27")))),
    "of", length(adopted), "\n")
cat("With-tier variant reproduces the published ranking:",
    S2$lake_id[S2$`With tier`==1] == "L27" && S2$lake_id[S2$`With tier`==2] == "L29", "\n")
