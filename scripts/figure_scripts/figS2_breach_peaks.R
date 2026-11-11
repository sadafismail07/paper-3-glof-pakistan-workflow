# Figure S2: breach peaks against empirical relations


library(ggplot2)
library(dplyr)
library(cowplot)
library(scales)

OUT_DIR   <- "figures"
dir.create(OUT_DIR, showWarnings = FALSE)
FIG_W     <- 17.4
FIG_H     <- 9.0
FIG_DPI   <- 800
BASE_SIZE <- 9

sim <- read.csv("data/processed/all_simulations_combined_v2.csv")

t3 <- read.csv("data/processed/table03_master_data.csv")
t3$scenario <- c(Low = "Low", Mid = "Mid_baseline", High = "High",
                 Mid_n04 = "Mid_n04", Mid_n06 = "Mid_n06",
                 Mid_b15 = "Mid_bulk15", Mid_b20 = "Mid_bulk20")[t3$scenario_code]
t3 <- t3[!is.na(t3$scenario) & t3$manning_n != 0.10, ]
idx <- match(paste(sim$lake, sim$scenario), paste(t3$lake_id, t3$scenario))
sim$peak_Q_input_m3s <- ifelse(is.na(idx), sim$peak_Q_input_m3s, round(t3$peak_q_m3s[idx]))

scens_show <- c("Low", "Mid_baseline", "High", "Mid_bulk20")

make_hecras <- function(df, lake_code, lake_label) {
  df |>
    filter(lake == lake_code, scenario %in% scens_show) |>
    mutate(
      lake   = lake_label,
      Q      = peak_Q_input_m3s,
      label  = case_when(
        scenario == "Low"          ~ "HEC-RAS: Low",
        scenario == "Mid_baseline" ~ "HEC-RAS: Mid",
        scenario == "High"         ~ "HEC-RAS: High",
        scenario == "Mid_bulk20"   ~ "HEC-RAS: Bulk ×2.0"
      ),
      source = "HEC-RAS"
    )
}

hecras_l29 <- make_hecras(sim, "L29", "L29 Passu")
hecras_l27 <- make_hecras(sim, "L27", "L27 Shisper")

bp  <- read.csv("data/processed/breach_parameters.csv")
mid <- function(l, col) bp[[col]][bp$lake_id == l & bp$scenario == "Mid"]
V29 <- mid("L29", "Vw_m3"); H29 <- mid("L29", "Hb_m")
V27 <- mid("L27", "Vw_m3"); H27 <- mid("L27", "Hb_m")

q_walder <- function(V) 0.045 * V^0.66
q_costa  <- function(V, H) 0.063 * (1000 * 9.81 * V * H)^0.42

emp_l29 <- data.frame(
  lake   = "L29 Passu",
  source = "Empirical",
  label  = c("Walder & O'Connor (1997)", "Costa & Schuster (1988)"),
  Q      = c(q_walder(V29), q_costa(V29, H29))
)

emp_l27 <- data.frame(
  lake   = "L27 Shisper",
  source = "Empirical",
  label  = c("Costa & Schuster (1988)"),
  Q      = c(q_costa(V27, H27))
)

lit_l27 <- data.frame(
  lake   = "L27 Shisper",
  source = "Literature",
  label  = c("Khan et al. (2021): low",
             "Khan et al. (2021): high"),
  Q      = c(5348, 6938)
)

cols <- c("lake","source","label","Q")

dat_l29 <- bind_rows(hecras_l29[,cols], emp_l29[,cols]) |>
  arrange(Q) |>
  mutate(label = factor(label, levels = unique(label)))

dat_l27 <- bind_rows(hecras_l27[,cols], emp_l27[,cols],
                     lit_l27[,cols]) |>
  arrange(Q) |>
  mutate(label = factor(label, levels = unique(label)))

label_mult <- 1.45

dat_l29 <- dat_l29 |> mutate(label_x = Q * label_mult)
dat_l27 <- dat_l27 |> mutate(label_x = Q * label_mult)

src_col <- c(
  "HEC-RAS"    = "#2166AC",
  "Empirical"  = "#D4AC0D",
  "Literature" = "#C77CFF"
)
src_shape <- c(
  "HEC-RAS"    = 21,
  "Empirical"  = 24,
  "Literature" = 22
)

theme_val <- function(base_size = BASE_SIZE) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.y = element_line(colour = "grey93",
                                        linewidth = 0.3),
      panel.grid.major.x = element_line(colour = "grey88",
                                        linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.border       = element_rect(fill = NA, colour = "grey30",
                                        linewidth = 0.4),
      plot.background    = element_rect(fill = "white", colour = NA),
      axis.title.x       = element_text(size = base_size,
                                        margin = margin(t = 3)),
      axis.title.y       = element_blank(),
      axis.text.x        = element_text(size = base_size - 1.5,
                                        colour = "grey20"),
      axis.text.y        = element_text(size = base_size - 0.5,
                                        colour = "grey15"),
      legend.position    = "none",
      plot.title         = element_text(size = base_size + 0.5,
                                        face = "bold",
                                        margin = margin(b = 1)),
      plot.subtitle      = element_text(size = base_size - 1,
                                        colour = "grey40",
                                        margin = margin(b = 3)),
      plot.margin        = margin(3, 5, 3, 3, "mm")
    )
}

build_val <- function(dat, title, subtitle,
                      x_breaks, x_limits,
                      sep_y = NULL,
                      show_legend = FALSE) {

  hr <- dat |> filter(source == "HEC-RAS")

  p <- ggplot(dat,
              aes(x      = Q,
                  y      = label,
                  fill   = source,
                  shape  = source,
                  colour = source)) +

    # HEC-RAS envelope band
    annotate("rect",
             xmin = min(hr$Q) * 0.88,
             xmax = max(hr$Q) * 1.12,
             ymin = -Inf, ymax = Inf,
             fill = "#2166AC", alpha = 0.07) +

    # Group separator
    {if (!is.null(sep_y))
      geom_hline(yintercept = sep_y,
                 colour = "grey65", linewidth = 0.4,
                 linetype = "dashed")
    } +

    # Points
    geom_point(size = 2.4, stroke = 0.5) +

geom_text(
  aes(x     = label_x,
      y     = label,
      label = scales::comma(round(Q))),
  hjust       = 0,
  vjust       = 0.45,
  size        = 1.9,
  colour      = "grey20",
  inherit.aes = FALSE
) +

    scale_fill_manual(
      name   = "Source",
      values = src_col,
      guide  = guide_legend(
        override.aes = list(size = 3),
        keywidth  = unit(0.4, "cm"),
        keyheight = unit(0.35, "cm")
      )
    ) +
    scale_colour_manual(name = "Source", values = src_col) +
    scale_shape_manual( name = "Source", values = src_shape) +

    scale_x_log10(
      name   = "Peak discharge (m³/s, log scale)",
      breaks = x_breaks,
      labels = scales::comma,
      limits = x_limits
    ) +

    labs(title = title, subtitle = subtitle) +
    theme_val()

  if (show_legend) {
    p <- p + theme(
      legend.position  = "bottom",
      legend.title     = element_text(size = BASE_SIZE,
                                      face = "bold"),
      legend.text      = element_text(size = BASE_SIZE - 1),
      legend.key.size  = unit(0.4, "cm"),
      legend.spacing.x = unit(0.4, "cm")
    )
  }

  return(p)
}

cat("── Building panels ──\n")

p_l29 <- build_val(
  dat      = dat_l29,
  title    = "(a) L29 Passu",
  subtitle = "Moraine-dammed",
  x_breaks = c(200, 1000, 5000),
  x_limits = c(150, max(dat_l29$Q) * 4.5),
  sep_y    = 3
)

p_l27 <- build_val(
  dat      = dat_l27,
  title    = "(b) L27 Shisper",
  subtitle = "Ice-dammed",
  x_breaks = c(1000, 10000, 100000),
  x_limits = c(800, max(dat_l27$Q) * 4.5),
  sep_y    = 3.5,
  show_legend = TRUE
)

cat("── Assembling ──\n")

leg      <- cowplot::get_legend(p_l27)
p_l27_nl <- p_l27 + theme(legend.position = "none")

p_maps <- cowplot::plot_grid(
  p_l29, p_l27_nl,
  ncol       = 2,
  rel_widths = c(1, 1),
  align      = "h",
  axis       = "tb"
)

p_final <- cowplot::plot_grid(
  p_maps, leg,
  ncol        = 1,
  rel_heights = c(1, 0.10)
)

cat("── Saving ──\n")

ggsave(file.path(OUT_DIR, "Figure_S2.png"),
       p_final, width = FIG_W, height = FIG_H,
       units = "cm", dpi = FIG_DPI, bg = "white")
cat("  Saved: Figure_S2.png\n")

ggsave(file.path(OUT_DIR, "Figure_S2.tiff"),
       p_final, width = FIG_W, height = FIG_H,
       units = "cm", dpi = FIG_DPI, bg = "white",
       compression = "lzw")
cat("  Saved: Figure_S2.tiff\n")

cat("\nFig 11 v4 complete.\n")
