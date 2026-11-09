# =====================================================================
# generate_plots.R
#
# Generates the three-sector sectoral-balance plot for Switzerland in
# French, German and English, reading `sectoral_balances_data.rds`.
#
# The data are prepared by `sectoral_balances.R`, which downloads the
# AMECO series through the ECFIN Redisstat SDMX API.
#
# Outputs
#   3sectors_balances_CHE_fr.jpg
#   3sectors_balances_CHE_de.jpg
#   3sectors_balances_CHE_en.jpg
#   3sectors_balances_CHE.jpg   (English default)
#
# Run with: Rscript generate_plots.R
# (run sectoral_balances.R first if the data need to be refreshed)
# =====================================================================

# Make `Rscript path/to/generate_plots.R` work from any directory.
local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!interactive() && length(file_arg)) {
    script_path <- sub("^--file=", "", file_arg[1])
    if (file.exists(script_path)) {
      setwd(dirname(normalizePath(script_path)))
    }
  }
})

library(ggplot2)
library(dplyr)
library(tidyr)

# If the prepared data are absent, fetch them first.
if (!file.exists("sectoral_balances_data.rds")) {
  source("sectoral_balances.R")
}

df <- readRDS("sectoral_balances_data.rds")
last_year <- max(df$year, na.rm = TRUE)
source_year <- format(Sys.Date(), "%Y")

## ---------------------------------------------------------------------
## Localized strings
## ---------------------------------------------------------------------
languages <- list(
  fr = list(
    code         = "fr",
    title        = "Balances sectorielles",
    subtitle     = sprintf("Suisse, 1995-%d", last_year),
    y            = "Balance (en % du revenu national brut)",
    x            = "Année",
    legend_title = "Soldes",
    labels       = c(
      priv_bal = "Secteur privé",
      pub_bal  = "Secteur public",
      row_bal  = "Reste du monde"
    ),
    caption = sprintf(
      paste0("Source : AMECO (Commission européenne), %s.\n",
             "Ligne pointillée : introduction du frein à l'endettement (2003)"),
      source_year
    )
  ),
  de = list(
    code         = "de",
    title        = "Sektorale Salden",
    subtitle     = sprintf("Schweiz, 1995-%d", last_year),
    y            = "Saldo (in % des Bruttonationaleinkommens)",
    x            = "Jahr",
    legend_title = "Salden",
    labels       = c(
      priv_bal = "Privatsektor",
      pub_bal  = "Öffentlicher Sektor",
      row_bal  = "Übrige Welt"
    ),
    caption = sprintf(
      paste0("Quelle: AMECO (Europäische Kommission), %s.\n",
             "Gepunktete Linie: Einführung der Schuldenbremse (2003)"),
      source_year
    )
  ),
  en = list(
    code         = "en",
    title        = "Sectoral balances",
    subtitle     = sprintf("Switzerland, 1995-%d", last_year),
    y            = "Balance (% of gross national income)",
    x            = "Year",
    legend_title = "Balances",
    labels       = c(
      priv_bal = "Private sector",
      pub_bal  = "Public sector",
      row_bal  = "Rest of the world"
    ),
    caption = sprintf(
      paste0("Source: AMECO (European Commission), %s.\n",
             "Dotted line: introduction of the debt brake (2003)"),
      source_year
    )
  )
)

## ---------------------------------------------------------------------
## Plot constants
## ---------------------------------------------------------------------
# The order of the factor levels controls the stacking order. With
# position_stack(), positive values are stacked from the last level
# upwards, and negative values from the last level downwards. This keeps
# the old plot's order: private sector above zero, rest of the world just
# below zero, and the public sector below it.
sector_levels <- c("priv_bal", "pub_bal", "row_bal")

colors3 <- c(
  priv_bal = "indianred3",
  pub_bal  = "chartreuse4",
  row_bal  = "turquoise3"
)

## ---------------------------------------------------------------------
## Build and save one plot per language
## ---------------------------------------------------------------------
for (lang in languages) {
  code <- lang$code

  plot_data <- df %>%
    select(year, priv_bal, pub_bal, row_bal) %>%
    pivot_longer(cols = -year, names_to = "variable", values_to = "value") %>%
    mutate(variable = factor(variable, levels = sector_levels))

  p <- ggplot(plot_data, aes(x = year, y = value, fill = variable)) +
    geom_col(width = 0.85) +
    geom_vline(xintercept = 2003, linetype = "dotted", linewidth = 0.8) +
    scale_fill_manual(
      values = colors3,
      breaks = sector_levels,
      labels = unname(lang$labels[sector_levels])
    ) +
    scale_x_continuous(
      breaks = seq(1995, last_year, by = 5),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(labels = function(x) paste0(x, " %")) +
    labs(
      title    = lang$title,
      subtitle = lang$subtitle,
      x        = lang$x,
      y        = lang$y,
      fill     = lang$legend_title,
      caption  = lang$caption
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title          = element_text(face = "bold", size = 16, hjust = 0),
      plot.subtitle       = element_text(size = 12, hjust = 0),
      panel.grid.minor    = element_blank(),
      legend.position     = "bottom",
      legend.title        = element_text(face = "plain"),
      plot.caption        = element_text(face = "italic", hjust = 1, size = 8),
      plot.caption.position = "plot",
      plot.margin         = margin(10, 15, 10, 10)
    ) +
    guides(fill = guide_legend(nrow = 1, title.position = "left"))

  out_file <- paste0("3sectors_balances_CHE_", code, ".jpg")
  ggsave(out_file, p, width = 18, height = 15, units = "cm", dpi = 300)

  # Keep the historical English filename up to date as the default output.
  if (code == "en") {
    ggsave("3sectors_balances_CHE.jpg", p, width = 18, height = 15,
           units = "cm", dpi = 300)
  }

  cat(sprintf("[plot] %s written\n", out_file))
}
