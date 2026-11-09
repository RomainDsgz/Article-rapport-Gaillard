# =====================================================================
# generate_plots.R
#
# Generates the five-sector sectoral-balance plot for Switzerland and
# Belgium in French, German and English, reading
# `sectoral_balances_5sectors_data.rds`.
#
# The data are prepared by `sectoral_balances_5sectors.R`, which downloads
# the Eurostat `nasa_10_nf_tr` dataset through the Eurostat statistics API.
#
# Outputs
#   5sectors_balances_CHE_BEL_fr.jpg
#   5sectors_balances_CHE_BEL_de.jpg
#   5sectors_balances_CHE_BEL_en.jpg
#   5sectors_balances_CHE_BEL.jpg   (English default)
#
# Run with: Rscript generate_plots.R
# (run sectoral_balances_5sectors.R first if the data need to be refreshed)
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
library(ggpubr)

# If the prepared data are absent, fetch them first.
if (!file.exists("sectoral_balances_5sectors_data.rds")) {
  source("sectoral_balances_5sectors.R")
}

df <- readRDS("sectoral_balances_5sectors_data.rds")
last_year <- max(df$year, na.rm = TRUE)
source_year <- format(Sys.Date(), "%Y")

## ---------------------------------------------------------------------
## Localized strings
## ---------------------------------------------------------------------
languages <- list(
  fr = list(
    code    = "fr",
    title   = "Balances sectorielles (approche à 5 secteurs)",
    y       = "Balance (en % du revenu national brut)",
    x       = "Année",
    legend_title = "Secteurs",
    countries = c(CH = "Suisse", BE = "Belgique"),
    labels = c(
      S13     = "Administrations publiques",
      S11     = "Entreprises non financières",
      S2      = "Reste du monde",
      S14_S15 = "Ménages",
      S12     = "Entreprises financières"
    ),
    caption = sprintf(
      "Source : Eurostat (nasa_10_nf_tr) 2026.",
      source_year
    )
  ),
  de = list(
    code    = "de",
    title   = "Sektorale Salden (5-Sektoren-Ansatz)",
    y       = "Saldo (in % des Bruttonationaleinkommens)",
    x       = "Jahr",
    legend_title = "Sektoren",
    countries = c(CH = "Schweiz", BE = "Belgien"),
    labels = c(
      S13     = "Staat",
      S11     = "Nichtfinanzielle Unternehmen",
      S2      = "Übrige Welt",
      S14_S15 = "Haushalte",
      S12     = "Finanzielle Unternehmen"
    ),
    caption = sprintf(
      "Quelle: Eurostat (nasa_10_nf_tr), %s.",
      source_year
    )
  ),
  en = list(
    code    = "en",
    title   = "Sectoral balances (5-sector approach)",
    y       = "Balance (% of gross national income)",
    x       = "Year",
    legend_title = "Sectors",
    countries = c(CH = "Switzerland", BE = "Belgium"),
    labels = c(
      S13     = "Government",
      S11     = "Non-financial corporations",
      S2      = "Rest of the world",
      S14_S15 = "Households",
      S12     = "Financial corporations"
    ),
    caption = sprintf(
      "Source: Eurostat (nasa_10_nf_tr), %s.",
      source_year
    )
  )
)

## ---------------------------------------------------------------------
## Plot constants
## ---------------------------------------------------------------------
# The order of the factor levels controls the stacking order. With
# position_stack(), positive values are stacked from the last level
# upwards, and negative values from the last level downwards. This gives
# households at the bottom of the positive stack, financial corporations
# above them and government on top; on the negative side the rest of the
# world is closest to zero and non-financial corporations below it.
stack_levels <- c("S11", "S2", "S13", "S12", "S14_S15")

# Legend order in the original chart.
legend_order <- c("S13", "S11", "S2", "S14_S15", "S12")

sector_colors <- c(
  S13     = "#C64B4C",  # government (red)
  S11     = "#3C8000",  # non-financial corporations (green)
  S2      = "#00BDC6",  # rest of the world (turquoise)
  S14_S15 = "#7F5B7F",  # households (purple)
  S12     = "#C5AEAB"   # financial corporations (beige)
)

## ---------------------------------------------------------------------
## Build and save one plot per language
## ---------------------------------------------------------------------
# One country panel. Keeping the two countries as separate plots (rather
# than facets) reproduces the reference chart's separate y-axis labels.
make_panel <- function(geo_code, lang) {
  d <- df %>%
    filter(geo == geo_code) %>%
    mutate(sector = factor(sector, levels = stack_levels))

  ggplot(d, aes(x = year, y = share, fill = sector)) +
    geom_col(width = 0.8) +
    scale_fill_manual(
      values = sector_colors,
      breaks = legend_order,
      labels = unname(lang$labels[legend_order])
    ) +
    scale_x_continuous(
      breaks = seq(1995, last_year, by = 1),
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(
      breaks = seq(-20, 20, by = 10),
      labels = function(x) paste0(x, " %")
    ) +
    coord_cartesian(ylim = c(-20, 20)) +
    labs(
      title = paste0(lang$countries[geo_code], ", 1995-", last_year),
      x     = lang$x,
      y     = lang$y,
      fill  = lang$legend_title
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title        = element_text(hjust = 0, size = 12, face = "plain",
                                       margin = margin(b = 4)),
      axis.text.x       = element_text(angle = 90, vjust = 0.5, hjust = 1,
                                       size = 7),
      panel.grid.minor  = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      legend.position   = "bottom",
      legend.title      = element_text(face = "plain"),
      legend.key.size   = unit(0.5, "cm"),
      plot.margin       = margin(5, 8, 5, 5)
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE,
                               title.position = "left"))
}

for (lang in languages) {
  code <- lang$code

  p_ch <- make_panel("CH", lang)
  p_be <- make_panel("BE", lang)

  combined <- ggarrange(
    p_ch, p_be,
    ncol = 2, nrow = 1,
    common.legend = TRUE,
    legend = "bottom"
  )
  combined <- annotate_figure(
    combined,
    top = text_grob(lang$title, face = "bold", size = 16),
    bottom = text_grob(lang$caption, x = 1, hjust = 1,
                       face = "italic", size = 8)
  )

  out_file <- paste0("5sectors_balances_CHE_BEL_", code, ".jpg")
  ggsave(out_file, combined, width = 22, height = 16, units = "cm", dpi = 300)

  # Keep a default English filename.
  if (code == "en") {
    ggsave("5sectors_balances_CHE_BEL.jpg", combined, width = 22, height = 16,
           units = "cm", dpi = 300)
  }

  cat(sprintf("[plot] %s written\n", out_file))
}
