# =====================================================================
# generate_updated_plots.R
#
# Generates, in three languages (FR / DE / EN):
#   (A) an updated grouped bar plot comparing the planned budget cuts
#       ("Message") with the cuts actually enacted by parliament
#       ("Decision de l'Assemblee federale"), aggregated per category;
#   (B) the public-debt line plot (debt in GDP-years);
#   (C) the debt-holders composition stacked bar plot;
#       (B) and (C) are combined side by side exactly like the old script.
#
# Reads : Data/gaillard_cut_updated_data.xlsx
#         Data/debt_OECD.csv
# Writes: cuts_bargraph_updated_{fr,de,en}.jpg
#         debt_and_composition_{fr,de,en}.jpg
#
# Run non-interactively with:  Rscript generate_updated_plots.R
# This script does NOT modify generate_plots.R nor anything in Data/,
# and does NOT overwrite the old outputs (cuts_bargraph_en.jpg,
# debt_and_composition.jpg).
# =====================================================================

library(readxl)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(dplyr)

## ---------------------------------------------------------------------
## Section 0: localized strings / configuration per language
## ---------------------------------------------------------------------
languages <- list(
  fr = list(
    code = "fr",
    # --- panel A: budget-cut bar plot ---
    bar = list(
      title   = "Total des coupes sur la période 2027 - 2029",
      ylab    = "Coupe (CHF mio)",
      xlab    = "",
      msg     = "Message",
      dec     = "Décision de l'Assemblée fédérale",
      caption = paste0("Source: Dep. Fédéral des Finances (2025); ",
                       "'Programme d'allégement budgétaire 2027 - Aperçu des mesures'")
    ),
    # --- panel B: public debt line plot ---
    debt = list(
      title   = "Endettement public",
      y       = "Dette en PIB-années",
      x       = "Année",
      caption = "Source des données: OCDE (2024)",
      legend_title = "Pays",
      countries = c(Switzerland = "Suisse", France = "France",
                    Germany = "Allemagne", Italy = "Italie",
                    Austria = "Autriche", Belgium = "Belgique")
    ),
    # --- panel C: debt-holders composition ---
    comp = list(
      title   = "Porteurs des titres",
      x       = "Année",
      y       = "Part (%)",
      caption = "Source des données: Banque Nationale Suisse (2024)",
      legend_title = "Catégorie",
      labels  = c(
        "Foreign.investors"   = "Investisseurs étrangers",
        "Investment.funds"    = "Fonds d'investissements (CH)",
        "Pension.funds"       = "Fonds de pension (CH)",
        "Insurance.companies" = "Compagnies d'assurance (CH)",
        "Others..Switzerland" = "Autres (CH)",
        "Swiss.bank.positions" = "Banques (CH)"
      )
    )
  ),
  de = list(
    code = "de",
    bar = list(
      title   = "Gesamte Kürzungen im Zeitraum 2027 - 2029",
      ylab    = "Kürzung (CHF Mio.)",
      xlab    = "",
      msg     = "Botschaft",
      dec     = "Beschluss der Bundesversammlung",
      caption = paste0("Quelle: Eidgenössisches Finanzdepartement (2025); ",
                       "'Entlastungsprogramm 2027 - Übersicht der Massnahmen'")
    ),
    debt = list(
      title   = "Öffentliche Verschuldung",
      y       = "Schulden in BIP-Jahren",
      x       = "Jahr",
      caption = "Datenquelle: OECD (2024)",
      legend_title = "Land",
      countries = c(Switzerland = "Schweiz", France = "Frankreich",
                    Germany = "Deutschland", Italy = "Italien",
                    Austria = "Österreich", Belgium = "Belgien")
    ),
    comp = list(
      title   = "Inhaber der Schuldtitel",
      x       = "Jahr",
      y       = "Anteil (%)",
      caption = "Datenquelle: Schweizerische Nationalbank (2024)",
      legend_title = "Kategorie",
      labels  = c(
        "Foreign.investors"   = "Ausländische Investoren",
        "Investment.funds"    = "Anlagefonds (CH)",
        "Pension.funds"       = "Pensionskassen (CH)",
        "Insurance.companies" = "Versicherungen (CH)",
        "Others..Switzerland" = "Andere (CH)",
        "Swiss.bank.positions" = "Banken (CH)"
      )
    )
  ),
  en = list(
    code = "en",
    bar = list(
      title   = "Total cuts over the period 2027 - 2029",
      ylab    = "Cut (CHF mio)",
      xlab    = "",
      msg     = "Message",
      dec     = "Decision of the Federal Assembly",
      caption = paste0("Source: Federal Department of Finance (2025); ",
                       "'Fiscal relief programme 2027 - Overview of measures'")
    ),
    debt = list(
      title   = "Public debt",
      y       = "Debt in GDP-years",
      x       = "Year",
      caption = "Data source: OECD (2024)",
      legend_title = "Country",
      countries = c(Switzerland = "Switzerland", France = "France",
                    Germany = "Germany", Italy = "Italy",
                    Austria = "Austria", Belgium = "Belgium")
    ),
    comp = list(
      title   = "Holders of the securities",
      x       = "Year",
      y       = "Share (%)",
      caption = "Data source: Swiss National Bank (2024)",
      legend_title = "Category",
      labels  = c(
        "Foreign.investors"   = "Foreign investors",
        "Investment.funds"    = "Investment funds (CH)",
        "Pension.funds"       = "Pension funds (CH)",
        "Insurance.companies" = "Insurance companies (CH)",
        "Others..Switzerland" = "Others (CH)",
        "Swiss.bank.positions" = "Swiss banks (CH)"
      )
    )
  )
)

## =====================================================================
## Section A: data for the updated budget-cut bar plot
## =====================================================================

# Read the sheet with a two-row header, without any name mangling.
cuts_raw <- read_excel("Data/gaillard_cut_updated_data.xlsx",
                       col_names = FALSE, .name_repair = "minimal")

# Assign column names manually (see sheet structure).
colnames(cuts_raw) <- c(
  "No",                                             # 1
  "DepFR",                                          # 2
  "NomFR",                                          # 3  Nom de la mesure [FR]
  "NomDE",                                          # 4  Bezeichnung der Maßnahme [DE]
  "NomEN",                                          # 5  Name of the fiscal policy measure [EN]
  "CatEN",                                          # 6  Category [EN]
  "CatDE",                                          # 7  Kategorie [DE]
  "CatFR",                                          # 8  Categorie [FR]
  "Msg2027", "Msg2028", "Msg2029",                  # 9-11  Message PF 2027/28/29
  "Dec2027", "Dec2028", "Dec2029"                   # 12-14 Décision PF 2027/28/29
)

# Convert the six numeric columns; non-numeric header cells become NA quietly.
num_cols <- c("Msg2027", "Msg2028", "Msg2029", "Dec2027", "Dec2028", "Dec2029")
cuts_raw[num_cols] <- lapply(cuts_raw[num_cols],
                             function(x) suppressWarnings(as.numeric(x)))

# Keep only the real measure rows (rows 6 to 68). A robust filter additionally
# requires a measure name (NomFR) and a French category (CatFR) to be present,
# which drops the summary rows ("Total", "Écart", "Écart en %") and any header.
measure_rows <- 6:68
cuts <- cuts_raw[measure_rows, ]
cuts <- cuts[!is.na(cuts$NomFR) & !is.na(cuts$CatFR), ]

# Per-measure totals across the three plan years.
cuts$Msg_Total <- rowSums(cuts[, c("Msg2027", "Msg2028", "Msg2029")], na.rm = TRUE)
cuts$Dec_Total <- rowSums(cuts[, c("Dec2027", "Dec2028", "Dec2029")], na.rm = TRUE)

# Sanity checks: the two official grand totals must match; failing loudly here
# protects against any future change in the spreadsheet structure.
msg_total <- sum(cuts$Msg_Total, na.rm = TRUE)
dec_total <- sum(cuts$Dec_Total, na.rm = TRUE)
cat(sprintf("[check] Message total  = %.1f (expected 8543.1)\n", msg_total))
cat(sprintf("[check] Decision total = %.1f (expected 5226.5)\n", dec_total))
stopifnot(
  "Message total should equal 8543.1"  = abs(msg_total - 8543.1) < 1e-6,
  "Decision total should equal 5226.5" = abs(dec_total - 5226.5) < 1e-6
)

# No missing category in any language for the included rows.
stopifnot(
  "CatFR contains NA" = !any(is.na(cuts$CatFR)),
  "CatDE contains NA" = !any(is.na(cuts$CatDE)),
  "CatEN contains NA" = !any(is.na(cuts$CatEN))
)

# Map a language code to the matching category column read from the sheet.
cuts_cat_col <- function(code) {
  switch(code,
         fr = "CatFR",   # Categorie [FR]
         de = "CatDE",   # Kategorie [DE]
         en = "CatEN",   # Category [EN]
         stop("unknown language code: ", code))
}

# Build the long-format table used by the grouped bar plot, per language.
# `catcol` selects the category column read from the sheet (FR / DE / EN).
build_cuts_long <- function(catcol, lab) {
  df <- data.frame(Category  = cuts[[catcol]],
                   Msg_Total = cuts$Msg_Total,
                   Dec_Total = cuts$Dec_Total)
  agg <- aggregate(cbind(Msg_Total, Dec_Total) ~ Category, data = df,
                   FUN = sum, na.rm = TRUE)

  # Order categories ascending by the planned (Message) amount so the
  # horizontal bar order stays stable across languages.
  agg <- agg[order(agg$Msg_Total), ]
  agg$Category <- factor(agg$Category, levels = agg$Category)

  long <- tidyr::pivot_longer(agg,
                              cols = c("Msg_Total", "Dec_Total"),
                              names_to = "Series", values_to = "Amount")
  # Localized legend labels as factor levels.
  long$Series <- factor(long$Series,
                        levels = c("Msg_Total", "Dec_Total"),
                        labels = c(lab$bar$msg, lab$bar$dec))
  long
}

## =====================================================================
## Section B / C: data for the debt plots (logic reused from generate_plots.R)
## =====================================================================

# Debt-to-GDP data -> wide format via base reshape.
debt_to_gdp_ratios <- read.csv("Data/debt_OECD.csv")

# Keep only the columns needed by reshape; this avoids the harmless
# "constant variables vary" reshape warning caused by the redundant
# REF_AREA column and yields exactly the same wide table.
debt_to_gdp_ratios <- debt_to_gdp_ratios[, c("TIME_PERIOD", "Reference.area", "OBS_VALUE")]

debt_wide <- reshape(debt_to_gdp_ratios,
                     idvar = "TIME_PERIOD",
                     timevar = "Reference.area",
                     direction = "wide",
                     v.names = "OBS_VALUE")

rownames(debt_wide) <- debt_wide$TIME_PERIOD
debt_wide <- debt_wide[, -1]                       # drop TIME_PERIOD
colnames(debt_wide) <- gsub("OBS_VALUE.", "", colnames(debt_wide))

# Proper dates for the time axis.
years <- as.numeric(rownames(debt_wide))
debt_wide$Year <- as.Date(paste0(years, "-01-01"))

# Countries of interest and their fixed color palette.
countries <- c("Switzerland", "France", "Germany", "Italy", "Austria", "Belgium")
custom_colors <- c("blue", "cyan3", "goldenrod3", "indianred3",
                   "chartreuse4", "palevioletred4")

# Wide -> long for the line plot.
line_data <- data.frame(Year = debt_wide$Year)
for (country in countries) {
  line_data[[country]] <- debt_wide[[country]]
}
line_data_long <- reshape(line_data,
                          direction = "long",
                          varying = countries,
                          v.names = "Value",
                          timevar = "Country",
                          times = countries,
                          ids = 1:nrow(line_data))
line_data_long$Value <- line_data_long$Value / 100

# Hardcoded debt-holders composition (same as the old script).
debt_composition <- data.frame(
  `Investment funds`     = c(26.2, 37.6),
  `Insurance companies`  = c(22.9, 27.4),
  `Pension funds`        = c(17.5, 9.2),
  `Swiss bank positions` = c(7.2, 6.2),
  `Others, Switzerland`  = c(10.2, 6.2),
  `Foreign investors`    = c(16.0, 13.4)
)
rownames(debt_composition) <- c("2010", "2024")
debt_composition$Year <- rownames(debt_composition)

# Long format for the stacked bar plot.
category_names <- names(debt_composition)[names(debt_composition) != "Year"]
debt_comp_long <- reshape(debt_composition,
                          direction = "long",
                          varying = category_names,
                          v.names = "Value",
                          timevar = "Category",
                          times = category_names,
                          idvar = "Year")
rownames(debt_comp_long) <- NULL
debt_comp_long$Year <- factor(debt_comp_long$Year, levels = c("2010", "2024"))

## =====================================================================
## Section D: build and save every plot, once per language
## =====================================================================
for (lang in languages) {
  code <- lang$code

  ## --- (A) updated grouped bar plot ---------------------------------
  cuts_long <- build_cuts_long(cuts_cat_col(code), lang)

  p_bar <- ggbarplot(
      cuts_long,
      x = "Category", y = "Amount",
      fill = "Series", color = "Series",
      palette = setNames(c("steelblue", "indianred3"),
                         c(lang$bar$msg, lang$bar$dec)),
      rotate = TRUE,                         # horizontal bars
      position = position_dodge(0.8),        # the two series side by side
      xlab = lang$bar$xlab,
      ylab = lang$bar$ylab,
      title = lang$bar$title
    ) +
    theme_minimal() +
    ylim(0, 3000) +
    labs(caption = paste(strwrap(lang$bar$caption, width = 95),
                         collapse = "\n")) +
    theme(plot.caption = element_text(face = "italic", hjust = 1, size = 8,
                                      margin = margin(t = 6)),
          legend.title = element_blank())

  ggsave(paste0("cuts_bargraph_updated_", code, ".jpg"),
         plot = p_bar, width = 20, height = 15, units = "cm", dpi = 300)

  ## --- (B) public debt line plot ------------------------------------
  p1 <- ggplot(line_data_long, aes(x = Year, y = Value, color = Country)) +
    geom_line(linewidth = 1) +
    labs(title = lang$debt$title,
         y = lang$debt$y,
         x = lang$debt$x) +
    theme_pubr() +
    scale_color_manual(values = custom_colors,
                       labels = lang$debt$countries) +
    labs(color = lang$debt$legend_title,
         caption = lang$debt$caption) +
    theme(plot.caption = element_text(size = 8, color = "gray40"))

  ## --- (C) debt-holders composition stacked bar plot ----------------
  p2 <- ggbarplot(debt_comp_long,
                  x = "Year", y = "Value",
                  fill = "Category", color = "Category",
                  palette = custom_colors,
                  title = lang$comp$title,
                  xlab = lang$comp$x,
                  ylab = lang$comp$y) +
    theme_pubr() +
    scale_fill_manual(values = custom_colors, labels = lang$comp$labels) +
    scale_color_manual(values = custom_colors, labels = lang$comp$labels) +
    labs(fill = lang$comp$legend_title,
         color = lang$comp$legend_title,
         caption = lang$comp$caption) +
    theme(plot.caption = element_text(size = 8, color = "gray40"))

  ## --- combine (B) and (C) and save ---------------------------------
  combined_plot <- ggarrange(p1, p2,
                             ncol = 2, nrow = 1,
                             common.legend = FALSE,
                             legend = "right")
  ggsave(paste0("debt_and_composition_", code, ".jpg"),
         plot = combined_plot, width = 16, height = 7, dpi = 300)

  cat(sprintf("[done] language '%s': %d categories plotted\n",
              code, length(unique(cuts_long$Category))))
  cat(sprintf("       categories (%s): %s\n", toupper(code),
              paste(levels(cuts_long$Category), collapse = " | ")))
}
