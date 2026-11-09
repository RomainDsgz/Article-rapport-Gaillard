# =====================================================================
# sectoral_balances.R
#
# Downloads AMECO data from the ECFIN Redisstat SDMX API and prepares the
# three-sector sectoral-balance series for Switzerland.
#
# Data source
#   AMECO (DG ECFIN, European Commission), ECFIN Redisstat SDMX 2.1 API
#   https://webgate.ec.europa.eu/ecfin/redisstat/api/dissemination/sdmx/2.1/
#   API guide:
#   https://economy-finance.ec.europa.eu/economic-research-and-databases/economic-databases/ameco-database/bulk-downloads-and-api-access_en
#
# Series retrieved (annual, Switzerland, current prices, Mrd CHF)
#   UBLA  Net lending (+) / net borrowing (-), total economy    (chapter 10-01)
#   UBLG  Net lending (+) / net borrowing (-), general government (chapter 16-03)
#   UVGN  Gross national income at current market prices         (chapter 05-01)
#   UBLC  Net lending (+) / net borrowing (-), corporations     (chapter 14-03)
#   UBLH  Net lending (+) / net borrowing (-), households       (chapter 15-03)
#
# The old manually downloaded AMECO .TXT files (`ameco0/`) and the cached
# `ameco.RData` are no longer used and have been removed.
#
# Outputs
#   sectoral_balances_data.csv
#   sectoral_balances_data.rds
#   3sectors_balances_CHE_fr.jpg
#   3sectors_balances_CHE_de.jpg
#   3sectors_balances_CHE_en.jpg
#   3sectors_balances_CHE.jpg   (English default, kept for backwards compatibility)
#
# Run with: Rscript sectoral_balances.R
# =====================================================================

# Make `Rscript path/to/sectoral_balances.R` work from any directory.
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

library(dplyr)
library(tidyr)
library(readr)

ameco_base <- "https://webgate.ec.europa.eu/ecfin/redisstat/api/dissemination/sdmx/2.1/data/ECFIN"

# Download one annual AMECO series through the SDMX-CSV endpoint.
fetch_ameco <- function(chapter, item, country = "CHE", start = 1995) {
  url <- sprintf(
    "%s,AMECO_CHAPTER%s,1.0/A.%s.1.0.0.0.%s?startPeriod=%d&format=SDMX-CSV",
    ameco_base, chapter, country, item, start
  )
  dat <- utils::read.csv(url, stringsAsFactors = FALSE, check.names = FALSE)
  needed <- c("ITEM", "TIME_PERIOD", "OBS_VALUE")
  if (!all(needed %in% names(dat))) {
    stop(
      "Unexpected response from the AMECO API for ", item, ".\n",
      "Columns returned: ", paste(names(dat), collapse = ", "),
      "\nURL: ", url
    )
  }
  keep <- intersect(c("ITEM", "TIME_PERIOD", "OBS_VALUE", "OBS_STATUS"), names(dat))
  dat[, keep, drop = FALSE]
}

# Chapter / item pairs. Each dataflow in the AMECO API contains one chapter.
series <- list(
  UBLA = c(chapter = "10-01", item = "UBLA"),
  UBLG = c(chapter = "16-03", item = "UBLG"),
  UVGN = c(chapter = "05-01", item = "UVGN"),
  UBLC = c(chapter = "14-03", item = "UBLC"),
  UBLH = c(chapter = "15-03", item = "UBLH")
)

# Download and bind the series.
ameco_raw <- bind_rows(lapply(names(series), function(nm) {
  x <- fetch_ameco(series[[nm]][["chapter"]], series[[nm]][["item"]])
  x$series <- nm
  x
}))

if (!"OBS_STATUS" %in% names(ameco_raw)) {
  ameco_raw$OBS_STATUS <- NA_character_
}

ameco <- ameco_raw %>%
  transmute(
    year   = as.integer(TIME_PERIOD),
    item   = ITEM,
    value  = suppressWarnings(as.numeric(OBS_VALUE)),
    status = OBS_STATUS
  ) %>%
  filter(!is.na(year)) %>%
  distinct(year, item, .keep_all = TRUE) %>%
  arrange(year, item)

# Long -> wide.
dfw <- ameco %>%
  select(year, item, value) %>%
  pivot_wider(names_from = item, values_from = value) %>%
  arrange(year)

required <- c("UBLA", "UBLG", "UVGN")
if (!all(required %in% names(dfw))) {
  stop("Missing required AMECO series: ",
       paste(setdiff(required, names(dfw)), collapse = ", "))
}

# ---------------------------------------------------------------------
# Sectoral balances
#   (S - I) + (T - G) + (-CAB) = 0
#
#   pub = UBLG                       (general government)
#   row = -UBLA                      (rest of the world)
#   priv = -pub - row                (private sector, from the identity)
# ---------------------------------------------------------------------
dfw <- dfw %>%
  mutate(
    pub      = UBLG,
    row      = -UBLA,
    priv     = -pub - row,
    priv_bal = priv / UVGN * 100,
    pub_bal  = pub / UVGN * 100,
    row_bal  = row / UVGN * 100,
    balances = priv_bal + pub_bal + row_bal
  )

# Use the latest uninterrupted year with complete data for the three
# required series. In the current AMECO vintage 2024 is missing for
# UBLA/UVGN while 2025-2027 are forecasts; plotting across that gap would
# break the stacked bars, so the sample stops in 2023.
years_all <- sort(unique(dfw$year))
complete_flag <- complete.cases(dfw[match(years_all, dfw$year), required])
first_incomplete <- which(!complete_flag)[1]
last_year <- if (is.na(first_incomplete)) {
  max(years_all)
} else {
  years_all[first_incomplete] - 1
}
if (is.na(last_year) || last_year < 1995) {
  stop("No complete year found in AMECO data.")
}

dfw <- dfw %>%
  filter(year >= 1995, year <= last_year) %>%
  arrange(year)

# Sanity check: the three balances must sum to zero.
if (any(is.na(dfw$balances))) {
  stop("Missing values remain in the sectoral balances.")
}
if (any(abs(dfw$balances) > 1e-8)) {
  stop("Sectoral balances do not sum to zero. Check the AMECO series.")
}

# Save the prepared data.
write_csv(dfw, "sectoral_balances_data.csv", na = "")
saveRDS(dfw, "sectoral_balances_data.rds")

cat(sprintf("[data] AMECO series downloaded: %s\n", paste(names(series), collapse = ", ")))
cat(sprintf("[data] Years: %d-%d (%d observations)\n", min(dfw$year), last_year, nrow(dfw)))
cat(sprintf("[data] Max |private + public + rest-of-world| = %.3e\n",
            max(abs(dfw$balances))))
cat("[data] Saved sectoral_balances_data.csv and sectoral_balances_data.rds\n")

# Regenerate the plots in French, German and English.
source("generate_plots.R")
