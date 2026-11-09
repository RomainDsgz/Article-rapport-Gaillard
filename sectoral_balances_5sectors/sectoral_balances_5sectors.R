# =====================================================================
# sectoral_balances_5sectors.R
#
# Downloads Eurostat sector-accounts data through the Eurostat statistics
# API and prepares the five-sector sectoral-balance series for Switzerland
# (CH) and Belgium (BE).
#
# Data source
#   Eurostat, dataset `nasa_10_nf_tr` (Non-financial transactions - annual)
#   https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/nasa_10_nf_tr
#
# Indicators retrieved (annual, current prices, million national currency)
#   B9   Net lending (+) / net borrowing (-), by institutional sector:
#          S11      non-financial corporations
#          S12      financial corporations
#          S13      general government
#          S14_S15  households and non-profit institutions serving households
#          S2       rest of the world
#   B5G  Gross national income (sector S1), used as the denominator.
#
# The shares are expressed in percent of gross national income and the
# five sectors sum to zero by construction (B9 sums to zero across S11,
# S12, S13, S14_S15 and S2).
#
# Note: the earlier version of this chart labelled the denominator "net
# national income", but its values correspond to gross national income
# (B5G). This script uses B5G and labels it accordingly.
#
# Outputs
#   sectoral_balances_5sectors_data.csv        (long format)
#   sectoral_balances_5sectors_data_wide.csv   (one row per country-year)
#   sectoral_balances_5sectors_data.rds
#   5sectors_balances_CHE_BEL_fr.jpg
#   5sectors_balances_CHE_BEL_de.jpg
#   5sectors_balances_CHE_BEL_en.jpg
#   5sectors_balances_CHE_BEL.jpg              (English default)
#
# Run with: Rscript sectoral_balances_5sectors.R
# =====================================================================

# Make `Rscript path/to/sectoral_balances_5sectors.R` work from any directory.
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
library(jsonlite)

eurostat_base <- paste0(
  "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/",
  "nasa_10_nf_tr"
)

# ---------------------------------------------------------------------
# Parse a Eurostat JSON-stat 2.0 response into a long data.frame.
# The API returns a flat `value` vector whose names are 0-based indices
# into the dimensions listed in `id` / `size`.
# ---------------------------------------------------------------------
parse_jsonstat <- function(url) {
  js <- jsonlite::fromJSON(url, simplifyVector = FALSE)

  if (is.null(js$value) || length(js$value) == 0) {
    return(data.frame())
  }

  dims  <- unlist(js$id, use.names = FALSE)
  sizes <- unlist(js$size, use.names = FALSE)

  cats <- lapply(seq_along(dims), function(i) {
    idx <- unlist(js$dimension[[dims[i]]]$category$index)
    names(sort(idx))
  })

  vals <- as.numeric(unlist(js$value))
  flat <- as.integer(names(js$value))

  status <- rep(NA_character_, length(vals))
  if (!is.null(js$status) && length(js$status) > 0) {
    st <- unlist(js$status)
    status <- unname(st[match(as.character(flat), names(st))])
  }

  out <- lapply(seq_along(vals), function(j) {
    rem <- flat[j]
    coords <- integer(length(sizes))
    for (i in rev(seq_along(sizes))) {
      coords[i] <- rem %% sizes[i]
      rem <- rem %/% sizes[i]
    }
    rec <- setNames(
      lapply(seq_along(dims), function(i) cats[[i]][coords[i] + 1]),
      dims
    )
    rec$value  <- vals[j]
    rec$status <- status[j]
    as.data.frame(rec, stringsAsFactors = FALSE)
  })

  do.call(rbind, out)
}

# Build a URL for a given indicator / sector / country selection.
eurostat_url <- function(na_item, sectors, geos = c("CH", "BE"), start = 1995) {
  paste0(
    eurostat_base,
    "?format=JSON&lang=EN&unit=CP_MNAC&direct=PAID",
    "&sinceTimePeriod=", start,
    "&na_item=", na_item,
    "&sector=", paste(sectors, collapse = "&sector="),
    "&geo=", paste(geos, collapse = "&geo=")
  )
}

# ---------------------------------------------------------------------
# Download B9 by sector and B5G (gross national income)
# ---------------------------------------------------------------------
sectors_5 <- c("S11", "S12", "S13", "S14_S15", "S2")

b9_raw <- parse_jsonstat(eurostat_url("B9", sectors_5))
gni_raw <- parse_jsonstat(eurostat_url("B5G", "S1"))

required_b9  <- c("geo", "sector", "time", "value")
required_gni <- c("geo", "time", "value")
if (!all(required_b9 %in% names(b9_raw))) {
  stop("Unexpected Eurostat response for B9. Columns: ",
       paste(names(b9_raw), collapse = ", "))
}
if (!all(required_gni %in% names(gni_raw))) {
  stop("Unexpected Eurostat response for B5G. Columns: ",
       paste(names(gni_raw), collapse = ", "))
}

b9 <- b9_raw %>%
  filter(direct == "PAID") %>%
  transmute(
    geo    = geo,
    sector = sector,
    year   = as.integer(time),
    value  = as.numeric(value),
    status = status
  )

gni <- gni_raw %>%
  filter(direct == "PAID", sector == "S1") %>%
  transmute(
    geo  = geo,
    year = as.integer(time),
    gni  = as.numeric(value)
  )

# ---------------------------------------------------------------------
# Labels and shares
# ---------------------------------------------------------------------
sector_labels <- c(
  S11     = "Non-financial corporations",
  S12     = "Financial corporations",
  S13     = "Government",
  S14_S15 = "Households",
  S2      = "Rest of the world"
)

country_labels <- c(
  CH = "Switzerland",
  BE = "Belgium"
)

df <- b9 %>%
  left_join(gni, by = c("geo", "year")) %>%
  mutate(
    country      = unname(country_labels[geo]),
    sector_label = unname(sector_labels[sector]),
    share        = value / gni * 100
  ) %>%
  select(geo, country, year, sector, sector_label, value, gni, share, status) %>%
  arrange(geo, year, sector)

# Keep the years for which B9 is available for both countries. In the
# current vintage CH ends in 2023 while BE has 2024-2025, so the common
# sample ends in 2023.
last_year <- df %>%
  filter(!is.na(share)) %>%
  group_by(geo) %>%
  summarise(last = max(year), .groups = "drop") %>%
  pull(last) %>%
  min()

df <- df %>%
  filter(year >= 1995, year <= last_year) %>%
  arrange(geo, year, sector)

# Each country-year must contain exactly the five sectors and no NA shares.
counts <- df %>%
  count(geo, year, name = "n_sectors") %>%
  filter(n_sectors != length(sectors_5))
if (nrow(counts) > 0) {
  stop("Some country-years do not contain exactly five sectors:\n",
       paste(capture.output(print(counts)), collapse = "\n"))
}
if (any(is.na(df$share))) {
  stop("Missing B9/B5G values remain after cleaning.")
}

# Sanity check: the five sectoral balances sum to zero. Eurostat publishes
# B9 rounded to millions of national currency, so allow a small rounding
# tolerance (0.01 percentage point of GNI).
tol <- 1e-2
sums <- df %>%
  group_by(geo, year) %>%
  summarise(total = sum(share), .groups = "drop")
if (any(abs(sums$total) > tol)) {
  bad <- sums %>% filter(abs(total) > tol)
  stop("Sectoral balances do not sum to zero:\n",
       paste(capture.output(print(bad)), collapse = "\n"))
}

# ---------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------
write_csv(df, "sectoral_balances_5sectors_data.csv", na = "")
saveRDS(df, "sectoral_balances_5sectors_data.rds")

wide <- df %>%
  select(geo, country, year, sector, share) %>%
  pivot_wider(names_from = sector, values_from = share) %>%
  arrange(geo, year)
write_csv(wide, "sectoral_balances_5sectors_data_wide.csv", na = "")

cat(sprintf("[data] Eurostat dataset: nasa_10_nf_tr\n"))
cat(sprintf("[data] Countries: %s\n", paste(unique(df$country), collapse = ", ")))
cat(sprintf("[data] Years: %d-%d (%d country-years)\n",
            min(df$year), last_year, nrow(wide)))
cat(sprintf("[data] Max |sum of five shares| = %.3e\n", max(abs(sums$total))))
cat("[data] Saved sectoral_balances_5sectors_data.csv, ",
    "sectoral_balances_5sectors_data_wide.csv and .rds\n", sep = "")

# Regenerate the plots in French, German and English.
source("generate_plots.R")
