# Sectoral balances — 5-sector approach (Switzerland and Belgium)

This folder contains an updated five-sector sectoral-balance chart for
Switzerland (CH) and Belgium (BE), in French, German and English.

## Data

Data are downloaded from the **Eurostat** statistics API, dataset
`nasa_10_nf_tr` (Non-financial transactions, annual data):

- `B9` — net lending (+) / net borrowing (−), by institutional sector:
  `S11` non-financial corporations, `S12` financial corporations,
  `S13` general government, `S14_S15` households and NPISH, `S2` rest of
  the world;
- `B5G` — gross national income (sector `S1`), used as the denominator.

Balances are expressed in percent of gross national income. The five
sectors sum to zero by construction.

The common sample ends in 2023 because B9 for Switzerland is not yet
available for 2024–2025 (Belgium is).

## Run

From this folder:

```r
Rscript sectoral_balances_5sectors.R
```

This downloads the data, saves the cleaned CSV/RDS files and sources
`generate_plots.R`, which creates:

- `5sectors_balances_CHE_BEL_fr.jpg`
- `5sectors_balances_CHE_BEL_de.jpg`
- `5sectors_balances_CHE_BEL_en.jpg`
- `5sectors_balances_CHE_BEL.jpg` (English default)

To regenerate the plots only, run:

```r
Rscript generate_plots.R
```

## Files

- `sectoral_balances_5sectors.R` — data acquisition and cleaning
- `generate_plots.R` — multilingual plotting
- `sectoral_balances_5sectors_data.csv` — long format
- `sectoral_balances_5sectors_data_wide.csv` — one row per country-year
- `sectoral_balances_5sectors_data.rds` — R data file used by the plots
