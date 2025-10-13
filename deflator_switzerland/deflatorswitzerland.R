# clean environment
rm(list = ls())

###################################################
# Obtaining and cleaning the data

# Manually download .txt zip from


files <- dir("C:/Users/rdesg/Documents/deflator_switzerland/ameco0", "*.TXT", full.names = TRUE)

## Open libraries 
# load packages
library(dplyr)
library(tidyr)
library(tibble)
library(ggpubr)

install.packages("ggpubr")
# Read files, bind together, clean, save
all_files <- lapply(files, function(file) {
  read.table(file, TRUE, ";", fill = TRUE,
             stringsAsFactors = FALSE, strip.white = TRUE)
})

#unlink(temp_dir, recursive = TRUE)

ameco <- do.call(rbind, all_files)
ameco <- tibble::as_tibble(ameco)
ameco <- ameco[, -ncol(ameco)] # Drop stray/empty last column
names(ameco) <- tolower(names(ameco))
# Extract short country names
ameco$cntry <- regmatches(ameco$code,regexpr("^[[:alnum:]]+", ameco$code))

# Convert to long format
ameco <- gather(ameco, key = year, value = value, starts_with("x"))
ameco$year <- gsub("x", "", ameco$year)
ameco$year <- as.numeric(ameco$year)
ameco$value <- suppressWarnings(as.numeric(ameco$value))

save(ameco, file = "C:/Users/rdesg/Documents/deflator_switzerland/ameco.RData", compress = "xz")

###################################################

# load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(tibble)

# Load the data
load("C:/Users/rdesg/Documents/deflator_switzerland/ameco.RData")

ch <- subset(ameco, cntry == "CHE")

OVGD <- subset(ch, code=="CHE.1.1.0.0.OVGD") # Gross domestic product / Gross domestic product at 2010 reference levels / Mrd EURO-CHE
UVGD <- subset(ch, code=="CHE.1.0.0.0.UVGD") # Gross domestic product / Gross domestic product at current prices / Mrd EURO-CHE
UWCD <- subset(ch, code=="CHE.1.0.0.0.UWCD") # Compensation of employees / Total Economy / Mrd EURO-CHE
UOGD <- subset(ch, code=="CHE.1.0.0.0.UOGD") # Gross Operating Surplus / Total Economy / Mrd EURO-CHE
UTVT <- subset(ch, code=="CHE.1.0.0.0.UTVT") # Taxes linked to imports and production and subsidies / Taxes linked to imports and production: total economy / Mrd EURO-CHE
NLHT <- subset(ch, code=="CHE.1.0.0.0.NLHT") # Gross domestic product per hour worked / Total annual hours worked / Millions
NWTN <- subset(ch, code=="CHE.1.0.0.0.NWTN") # Wage and salary earners, persons (national accounts) / Employees (persons) Total economy: national accounts / 1000 persons
UQGD <- subset(ch, code=="CHE.1.0.0.0.UQGD") # Wage and salary earners, persons (national accounts) / Employees (persons) Total economy: national accounts / 1000 persons


# OVGD - Gross domestic product / Gross domestic product at 2010 reference levels / Mrd EURO-CHE
# UVGD - Gross domestic product / Gross domestic product at current prices / Mrd EURO-CHE
# UWCD - Compensation of employees / Total Economy / Mrd EURO-CHE
# UOGD - Gross Operating Surplus / Total Economy / Mrd EURO-CHE
# UTVT - Taxes linked to imports and production and subsidies / Taxes linked to imports and production: total economy / Mrd EURO-CHE
# NLHT - Gross domestic product per hour worked / Total annual hours worked / Millions
# NWTN - Wage and salary earners, persons (national accounts) / Employees (persons) Total economy: national accounts / 1000 persons

# merge them under one df
df <- rbind(OVGD, UVGD, UWCD, UOGD, UTVT, NLHT, NWTN, UQGD)

# select vars
myvars <- c("year", "code", "value")
df <- df[myvars]

# long to wide
dfw <- spread(df,
              key = code,
              value = value)

# rename
dfw <- rename(dfw, OVGD = "CHE.1.1.0.0.OVGD")
dfw <- rename(dfw, UVGD = "CHE.1.0.0.0.UVGD")
dfw <- rename(dfw, UWCD = "CHE.1.0.0.0.UWCD")
dfw <- rename(dfw, UOGD = "CHE.1.0.0.0.UOGD")
dfw <- rename(dfw, UTVT = "CHE.1.0.0.0.UTVT")
dfw <- rename(dfw, NLHT = "CHE.1.0.0.0.NLHT")
dfw <- rename(dfw, NWTN = "CHE.1.0.0.0.NWTN")
dfw <- rename(dfw, UQGD = "CHE.1.0.0.0.UQGD")

dfw$sum_UTVT_UWCD_UOGD <- dfw$UTVT + dfw$UWCD + dfw$UOGD
dfw$sum_UTVT_UWCD_UQGD <- dfw$UTVT + dfw$UWCD + dfw$UQGD

# Calculate the GDP deflator
dfw <- dfw %>% mutate(GDP_deflator = UVGD / OVGD)

# Isolate GDP deflator (for project gofog)
GDPdefl <- dfw$GDP_deflator[c(36:64)]
GDPdefl
# Compute components
dfw <- dfw %>% mutate(
  unit_labor_costs = UWCD / OVGD,
  wages = UWCD / (NWTN * 1000),
  labor_prod = OVGD / (NWTN * 1000),
  unit_gross_operating_surplus = UOGD / OVGD,
  unit_net_indirect_taxes = UTVT / OVGD
)

# Calculate percentage changes
dfw <- dfw %>% mutate(
  perc_change_GDP_deflator = (GDP_deflator / lag(GDP_deflator) - 1) * 100,
  perc_change_unit_labor_costs = (unit_labor_costs / lag(unit_labor_costs) - 1) * 100,
  perc_change_wages = (wages / lag(wages) - 1) * 100,
  perc_change_labor_prod = (labor_prod / lag(labor_prod) - 1) * 100,
  perc_change_unit_gross_operating_surplus = (unit_gross_operating_surplus / lag(unit_gross_operating_surplus) - 1) * 100,
  perc_change_unit_net_indirect_taxes = (unit_net_indirect_taxes / lag(unit_net_indirect_taxes) - 1) * 100
)

# Calculate contributions
dfw <- dfw %>% mutate(
  labor_costs_contrib = perc_change_unit_labor_costs * lag(unit_labor_costs / GDP_deflator),
  labor_prod_contrib = (labor_costs_contrib - perc_change_wages * lag(wages / GDP_deflator)) / (1 + perc_change_wages),
  wages_contrib = labor_costs_contrib - labor_prod_contrib,
  gross_operating_surplus_contrib = perc_change_unit_gross_operating_surplus * lag(unit_gross_operating_surplus / GDP_deflator),
  net_indirect_taxes_contrib = perc_change_unit_net_indirect_taxes * lag(unit_net_indirect_taxes / GDP_deflator))
# Prepare the data for plotting
melted_df <- dfw %>% select(year, labor_prod_contrib, wages_contrib, gross_operating_surplus_contrib, net_indirect_taxes_contrib) %>% 
  gather(key = "variable", value = "value", -year)

# Create the stacked bar plot with total GDP deflator as a line on top
my_plot <- ggplot() +
  geom_bar(data = subset(melted_df, year >= 1995 & year <= 2025), aes(x = year, y = value, fill = variable), stat = "identity") +
  geom_line(data = subset(dfw, year >= 1995 & year <= 2025), aes(x = year, y = perc_change_GDP_deflator, color = "GDP deflator"), size = 1) +
  geom_vline(xintercept=2003, linetype="dotted", color="blue", size=0.8) +
  scale_fill_manual(values = c("gross_operating_surplus_contrib" = "cyan3", "labor_prod_contrib" = "goldenrod3", "net_indirect_taxes_contrib" = "indianred3", "wages_contrib" = "chartreuse4"),
                    labels = c("Unit Profits", "Labor Productivity", "Unit Taxes",  "Wages")) +
  labs(x = "Year", y = "Annual percentage changes; p.p. contributions", fill = "Contributions", color = "") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1960, 2025, by = 5)) +
  scale_color_manual(name = "", values = "black", labels = "GDP Deflator") +
  labs(title = "GDP Deflator income side", 
       subtitle = "Switzerland 1995-2025", 
       caption = "Source: AMECO. Original Script by @gmvidl. Adaptation from Desgraz (2023)")

print(my_plot)
ggsave(filename = "my_plot2025.png", plot = last_plot(), device = png, path = NULL) 


my_plot2020 <- ggplot() +
  geom_bar(data = subset(melted_df, year >= 1995 & year <= 2020), aes(x = year, y = value, fill = variable), stat = "identity") +
  geom_line(data = subset(dfw, year >= 1995 & year <= 2020), aes(x = year, y = perc_change_GDP_deflator, color = "GDP deflator"), size = 1) +
  #geom_point(data = subset(dfw, year >= 1995 & year <= 2019),aes(x=year, y = unit_labor_costs)) +
  scale_fill_manual(values = c("gross_operating_surplus_contrib" = "palevioletred4", "labor_prod_contrib" = "green", "net_indirect_taxes_contrib" = "indianred3", "wages_contrib" = "chartreuse4"),
                    labels = c("Unit Profits", "Labor Productivity", "Unit Taxes",  "Wages")) +
  labs(x = "Year", y = "Annual percentage changes; p.p. contributions", fill = "Contributions", color = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) + 
  scale_x_continuous(breaks = seq(1960, 2020, by = 5)) +
  scale_color_manual(name = "", values = "black", labels = "GDP Deflator") +
  labs(title = "GDP Deflator income side", 
       subtitle = "Switzerland 1995-2020", 
       caption = "Source: AMECO. Original Script by @gmvidl. Adaptation from author")
figure <- ggarrange(my_plot2019, my_plot2020,
                    ncol = 2, nrow =1)
figure
