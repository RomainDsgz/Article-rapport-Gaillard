# clean environment 
rm(list=ls())

# Packages

# load packages
library(dplyr)
library(tidyr)
library(tibble)
library(readxl)
library(ggplot2)

# Import data
df <- read_xlsx("data_ofdf.xlsx")

# Create variable for the radio between key imports (food and energy products) compared to total imports
df$foodenergy <- df$energy + df$food

df$ratio <- (df$foodenergy / df$total) * 100

ggplot(df, aes(x=year, y=ratio)) +
  geom_line(size=0.8) +
  # geom_vline(xintercept= 2003, linetype ="dotted", color="blue", size = 1) +
  theme(plot.title = element_text(hjust=0.5, size=10), legend.position="bottom") +
  labs(title="Share of food and energy in total imports",
       subtitle = "Switzerland, 2000-Q1 - 2025-Q2",
       x="Year",
       y="Ratio between food and energy compared to total imports (in %)",
       caption = "Source: Federal Office for Customs and Border Security (FOCBS), 2025") + 
  theme_minimal()
ggsave("ratiofoodenergy.jpg", width = 18, height = 15, units = "cm")