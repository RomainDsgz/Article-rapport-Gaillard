# clean environment 
rm(list=ls())

# Packages

# load packages
install.packages("scales")
library(dplyr)
library(tidyr)
library(tibble)
library(readxl)
library(ggplot2)
library(areaplot)
library(scales)

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

# Create a graph with stacked food and energy

# Transform data format
df_long <- df %>%
  pivot_longer(cols = c(energy, food),
               names_to = "category",
               values_to = "value")

df_long$category <- factor(df_long$category,
                           levels = c("energy", "food"))

# Coefficient for second Y-axis
coef <- max(df_long$value, na.rm = TRUE) / max(df$ratio, na.rm = TRUE)


# ---- 2. Facteur avec food au-dessus et energy en bas ----
# (le premier niveau = bas du graphique)
df_long$category <- factor(df_long$category, levels = c("food", "energy"))

# ---- 3. Coefficient pour 2e axe ----
coef <- max(df_long$value, na.rm = TRUE) / max(df$ratio, na.rm = TRUE)

# ---- 4. Graphique ----
ggplot(df_long, aes(x = year)) +
  geom_area(
    aes(y = value, fill = category),
    position = "stack",
    alpha = 0.8
  ) +
  
  # Ligne ratio — ajout de aes(color = "Ratio")
  geom_line(
    data = df,
    aes(y = ratio * coef, color = "Ratio"),
    linewidth = 1.1
  ) +
 
  scale_y_continuous(
    name = "Imports (thousands CHF)",
    labels = scales::comma,
    sec.axis = sec_axis(~./coef, name = "Share of total imports (%)")
  ) +
  
  # Même nom de légende pour fill et color
  scale_color_manual(
    name = NULL,
    values = c("Ratio" = "black"),
    labels = c("Share of food and energy in total imports (right-side axis) ")
  ) +
  
  scale_fill_manual(
    name = NULL,
    values = c("energy" = "#E69F00", "food" = "#56B4E9"),
    labels = c("Food", "Energy")
  ) +

  labs(
    title = "Share of food and energy in total imports",
    subtitle = "Switzerland, 2000-Q1 – 2025-Q2",
    x = "Year",
    caption = "Source: Federal Office for Customs and Border Security (FOCBS), 2025"
  ) +
  guides(
    fill = guide_legend(order = 1, override.aes = list(color = NA)),
    color = guide_legend(order = 2)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.title.y.right = element_text(color = "black")
  )
ggsave("imports_foodenergy.jpg", width = 18, height = 15, units = "cm")
