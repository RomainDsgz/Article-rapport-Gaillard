library(readxl)
library(ggplot2)
library(ggpubr)

#######################################################
##          
##          
##          Section 1: 
##          Bar graph of the budget cuts
##          
##          
#######################################################

# Read and preprocess data
mesures <- read_excel("Data/gaillard_cut_data.xlsx")
mesures <- na.omit(mesures)

# Set row names and remove Number column
rownames(mesures) <- mesures$Number
mesures <- mesures[, -which(names(mesures) == "Number")]

# Rename columns
colnames(mesures) <- c('Measure (FR)', 
                       'Measure (DE)', 
                       'Measure (EN, deepl)',
                       'Category',                  
                       '2027',                  
                       '2028',
                       '2029')

# Calculate total
mesures$'Total from 2027 to 2029' <- mesures$`2027` + mesures$`2028` + mesures$`2029`

# Group by category and sum
measures_cat <- aggregate(`Total from 2027 to 2029` ~ Category, 
                          data = mesures, 
                          FUN = sum)

# Rename column
colnames(measures_cat)[2] <- 'Amount'

# Set row names
rownames(measures_cat) <- measures_cat$Category

# Sort by Amount for the plot
measures_cat <- measures_cat[order(measures_cat$Amount), ]

# Create plot with ggpubr
p <- ggbarplot(measures_cat, 
               x = "Category", 
               y = "Amount",
               fill = "steelblue",
               color = "steelblue",
               sort.val = "asc",          # Sort ascending
               rotate = TRUE,             # Horizontal bars
               xlab = "",
               ylab = "Budget cut (CHF mio)",
               title = "Total 2027 to 2029") +
  theme_minimal() +
  ylim(0, 3000) +
  labs(caption = "Data source: Federal Finance Dep. (2025); 'Programme d’allégement budgétaire 2027 - Aperçu des mesures'") +
  theme(plot.caption = element_text(face = "italic", hjust = 0))

# Display plot
print(p)

# Save plot
ggsave("cuts_bargraph_en.jpg", width = 18, height = 15, units = "cm", dpi = 300)

#######################################################
##          
##          
##          Section 2: 
##          Debt in GDP-Years and debt owners
##          
##          
#######################################################

# Read and process debt to GDP data
debt_to_gdp_ratios <- read.csv('Data/debt_OECD.csv')

# Create debt composition dataframe
debt_composition <- data.frame(
  `Investment funds` = c(26.2, 35.1, 37.6),
  `Insurance companies` = c(22.9, 24.5, 27.4),
  `Pension funds` = c(17.5, 8.8, 9.2),
  `Swiss bank positions` = c(7.2, 6.1, 6.2),
  `Others, Switzerland` = c(10.2, 6.7, 6.2),
  `Foreign investors` = c(16.0, 18.8, 13.4)
)

# Set row names
rownames(debt_composition) <- c("2010", "2023", "2024")

print(debt_composition)

# Pivot the data to wide format using base R
debt_wide <- reshape(debt_to_gdp_ratios, 
                     idvar = "TIME_PERIOD", 
                     timevar = "Reference.area", 
                     direction = "wide",
                     v.names = "OBS_VALUE")

# Set row names and clean up column names
rownames(debt_wide) <- debt_wide$TIME_PERIOD
debt_wide <- debt_wide[, -1]  # Remove TIME_PERIOD column
colnames(debt_wide) <- gsub("OBS_VALUE.", "", colnames(debt_wide))
# debt_to_gdp_ratios_wide <- debt_wide / 100

# Convert row names to proper dates
years <- as.numeric(rownames(debt_wide))
debt_wide$Year <- as.Date(paste0(years, "-01-01"))

# Define countries of interest
countries <- c('Switzerland', 'France', 'Germany', 'Italy', 'Austria', 'Belgium')

# Prepare data for line plot (convert to long format for ggplot)
line_data <- data.frame(Year = debt_wide$Year)
for(country in countries) {
  line_data[[country]] <- debt_wide[[country]]
}

# Convert to long format using base R
line_data_long <- reshape(line_data,
                          direction = "long",
                          varying = countries,
                          v.names = "Value",
                          timevar = "Country",
                          times = countries,
                          ids = 1:nrow(line_data))
# Define custom color palette
custom_colors <- c('blue', 'cyan3', 'goldenrod3', 'indianred3', 'chartreuse4', 'palevioletred4')

# Create line plot with custom colors and data source
p1 <- ggplot(line_data_long, aes(x = Year, y = Value, color = Country)) +
  geom_line(linewidth = 1) +
  labs(title = "Endettement public",
       y = "Dette en PIB-années",
       x = "Année",
       caption = "Source: OECD (2024)") +
  theme_pubr() +
  scale_color_manual(
    values = custom_colors,
    labels = c(
      'Switzerland'='Suisse', 
      'France'='France', 
      'Germany'='Allemagne', 
      'Italy'='Italie', 
      'Austria'='Autriche', 
      'Belgium'='Belgique'
    )
  ) 
  theme(plot.caption = element_text(size = 8, color = "gray40"))



print(p1)

# FIXED: Create stacked bar plot
# Convert the row names (years) to a column first
debt_composition$Year <- rownames(debt_composition)

# Get the category names (all columns except Year)
category_names <- names(debt_composition)[names(debt_composition) != "Year"]

# Reshape to long format using base R - FIXED VERSION
debt_comp_long <- reshape(debt_composition,
                          direction = "long",
                          varying = category_names,
                          v.names = "Value",
                          timevar = "Category",
                          times = category_names,
                          idvar = "Year")

# Reset row names to avoid issues
rownames(debt_comp_long) <- NULL

# Convert Year to factor to ensure proper ordering
debt_comp_long$Year <- factor(debt_comp_long$Year, levels = c("2010", "2023", "2024"))

# Create the bar plot - FIXED
p2 <- ggbarplot(debt_comp_long, 
                x = "Year", 
                y = "Value", 
                fill = "Category",
                color = "Category",
                palette = custom_colors,
                title = "Porteurs des titres",
                xlab = "Année",
                ylab = "Part (%)") +
  theme_pubr() +
  scale_fill_manual(values = custom_colors,
                    labels = c(
                      "Foreign.investors" = "Investisseurs étrangers",
                      "Investment.funds" = "Fonds d'investissements (CH)",
                      "Pension.funds" = "Fonds de pension (CH)",
                      "Insurance.companies" = "Compagnies d'assurance (CH)",
                      "Others..Switzerland" = "Autres (CH)",
                      "Swiss.bank.positions" = "Banques (CH)"
                    )) +
  scale_color_manual(values = custom_colors,
                     labels = c(
                       "Foreign.investors" = "Investisseurs étrangers",
                       "Investment.funds" = "Fonds d'investissements (CH)",
                       "Pension.funds" = "Fonds de pension (CH)",
                       "Insurance.companies" = "Compagnies d'assurance (CH)",
                       "Others..Switzerland" = "Autres (CH)",
                       "Swiss.bank.positions" = "Banques (CH)"
                     )) +
  labs(caption = "Data source: Swiss National Bank (2024)") +
  theme(plot.caption = element_text(size = 8, color = "gray40")) 

print(p2)


# Arrange both plots side by side
combined_plot <- ggarrange(p1, p2, 
                           ncol = 2, 
                           nrow = 1,
                           common.legend = FALSE,
                           legend = "right")

# Display the plot
print(combined_plot)

# Save the plot
ggsave('debt_and_composition.jpg', plot = combined_plot, width = 16, height = 7, dpi = 300)

