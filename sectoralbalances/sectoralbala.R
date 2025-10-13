# Load librariries ----
library(readxl)
library(ggplot2)
library(tidyr)

# Load data ----
df <- read_xlsx("fiscalspace.xlsx")
df

# Prepare data for three and five sectors 
threesec <- c("year", "Private Sector", "Government", "Rest of the World")
threesector <- df[threesec]
#threesector <- threesector[2:4]*100
threesector <- threesector %>%
  pivot_longer(2:4, names_to = "Sectors", values_to = "Balance",
               )
fivesec <- c("year", "Households", "Government", "Rest of the World", "Financial Corporations", "Non-Financial Corporations")
fivesector <- df[fivesec]
fivesector <- fivesector %>% 
  pivot_longer(2:6, names_to = "Sectors", values_to = "Balance")

# Prepare graphs ----
# Graph for three sectors ----- 
# Colors
colors3 <- c("indianred3", "chartreuse4", "turquoise3" )
plot_3 <- ggplot(threesector, aes(x=year, y= Balance, fill = Sectors)) +
  geom_bar(stat = "identity", position = "stack") +  
  geom_vline(xintercept = 2003, linetype="dotted", color="blue", size=1) + 
  labs(title = "Sectoral balances (Three sectors)",
       subtitle = "Switzerland, 1995-2020",
       x = "Year",
       y = "Balance in percentage of GDP",
       fill = "Sectors",
       caption = "Source:Eurostats, 2021") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  theme_minimal() + 
  scale_fill_manual(values = colors3) +
  theme(legend.position = "bottom")
plot_3
#Graph for five sectors -----
# Change colors 
c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF")
sector_colors <- c("plum4", "indianred3", "chartreuse4", "mistyrose3", "turquoise3")

ggplot(fivesector, aes(x=year, y= Balance, fill = Sectors)) +
  geom_bar(stat = "identity", position = "stack") +  
  geom_vline(xintercept = 2003, linetype="dotted", color="blue", size = 1) + 
  labs(title = "Sectoral balances(Five sectors)",
       subtitle = "Switzerland, 1995-2020)",
       x = "Year",
       y = "Balance in percentage of GDP",
       fill = "Sectors",
       caption = "Source: Eurostats, 2021") + 
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_fill_discrete() +
  scale_fill_manual(values = sector_colors) +
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(nrow=2))
      #  legend.key.size = unit(0.6, "lines"))
