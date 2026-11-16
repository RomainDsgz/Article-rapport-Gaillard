# clean environment
rm(list = ls())

library(readxl)
library(ggplot2)
library(dplyr)
library(fmsb)
library(tidyr)
library(purrr)
library(scales)
library(devtools)


df <- read_xlsx("productivecapacities.xlsx") |> as.data.frame()

# Réinitialise les noms de lignes pour éviter les "1", "2", etc.
rownames(df) <- NULL

df
str(df)

# Supprimer colonne country
df_radar <- df[, -1]


# Ajoute les lignes max et min au début
df_radar <- rbind(
 rep(100, ncol(df_radar)),   # max
 rep(0, ncol(df_radar)),   # min
  df_radar)

# df_radar[1, ] <- 100   # ligne max
df_radar[2, ] <- 0     # ligne min

rownames(df_radar) <- c("max", "min", df$country)
rownames(df_radar)
head(df_radar)

any(is.na(df_radar[1:2, ]))
df_radar[is.na(df_radar)] <- 0

df_radar <- as.data.frame(df_radar)

rownames(df_radar)
colnames(df_radar)
cols_to_plot <- colnames(df_radar)
df_radar_max <- df_radar
df_radar
# Définir une palette de couleurs
colors_border <- c(
 # "#003366", # OECD
  "#E94F37", # Switzerland
  "#2E8B57", # Italy
  "#F4A300", # Germany
  "#7B1FA2", # France
  "darkblue", # Belgium
  "#FF69B4",  # Austria
  "#D32F2F",  # China – rouge profond
  "#1E88E5",  # United States – bleu clair
  "#00A86B",  # Viet Nam – vert jade
  "#FF8C00",  # Tonga – orange soutenu
  "#008B8B",  # Singapore – turquoise sombre
  "#9E9E9E"   # Sierra Leone – gris moyen (neutre mais visible)
)

colors_fill <- alpha(colors_border, 0.3)

pcol <- colors_border
plwd <- c(3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)  # Suisse (2e) plus épaisse

df_radar

png("radar_capacites.png", width = 15, height = 15, unit = "cm", res = 300)

radarchart(
  df_radar[c("max", "min", "Switzerland", "China", "Viet Nam", "Tonga", "Singapore", "Sierra Leone")],
  axistype = 1,
  pcol = pcol,
  pfcol = NA,
  plwd = plwd,
  cglcol = "grey",
  caxislabels = seq(0, 100, 20),
  cglty = 1,
  vlcex = 0.9,
  title = "Capacités productives, 2024"
)

legend(
  "topright",
  legend = c("Suisse", "Chine", "Viet Nam", "Tonga", "Singapour", "Sierra Leone"),
  col = pcol,
  lty = 1,
  lwd = plwd,
  pch = NA,
  bty = "n",
  seg.len = 3,
  cex = 0.9
)

# 👉 Ajoute la source sous le graphique
mtext(
  "Source : CNUCED, 2024",
  side = 1,        # position sous le graphique
  line = 3,        # distance depuis le bord
  adj = 1,         # alignement à gauche (0 = gauche, 1 = droite, 0.5 = centré)
  cex = 0.8,       # taille du texte
  col = "black"   # couleur du texte
)

dev.off()

radarchart(
  df_radar[c("max", "min", "Switzerland", "China", "Viet Nam", "Tonga", "Singapore", "Sierra Leone")],
  axistype = 1,
  pcol = pcol,
  pfcol = NA,
  plwd = plwd,
  cglcol = "grey",
  caxislabels = seq(0, 100, 20),
  cglty = 1,
  vlcex = 1,                # taille du texte (peut aussi l’agrandir un peu)
  paxispos = 0.8,           # ← réduit la longueur des axes à 80% → labels plus éloignés
  title = "Capacités productives, 2024"
)
