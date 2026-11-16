# clean environment
rm(list = ls())

library(readxl)
library(dplyr)
library(fmsb)
library(scales)
library(stringr)

# --- Lecture du fichier Excel ---
df <- read_xlsx("productivecapacities.xlsx") |> as.data.frame()
rownames(df) <- NULL

# --- Dictionnaire de traduction des colonnes ---
nom_fr <- c(
  "Overall index: Productive capacities index" = "Indice des capacités productives",
  "Natural capital"      = "Capital naturel",
  "Human capital"        = "Capital humain",
  "Energy"               = "Énergie",
  "Transport"            = "Transport",
  "Information and Communication Technology" = "Technologies d'information et de communication",
  "Institutional"        = "Institutions",
  "Private sector"       = "Secteur privé",
  "Structural Change"    = "Transformation structurelle"
)

# labels <- str_wrap("Indice des capacités productives", "Capital naturel", "Énergie", "Transport", "Technologies d'information et de communication", "Institutions", "Secteur privé", "Transformation structurelle")
# Applique la traduction si les colonnes existent dans le fichier
names(df) <- ifelse(names(df) %in% names(nom_fr),
                    nom_fr[names(df)],
                    names(df))

# --- Préparation des données pour le radar ---
df_radar <- df[, -1, drop = FALSE]

df_radar <- rbind(
  rep(100, ncol(df_radar)),
  rep(0, ncol(df_radar)),
  df_radar
)

rownames(df_radar) <- c("max", "min", trimws(df[[1]]))
df_radar[] <- lapply(df_radar, function(x) as.numeric(as.character(x)))
df_radar[is.na(df_radar)] <- 0

str(df_radar)
# --- Sélection des pays à afficher ---
rows_to_plot <- c(
  "max", "min",
  "Switzerland", "China", "United States", "Viet Nam", "Tonga", "Singapore", "Sierra Leone"
)

missing <- setdiff(rows_to_plot, rownames(df_radar))
if (length(missing) > 0) {
  message("⚠️ Noms absents : ", paste(missing, collapse = ", "))
  message("Vérifie les noms disponibles :")
  print(rownames(df_radar))
}
summary(df_radar)
# --- Palette de couleurs ---
pcol <- c(
  "#E94F37",  # Switzerland
  "#D32F2F",  # China
  "#1E88E5",  # United States
  "#00A86B",  # Viet Nam
  "#FF8C00",  # Tonga
  "#008B8B",  # Singapore
  "#9E9E9E"   # Sierra Leone
)
plwd <- c(3, rep(2, length(pcol) - 1))

# --- Création du radar chart ---
png("radar_PCI.png", width = 30, height = 20, units = "cm", res = 300) 

par(mar = c(7, 4, 6, 4))
par(xpd = TRUE)

radarchart(
  df_radar[rows_to_plot, ],
  axistype = 1,
  pcol = pcol,
  pfcol = NA,
  plwd = plwd,
  cglcol = "grey",
  caxislabels = seq(0, 100, 20),
  maxmin = TRUE,
  cglty = 1,
  vlcex = 0.8,           # ⬅️ taille des labels plus petite
  title = "Capacités productives, 2024",
  centerzero = FALSE      # ⬅️ évite le chevauchement entre axes
) 

# --- Légende en bas sur deux lignes ---
legend(
  "bottom",
  inset = c(0, -0.2),
  legend = c("Suisse", "Chine", "États-Unis", "Viet Nam", "Tonga", "Singapour", "Sierra Leone"),
  col = pcol,
  lty = 1,
  lwd = plwd,
  bty = "n",
  seg.len = 3,
  cex = 0.9,
  ncol = 3
) 

# --- Source sous la légende ---
mtext(
  "Source : CNUCED, 2024",
  side = 1,
  line = 5,
  adj = 1,
  cex = 0.8,
  col = "black"
)

dev.off()

