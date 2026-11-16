library(readxl); library(dplyr); library(fmsb); library(stringr); library(scales)

# --- Lecture (ton code) ---
df <- read_xlsx("productivecapacities.xlsx") |> as.data.frame()
rownames(df) <- NULL

# --- Traduction (ton dictionnaire) ---
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

names(df) <- ifelse(names(df) %in% names(nom_fr),
                    nom_fr[names(df)],
                    names(df))

# --- Préparer df_radar (exclure la 1ère colonne si c'est le nom des pays) ---
# Ajuste si ta structure est différente
df_radar <- df[, -1, drop = FALSE]   # on enlève la colonne des pays
df_radar[] <- lapply(df_radar, as.numeric)

# Ajoute les bornes max/min
df_radar <- rbind(rep(100, ncol(df_radar)),
                  rep(0, ncol(df_radar)),
                  df_radar)

# ✅ Maintenant définis les rownames proprement :
rownames(df_radar) <- c("max", "min", trimws(df[[1]]))

# Vérifie :
head(rownames(df_radar))

# --- Convertir proprement en numérique ---
df_radar[] <- lapply(df_radar, function(x) {
  # enlever espaces, virgules en milliers, etc.
  x <- gsub(",", ".", as.character(x))
  x <- trimws(x)
  as.numeric(x)
})

# debug: vérifier types et NA
print("STRUCTURE df_radar :")
print(str(df_radar))
print("Sommaries (par colonne) :")
print(sapply(df_radar, summary))

# --- Si tes données ne sont pas sur l'échelle 0-100, tu peux normaliser automatiquement
# Détection simple : si max absolu <=1 on suppose 0-1 ; si <=10 on suppose 0-10 ; sinon 0-100.
maxval <- max(df_radar, na.rm = TRUE)
if (is.finite(maxval) && maxval <= 1) {
  df_radar <- df_radar * 100
  message("⚙️ Données multipliées par 100 (échelle 0-1 détectée).")
} else if (is.finite(maxval) && maxval <= 10) {
  df_radar <- (df_radar / 10) * 100
  message("⚙️ Données re-scalées de 0-10 → 0-100.")
} else {
  message("⚙️ Données supposées déjà sur une échelle proche de 0-100 (max detecté = ", maxval, ").")
}

# --- Mettre explicitement les lignes max/min ---
max_row <- rep(100, ncol(df_radar))
min_row <- rep(0,   ncol(df_radar))

df_radar <- rbind(max = max_row, min = min_row, df_radar)

# Vérifie que max/min sont bien là
print("Rownames après ajout :")
print(rownames(df_radar)[1:6])

# --- Choix des pays/lignes à tracer : inclure 'max' et 'min' impérativement ---
rows_to_plot <- c("max", "min",
                  "Switzerland", "China", "United States", "Viet Nam",
                  "Tonga", "Singapore", "Sierra Leone")

missing <- setdiff(rows_to_plot, rownames(df_radar))
if (length(missing) > 0) {
  warning("⚠️ Noms absents : ", paste(missing, collapse = ", "),
          "\nVérifie les noms exacts (casse / espaces). Rownames disponibles :")
  print(rownames(df_radar))
  # on arrête ici si manque max/min
  stop("Arrêt : certaines lignes demandées sont absentes.")
}

rownames(df_radar)
# --- Tracer en forçant seg = 5 (0-20-40-60-80-100) et caxislabels adaptés ---
png("radar_PCI_forced_0_100.png", width = 35, height = 25, units = "cm", res = 300)
par(mar = c(7, 4, 6, 4)); par(xpd = TRUE)

radarchart(
  df_radar[rows_to_plot, , drop = FALSE],
  axistype = 1,
  seg = 5,                          # 5 segments => 6 graduations 0..100
  caxislabels = seq(0, 100, 20),    # graduations explicites
  pcol = c("#E94F37", "#D32F2F", "#1E88E5", "#00A86B", "#FF8C00", "#008B8B", "#9E9E9E"),
  pfcol = NA,
  plwd = c(3, rep(2,6)),
  cglcol = "grey",
  cglty = 1,
  cglwd = 0.8,
  vlcex = 0.8,
  title = "Capacités productives, 2024",
  centerzero = FALSE
)

legend("bottom", inset = c(0, -0.2),
       legend = c("Suisse", "Chine", "États-Unis", "Viet Nam", "Tonga", "Singapour", "Sierra Leone"),
       col = c("#E94F37", "#D32F2F", "#1E88E5", "#00A86B", "#FF8C00", "#008B8B", "#9E9E9E"),
       lty = 1, lwd = c(3, rep(2,6)), bty = "n", seg.len = 3, cex = 0.9, ncol = 3)

mtext("Source : CNUCED, 2024", side = 1, line = 5, adj = 1, cex = 0.8)
dev.off()

