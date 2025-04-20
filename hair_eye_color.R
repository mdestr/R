# -------------------------------
# 1. Création du tableau de contingence
# -------------------------------

# Données : chaque ligne représente une couleur des yeux,
# chaque colonne une couleur de cheveux
donnees <- matrix(c(
  68, 119, 26, 7,     # yeux marron
  15, 54, 14, 10,     # yeux noisette
  5, 29, 14, 16,      # yeux vert
  20, 84, 17, 94      # yeux bleu
),
nrow = 4, byrow = TRUE)

# Nommer les lignes (couleurs des yeux)
rownames(donnees) <- c("marron", "noisette", "vert", "bleu")

# Nommer les colonnes (couleurs des cheveux)
colnames(donnees) <- c("brun", "chatain", "roux", "blond")

# Afficher le tableau
print("Tableau de contingence :")
donnees

# -------------------------------
# 2. Calcul des profils-lignes
# -------------------------------
# Chaque ligne est transformée en fréquence relative (somme de chaque ligne = 1)

profils_lignes <- prop.table(donnees, margin = 1)

print("Profils-lignes (distribution des cheveux pour chaque couleur d’yeux) :")
round(profils_lignes, 3)  # Arrondi à 3 décimales

# -------------------------------
# 3. Calcul des profils-colonnes
# -------------------------------
# Chaque colonne est transformée en fréquence relative (somme de chaque colonne = 1)

profils_colonnes <- prop.table(donnees, margin = 2)

print("Profils-colonnes (distribution des yeux pour chaque couleur de cheveux) :")
round(profils_colonnes, 3)  # Arrondi à 3 décimales

# -------------------------------
# 4. Calcul des effectifs attendus
# -------------------------------

# Calcul des totaux ligne, colonne et total général
totaux_lignes <- rowSums(donnees)
totaux_colonnes <- colSums(donnees)
total_global <- sum(donnees)

# Initialisation d’une matrice vide de même dimension
effectifs_attendus <- matrix(0, nrow = nrow(donnees), ncol = ncol(donnees))

# Calcul des effectifs attendus pour chaque cellule
for (i in 1:nrow(donnees)) {
  for (j in 1:ncol(donnees)) {
    effectifs_attendus[i, j] <- (totaux_lignes[i] * totaux_colonnes[j]) / total_global
  }
}

# Ajout des noms
rownames(effectifs_attendus) <- rownames(donnees)
colnames(effectifs_attendus) <- colnames(donnees)

# Affichage arrondi
print("Effectifs attendus sous l'hypothèse d'indépendance :")
round(effectifs_attendus, 2)

# -------------------------------
# 5. Calcul du chi-deux
# -------------------------------
chi2 <- sum((donnees - effectifs_attendus)^2 / effectifs_attendus)

# -------------------------------
# 4. Degrés de liberté
# -------------------------------
ddl <- (nrow(donnees) - 1) * (ncol(donnees) - 1)

# -------------------------------
# 5. Valeur critique au seuil alpha = 5%
# -------------------------------
valeur_critique <- qchisq(0.95, df = ddl)

# -------------------------------
# 6. Affichage des résultats
# -------------------------------
cat("Statistique chi2 :", round(chi2, 2), "\n")
cat("Degrés de liberté :", ddl, "\n")
cat("Valeur critique (α = 5%) :", round(valeur_critique, 2), "\n")

if (chi2 > valeur_critique) {
  cat("➡️ On rejette l'hypothèse d'indépendance au seuil de 5%.\n")
} else {
  cat("✅ On ne rejette pas l'hypothèse d'indépendance au seuil de 5%.\n")
}


# Test du chi2 (permet de récupérer directement la statistique)
chi2_test <- chisq.test(donnees)
chi2_val <- chi2_test$statistic

# Dimensions du tableau
r <- nrow(donnees)
k <- ncol(donnees)

# Calcul du C de Cramer
cramer_C <- sqrt(chi2_val / (total_global * (min(r, k) - 1)))

# Affichage
cat("C de Cramer :", round(cramer_C, 4), "\n")
