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
