# Reconnaissance des saveurs sucré, acide et amer par un panel des consommateurs

#       Perçu sucré Perçu acide Perçu amer
# Sucré           9           1          0
# Acide           0           7          3
# Amer            0           4          6


# Créer la matrice
M <- matrix(c(9, 1, 0,
                                0, 7, 3,
                                0, 4, 6),
                              nrow = 3, byrow = TRUE,
                              dimnames = list(c("Sucré", "Acide", "Amer"),
                                              c("Perçu sucré", "Perçu acide", "Perçu amer")))

# Afficher le résultat
print(M)

# Fréquences relatives par rapport au total général
freq_totale <- M / sum(M)

# Fréquences lignes (profil ligne)
freq_lignes <- prop.table(M, margin = 1)

# Fréquences colonnes (profil colonne)
freq_colonnes <- prop.table(M, margin = 2)

# Matrice originale
print("Tableau de contingence initial:")
print(M)

# Fréquences totales
print("\nFréquences relatives (total):")
print(round(freq_totale, 3))

# Profils lignes
print("\nProfils lignes (fréquences conditionnelles par stimulus):")
print(round(freq_lignes, 3))

# Profils colonnes
print("\nProfils colonnes (fréquences conditionnelles par perception):")
print(round(freq_colonnes, 3))




# Profils lignes
profils_lignes <- prop.table(M, margin=1)
print(profils_lignes)


# Profil marginal (gi)
gi <- colSums(M) / sum(M)  # (0.3, 0.4, 0.3)
print(gi)

# Vérifier que gi = moyenne pondérée des profils
weights <- rowSums(M) / sum(M)  # Poids (10/30, 10/30, 10/30)
all.equal(gi, colSums(profils_lignes * weights))  # Renvoie TRUE



# Profils colonnes
profils_colonnes <- prop.table(M, margin=2)  # Chaque colonne divisée par son total

# Profil marginal colonne (Gⱼ)
GJ <- rowSums(M) / sum(M)  # (0.3, 0.4, 0.3)

# Vérification
print("Profil marginal colonne (Gⱼ):")
print(GJ)

library(FactoMineR)
res.CA <- CA(M, graph=FALSE)
res.CA$eig  # Affiche l'inertie totale et par axe
