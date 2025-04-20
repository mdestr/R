# Une entreprise P est composée de deux établissements P1 et P2 . 
# Le tableau ci-dessous donne la répartition des salaires par catégorie socio-professionnelle. 
# Les salaires sont exprimés en milliers d'euros :

# Catégories	P1	P2
# Classe (x1 )	Effectif (n1 )	Classe (x2 )	Effectif (n2 )
# Ouvriers	[20,25[	60	[15,20[	5
# Employés	[30,35[	95	[25,60[	15
# Cadres	[80,100[	5	[70,80[	30

# Pour répondre aux diérentes questions, on considèrera que tous les individus d'une même classe 
# ont un salaire identique, égal au centre de la classe.

# 1. Définir les centres de classe et les effectifs
centres_global   <- c((20+25)/2,  (30+35)/2,  (80+100)/2,   # P1 : ouvriers, employés, cadres
                      (15+20)/2,  (25+60)/2,  (70+80)/2)    # P2 : ouvriers, employés, cadres

effectifs_global <- c(60, 95, 5,   # P1
                      5,  15, 30)  # P2

# 2. Calculer les poids (effectif / total)
total <- sum(effectifs_global)            
poids <- effectifs_global / total        

# Vérification visuelle
data.frame(
  centre = centres_global,
  effectif = effectifs_global,
  poids = round(poids, 6)
)

# 3. Moyenne pondérée
moyenne_globale <- sum(centres_global * poids)

# 4. Variance pondérée (populationnelle)
variance_globale <- sum(poids * (centres_global - moyenne_globale)^2)

# 5. Écart‑type pondéré
ecart_type_globale <- sqrt(variance_globale)

# 6. Affichage
cat("Moyenne globale pondérée :", round(moyenne_globale, 2), "k€\n")
cat("Variance globale pondérée:", round(variance_globale, 2), "\n")
cat("Écart‑type pondéré       :", round(ecart_type_globale, 2), "k€\n")

# -------------------------------
# moyennes et variances pondérées par établissement
# -------------------------------

# 1. Centres de classe et effectifs par établissement
centres_P1    <- c((20+25)/2, (30+35)/2, (80+100)/2)  # P1 : ouvriers, employés, cadres
effectifs_P1  <- c(60, 95, 5)

centres_P2    <- c((15+20)/2, (25+60)/2, (70+80)/2)   # P2 : ouvriers, employés, cadres
effectifs_P2  <- c(5, 15, 30)

# 2. Calcul des poids (effectif / total de l’établissement)
poids_P1 <- effectifs_P1 / sum(effectifs_P1)
poids_P2 <- effectifs_P2 / sum(effectifs_P2)

# 3. Moyennes pondérées
moyenne_P1 <- sum(centres_P1 * poids_P1)
moyenne_P2 <- sum(centres_P2 * poids_P2)

# 4. Variances pondérées (populationnelles)
variance_P1 <- sum(poids_P1 * (centres_P1 - moyenne_P1)^2)
variance_P2 <- sum(poids_P2 * (centres_P2 - moyenne_P2)^2)

# 5. Écarts‑types
sd_P1 <- sqrt(variance_P1)
sd_P2 <- sqrt(variance_P2)

# 6. Affichage des résultats
cat("=== Établissement P1 ===\n")
cat("  Moyenne pondérée :", round(moyenne_P1,   2), "k€\n")
cat("  Variance pondérée:", round(variance_P1,   2), "\n")
cat("  Écart‑type       :", round(sd_P1,         2), "k€\n\n")

cat("=== Établissement P2 ===\n")
cat("  Moyenne pondérée :", round(moyenne_P2,   2), "k€\n")
cat("  Variance pondérée:", round(variance_P2,   2), "\n")
cat("  Écart‑type       :", round(sd_P2,         2), "k€\n")


# -------------------------------
# Calcul de la variance inter‑établissements
# -------------------------------

# 1. Effectifs par établissement
n1 <- sum(effectifs_P1)  # total P1
n2 <- sum(effectifs_P2)  # total P2
N  <- n1 + n2            # total global

# 2. Moyennes déjà calculées (voir code précédent)
#    moyenne_P1, moyenne_P2, moyenne_globale

# 3. Calcul de la variance inter‑établissements
#    Variante 1 : formules brute avec effectifs
variance_inter <- ( n1 * (moyenne_P1 - moyenne_globale)^2 +
                      n2 * (moyenne_P2 - moyenne_globale)^2 ) / N

#    Variante 2 : même chose via poids
poids1 <- n1 / N
poids2 <- n2 / N
variance_inter2 <- poids1 * (moyenne_P1 - moyenne_globale)^2 +
  poids2 * (moyenne_P2 - moyenne_globale)^2

# 4. Affichage
cat("Variance inter‑établissements :", round(variance_inter2, 2), "\n")
# (variance_inter et variance_inter2 doivent être identiques)

# -------------------------------
# Calcul de la variance intra‑établissements
# -------------------------------

# 1. Totaux par établissement et global
n1 <- sum(effectifs_P1)   # total P1
n2 <- sum(effectifs_P2)   # total P2
N  <- n1 + n2             # total global

# 2. Variances intra‑établissements déjà calculées
#    variance_P1 et variance_P2 (populationnelles)

# 3. Variante brute avec effectifs
variance_intra <- (n1 * variance_P1 + n2 * variance_P2) / N

# 4. Variante via poids
poids1 <- n1 / N
poids2 <- n2 / N
variance_intra2 <- poids1 * variance_P1 + poids2 * variance_P2

# 5. Affichage
cat("Variance intra‑établissements :", round(variance_intra, 2), "\n")
# variance_intra et variance_intra2 sont identiques


