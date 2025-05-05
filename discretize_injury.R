# Données d'entrée
blessures <- c("toe", "forearm", "hamstring", "shoulder", "hamstring", "ankle", 
               "knee", "ankle", "knee", "rib", "hamstring", "back", "illness", 
               "foot", "hand", "mononucleosis", "thumb", "ankle", "Achilles", 
               "toe", "knee", "finger", "calf", "Achilles", "thumb", "calf", 
               "ankle", "Achilles", "groin", "wrist", "knee", "knee", "knee", 
               "shoulder", "hamstring", "Achilles", "ineligible", "knee", "hip", 
               "hip", "leg", "ankle", "thumb", "hand", "shoulder", "knee", "knee", 
               "shoulder", "wrist", "knee", "back", "knee", "knee", "ankle", "foot", 
               "back", "elbow", "hamstring", "knee", "ribs", "hamstring", "heel", 
               "knee", "ankle", "knee", "illness", "shoulder", "hip", "knee", "knee", 
               "knee", "wrist", "ankle", "calf", "knee", "elbow", "knee", "ankle", 
               "wrist", "calf", "muscle", "calf", "shoulder", "shoulder", "ankle", 
               "knee", "concussion", "knee", "leg", "ankle", "foot", "shoulder", 
               "ankle", "finger", "knee", "shoulder", "concussion", "ankle", "knee", 
               "back", "knee")

# Définition des catégories
blessure_haut_du_corps <- c("shoulder", "back", "hand", "thumb", "finger", "wrist", 
                            "elbow", "rib", "ribs", "concussion", "mononucleosis", 
                            "illness", "ineligible", "muscle")

blessure_bas_du_corps <- c("toe", "forearm", "hamstring", "ankle", "knee", "foot", 
                           "Achilles", "calf", "groin", "hip", "leg", "heel")

# Création des variables binaires
resultat <- data.frame(
  blessure_originale = blessures,
  blessure_haut_du_corps = ifelse(blessures %in% blessure_haut_du_corps, 1, 0),
  blessure_bas_du_corps = ifelse(blessures %in% blessure_bas_du_corps, 1, 0)
)

# Vérification des catégories non classées
non_classees <- blessures[!(blessures %in% c(blessure_haut_du_corps, blessure_bas_du_corps))]
unique(non_classees)  # Affiche les valeurs non classées

# Affichage des résultats
head(resultat, 10)