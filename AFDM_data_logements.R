# Mise en oeuvre AFDM (Analyse Factorielle des données mixtes)

# Objectifs : 
#   - identifier les ressemblances et différences entre logements du point de vue des habitudes des ménages
#   - dresser un bilan des liaisons entre les variables décrivant ces habitudes
#   - associer à des groupes de logements qui se ressemblent ou s’opposent 
#     du point de vue des habitudes des ménages un profil d’habitudes particulier
#   - identifier le lien entre les habitudes des ménages et la présence de polluant.


# Définir le répertoire de travail comme celui du script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


data_hab_type <- read.csv("data_hab_type_TD.csv",
                          header = TRUE,
                          stringsAsFactors = TRUE)

# 516 logements décrits par 57 variables (71 en comptant les duplications des variables de pollution)
dim(data_hab_type)

# résumé univarié des données
head(data_hab_type)
summary(data_hab_type)
# pas de n/a, pas de variable constante, import des données correct (types bien spécifiés)

# pour faciliter l'interprétation, modification des noms des modalités
levels(data_hab_type$VoitureDansGarage) <- paste0("garage_", levels(data_hab_type$VoitureDansGarage))
levels(data_hab_type$NettASecVetements) <- paste0("NetSecVet_", levels(data_hab_type$NettASecVetements))

# on réordonne les modalités afin pour avoir un ordre croissant
levels(data_hab_type$SortirOrduresExterieur) <- paste0("OrdureSortie_",c("3","2","1"))

