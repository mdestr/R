library(FactoMineR)

# Définir le répertoire de travail comme celui du script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# chocobars<-read.table("./openfoodfacts_acp_reduit.txt", 
#                       stringsAsFactors = TRUE)

# 1. Charger les données avec les bons paramètres
chocobars <- read.table("./openfoodfacts_acp.txt", 
                        header = TRUE,    # La première ligne contient les noms de colonnes
                        sep = "",         # Séparateur = espace
                        dec = ".",        # Point comme séparateur décimal
                        stringsAsFactors = FALSE)

# on effectue l'ACP (réduite et avec 5 dimensions par défaut)
# res.pca <- PCA(chocobars,
#                graph = FALSE)

res.pca <- PCA(chocobars,
               quanti.sup = c(13, 14),  # Colonnes 13 et 14 comme variables supplémentaires
               quali.sup = 1,           # Colonne 1 (noms des produits) comme variable qualitative
               graph = FALSE)

# export des résultats dans un fichier
write.infile(res.pca, file = "./resultats_ACP.csv")

#valeurs propres
res.pca$eig

barplot(res.pca$eig[,1], las = 2)


# On analyse d’abord le plan principal (appelé aussi plan 1-2).
#graphes des individus
plot.PCA(res.pca, choix = "ind")

#graphes des variables
plot.PCA(res.pca, choix = "var")

round(res.pca$ind$cos, 2)

res.pca <- PCA(chocobars,
               scale.unit = TRUE,
               quanti.sup = c(13:14),
               quali.sup = c(1),
               graph = FALSE, ncp = Inf)


#graphes des individus
plot.PCA(res.pca, axes = c(3, 4), choix = "ind")

#graphes des variables
plot.PCA(res.pca, axes=c(3, 4), choix = "var")
