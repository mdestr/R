# L’analyse multivariée va permettre notamment de résumer les liaisons entre les variables et d’identifier 
# des groupes d’individus aux profils de performances similaires. 
# Les variables actives sont ici celles indiquant les résultats aux épreuves. 
# En effet, nous sommes intéressés par la mise en évidence des différents profils de performances, 
# qui sont uniquement déterminés par les résultats aux 10 épreuves. 
# Les variables Nombres de points et Classement se déduisent des résultats aux épreuves et n’apportent donc pas d’information supplémentaire. 
# Aussi, la variable Epreuve ne caractérise pas les performances de l’athlète. 
# Ainsi, on considèrera les variables Nombre de points, Classement et Epreuve comme des variables supplémentaires (illustratives). 
# Ici nous appliquons une ACP car les variables actives sont toutes quantitatives. 
# Les variables actives n’ayant pas les mêmes unités, il s’impose d’effectuer une ACP réduite.

# Définir le répertoire de travail comme celui du script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

decathlon<-read.table("./decathlon/decathlon.txt", 
                       stringsAsFactors = TRUE)

# install.packages("FactoMineR")
library(FactoMineR)

# on effectue l'ACP (réduite et avec 5 dimensions par défaut)
res.pca <- PCA(decathlon,
               quanti.sup = c(11: 12),
               quali.sup = c(13),
               graph = FALSE)

# export des résultats dans un fichier
write.infile(res.pca, file = "./resultats_ACP.csv")

#valeurs propres
res.pca$eig

barplot(res.pca$eig[,1], las = 2)
