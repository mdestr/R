# 1- classification ascendante hiérarchique
# 2- classification par agrégation autour des centres mobiles (k- means)
# 3- méthode mixte avec les deux approches combinées
# 
# La classification sera effectuée sur les composantes principales de l’ACP (approche Tandem)
# => portée plus générale que la classification directement sur les données car :
#   - elle débruiter les données avant d’effectuer la classification et
#   - elle est équivalente à la classification appliquée sur les données brutes dans le cas particulier où 
#   l’intégralité des composantes sont utilisées
#   - approche classique pour effectuer une classification sur données qualitatives 
#   (dans ce cas utiliser les composantes de l’Analyse des Correspondances Multiples)

# Définir le répertoire de travail comme celui du script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

decathlon<-read.table("./decathlon/decathlon.txt", 
                      stringsAsFactors = TRUE)

