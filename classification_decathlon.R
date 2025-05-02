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

# Métrique M1/s2 utilisée pour définir la notion de ressemblance entre individus. 
# => choix de la réduction des données car les variables quanti ont des unités différentes.
# 
# En pratique, 2 façons de faire : 
#   - soit réduire les données puis effectuer la classification à l’aide d’une distance ne tenant pas compte des variances des variables
#   - soit utiliser directement une métrique tenant compte de cette variance sur les données non réduites. 
# 
# => choix seconde solution car cela permettra : 
#   - une description des classes à partir des variables dans leurs unités d’origine 
#   - d’intégrer facilement les variables qualitatives dans l’analyse

# On effectue l’ACP en calculant toutes les composantes
library(FactoMineR)

res.pca <- PCA(decathlon,
               quanti.sup = c(11: 12),
               quali.sup = c(13),
               graph = FALSE,
               scale.unit = TRUE,
               ncp = Inf)

# Spécifier ncp=Inf sinon seules les 5 premières composantes seront déterminées.
# A ce stade, on ne sait pas encore quel est le nombre de composantes à utiliser pour la classification. 
# Par défaut l'ACP est réduite (scale.unit=TRUE) => pour utiliser une distance euclidienne classique, spécifier scale.unit=FALSE

# Déterminer le nombre de composantes utilisées pour la classification. 
# Le critère utilisé est généralement différent de celui utilisé en ACP 
# car l’ACP n’est pas ici utilisée en tant que telle mais constitue simplement 
# une méthode de pré-traitement des données pour appliquer une méthode de classification. 
# Ici, on cherchera à conserver un maximum d’information pertinente, tandis qu’en ACP 
# le nombre de composantes doit être limité pour que l’interprétation ne soit pas trop laborieuse. 
# 
# Plusieurs choix :
#   - utiliser le nombre minimal de composantes pour atteindre ~90% d’inertie
#   - utiliser l’intégralité des composantes pour ne perdre aucune information
#   - examiner le diagramme des valeurs propres pour trouver un bon compromis entre information liée à la structure des données 
#     et information parasite

#valeurs propres
res.pca$eig

# => nécessaire d’utiliser 7 composantes pour utiliser 90% de l’information, 10 pour ne perdre aucune information.
barplot(res.pca$eig[,1], las = 2)

# Le diagramme des valeurs propres suggérerait d’utiliser 8 composantes (saut entre les valeurs propres 8 et 9). 
# On retient 8 composantes afin de conserver une grande quantité d’inertie de projection. 
# On relance donc l’ACP avec 8 composantes et on utilise par la suite la sortie obtenue pour effectuer la classification.
res.pca <- PCA(decathlon,
               quanti.sup = c(11: 12),
               quali.sup = c(13),
               graph = FALSE,
               scale.unit = TRUE,
               ncp = 8)


# CAH selon la méthode de Ward, classique pour des distances euclidiennes 
# car elle consiste à optimiser un critère d’inertie intra-classe ou inter-classes, 
# définissant une “bonne classification”.
library(cluster)
res.agnes <- agnes(res.pca$ind$coord, method = "ward")
plot(res.agnes, main = "Dendogramme", xlab = "Individus", which.plots = 2)

# => découpage en 4 classes
# Confirmer avec le diagramme des gains d’inertie.
barplot(sort(res.agnes$height, decreasing = TRUE),
        las = 2,
        names.arg = 1:40,
        xlab = "index",
        ylab = "hauteur de fusion") # les hauteurs de fusions (contenues dans l'objet res.agnes$height) ne sont pas rangées dans l'ordre, 
                                    # on les trie à l'aide de la fonction sort par ordre decroissant (decreasing=TRUE) 
                                    # avant de les représenter à l'aide de la fonction barplot 

# => coude net entre les 3ème et 4ème barres, indiquant qu’il est difficile de passer d’une partition de 4 à 3 classes. 


# classification par k-means avec 4 centres et 100 initialisations différentes
classe <- kmeans(res.pca$ind$coord,
                 centers = 4,
                 nstart = 100)

# Choix du nombre de classes par rapport aux résultats fournis par la CAH
# Autre possibilité : considérer plusieurs nombres de classes et rechercher 
# un coude dans le diagramme des inerties intra-classe. 
# Ici on inspecte un nombre de classes variant entre 1 et 15.
inertie.intra<-sapply(1:15,
                      FUN=function(centers){
                        kmeans(res.pca$ind$coord,
                               centers = centers,
                               nstart = 100)$tot.withinss
                      }
)
barplot(inertie.intra,
        names.arg = 1:15,
        ylab = "inertie intra-classe",
        xlab = "nombre de classes")
