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

# On perd une quantité importante d’inertie intra-classe en passant de 1 à 2 classes
# => considérer 2 classes plutôt qu’une seule
# On perd à nouveau une quantité importante en passant de 2 à 3 classes, etc. 
# La perte d’inertie diminue de façon moins importante en passant de 6 à 7 classes => retenir 6 classes. 
# 
# Indice de qualité interne Silhouette pour choisir le nombre de classes avec le package NbClust.
# install.packages("NbClust")
library(NbClust)
res.nbclust <- NbClust(
  data = res.pca$ind$coord,
  method = "kmeans", #algorithme de classification
  distance = "euclidean",#distance utilisée
  min.nc = 2,#nombre minimal de classes
  max.nc = 15, #nombre maximal
  index = "silhouette"# choix de l'indice de qualité
)

plot(2:15,
     res.nbclust$All.index,
     ylab =  "Indice Silhouette",
     xlab = "Nombre de classes")

# L’indice Silhouette suggèrerait de retenir 15 classes (partition pour laquelle l’indice est le plus élevé)
# Trop élevé relativement au nombre d’observations : un choix de 5 ou 9 classes pourrait être envisagé selon cet indice.
# 
# Voir aussi le nuage des individus pour choisir le nombre de classes : si on observe 3 groupes d’individus bien distincts, on choisit 3 classes. 
# => complémentarité des méthodes factorielles et des méthodes de classification non-supervisée.
# 
# Pour étudier la stabilité de la partition on analyse aussi les formes fortes en considérant 3 initialisations différentes de l’algorithme des k-means.
# On utilise 4 classes comme suggéré par le dendogramme.
# on applique l'algorithme 3 fois et on récupère la partition
classe  <- sapply(1:3,
                  FUN = function(xx,res.pca){
                    kmeans(res.pca$ind$coord,nstart = 1,centers = 4)$cluster
                  }, res.pca = res.pca)

#on construit l'hypercube de contingence
tableintersect <- table(as.data.frame(classe))

#on le transforme sous forme d'un tableau à deux dimensions (combinaisons de classes en ligne)
formefortes <- as.data.frame(tableintersect)

#on affiche les combinaisons observées au moins une fois
formefortes[formefortes$Freq>0, ]

# L’algo semble sensible à l’initialisation dans la mesure où les individus n’appartiennent pas souvent à la même classe.
# Les fréquences des combinaisons ne sont pas très élevées.
# Si l’algorithme n’était pas sensible on n’observerait que 3 combinaisons avec un effectif non nul. 
# La partition en 4 classes doit donc être nuancée car elle ne correspond pas à une structure très nette dans les données.
# 
# Les individus suivants constituent des groupements stables traduisant une structure forte dans les données : 
# on identifie les individus correspondant aux formes les plus fortes
forme112 <- which(apply(classe, 1, FUN = identical , y = c(1L,1L,2L)))
forme243 <- which(apply(classe, 1, FUN = identical,  y = c(2L,4L,3L)))

# la fonction identical teste l'égalité entre les différentes lignes de l'objet classe et un vecteur y que l'on a renseigné. 
# Cette fonction est sensible au type des données. 
# Comme les lignes de l'objet classe contiennent des valeurs entières, il faut que le vecteur y soit aussi composé d'entiers. 
# On n'écrit par exemple donc pas y = c(2,4,3) mais c(2L,4L,3L). 
# Pour constater la différence, on pourra exécuter les commandes typeof(c(2L,4L,3L)) et typeof(c(2,4,3)).
forme112
forme243

# L’analyse des formes fortes sert plus généralement à comparer différentes partitions d’un même ensemble d’individus. 
# Ici on l’utilise pour comparer des partitions issues d’initialisation différentes de l’algorithme des kmeans.
# Ceci permettant d’évaluer la sensibilité de l’algorithme à son initialisation. 
# On pourrait aussi s’en servir pour comparer les partitions obtenues par CAH et par kmeans et ainsi :
#   - illustrer la sensibilité de la partition à la technique de classification utilisée,
#   - illustrer la sensibilité à différents critères d’agrégation en CAH


# Combiner les approches en effectuant :
#   - une classification par k-means pour un grand nombre de clusters (permet de limiter le nombre d’éléments à agréger pour la CAH afin de gagner en temps de calcul)
#   - une CAH en partant de la partition précédemment obtenue (permet un choix simple du nombre de classes)
#   - une consolidation des classes obtenues par k-means (permet d’optimiser la partition du point de vue du critère de l’inertie)
# 
# Démarche ici purement illustrative parce que le nombre d’observations est modeste, la première étape n’est donc pas justifiée.
# Mais en général, c’est de cette façon qu’il faut procéder.
# k-means (30 classes) à partir des 41 individus
classe <- kmeans(res.pca$ind$coord,
                 centers = 30,
                 nstart = 100)
# CAH des 30 classes
centre.gravite  <-  classe$centers# coordonnées des centres de gravités
D <- dist(centre.gravite)# distance euclidienne entre centres
res.hclust  <-  hclust(D,
                       members = classe$size,
                       method = "ward.D2") # CAH par méthode de Ward en partant de la matrice de distances 
                                           # entre les 30 centres de gravité et en tenant compte de l'effectif 
                                           # des classes obtenues précédemment 
barplot(sort(res.hclust$height, decreasing = TRUE),
        names.arg = 1:29,
        xlab = "index",
        ylab = "hauteur de fusion")#diagramme des gains d'inertie, on choisit 5 classes

partition <-  cutree(res.hclust, k = 5) #élagage de l'arbre
partition.cah <-  partition[classe$cluster]#on en déduit la partition des 41 individus de départ

# Consolidation
# Détermination des centres de gravité des classes
centres.gravite.2 <- by(res.pca$ind$coord,
                        INDICES = partition.cah,
                        FUN = colMeans) 

centres.gravite.2 # donne une liste, mais la fonction kmeans nécessite que les centres de gravité soit donnés 
                  # sous la forme d'un objet de type "matrix"

centres.gravite.2<-do.call(rbind,
                           centres.gravite.2)#donne un objet de type "matrix"
classe.final<- kmeans(res.pca$ind$coord,
                      centers = centres.gravite.2)
#partition obtenue 
classe.final$cluster

# Pour interpréter les classes on peut :
#   - analyser les positions des classes sur les plans d’ACP
#   - utiliser les variables du jeu de données
#   - étudier les centres de gravité des classes
#   - étudier les individus qui composent la classe, en particulier les parangons et les individus spécifiques
#creation d'une variable "classe" dans le jeu de données
decathlon$classe <-  as.factor(classe.final$cluster)
summary(decathlon)

library(FactoMineR)
# Description à partir des plans d’ACP
res.pca<-PCA(decathlon,
             quanti.sup = c(11: 12),
             quali.sup = c(13,14),
             graph = FALSE)

plot.PCA(res.pca,
         choix = "ind",
         habillage = "classe") # affichage du plan des individus en coloriant selon les modalités de la variable classe.

plot(res.pca,
     choix = "ind",
     habillage = "classe",
     invisible = "ind" # pour voir uniquement les centres de gravité
)

# L’interprétation préalable de l’axe 1 permet déjà d’opposer les classes 1 et 3.
# La première est constituée d’athlètes globalement peu performants, 
# tandis que la seconde est constituée d’individus globalement très performants. 
# La classe 5 est également une classe d’individus globalement peu performants, 
# mais de par sa position selon l’axe 2, elle devrait se différencier de la classe 3 
# par une grande proportion d’athlètes performants au lancer du disque et du poids.

#description à l'aide de la fonction catdes
res.catdes <- catdes(decathlon, num.var = 14)

round(res.catdes$quanti.var, digits = 2) # pour arrondir avec 2 décimales les sorties correspondant 
                                         # à la liaison entre les variables quantitatives et la partition
sapply(res.catdes$quanti, FUN = round, digits = 2) # pour arrondir avec 2 décimales les sorties portant sur chaque classe

# Finalement, la description par les variable indique que la classe 4 est surtout caractérisée 
# par des athlètes très mauvais au 1500m et au 400m et non par rapport 
# à de bonnes performances au lancer de disque et poids.

# Description à partir des individus
# Les centres de gravité de chaque classe correspondent à l’individu moyen de chacune des classes. 
# On retrouvait déjà cette information lors de la description par les variables (colonne “Mean in category”).
by(decathlon[,1:12],
   INDICES = decathlon$classe,
   colMeans)

# L’objet classe.final contient une matrice indiquant les coordonnées des centres de gravité des classes.
# Comme la classification a été effectuée sur les composantes principales, il est beaucoup plus difficile 
# d’interpréter les coordonnées de ces centres de gravité.
classe.final$centers


# Les parangons sont les individus les plus proches du centre de gravité 
# de la classe au sens de la métrique choisie, ici M1/s2.

# on crée une fonction qui prend en entrée un tableau (xx) et renvoie la distance euclidienne 
# (usuelle) de chaque individu au centre de gravité.

myparangon<-function(xx){
  centre.gravite <- colMeans(xx)
  yy <- rbind("centre" = centre.gravite,xx)#on ajoute une ligne pour le centre de gravité
  D <- dist(yy)#on calcul les distances entre les individus
  d <- as.matrix(D)[-1,1]#on extrait les distances au centre de gravité
  d.order <- sort(d)# on trie les valeurs par ordre croissant
  return(d.order)#on renvoie les résultats
}

#parangons
by(scale(decathlon[,1:10]),
   INDICES = decathlon$classe,
   FUN = myparangon)

# Les individus spécifiques sont ceux qui sont les plus distants des autres classes
# On crée une fonction qui prend en entrée un tableau (xx) et des centres de gravités (centres) 
# et renvoie la distance euclidienne (usuelle) minimum entre les individus de xx 
# et les centres de gravité des classes (auxquelles ces individus n'appartiennent pas.

myspecifique<-function(xx,centres){
  yy <- rbind(centres,xx)#on ajoute les centres des classes au tableau des individus
  D <- as.matrix(dist(yy))[-c(1:nrow(centres)), c(1:nrow(centres))]#on calcul les distance entre les indivudus et les centres des classes
  centre.suppr<-unique(apply(D, 1, which.min))#on identifie le centre correspondant à la classe des individus
  D <- D[,-centre.suppr]#on supprime ce centre de la matrice des distances
  res <- sort(apply(D,1,min), decreasing=TRUE)#on trie les individus selon la distances au centre qui leur est le plus proche
  return(res)#on renvoie les résultats
}

# On applique la fonction précédente sur chaque classe en partant des données centrées réduites
centres.cr <- do.call(rbind, by(scale(decathlon[,1:10]),
                             INDICES = decathlon$classe,
                             FUN = colMeans))#on calcule les centres de gravite de chaque classes sur les données centrées-réduites
by(scale(decathlon[,1:10]),
   INDICES = decathlon$classe,
   FUN = myspecifique,
   centres = centres.cr)

# Par exemple, Casarsa est l’individu dont la distance au 
# centre de gravité le plus proche est la plus élevée pour la classe 4. 
# Cet individu est le plus spécifique de la classe 4. 
# D’ailleurs, sur le plan des individus, il semblait en effet assez éloigné 
# des centres de gravité des autres classes mais attention ce n’est pas toujours le cas 
# car la distance est ici calculée dans l’espace des individus tout entier, 
# alors que le plan des individus est simplement un sous-espace en dimension 2 
# qui “déforme” un peu les proximités entre individus). 
# Il pourra donc être utile d’aller regarder de près le profil Casarsa 
# pour interpréter la classe 4 et la différencier des autres classes.
# 
# 
# => l’interprétation d’une partition des individus est essentielle
# => tous les outils permettant d'y parvenir sont complémentaires
# => l’interprétation à partir des variables reste probablement la plus riche d’information
# => la description par les plans d’ACP offre un outil précieux de visualisation 
# pour mettre en évidence les oppositions entre les classes.
# 
# Concernant les nombres différents de classes selon les méthodes employées pour un même jeu de données : 
#    - Il n’y a pas de nombre “vrai”, tous les nombres proposés sont des choix possibles et justifiés. 
#    - C’est au statisticien de choisir. 
#    - En général, il faudra envisager plusieurs nombres de classes, pour au final n’en présenter qu’un seul. 
#    - Ce choix se fera généralement une fois les différentes partitions interprétées. 
#    - On pourra aussi s’appuyer sur des critères de validation internes.
