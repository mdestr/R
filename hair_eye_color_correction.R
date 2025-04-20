# On s’intéresse à la liaison entre deux variables qualitatives X (couleur des yeux) et Y (couleurs des cheveux)
# à k=4 et k′=4 modalités respectivement.

don <- matrix(c(68,15,5,20,
                119,54,29,84,
                26,14,14,17
                ,7,10,16,94),4,4)
rownames(don) <- c("marron","noisette","vert","bleu")
colnames(don) <- c("brune","chatain","roux","blond")

tablecont <- addmargins(don,FUN=sum,quiet=TRUE)
tablecont

# tableau des profils-lignes
addmargins(prop.table(don,1), FUN=sum, quiet=TRUE, margin=2)

# On remarque que la distribution des couleurs des cheveux est assez différente selon la couleur des yeux.
# A titre d’exemple, la part des blondes est bien plus grande pour les femmes aux yeux bleus 
# qu’aux yeux marrons

# On peut représenter la table de contingence par un mosaic plot
library(vcd)
mosaic(don)

# Tableau des profils-colonnes
addmargins(prop.table(don,2), FUN = sum, quiet = TRUE, margin = 1)

# Réciproquement, on remarque que la distribution des couleurs des yeux est assez différente 
# selon la couleur des cheveux. On note par exemple que parmi les femmes aux cheveux brun, 
# 5% ont les yeux verts, tandis que cette proportion atteint 20% chez les personnes aux cheveux roux.

mosaic(t(don))

# Les valeurs attendues s’obtiennent en faisant le produit des marges ligne et colonne (multipliée par n=592).
# marge ligne
colSums(don)/sum(don)
# marge colonne
rowSums(don)/sum(don)

res.ch <- chisq.test(don)
res.ch$expected


# A partir des effectifs observés et attendus sous l’hypothèse d’indépendance, 
# on peut calculer la statistique du χ2
# Une valeur élevée d’un des termes (nqq′−nq.n.q′n)2nq.n.q′n au sein de la somme 
# impliquerait une grande valeur pour la statistique et donc une liaison forte. 
# Il est donc intéressant d’analyser chacun de ces termes 
# pour avoir une première idée de la liaison entre les variables
res.ch$residuals^2

# => Si liaison il y a, alors celle-ci semblerait liée à l’association 
# entre les modalités bleu/blond, marron/blond et marron/brune essentiellement.
# On calcule la statistique du χ2
sum(res.ch$residuals^2)
res.ch$statistic

# L’interprétation de cette valeur n’est pas aisée car non bornée par 1. 
# On sait néanmoins qu’une valeur de 0 traduit une absence totale de lien. 
# Pour aller plus loin, on peut s’appuyer sur des arguments statistiques : 
# on connaît la distribution de ce critère sous l’hypothèse d’indépendance (loi du chi-deux). 
# Une valeur au delà du quantile à 95% de la loi du chu-deux traduit une valeur significativement non nulle. 
# Une autre possibilité est de normaliser ce critère pour qu’il soit borné par 1. 
# Pour cela on peut utiliser le C de Cramer.
sqrt((res.ch$statistic/592)/3)

# Sous l’hypothèse d’indépendance, la statistique du χ2 est proche de 0. 
# De grandes valeurs de cette statistique amèneront à rejeter cette hypothèse. 
# Sous l’hypothèse d’indépendance, la statistique suit une loi du chi-deux 
# à (k−1)×(k′−1) degrés de liberté dont voici le graphe de la densité
x<-seq(0, 30, 1/100)
plot(x, dchisq(x,df=9), type="l", ylab="")

# Quantile d’ordre 95% de la loi du chi deux à (4−1)×(4−1) degrés de liberté
qchisq(0.95, df=9)

# Donc la zone de rejet du test est [16.9,+∞[ au risque α=5%
# Or, la statistique de test prend la valeur 138.3.
# On rejette donc l’hypothèse nulle au risque α=5%
# et on conclut qu’il existe un lien significatif entre la couleur des yeux 
# et celle des cheveux (au risque α=5%).