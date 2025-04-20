# corrigé TD2
don <- data.frame(niveau=factor(c("CP", "CE1", "CE2", "CM1", "CM2")), 
                  temps = c(3139, 1564, 1135, 748, 663, 
                            2972, 1499, 1029, 884, 743, 
                            3055, 1518, 1032, 769, 775, 
                            3098, 1429, 1021, 794, 683))

# on définit l'ordre des niveau du facteur
don$niveau <- ordered(don$niveau, levels = c("CP", "CE1", "CE2", "CM1", "CM2"))

# on affiche
stripchart(temps~niveau, vertical = TRUE, data = don, pch = 20)

# moyenne des temps de réaction au sein de chaque groupe
tapply(don$temps, INDEX = don$niveau, FUN = mean)

# 99.7% de la variation des temps de réaction s’explique par le niveau scolaire de l’enfant
library(BioStatR)
eta2(don$temps,don$niveau)

# La distribution de la statistique de test sous H0 admet pour densité
x<-seq(0,10,1/100)
plot(x, df(x,df1 = 5-1,df2=20-5),type="l", ylab="")

# Si η2 est proche de 0, on s’attend à ce que la statistique de test soit proche de 0. 
# On rejeterra l’hypothèse nulle seulement si la valeur de la statistique de test est grande, 
# on définit donc une zone de rejet unilatérale.
# Le quantile d’ordre 95% de la loi de Fisher à k−1 et n−k degrés de liberté est
qf(0.95, df1 = 5-1, df2 = 20-5)



# Donc la zone de rejet du test est [3.06,+∞[ au risque α=5%
# La statistique de test prend la valeur Fobs=0.997×154×0.003=1246.25
# On rejette donc l’hypothèse nulle au risque α=5%
# et on conclut qu’il existe un lien significatif entre le niveau scolaire 
# et le temps de réaction (au risque α=5%).