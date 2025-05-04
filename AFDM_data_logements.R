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

# Identifier les modalités rares afin de ne pas polluer les futures analyses. 
# => modalités 2 et 3 de la variable garage préalablement fusionnées.
table(data_hab_type$VoitureDansGarage)

# Identification des variables selon leur type
quali <- which(sapply(data_hab_type, is.factor))
quanti <- which(sapply(data_hab_type, is.numeric))
quanti.discr <- which(sapply(data_hab_type,
                             FUN = function(xx){
                               (is.numeric(xx))&(length(table(xx)))<10
                             })
)
quanti.cont <- quanti[-quanti.discr]
quanti.polluant <- which(regexpr("outcome", colnames(data_hab_type))>0)

# Analyses univariées
### Boxplots
install.packages("car")
library(car)
mapply(data_hab_type[,quanti.cont],
       FUN = function(xx,name){Boxplot(xx, main = name,id.n = 2, ylab = "")},
       name = names(quanti.cont))

### Diagrammes en barres
par(mfrow = c(2, 3), mar = c(3, 3, 2, 1))  # Marges plus serrées
par(mfrow = c(2, 3))
mapply(data_hab_type[,quanti.discr],
       FUN = function(xx,name){barplot(table(xx),main = name)},
       name = names(quanti.discr))

mapply(data_hab_type[,quali],
       FUN = function(xx,name){barplot(table(xx),main = name)},
       name = names(quali))

### Tableaux
# quanti continu
install.packages("stargazer")
library(stargazer)
stargazer(data_hab_type[,quanti.cont],
          summary.stat = c("n","min","p25","median","mean","p75","max","sd"),
          type = "text")

# quanti discret
install.packages("questionr")
library(questionr)
univ_quanti.discr <- lapply(data_hab_type[,quanti.discr], freq)
univ_quanti.discr

# quali
univ_quali <- lapply(data_hab_type[,quali], freq)
univ_quali



# Analyses univariées
### lien entre variables quanti
# Pearson
install.packages("DescTools")
library(DescTools)
matcor.pears <-cor(data_hab_type[,quanti.cont])
png("matrice_corr_pearson.png", width = 1000, height = 1000)
PlotCorr(matcor.pears, cex.lab = par("cex.lab"), cex = 0.55)
dev.off()

# Spearman
matcor.spear <- cor(data_hab_type[,quanti.cont],method = "spearman")
png("matrice_corr_spearman.png", width = 1000, height = 1000)
PlotCorr(matcor.spear, cex.lab = par("cex.lab"), cex = 0.55)
dev.off()



# lien entre variables quali et discrètes
matcram <- PairApply(data_hab_type[,c(quali,quanti.discr)], CramerV, symmetric = TRUE)
png("matrice_corr_cramer.png", width = 1000, height = 1000)
PlotCorr(matcram,
         cols = colorRampPalette(c("white", "steelblue"), space = "rgb")(20),
         breaks = seq(0, 1, length = 21),
         cex.lab = par("cex.lab"), cex = 0.55,
         args.colorlegend = list(labels=sprintf("%.1f", seq(0, 1, length = 11)), frame=TRUE))
dev.off()
