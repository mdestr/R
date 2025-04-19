# Données
niveaux <- rep(c("CP", "CE1", "CE2", "CM1", "CM2"), each = 4)
eleves <- c(3139, 2972, 3055, 3098, 1564, 1499, 1518, 1429, 1135, 1029, 1032, 1021, 748, 884, 769, 794, 663, 743, 775, 683)

# Créer un data frame
data <- data.frame(Niveau = factor(niveaux, levels = c("CP", "CE1", "CE2", "CM1", "CM2")), Eleve = eleves)

# Création du scatterplot
library(ggplot2)

ggplot(data, aes(x = Niveau, y = Eleve, color = Niveau)) +
  geom_point(size = 4) +
  labs(title = "Scatterplot des élèves par niveau scolaire", x = "Niveau Scolaire", y = "Valeur de l'élève") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "green", "red", "purple", "orange"))

