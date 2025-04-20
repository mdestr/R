# Données
niveaux <- rep(c("CP", "CE1", "CE2", "CM1", "CM2"), each = 4)
eleves <- c(3139, 2972, 3055, 3098, 1564, 1499, 1518, 1429, 1135, 1029, 1032, 1021, 748, 884, 769, 794, 663, 743, 775, 683)

# Créer un data frame avec l'ordre des niveaux
data <- data.frame(
  Niveau = factor(niveaux, levels = c("CP", "CE1", "CE2", "CM1", "CM2")),
  Eleve = eleves
)

# Affichage en boxplot
library(ggplot2)

ggplot(data, aes(x = Niveau, y = Eleve, fill = Niveau)) +
  geom_boxplot(width = 0.6) +
  labs(
    title = "Répartition des valeurs par niveau scolaire",
    x = "Niveau scolaire",
    y = "Valeur de l'élève"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "green", "red", "purple", "orange"))


# Statistiques globales
moyenne_globale <- mean(data$Eleve)
taille_totale <- length(data$Eleve)

cat("Moyenne globale :", moyenne_globale, "\n")
cat("Taille totale :", taille_totale, "\n\n")

# Statistiques globales
moyenne_globale <- mean(data$Eleve)
taille_totale <- length(data$Eleve)

cat("---- Statistiques globales ----\n")
cat("Moyenne globale :", moyenne_globale, "\n")
cat("Taille totale :", taille_totale, "\n\n")

# Statistiques par niveau
stats_par_niveau <- data %>%
  group_by(Niveau) %>%
  summarise(
    Moyenne = mean(Eleve),
    Effectif = n()
  )

cat("---- Statistiques par niveau ----\n")
print(stats_par_niveau)

# Optionnel : joli tableau si tu as knitr
if (require(knitr)) {
  knitr::kable(stats_par_niveau, caption = "Moyenne et effectif par niveau")
}
