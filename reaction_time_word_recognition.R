# Données
niveaux <- rep(c("CP", "CE1", "CE2", "CM1", "CM2"), each = 4)
eleves <- c(3139, 2972, 3055, 3098, 1564, 1499, 1518, 1429, 1135, 1029, 1032, 1021, 748, 884, 769, 794, 663, 743, 775, 683)

# Créer un data frame
data <- data.frame(Niveau = niveaux, Eleve = eleves)

# Création du bar chart
library(ggplot2)

ggplot(data, aes(x = interaction(Niveau, 1:length(Eleve)), y = Eleve, fill = Niveau)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bar Chart des élèves par niveau scolaire", x = "Élèves", y = "Valeurs") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "green", "red", "purple", "orange")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(aes(label = Eleve), position = position_dodge(width = 1), vjust = 0.5, angle = 90, color = "black")
