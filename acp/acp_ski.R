# 6 personnes ont été invitées à noter de 0 à 6  limportance accordée dans un séjour au ski selon ces 3 critères : 
# - qualité des animations nocturnes (N)
# - qualité du domaine skiable (S)
# - qualité des activités non skiables (A)
# 
# Individu N S A
# Alice    4 5 5 
# Bob      2 6 6 
# Charlie  6 4 4 
# Dan      0 5 3 
# Erin     4 3 1 
# Frank    2 1 5
# 

# Calculer  les moyennes des 3 variables. 
# En déduire la matrice Z des données centrées. 
# Pour simplifier les calculs, on choisira la métrique M = I. 
# Commenter ce choix.

df <- data.frame(
  Individu = c("Alice", "Bob", "Charlie", "Dan", "Erin", "Frank"),
  N = c(4, 2, 6, 0, 4, 2),
  S = c(5, 6, 4, 5, 3, 1),
  A = c(5, 6, 4, 3, 1, 5)
)

moyennes <- colMeans(df[, c("N", "S", "A")])
print(moyennes)
