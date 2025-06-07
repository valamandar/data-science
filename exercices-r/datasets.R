
set.seed(2025)

n <- 200  

prenoms <- c("Alice","Benoît","Carla","David","Emma","Fabrice","Gabrielle","Hugo","Inès","Jules")
noms    <- c("Ngoma","Kabila","Mbemba","Ilunga","Mputu","Kasongo","Kalala","Kanza","Mukeba","Mubemba")
classes <- c("L1","L2","L3","M1","M2")
cours   <- c("Statistiques","Programmation","Économie","Biologie","Histoire")


df_students <- data.frame(
  id       = 1:n,
  nom      = sample(noms, n, replace = TRUE),
  prenom   = sample(prenoms, n, replace = TRUE),
  age      = sample(18:30, n, replace = TRUE),
  classe   = sample(classes, n, replace = TRUE),
  cours    = sample(cours, n, replace = TRUE),
  moyenne  = round(runif(n, min = 10, max = 20), 2),
  absences = sample(0:10, n, replace = TRUE),
  stringsAsFactors = FALSE
)

# Ajout d'une note finale calculée (ex : 70% moyenne + 30% participation)
df_students$note_finale <- round(0.7 * df_students$moyenne + 
                                   0.3 * (20 - df_students$absences) * (20/20), 2)


write.csv(df_students, "data/etudiants_annuel.csv", row.names = FALSE)


print(head(df_students, 10))
