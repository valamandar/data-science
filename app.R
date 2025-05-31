if (!require("pacman")) install.packages("pacman")
library(pacman)


p_load("ggplot2", "dplyr")

a <- 3
b <- 5L
c <- "R-lang"
d <- TRUE

class(b)


notes <- c(12, 15.5, 8, 19, 14)
notes * 2

rando  = seq(0, 1, length =5)

print(notes)
print(rando)
notes + rando


x <- "15"
y <- as.logical(0)
typeof(d)


max(notes)
notes[notes>=15]


bonus = rep(2, length(notes))

notes + bonus

mat <- matrix(1:9, nrow = 3, byrow = TRUE)
rowSums(mat); colSums(mat)
mat[2,3]

mat_scores <- matrix(sample(0:20, 12, TRUE), nrow = 4)
rowSums(mat_scores)

ma_liste <- list(
  etudiant = "Jarvis",
  notes
  = notes,
  stats
  = summary(notes)
)

my_train <- list(
  raw = mat_scores, 
  mean = mean(mat_scores), 
  high = mat_scores >= 10 
)

df <- data.frame(
  nom
  = c("Alice","Bob"),
  note
  = c(15, 13.5),
  present = c(TRUE, FALSE),
  stringsAsFactors = TRUE
)

str(df);
head(df)
df$note
df[df$note > 14, ]


df_students <- read.csv("etudiants_annuel.csv",stringsAsFactors=FALSE)
str(df_students)
head(df_students, 6)

df_students[(df_students$classe == "L1" | df_students$classe == "L2") & df_students$moyenne>14,]

df_students$classe <- as.factor(df_students$classe)
levels(df_students$classe)

mat_notes <- as.matrix(df_students[, c("moyenne", "absences")]) 
df_students$prenom <- toupper(df_students$prenom)

summary(df_students)


p_load(readr, readxl)

df_csv1 <- read.csv("etudiants_annuel.csv", stringsAsFactors = FALSE)
df_csv2 <- read_csv("etudiants_annuel.csv", show_col_types = FALSE)

path_file <- "data/liste_cloud.xlsx"
df_xl <- read_excel(path_file, sheet = excel_sheets(path_file)[1])
typeof(excel_sheets(path_file))
typeof(excel_sheets(path_file)[1])

summary(df_xl)
str(df_xl)

read.table("data/etudiants.txt", header = TRUE, sep = "\t")
read_table("data/etudiants.txt", show_col_types = FALSE)
colSums(is.na(df_xl))

mean(df_students$moyenne)
names(df_students)[names(df_students) == "nom"] <- "last_name"
names(df_students)

tolower(df_students$cours)

df_students %>%
  filter(age > 20) %>%
  mutate(score2 = moyenne * 2) %>%
  arrange(desc(score2))


df_clean <- df_students %>%
  select(id, prenom, last_name = nom, classe, moyenne, absences) %>%
  filter(classe == "L3", moyenne > 14) %>%
  mutate(
    presence_rate = as.double(1 - absences / max(absences)),
    grade_cat = case_when(
      moyenne < 12 ~ "low",
      moyenne < 16 ~ "medium",
      TRUE ~ "high"
    )
  ) %>%
  arrange(presence_rate, desc(moyenne))

names(df_clean) <- toupper(names(df_clean))
df_clean


df_sub <- df_students %>%
    select(prenom, last_name = nom, classe, note_finale, moyenne) %>%
    filter(classe == "M2" | note_finale >15) %>%
    mutate(passed = as.logical(moyenne >= 10)) %>% 
    group_by(classe) %>% 
    summarise(
      moyenne_moyenne = mean(moyenne, na.rm = TRUE),
      effectif = n()
    )
df_sub


mean(df_students$moyenne)
median(df_students$moyenne)
sd(df_students$moyenne)
summary(df_students$moyenne)


df_students %>%
  summarise(
    moy = mean(absences),
    med = median(absences),
    sd= sd(absences),
    q1= quantile(absences, .25),
    q3= quantile(absences, .75)
  )

mean(df_students$absences)  
median(df_students$absences) 

?summarise

table(df_students$classe)
prop.table(table(df_students$classe))

df_students %>%
  count(classe) %>%
  mutate(pct = n / sum(n))

library(dplyr)

df_students %>%
  count(cours) %>%                       
  mutate(pct = round(n / sum(n), 3)) %>%   
  mutate(pct_txt = paste0(round(pct * 100), "%"))

df_students %>%
  count(classe) %>%
  arrange(desc(n)) %>%
  slice(1)

tapply(df_students$moyenne, df_students$classe, mean)
aggregate(moyenne ~ classe, df_students, mean)


df_students %>%
  group_by(classe) %>%
  summarise(
    moy_cl = mean(moyenne),
    sd_cl= sd(moyenne),
    n = n()
  )




df_absences_par_cours <- df_students %>%
  group_by(cours) %>%
  summarise(
    moy_absences = mean(absences, na.rm = TRUE),
    ecart_type_absences = sd(absences, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(moy_absences))  # Trie par moyenne décroissante

# Afficher le résultat
print(df_absences_par_cours)



hist(df_students$moyenne,
     main = "Histogramme des moyennes",
     xlab = "Moyenne",
     breaks = 15)

library(ggplot2)
ggplot(df_students, aes(x = moyenne)) +
  geom_histogram() +
  labs(title = "Histogramme des moyennes", x = "Moyenne")
?aes


boxplot(df_students$moyenne,
        main = "Boxplot des moyennes" ,
        ylab = "Moyenne" )

ggplot(df_students, aes(y = moyenne)) +
  geom_boxplot() +
  labs(title = "Boxplot des moyennes" )


hist(df_students$absences,
     main = "Histogramme des absences",
     xlab = "Absences",
     breaks = 15)


boxplot(df_students$absences,
        main = "Boxplot des absences" ,
        ylab = "Absences" )

sd(df_students$absences)
sd(df_students$note_finale)


counts <- table(df_students$classe)
barplot(counts,
        main = "Effectifs par classe",
        xlab = "Classe",
        ylab = "Nombre d'étudiants")


ggplot(df_students, aes(x = classe)) +
  geom_bar() +
  labs(title = "Effectifs par classe", x = "Classe")


pie(counts, labels = names(counts), main = "Répartition des classes")


counts <- table(df_students$cours)
barplot(counts,
        main = "Effectifs par classe",
        xlab = "Classe",
        ylab = "Nombre d'étudiants")


boxplot(moyenne ~ classe, data = df_students,
main = "Moyenne par classe",
xlab = "Classe", ylab = "Moyenne")

eff_cours <- table(df_students$cours)

# Calcul des pourcentages
pourcent_cours <- round(100 * prop.table(eff_cours), 1)

# Barplot avec stockage des coordonnées pour placer les labels
bp <- barplot(eff_cours,
              main = "Répartition des étudiants par cours",
              xlab = "Cours",
              ylab = "Effectif",
              col = "steelblue",
              las = 2)  # rotation des étiquettes

# Ajouter les pourcentages au-dessus des barres
text(x = bp,
     y = eff_cours,
     labels = paste0(pourcent_cours, "%"),
     pos = 3,         # au-dessus
     cex = 0.9,
     col = "black")




library(ggplot2)
library(dplyr)

# Calcul des effectifs + pourcentages
df_plot <- df_students %>%
  count(cours) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

# Barplot avec pourcentage au-dessus des barres
ggplot(df_plot, aes(x = cours, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(pct, "%")),
            vjust = -0.5, size = 4) +
  labs(title = "Répartition des étudiants par cours",
       x = "Cours",
       y = "Effectif") +
  theme_minimal()



plot(df_students$age, df_students$moyenne,
     main = "Âge vs Moyenne",
     xlab = "Âge", ylab = "Moyenne", pch = 19)
abline(lm(moyenne ~ age, data = df_students))



ggplot(df_students, aes(x = age, y = moyenne)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Âge vs Moyenne")



library(ggplot2)

ggplot(df_students, aes(x = absences, y = note_finale)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +  # nuage de points
  geom_smooth(method = "lm", se = TRUE, color = "red") +    # courbe de régression linéaire
  labs(
    title = "Évolution de la note finale en fonction des absences",
    x = "Nombre d'absences",
    y = "Note finale"
  ) +
  theme_minimal()


boxplot(moyenne ~ classe, data = df_students,
        main = "Moyenne par classe",
        xlab = "Classe", ylab = "Moyenne")


library(ggplot2)
ggplot(df_students, aes(x = classe, y = moyenne)) +
  geom_boxplot() +
  labs(title = "Moyenne par classe")


tab <- table(df_students$classe, df_students$cours)
tab

mosaicplot(tab, main = "Classe vs Cours")


library(ggplot2)

ggplot(df_students, aes(x = cours, y = note_finale)) +
  geom_boxplot() +
  labs(
    title = "Comparaison des notes finales par cours",
    x = "Cours",
    y = "Note finale"
  ) +
  theme_minimal()


library(ggplot2)

ggplot(df_students, aes(x = absences, y = note_finale)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +  # nuage de points
  geom_smooth(method = "lm", se = TRUE, color = "red") +    # courbe de régression linéaire
  labs(
    title = "Évolution de la note finale en fonction des absences",
    x = "Nombre d'absences",
    y = "Note finale"
  ) +
  theme_minimal()



library(ggplot2)
library(dplyr)

# Calcul des effectifs + pourcentages
df_plot <- df_students %>%
  count(cours) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

# Barplot avec pourcentage au-dessus des barres
ggplot(df_plot, aes(x = cours, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(pct, "%")),
            vjust = -0.5, size = 4) +
  labs(title = "Répartition des étudiants par cours",
       x = "Cours",
       y = "Effectif") +
  theme_minimal()


