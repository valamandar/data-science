

x <- 10

print(x * 2)

install.packages('readr')
library(readr)


install.packages('pacman')
library(pacman)

p_load(ggplot2, dplyr)

a <- 3.14        # double
b <- 5L          # integer
c <- "R-lang"    # character
d <- TRUE        # logical


typeof(5L)

notes <- c(12, 15.5, 8, 19, 14)
cotes <- seq(1, 20, length=8)

bonus <- rep(x=2, 8);

cotes + bonus

typeof(5L)

as.numeric("5")

as.logical(0)
as.logical(5)

min(notes)
mean(notes)
max(bonus)

notes[notes >= 3]


mat <- matrix(1:9, nrow = 3, byrow = TRUE)
rowSums(mat)
colSums(mat)
mat[2,3]



mat_scores <- matrix(sample(0:20, 12, TRUE), nrow = 4)

ma_liste <- list(
  etudiant = "Jarvis",
  notes     = notes,
  stats     = summary(notes)
)

print(notes[1])
print(mat[1,1])
ma_liste$notes
ma_liste[[2]]

temp = seq(2, 20, length=50)

df <- data.frame(
  nom     = c("Alice","Bob"),
  note    = temp,
  present = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

str(df); head(df)
?head

df[df$note > 14,]

df$present <- as.factor(df$present)

levels(df$present)

etudiants <- read.csv("data/etudiants_annuel.csv", stringsAsFactors = FALSE)

p_load(readxl)

df_xl <- read_excel("data/sample.xlsx", sheet = "F1")

text_tab <- read.table("data/etudiants.txt", header = TRUE, sep = "\t")



View(etudiants)
any(is.na(etudiants))
colSums(is.na(etudiants))

names(etudiants)


etudiants$nom

tolower(etudiants$nom)
library(dplyr )

names(df)[names(df)=="nom"] <- "name"

?rename
names(etudiants)


library(dplyr)
df_clean <- etudiants %>%
  select(id, prenom, last_name = nom, classe, moyenne, absences) %>%
  filter(classe == "L3", moyenne > 14) %>%
  mutate(
    presence_rate = 1 - absences / max(absences),
    grade_cat = case_when(
      moyenne < 12 ~ "low",
      moyenne < 16 ~ "medium",
      TRUE         ~ "high"
    )
  ) %>%
  arrange(desc(moyenne))

df_clean







df_sub <- df_students %>%
  select(prenom, postnom = nom, classe, note_finale, moyenne) %>%
  filter(classe == "M2" | note_finale > 15) %>%
  mutate(passed = as.logical(moyenne >= 12)) %>%
  group_by(classe)  %>% 
  summarise(
      m_total = mean(moyenne, na.rm = TRUE),
      effectif = n()
    )

  
ibrary(dplyr)
etudiants %>%
  summarise(
    moy = mean(moyenne),
    med = median(moyenne),
    sd  = sd(moyenne),
    q1  = quantile(moyenne, .25),
    q3  = quantile(moyenne, .75)
  )


View(df_sub)

df_students = etudiants


table(df_students$classe)

library(dplyr)
df_students %>%
  count(classe) %>%
  mutate(ratio = n / sum(n))

tapply(df_students$moyenne, df_students$classe, mean)

aggregate(etudiants$moyenne ~ etudiants$classe, etudiants, mean)


