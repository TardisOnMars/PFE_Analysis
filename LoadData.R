library(readxl)
library(dplyr)
library(stringi)
library(ggplot2)

# Create dataframe from excel sheets
X5_Traits <- read_excel("Les Cinq Facteurs des Traits De Joueurs (réponses).xlsx")

# Reverse answers for questions marked with "[R]" (likert 7)
for(i in 1:length(X5_Traits)){
  if(grepl("[R]", colnames(X5_Traits[i]))){
    X5_Traits[i] <- 8 - X5_Traits[i]
  }
}

X5_Traits <-mutate(rowwise(X5_Traits), aesthetic=ceiling(sum(c_across(15:19), -5) * 100/30))
X5_Traits <-mutate(rowwise(X5_Traits), challenge=ceiling(sum(c_across(25:29), -5) * 100/30))
X5_Traits <-mutate(rowwise(X5_Traits), narrative=ceiling(sum(c_across(20:24), -5) * 100/30))
X5_Traits <-mutate(rowwise(X5_Traits), objectives=ceiling(sum(c_across(30:34), -5) * 100/30))
X5_Traits <-mutate(rowwise(X5_Traits), social=ceiling(sum(c_across(10:14), -5) * 100/30))

X5_Traits <- rename_all(X5_Traits, make.names)
colnames(X5_Traits) = stri_trans_general(tolower(colnames(X5_Traits)), "Latin-ASCII")

DFS_TPI <- read_excel("Dispositional Flow Scale + TPI (réponses).xlsx")
DFS_TPI <- rename_all(DFS_TPI, make.names)
colnames(DFS_TPI) = stri_trans_general(tolower(colnames(DFS_TPI)), "Latin-ASCII")

DFS_TPI <- mutate(rowwise(DFS_TPI), challenge_skill_balance=sum(c_across(seq.int(from=2, length.out=4, by=9))))
DFS_TPI <- mutate(rowwise(DFS_TPI), action_awareness=sum(c_across(seq.int(from=3, length.out=4, by=9))))
DFS_TPI <- mutate(rowwise(DFS_TPI), clear_goals=sum(c_across(seq.int(from=4, length.out=4, by=9))))
DFS_TPI <- mutate(rowwise(DFS_TPI), unambiguous_feedback=sum(c_across(seq.int(from=5, length.out=4, by=9))))
DFS_TPI <- mutate(rowwise(DFS_TPI), concentration=sum(c_across(seq.int(from=6, length.out=4, by=9))))
DFS_TPI <- mutate(rowwise(DFS_TPI), sense_of_control=sum(c_across(seq.int(from=7, length.out=4, by=9))))
DFS_TPI <- mutate(rowwise(DFS_TPI), loss_self_consciousness=sum(c_across(seq.int(from=8, length.out=4, by=9))))
DFS_TPI <- mutate(rowwise(DFS_TPI), time_transformation=sum(c_across(seq.int(from=9, length.out=4, by=9))))
DFS_TPI <- mutate(rowwise(DFS_TPI), autotelic_experience=sum(c_across(seq.int(from=10, length.out=4, by=9))))

DFS_TPI <- mutate(rowwise(DFS_TPI), engagement=sum(c_across(38:43))/6)

#### Personnality Traits

# Erase test data (from before 21st of May)
X5_Traits <- filter(X5_Traits, horodateur >= "2021-05-21")
X5_Traits <- filter(X5_Traits, !identifiant.participant %in% c(8, 9, 12, 13, 21, 23, 27, 31, 33, 36, 54, 27))

# X5_Traits <- filter(X5_Traits, a.quelle.frequence.jouez.vous.aux.jeux.videos.. != "Jamais")

# Erase double from data
X5_Traits <- distinct(X5_Traits, adresse.e.mail, .keep_all = TRUE)
# Lower and remove accents from all names
X5_Traits$quel.est.votre.prenom.. <- stri_trans_general(tolower(X5_Traits$quel.est.votre.prenom..), "Latin-ASCII")
X5_Traits$quel.est.votre.nom.de.famille.. <- stri_trans_general(tolower(X5_Traits$quel.est.votre.nom.de.famille..), "Latin-ASCII")


#### Flow and Presence Questionnaires

# Erase test data (from before 25th of May)
DFS_TPI <- filter(DFS_TPI, horodateur >= "2021-05-25")
# Lower and remove accents from all names
DFS_TPI$quel.est.votre.prenom.. <- stri_trans_general(tolower(DFS_TPI$quel.est.votre.prenom..), "Latin-ASCII")
DFS_TPI$quel.est.votre.nom.de.famille.. <- stri_trans_general(tolower(DFS_TPI$quel.est.votre.nom.de.famille..), "Latin-ASCII")

#### Join
traits_join_DFS <- inner_join(X5_Traits, DFS_TPI, by=c("quel.est.votre.nom.de.famille..", "quel.est.votre.prenom.."))

# Separate data depending on scenario
traits_join_DFS_Objective <- filter(traits_join_DFS, qu.avez.vous.vu.lors.de.l.experience.. == "Des zombies")
traits_join_DFS_Narrative <- filter(traits_join_DFS, qu.avez.vous.vu.lors.de.l.experience.. == "Les recherches d'Isidore")
traits_join_DFS_Aesthetic <- filter(traits_join_DFS, qu.avez.vous.vu.lors.de.l.experience.. == "Des portails")

