library(readxl)
library(dplyr)
library(stringi)

X5_Traits <- read_excel("Les Cinq Facteurs des Traits De Joueurs (réponses).xlsx")
DFS_TPI <- read_excel("Dispositional Flow Scale + TPI (réponses).xlsx")

# Erase test data (from before 21st of May)
X5_Traits <- filter(X5_Traits, X5_Traits$Horodateur >= "2021-05-21")
# Erase double from data
X5_Traits <- distinct(X5_Traits, X5_Traits$`Adresse e-mail`, .keep_all = TRUE)
# Lower all names
X5_Traits$`Quel est votre prénom ?` <- tolower(X5_Traits$`Quel est votre prénom ?`)
X5_Traits$`Quel est votre nom de famille ?` <- tolower(X5_Traits$`Quel est votre nom de famille ?`)
# Remove accents from names
stri_trans_general(X5_Traits$`Quel est votre prénom ?`, "Latin-ASCII")
stri_trans_general(X5_Traits$`Quel est votre nom de famille ?`, "Latin-ASCII")


# Erase test data (from before 25th of May)
DFS_TPI <- filter(DFS_TPI, DFS_TPI$Horodateur >= "2021-05-25")
# Lower all names
DFS_TPI$`Quel est votre prénom ?` <- tolower(DFS_TPI$`Quel est votre prénom ?`)
DFS_TPI$`Quel est votre nom de famille ?` <- tolower(DFS_TPI$`Quel est votre nom de famille ?`)
# Remove accents from names
stri_trans_general(DFS_TPI$`Quel est votre prénom ?`, "Latin-ASCII")
stri_trans_general(DFS_TPI$`Quel est votre nom de famille ?`, "Latin-ASCII")

traits_join_DFS <- inner_join(X5_Traits, DFS_TPI, by=c("Quel est votre nom de famille ?", "Quel est votre prénom ?"))

traits_join_DFS_Objective <- filter(traits_join_DFS, traits_join_DFS$`Qu'avez vous vu lors de l'expérience ?` == "Des zombies")
traits_join_DFS_Narrative <- filter(traits_join_DFS, traits_join_DFS$`Qu'avez vous vu lors de l'expérience ?` == "Les recherches d'Isidore")
traits_join_DFS_Aesthetic <- filter(traits_join_DFS, traits_join_DFS$`Qu'avez vous vu lors de l'expérience ?` == "Des portails")

correlations_objective = setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("Trait", "S","A", "G", "U", "C", "O", "L", "TT", "X"))
correlations_objective[, c(2:10)] <- sapply(correlations_objective[, c(2:10)], as.numeric)
correlations_objective$Trait = as.character(correlations_objective$Trait)


for(i in 0:4){
  trait_correlations = c()
  for(j in 0:8){
     trait_correlations = c(trait_correlations, cor(traits_join_DFS_Objective[36+i], traits_join_DFS_Objective[90+j]))
  }
  correlations_objective <- add_row(correlations_objective, Trait=colnames(traits_join_DFS_Objective[36+i]), S=trait_correlations[1], A=trait_correlations[2],
          G=trait_correlations[3], U=trait_correlations[4], C=trait_correlations[5], O=trait_correlations[6], L=trait_correlations[7],
          TT=trait_correlations[8], X=trait_correlations[9])
}

correlations_narrative = setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("Trait", "S","A", "G", "U", "C", "O", "L", "TT", "X"))
correlations_narrative[, c(2:10)] <- sapply(correlations_narrative[, c(2:10)], as.numeric)
correlations_narrative$Trait = as.character(correlations_narrative$Trait)

for(i in 0:4){
  trait_correlations = c()
  for(j in 0:8){
    trait_correlations = c(trait_correlations, cor(traits_join_DFS_Narrative[36+i], traits_join_DFS_Narrative[90+j]))
  }
  correlations_narrative <- add_row(correlations_narrative, Trait=colnames(traits_join_DFS_Objective[36+i]), S=trait_correlations[1], A=trait_correlations[2],
                                    G=trait_correlations[3], U=trait_correlations[4], C=trait_correlations[5], O=trait_correlations[6], L=trait_correlations[7],
                                    TT=trait_correlations[8], X=trait_correlations[9])
}

correlations_aesthetic = setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("Trait", "S","A", "G", "U", "C", "O", "L", "TT", "X"))
correlations_aesthetic[, c(2:10)] <- sapply(correlations_aesthetic[, c(2:10)], as.numeric)
correlations_aesthetic$Trait = as.character(correlations_aesthetic$Trait)

for(i in 0:4){
  trait_correlations = c()
  for(j in 0:8){
    trait_correlations = c(trait_correlations, cor(traits_join_DFS_Aesthetic[36+i], traits_join_DFS_Aesthetic[90+j]))
  }
  correlations_aesthetic <- add_row(correlations_aesthetic, Trait=colnames(traits_join_DFS_Objective[36+i]), S=trait_correlations[1], A=trait_correlations[2],
                                    G=trait_correlations[3], U=trait_correlations[4], C=trait_correlations[5], O=trait_correlations[6], L=trait_correlations[7],
                                    TT=trait_correlations[8], X=trait_correlations[9])
}