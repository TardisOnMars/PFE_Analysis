library(readxl)
library(dplyr)
library(stringi)

# Create dataframe from excel sheets
X5_Traits <- read_excel("Les Cinq Facteurs des Traits De Joueurs (réponses).xlsx")
DFS_TPI <- read_excel("Dispositional Flow Scale + TPI (réponses).xlsx")

# Erase test data (from before 21st of May)
X5_Traits <- filter(X5_Traits, X5_Traits$Horodateur >= "2021-05-21")
# Erase double from data
X5_Traits <- distinct(X5_Traits, X5_Traits$`Adresse e-mail`, .keep_all = TRUE)
# Lower and remove accents from all names
X5_Traits$`Quel est votre prénom ?` <- stri_trans_general(tolower(X5_Traits$`Quel est votre prénom ?`), "Latin-ASCII")
X5_Traits$`Quel est votre nom de famille ?` <- stri_trans_general(tolower(X5_Traits$`Quel est votre nom de famille ?`), "Latin-ASCII")

# Erase test data (from before 25th of May)
DFS_TPI <- filter(DFS_TPI, DFS_TPI$Horodateur >= "2021-05-25")
# Lower and remove accents from all names
DFS_TPI$`Quel est votre prénom ?` <- stri_trans_general(tolower(DFS_TPI$`Quel est votre prénom ?`), "Latin-ASCII")
DFS_TPI$`Quel est votre nom de famille ?` <- stri_trans_general(tolower(DFS_TPI$`Quel est votre nom de famille ?`), "Latin-ASCII")

traits_join_DFS <- inner_join(X5_Traits, DFS_TPI, by=c("Quel est votre nom de famille ?", "Quel est votre prénom ?"))

traits_join_DFS_Objective <- filter(traits_join_DFS, traits_join_DFS$`Qu'avez vous vu lors de l'expérience ?` == "Des zombies")
traits_join_DFS_Narrative <- filter(traits_join_DFS, traits_join_DFS$`Qu'avez vous vu lors de l'expérience ?` == "Les recherches d'Isidore")
traits_join_DFS_Aesthetic <- filter(traits_join_DFS, traits_join_DFS$`Qu'avez vous vu lors de l'expérience ?` == "Des portails")

# Correlations computation for objective scenario

correlations_objective <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("Trait", "S","A", "G", "U", "C", "O", "L", "TT", "X"))
correlations_objective[, c(2:10)] <- sapply(correlations_objective[, c(2:10)], as.numeric)
correlations_objective$Trait <- as.character(correlations_objective$Trait)

correlations_objective_p <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("Trait", "S","A", "G", "U", "C", "O", "L", "TT", "X"))
correlations_objective_p[, c(2:10)] <- sapply(correlations_objective_p[, c(2:10)], as.numeric)
correlations_objective_p$Trait <- as.character(correlations_objective_p$Trait)

for(i in 0:4){
  trait_correlations <- c()
  trait_correlations_p <- c()
  for(j in 0:8){
     trait_correlations <- c(trait_correlations, cor(traits_join_DFS_Objective[36+i], traits_join_DFS_Objective[90+j]))
     trait_correlations_p <- c(trait_correlations_p, cor.test(traits_join_DFS_Objective[[36+i]], traits_join_DFS_Objective[[90+j]])$p.value)
  }
  correlations_objective <- add_row(correlations_objective, Trait=colnames(traits_join_DFS_Objective[36+i]), S=trait_correlations[1], A=trait_correlations[2],
          G=trait_correlations[3], U=trait_correlations[4], C=trait_correlations[5], O=trait_correlations[6], L=trait_correlations[7],
          TT=trait_correlations[8], X=trait_correlations[9])
  correlations_objective_p <- add_row(correlations_objective_p, Trait=colnames(traits_join_DFS_Objective[36+i]), S=trait_correlations_p[1], A=trait_correlations_p[2],
                                      G=trait_correlations_p[3], U=trait_correlations_p[4], C=trait_correlations_p[5], O=trait_correlations_p[6], L=trait_correlations_p[7],
                                      TT=trait_correlations_p[8], X=trait_correlations_p[9])
}

# Correlations computation for narrative scenario

correlations_narrative <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("Trait", "S","A", "G", "U", "C", "O", "L", "TT", "X"))
correlations_narrative[, c(2:10)] <- sapply(correlations_narrative[, c(2:10)], as.numeric)
correlations_narrative$Trait <- as.character(correlations_narrative$Trait)

correlations_narrative_p <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("Trait", "S","A", "G", "U", "C", "O", "L", "TT", "X"))
correlations_narrative_p[, c(2:10)] <- sapply(correlations_narrative_p[, c(2:10)], as.numeric)
correlations_narrative_p$Trait <- as.character(correlations_narrative_p$Trait)

for(i in 0:4){
  trait_correlations <- c()
  trait_correlations_p <- c()
  for(j in 0:8){
    trait_correlations <- c(trait_correlations, cor(traits_join_DFS_Narrative[36+i], traits_join_DFS_Narrative[90+j]))
    trait_correlations_p <- c(trait_correlations_p, cor.test(traits_join_DFS_Narrative[[36+i]], traits_join_DFS_Narrative[[90+j]])$p.value)
  }
  correlations_narrative <- add_row(correlations_narrative, Trait=colnames(traits_join_DFS_Narrative[36+i]), S=trait_correlations[1], A=trait_correlations[2],
                                    G=trait_correlations[3], U=trait_correlations[4], C=trait_correlations[5], O=trait_correlations[6], L=trait_correlations[7],
                                    TT=trait_correlations[8], X=trait_correlations[9])
  correlations_narrative_p <- add_row(correlations_narrative_p, Trait=colnames(traits_join_DFS_Narrative[36+i]), S=trait_correlations_p[1], A=trait_correlations_p[2],
                                    G=trait_correlations_p[3], U=trait_correlations_p[4], C=trait_correlations_p[5], O=trait_correlations_p[6], L=trait_correlations_p[7],
                                    TT=trait_correlations_p[8], X=trait_correlations_p[9])
}

# Correlations computation for aesthetic scenario

correlations_aesthetic <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("Trait", "S","A", "G", "U", "C", "O", "L", "TT", "X"))
correlations_aesthetic[, c(2:10)] <- sapply(correlations_aesthetic[, c(2:10)], as.numeric)
correlations_aesthetic$Trait <- as.character(correlations_aesthetic$Trait)

correlations_aesthetic_p <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("Trait", "S","A", "G", "U", "C", "O", "L", "TT", "X"))
correlations_aesthetic_p[, c(2:10)] <- sapply(correlations_aesthetic_p[, c(2:10)], as.numeric)
correlations_aesthetic_p$Trait <- as.character(correlations_aesthetic_p$Trait)

for(i in 0:4){
  trait_correlations <- c()
  trait_correlations_p <- c()
  for(j in 0:8){
    trait_correlations <- c(trait_correlations, cor(traits_join_DFS_Aesthetic[36+i], traits_join_DFS_Aesthetic[90+j]))
    trait_correlations_p <- c(trait_correlations_p, cor.test(traits_join_DFS_Aesthetic[[36+i]], traits_join_DFS_Aesthetic[[90+j]])$p.value)
  }
  correlations_aesthetic <- add_row(correlations_aesthetic, Trait=colnames(traits_join_DFS_Aesthetic[36+i]), S=trait_correlations[1], A=trait_correlations[2],
                                    G=trait_correlations[3], U=trait_correlations[4], C=trait_correlations[5], O=trait_correlations[6], L=trait_correlations[7],
                                    TT=trait_correlations[8], X=trait_correlations[9])
  correlations_aesthetic_p <- add_row(correlations_aesthetic_p, Trait=colnames(traits_join_DFS_Aesthetic[36+i]), S=trait_correlations_p[1], A=trait_correlations_p[2],
                                      G=trait_correlations_p[3], U=trait_correlations_p[4], C=trait_correlations_p[5], O=trait_correlations_p[6], L=trait_correlations_p[7],
                                      TT=trait_correlations_p[8], X=trait_correlations_p[9])
}