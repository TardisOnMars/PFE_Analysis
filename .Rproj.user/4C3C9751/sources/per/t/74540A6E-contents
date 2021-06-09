library(readxl)
library(dplyr)

X5_Traits <- read_excel("5_Traits.xlsx")
DFS_TPI <- read_excel("DFS_TPI.xlsx")

# Erase test data (from before 21st of May)
X5_Traits <- filter(X5_Traits, X5_Traits$Horodateur >= "2021-05-21")
# Erase double from data
X5_Traits <- distinct(X5_Traits, X5_Traits$`Adresse e-mail`, .keep_all = TRUE)
# Lower all names
X5_Traits$`Quel est votre prénom ?` <- tolower(X5_Traits$`Quel est votre prénom ?`)
X5_Traits$`Quel est votre nom de famille ?` <- tolower(X5_Traits$`Quel est votre nom de famille ?`)

DFS_TPI$`Quel est votre prénom ?` <- tolower(DFS_TPI$`Quel est votre prénom ?`)
DFS_TPI$`Quel est votre nom de famille ?` <- tolower(DFS_TPI$`Quel est votre nom de famille ?`)

# Erase test data (from before 25th of May)
DFS_TPI <- filter(DFS_TPI, DFS_TPI$Horodateur >= "2021-05-25")

panelist <- setClass("panelist", slots=c(surname="character", forname="character", aesthetic="numeric",
                                challenge="numeric", objective="numeric", narrative="numeric", social="numeric",
                                skill="numeric", awareness="numeric", goal="numeric", unambiguous="numeric", concentration="numeric",
                                control="numeric", loss="numeric", transformation="numeric", autotelic="numeric"))
panel_list_objective <- list()
for (i in 1:length(X5_Traits)){
  param_DFS <- filter(DFS_TPI, DFS_TPI$`Quel est votre nom de famille ?` == X5_Traits$`Quel est votre nom de famille ?`[i] & DFS_TPI$`Qu'avez vous vu lors de l'expérience ?` == "Des zombies")
  panel_list[paste("panel",i)] <- panelist(surname = X5_Traits$`Quel est votre nom de famille ?`[i], forname = X5_Traits$`Quel est votre prénom ?`[i],
                                           aesthetic = X5_Traits$`Score Esthétique`[i], challenge = X5_Traits$`Score Défi`[i],
                                           objective = X5_Traits$`Score Objectifs`[i], narrative = X5_Traits$`Score Narratif`[i],
                                           social = X5_Traits$`Score Social`[i], 
                                           skill = param_DFS$`Challenge-Skill Balance (S)`, awareness = param_DFS$`Action Awareness (A)`,
                                           goal = param_DFS$`Clear Goals (G)`, unambiguous = param_DFS$`Unambiguous Feedback (U)`,
                                           concentration = param_DFS$`Concentration (C)`, control = param_DFS$`Sense of Control (O)`,
                                           loss = param_DFS$`Loss of Self-Consciousness (L)`, transformation = param_DFS$`Time Transformation (T)`,
                                           autotelic = param_DFS$`Autotelic experience (X)`)
}

panel_list_aesthetic <- list()
for (i in 1:length(X5_Traits)){
  param_DFS <- filter(DFS_TPI, DFS_TPI$`Quel est votre nom de famille ?` == X5_Traits$`Quel est votre nom de famille ?`[i] & DFS_TPI$`Qu'avez vous vu lors de l'expérience ?` == "Des portails")
  panel_list[paste("panel",i)] <- panelist(surname = X5_Traits$`Quel est votre nom de famille ?`[i], forname = X5_Traits$`Quel est votre prénom ?`[i],
                                           aesthetic = X5_Traits$`Score Esthétique`[i], challenge = X5_Traits$`Score Défi`[i],
                                           objective = X5_Traits$`Score Objectifs`[i], narrative = X5_Traits$`Score Narratif`[i],
                                           social = X5_Traits$`Score Social`[i], 
                                           skill = param_DFS$`Challenge-Skill Balance (S)`, awareness = param_DFS$`Action Awareness (A)`,
                                           goal = param_DFS$`Clear Goals (G)`, unambiguous = param_DFS$`Unambiguous Feedback (U)`,
                                           concentration = param_DFS$`Concentration (C)`, control = param_DFS$`Sense of Control (O)`,
                                           loss = param_DFS$`Loss of Self-Consciousness (L)`, transformation = param_DFS$`Time Transformation (T)`,
                                           autotelic = param_DFS$`Autotelic experience (X)`)
}

panel_list_narrative <- list()
for (i in 1:length(X5_Traits)){
  param_DFS <- filter(DFS_TPI, DFS_TPI$`Quel est votre nom de famille ?` == X5_Traits$`Quel est votre nom de famille ?`[i] & DFS_TPI$`Qu'avez vous vu lors de l'expérience ?` == "Les recherches d'Isidore")
  panel_list[paste("panel",i)] <- panelist(surname = X5_Traits$`Quel est votre nom de famille ?`[i], forname = X5_Traits$`Quel est votre prénom ?`[i],
                                           aesthetic = X5_Traits$`Score Esthétique`[i], challenge = X5_Traits$`Score Défi`[i],
                                           objective = X5_Traits$`Score Objectifs`[i], narrative = X5_Traits$`Score Narratif`[i],
                                           social = X5_Traits$`Score Social`[i], 
                                           skill = param_DFS$`Challenge-Skill Balance (S)`, awareness = param_DFS$`Action Awareness (A)`,
                                           goal = param_DFS$`Clear Goals (G)`, unambiguous = param_DFS$`Unambiguous Feedback (U)`,
                                           concentration = param_DFS$`Concentration (C)`, control = param_DFS$`Sense of Control (O)`,
                                           loss = param_DFS$`Loss of Self-Consciousness (L)`, transformation = param_DFS$`Time Transformation (T)`,
                                           autotelic = param_DFS$`Autotelic experience (X)`)
}
