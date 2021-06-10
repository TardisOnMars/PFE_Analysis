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

panel_list_objective <- vector("list", length(X5_Traits))
for (i in 1:length(X5_Traits)){
  param_DFS <- filter(DFS_TPI, DFS_TPI$`Quel est votre nom de famille ?` == X5_Traits$`Quel est votre nom de famille ?`[i] & DFS_TPI$`Qu'avez vous vu lors de l'expérience ?` == "Des zombies")
  if(length(param_DFS$`Autotelic experience (X)`) != 0){
    panel_list_objective[i] = panelist(surname = X5_Traits$`Quel est votre nom de famille ?`[i], forname = X5_Traits$`Quel est votre prénom ?`[i],
                                       aesthetic = X5_Traits$`Score Esthétique`[i], challenge = X5_Traits$`Score Défi`[i],
                                       objective = X5_Traits$`Score Objectifs`[i], narrative = X5_Traits$`Score Narratif`[i],
                                       social = X5_Traits$`Score Social`[i], 
                                       skill = param_DFS$`Challenge-Skill Balance (S)`, awareness = param_DFS$`Action Awareness (A)`,
                                       goal = param_DFS$`Clear Goals (G)`, unambiguous = param_DFS$`Unambiguous Feedback (U)`,
                                       concentration = param_DFS$`Concentration (C)`, control = param_DFS$`Sense of Control (O)`,
                                       loss = param_DFS$`Loss of Self-Consciousness (L)`, transformation = param_DFS$`Time Transformation (T)`,
                                       autotelic = param_DFS$`Autotelic experience (X)`)
  }
}

panel_list_aesthetic <- vector("list", length(X5_Traits))
for (i in 1:length(X5_Traits)){
  param_DFS <- filter(DFS_TPI, DFS_TPI$`Quel est votre nom de famille ?` == X5_Traits$`Quel est votre nom de famille ?`[i] & DFS_TPI$`Qu'avez vous vu lors de l'expérience ?` == "Des portails")
  if(length(param_DFS$`Autotelic experience (X)`) != 0){
  panel_list_aesthetic[i] = panelist(surname = X5_Traits$`Quel est votre nom de famille ?`[i], forname = X5_Traits$`Quel est votre prénom ?`[i],
                                           aesthetic = X5_Traits$`Score Esthétique`[i], challenge = X5_Traits$`Score Défi`[i],
                                           objective = X5_Traits$`Score Objectifs`[i], narrative = X5_Traits$`Score Narratif`[i],
                                           social = X5_Traits$`Score Social`[i], 
                                           skill = param_DFS$`Challenge-Skill Balance (S)`, awareness = param_DFS$`Action Awareness (A)`,
                                           goal = param_DFS$`Clear Goals (G)`, unambiguous = param_DFS$`Unambiguous Feedback (U)`,
                                           concentration = param_DFS$`Concentration (C)`, control = param_DFS$`Sense of Control (O)`,
                                           loss = param_DFS$`Loss of Self-Consciousness (L)`, transformation = param_DFS$`Time Transformation (T)`,
                                           autotelic = param_DFS$`Autotelic experience (X)`)
  }
}

panel_list_narrative <- vector("list", length(X5_Traits))
for (i in 1:length(X5_Traits)){
  param_DFS <- filter(DFS_TPI, DFS_TPI$`Quel est votre nom de famille ?` == X5_Traits$`Quel est votre nom de famille ?`[i] & DFS_TPI$`Qu'avez vous vu lors de l'expérience ?` == "Les recherches d'Isidore")
  if(length(param_DFS$`Autotelic experience (X)`) != 0){
  panel_list_narrative[i] = panelist(surname = X5_Traits$`Quel est votre nom de famille ?`[i], forname = X5_Traits$`Quel est votre prénom ?`[i],
                                           aesthetic = X5_Traits$`Score Esthétique`[i], challenge = X5_Traits$`Score Défi`[i],
                                           objective = X5_Traits$`Score Objectifs`[i], narrative = X5_Traits$`Score Narratif`[i],
                                           social = X5_Traits$`Score Social`[i], 
                                           skill = param_DFS$`Challenge-Skill Balance (S)`, awareness = param_DFS$`Action Awareness (A)`,
                                           goal = param_DFS$`Clear Goals (G)`, unambiguous = param_DFS$`Unambiguous Feedback (U)`,
                                           concentration = param_DFS$`Concentration (C)`, control = param_DFS$`Sense of Control (O)`,
                                           loss = param_DFS$`Loss of Self-Consciousness (L)`, transformation = param_DFS$`Time Transformation (T)`,
                                           autotelic = param_DFS$`Autotelic experience (X)`)
  }
}

objective_o <- vector("numeric", length(X5_Traits))
aesthetic_o <- vector("numeric", length(X5_Traits))
narrative_o <- vector("numeric", length(X5_Traits))
social_o <- vector("numeric", length(X5_Traits))
challenge_o <- vector("numeric", length(X5_Traits))
skill_o <- vector("numeric", length(X5_Traits))
awareness_o <- vector("numeric", length(X5_Traits))
goal_o <- vector("numeric", length(X5_Traits))
unanmbiguous_o <- vector("numeric", length(X5_Traits))
concentration_o <- vector("numeric", length(X5_Traits))
control_o <- vector("numeric", length(X5_Traits))
loss_o <- vector("numeric", length(X5_Traits))
transformation_o <- vector("numeric", length(X5_Traits))
autotelic_o <- vector("numeric", length(X5_Traits))

for (i in 1:length(panel_list_objective)){
  if(!is.null(panel_list_objective[[i]])){
    objective_o[i] = panel_list_objective[[i]]@objective
    aesthetic_o[i] = panel_list_objective[[i]]@aesthetic
    narrative_o[i] = panel_list_objective[[i]]@narrative
    social_o[i] = panel_list_objective[[i]]@social
    challenge_o[i] = panel_list_objective[[i]]@challenge
    
    skill_o[i] = panel_list_objective[[i]]@skill
    awareness_o[i] = panel_list_objective[[i]]@awareness
    goal_o[i] = panel_list_objective[[i]]@goal
    unanmbiguous_o[i] = panel_list_objective[[i]]@unambiguous
    concentration_o[i] = panel_list_objective[[i]]@concentration
    control_o[i] = panel_list_objective[[i]]@control
    loss_o[i] = panel_list_objective[[i]]@loss
    transformation_o[i] = panel_list_objective[[i]]@transformation
    autotelic_o[i] = panel_list_objective[[i]]@autotelic
  }
}



objective_a <- vector("numeric", length(X5_Traits))
aesthetic_a <- vector("numeric", length(X5_Traits))
narrative_a <- vector("numeric", length(X5_Traits))
social_a <- vector("numeric", length(X5_Traits))
challenge_a <- vector("numeric", length(X5_Traits))
skill_a <- vector("numeric", length(X5_Traits))
awareness_a <- vector("numeric", length(X5_Traits))
goal_a <- vector("numeric", length(X5_Traits))
unanmbiguous_a <- vector("numeric", length(X5_Traits))
concentration_a <- vector("numeric", length(X5_Traits))
control_a <- vector("numeric", length(X5_Traits))
loss_a <- vector("numeric", length(X5_Traits))
transformation_a <- vector("numeric", length(X5_Traits))
autotelic_a <- vector("numeric", length(X5_Traits))

for (i in 1:length(panel_list_aesthetic)){
  if(!is.null(panel_list_aesthetic[[i]])){
    objective_a[i] = panel_list_aesthetic[[i]]@objective
    aesthetic_a[i] = panel_list_aesthetic[[i]]@aesthetic
    narrative_a[i] = panel_list_aesthetic[[i]]@narrative
    social_a[i] = panel_list_aesthetic[[i]]@social
    challenge_a[i] = panel_list_aesthetic[[i]]@challenge
    
    skill_a[i] = panel_list_aesthetic[[i]]@skill
    awareness_a[i] = panel_list_aesthetic[[i]]@awareness
    goal_a[i] = panel_list_aesthetic[[i]]@goal
    unanmbiguous_a[i] = panel_list_aesthetic[[i]]@unambiguous
    concentration_a[i] = panel_list_aesthetic[[i]]@concentration
    control_a[i] = panel_list_aesthetic[[i]]@control
    loss_a[i] = panel_list_aesthetic[[i]]@loss
    transformation_a[i] = panel_list_aesthetic[[i]]@transformation
    autotelic_a[i] = panel_list_aesthetic[[i]]@autotelic
  }
}



objective_n <- vector("numeric", length(X5_Traits))
aesthetic_n <- vector("numeric", length(X5_Traits))
narrative_n <- vector("numeric", length(X5_Traits))
social_n <- vector("numeric", length(X5_Traits))
challenge_n <- vector("numeric", length(X5_Traits))
skill_n <- vector("numeric", length(X5_Traits))
awareness_n <- vector("numeric", length(X5_Traits))
goal_n <- vector("numeric", length(X5_Traits))
unanmbiguous_n <- vector("numeric", length(X5_Traits))
concentration_n <- vector("numeric", length(X5_Traits))
control_n <- vector("numeric", length(X5_Traits))
loss_n <- vector("numeric", length(X5_Traits))
transformation_n <- vector("numeric", length(X5_Traits))
autotelic_n <- vector("numeric", length(X5_Traits))

for (i in 1:length(panel_list_narrative)){
  if(!is.null(panel_list_narrative[[i]])){
    objective_n[i] = panel_list_narrative[[i]]@objective
    aesthetic_n[i] = panel_list_narrative[[i]]@aesthetic
    narrative_n[i] = panel_list_narrative[[i]]@narrative
    social_n[i] = panel_list_narrative[[i]]@social
    challenge_n[i] = panel_list_narrative[[i]]@challenge
    
    skill_n[i] = panel_list_narrative[[i]]@skill
    awareness_n[i] = panel_list_narrative[[i]]@awareness
    goal_n[i] = panel_list_narrative[[i]]@goal
    unanmbiguous_n[i] = panel_list_narrative[[i]]@unambiguous
    concentration_n[i] = panel_list_narrative[[i]]@concentration
    control_n[i] = panel_list_narrative[[i]]@control
    loss_n[i] = panel_list_narrative[[i]]@loss
    transformation_n[i] = panel_list_narrative[[i]]@transformation
    autotelic_n[i] = panel_list_narrative[[i]]@autotelic
  }
}

