library(lavaan)

## TODO : Créer les matrices avec les données pour coller aux besoins de lavaan

aesthetic = aesthetic_df[15:19]
colnames(aesthetic) = c("AEST1", "AEST2", "AEST3", "AEST4", "AEST5")

challenge = aesthetic_df[25:29]
colnames(challenge) = c("CHAL1", "CHAL2", "CHAL3", "CHAL4", "CHAL5")

narrative = aesthetic_df[20:24]
colnames(narrative) = c("NARR1", "NARR2", "NARR3", "NARR4", "NARR5")

goals = aesthetic_df[30:34]
colnames(goals) = c("GOAL1", "GOAL2", "GOAL3", "GOAL4", "GOAL5")

social = aesthetic_df[10:14]
colnames(social) = c("SOCI1", "SOCI2", "SOCI3", "SOCI4", "SOCI5")

traits_data = cbind(aesthetic, challenge, narrative, goals, social)

cov(traits_data)

traits_model <- '
# measurement model
Aesthetic =~ AEST1 + AEST2 + AEST3 + AEST4 + AEST5
Challenge =~ CHAL1 + CHAL2 + CHAL3 + CHAL4 + CHAL5
Goals =~ GOAL1 + GOAL2 + GOAL3 + GOAL4 + GOAL5
Narrative =~ NARR1 + NARR2 + NARR3 + NARR4 + NARR5
Social =~ SOCI1 + SOCI2 + SOCI3 + SOCI4 + SOCI5
'

fit_traits_model <- sem(traits_model, data=traits_data)

summary(fit_traits_model, standardized=TRUE, fit.measures=TRUE)
