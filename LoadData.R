library(readxl)
library(dplyr)
library(stringi)
library(ggplot2)

#### Personnality Traits

# Create dataframe from excel sheets
traits <- read_excel("Les Cinq Facteurs des Traits De Joueurs (réponses).xlsx")

# Reverse answers for questions marked with "[R]" (likert 7)
for(i in 1:length(traits)){
  if(grepl("[R]", colnames(traits[i]))){
    traits[i] <- 8 - traits[i]
  }
}

# Compute traits
traits <-mutate(rowwise(traits), aesthetic=ceiling(sum(c_across(15:19), -5) * 100/30))
traits <-mutate(rowwise(traits), challenge=ceiling(sum(c_across(25:29), -5) * 100/30))
traits <-mutate(rowwise(traits), narrative=ceiling(sum(c_across(20:24), -5) * 100/30))
traits <-mutate(rowwise(traits), objectives=ceiling(sum(c_across(30:34), -5) * 100/30))
traits <-mutate(rowwise(traits), social=ceiling(sum(c_across(10:14), -5) * 100/30))

traits <- rename_all(traits, make.names)
colnames(traits) = stri_trans_general(tolower(colnames(traits)), "Latin-ASCII")

# Erase test data (from before 21st of May)
traits <- filter(traits, horodateur >= "2021-05-21")
traits <- filter(traits, !identifiant.participant %in% c(8, 9, 12, 13, 21, 23, 27, 31, 33, 36, 54, 27))

# Erase double from data
traits <- distinct(traits, adresse.e.mail, .keep_all = TRUE)

# Lower and remove accents from all names
traits$quel.est.votre.prenom.. <- stri_trans_general(tolower(traits$quel.est.votre.prenom..), "Latin-ASCII")
traits$quel.est.votre.nom.de.famille.. <- stri_trans_general(tolower(traits$quel.est.votre.nom.de.famille..), "Latin-ASCII")


#### Flow and Presence Questionnaires

# Create dataframe from excel sheets
dfs_tpi <- read_excel("Dispositional Flow Scale + TPI (réponses).xlsx")
dfs_tpi <- rename_all(dfs_tpi, make.names)
colnames(dfs_tpi) = stri_trans_general(tolower(colnames(dfs_tpi)), "Latin-ASCII")

# Compute flow and engagement scores
dfs_tpi <- mutate(rowwise(dfs_tpi), challenge_skill_balance=sum(c_across(seq.int(from=2, length.out=4, by=9))))
dfs_tpi <- mutate(rowwise(dfs_tpi), action_awareness=sum(c_across(seq.int(from=3, length.out=4, by=9))))
dfs_tpi <- mutate(rowwise(dfs_tpi), clear_goals=sum(c_across(seq.int(from=4, length.out=4, by=9))))
dfs_tpi <- mutate(rowwise(dfs_tpi), unambiguous_feedback=sum(c_across(seq.int(from=5, length.out=4, by=9))))
dfs_tpi <- mutate(rowwise(dfs_tpi), concentration=sum(c_across(seq.int(from=6, length.out=4, by=9))))
dfs_tpi <- mutate(rowwise(dfs_tpi), sense_of_control=sum(c_across(seq.int(from=7, length.out=4, by=9))))
dfs_tpi <- mutate(rowwise(dfs_tpi), loss_self_consciousness=sum(c_across(seq.int(from=8, length.out=4, by=9))))
dfs_tpi <- mutate(rowwise(dfs_tpi), time_transformation=sum(c_across(seq.int(from=9, length.out=4, by=9))))
dfs_tpi <- mutate(rowwise(dfs_tpi), autotelic_experience=sum(c_across(seq.int(from=10, length.out=4, by=9))))

dfs_tpi <- mutate(rowwise(dfs_tpi), engagement=sum(c_across(38:43))/6)

# Erase test data (from before 25th of May)
dfs_tpi <- filter(dfs_tpi, horodateur >= "2021-05-25")
# Lower and remove accents from all names
dfs_tpi$quel.est.votre.prenom.. <- stri_trans_general(tolower(dfs_tpi$quel.est.votre.prenom..), "Latin-ASCII")
dfs_tpi$quel.est.votre.nom.de.famille.. <- stri_trans_general(tolower(dfs_tpi$quel.est.votre.nom.de.famille..), "Latin-ASCII")

#### Experiments Time
exp_times = read.csv(file="temps_xp.csv")
exp_times[2:7] = lapply(exp_times[2:7], as.character)
exp_times[2:7] = lapply(exp_times[2:7], as.ITime)
exp_times["aesthetic_duration"] = exp_times$aesthetic_end - exp_times$aesthetic_begin
exp_times["narrative_duration"] = exp_times$narrative_end - exp_times$narrative_begin
exp_times["goals_duration"] = exp_times$goals_end - exp_times$goals_begin
exp_durations = data.frame(Aesthetic_Duration=exp_times[8], Narrative_Duration=exp_times[9], Goals_Duration=exp_times[10])
sum_durations = summary(exp_durations)

#### Join
traits_dfs_df <- inner_join(traits, dfs_tpi, by=c("quel.est.votre.nom.de.famille..", "quel.est.votre.prenom.."))

# Separate data depending on scenario
objectives_df <- filter(traits_dfs_df, qu.avez.vous.vu.lors.de.l.experience.. == "Des zombies")
narrative_df <- filter(traits_dfs_df, qu.avez.vous.vu.lors.de.l.experience.. == "Les recherches d'Isidore")
aesthetic_df <- filter(traits_dfs_df, qu.avez.vous.vu.lors.de.l.experience.. == "Des portails")

# Separate data depending on order
first_df = filter(traits_dfs_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Première")
second_df = filter(traits_dfs_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Deuxième")
third_df = filter(traits_dfs_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Troisième")

# Separate data depending on scenario AND order
aesthetic_first = filter(aesthetic_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Première")
aesthetic_second = filter(aesthetic_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Deuxième")
aesthetic_third = filter(aesthetic_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Troisième")

narrative_first = filter(narrative_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Première")
narrative_second = filter(narrative_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Deuxième")
narrative_third = filter(narrative_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Troisième")

objectives_first = filter(objectives_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Première")
objectives_second = filter(objectives_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Deuxième")
objectives_third = filter(objectives_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Troisième")

# Separate data depending on site
enise_df = filter(traits_dfs_df, sur.quel.site.allez.vous.participer.a.l.experimentation.. == "ENISE (Saint-Etienne)")
insa_df = filter(traits_dfs_df, sur.quel.site.allez.vous.participer.a.l.experimentation.. == "INSA Lyon")

# Separate data depending on site AND scenario ??

# Separate data depending on game experience
nevergame_bf = filter(traits_dfs_df, a.quelle.frequence.jouez.vous.aux.jeux.videos.. == "Jamais")
lowgame_bf = filter(traits_dfs_df, a.quelle.frequence.jouez.vous.aux.jeux.videos.. == "Occasionnellement")
midgame_bf = filter(traits_dfs_df, a.quelle.frequence.jouez.vous.aux.jeux.videos.. == "Régulièrement")
highgame_bf = filter(traits_dfs_df, a.quelle.frequence.jouez.vous.aux.jeux.videos.. == "Tous les jours")
