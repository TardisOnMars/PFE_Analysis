source("LoadData.R",encoding="utf-8")

scenario_bp = function(dimension = "", y_range=c(0,28)){
  boxplot(data.frame(Objectives=traits_join_DFS_Objective[[dimension]],
                     Aesthetic=traits_join_DFS_Aesthetic[[dimension]],
                     Narrative=traits_join_DFS_Narrative[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
  mtext(dimension, line=1)
  sumaov <- summary.aov(aov(traits_join_DFS[[dimension]] ~ qu.avez.vous.vu.lors.de.l.experience..,data=traits_join_DFS))
  mtext(paste("P-Value of ANOVA Analysis", round(sumaov[[1]][[1,"Pr(>F)"]], digits=3)), side=1, line=3)
}

par(mfrow=c(1,5))
scenario_bp("challenge_skill_balance")
scenario_bp("action_awareness")
scenario_bp("clear_goals")
scenario_bp("unambiguous_feedback")
scenario_bp("concentration")
par(mfrow=c(1,5))
scenario_bp("sense_of_control")
scenario_bp("loss_self_consciousness")
scenario_bp("time_transformation")
scenario_bp("autotelic_experience")
scenario_bp("engagement", y_range=c(0, 7))

par(mfrow=c(1,5))
hist(traits_join_DFS_Aesthetic$aesthetic, xlim=c(0, 100), ylim=c(0, 15), main="Répartition du score Esthétique", xlab="Score Esthétique")
mtext(paste("Shapiro-Wilk normality test", round(shapiro.test(traits_join_DFS_Aesthetic$aesthetic)$p.value, 3)))
hist(traits_join_DFS_Aesthetic$challenge, xlim=c(0, 100), ylim=c(0, 15), main="Répartition du score Défi", xlab="Score Défi")
mtext(paste("Shapiro-Wilk normality test", round(shapiro.test(traits_join_DFS_Aesthetic$challenge)$p.value, 3)))
hist(traits_join_DFS_Aesthetic$narrative, xlim=c(0, 100), ylim=c(0, 15), main="Répartition du score Narration", xlab="Score Narration")
mtext(paste("Shapiro-Wilk normality test", round(shapiro.test(traits_join_DFS_Aesthetic$narrative)$p.value, 3)))
hist(traits_join_DFS_Aesthetic$objectives, xlim=c(0, 100), ylim=c(0, 15), main="Répartition du score Objectifs", xlab="Score Objectifs")
mtext(paste("Shapiro-Wilk normality test", round(shapiro.test(traits_join_DFS_Aesthetic$objectives)$p.value, 3)))
hist(traits_join_DFS_Aesthetic$social, xlim=c(0, 100), ylim=c(0, 15), main="Répartition du score Social", xlab="Score Social")
mtext(paste("Shapiro-Wilk normality test", round(shapiro.test(traits_join_DFS_Aesthetic$social)$p.value, 3)))