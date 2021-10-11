library(stringr)
source("LoadData.R",encoding="utf-8")

scenario_bp = function(dimension = "", y_range=c(0,28)){
  
  sumaov <- summary.aov(aov(traits_dfs_df[[dimension]] ~ qu.avez.vous.vu.lors.de.l.experience..,data=traits_dfs_df))
  if(round(sumaov[[1]][[1,"Pr(>F)"]] < 1.0)){
    dimensions_df = data.frame(Objectives=objectives_df[[dimension]],
                               Aesthetic=aesthetic_df[[dimension]],
                               Narrative=narrative_df[[dimension]])
    boxplot(dimensions_df, ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
    means <- colMeans(dimensions_df)
    points(means, col = "red", pch = 19, cex=1.5)
    mtext("Score", side=2, line=2, cex=1.75)
    
    #mtext(paste(str_to_title(dimension), "\n All Orders"), line=1)
    #mtext(paste("P-Value of ANOVA", round(sumaov[[1]][[1,"Pr(>F)"]], digits=3)), side=1, line=3)
  }
}

order_bp = function(dimension = "", y_range=c(0,28)){
  sumaov <- summary.aov(aov(traits_dfs_df[[dimension]] ~ est.ce.votre.premiere..deuxieme.ou.troisieme.experience.., data=traits_dfs_df))
  if(round(sumaov[[1]][[1,"Pr(>F)"]] < 1.0)){
    dimensions_df = data.frame(First=first_df[[dimension]],
                               Second=second_df[[dimension]],
                               Third=third_df[[dimension]])
    boxplot(dimensions_df, ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
    means <- colMeans(dimensions_df)
    points(means, col = "red", pch = 19, cex=1.5)
    mtext("Score", side=2, line=2, cex=1.75)
    #mtext(paste(str_to_title(dimension), "\n All Scenario"), line=1)
    #mtext(paste("P-Value of ANOVA", round(sumaov[[1]][[1,"Pr(>F)"]], digits=3)), side=1, line=3)
  }
}

scenario_order_bp = function(dimension = "", y_range=c(0,28), order=1){
  if(order == 1){
    sumaov <- summary.aov(aov(first_df[[dimension]] ~ qu.avez.vous.vu.lors.de.l.experience.., data=first_df))
    if(round(sumaov[[1]][[1,"Pr(>F)"]] < 1.0)){
      boxplot(data.frame(Objectives=objectives_first[[dimension]],
                       Aesthetic=aesthetic_first[[dimension]],
                       Narrative=narrative_first[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
    }
  }else if(order == 2){
    sumaov <- summary.aov(aov(second_df[[dimension]] ~ qu.avez.vous.vu.lors.de.l.experience.., data=second_df))
    if(round(sumaov[[1]][[1,"Pr(>F)"]] < 1.0)){
      boxplot(data.frame(Objectives=objectives_second[[dimension]],
                       Aesthetic=aesthetic_second[[dimension]],
                       Narrative=narrative_second[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
    }
  }else if(order == 3){
    sumaov <- summary.aov(aov(third_df[[dimension]] ~ qu.avez.vous.vu.lors.de.l.experience.., data=third_df))
    if(round(sumaov[[1]][[1,"Pr(>F)"]] < 1.0)){
      boxplot(data.frame(Objectives=objectives_third[[dimension]],
                       Aesthetic=aesthetic_third[[dimension]],
                       Narrative=narrative_third[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
    }
  }
  if(round(sumaov[[1]][[1,"Pr(>F)"]] < 1.0)){
    mtext(paste(str_to_title(dimension), "\n Order :", order), line=1)
    mtext(paste("P-Value of ANOVA", round(sumaov[[1]][[1,"Pr(>F)"]], digits=3)), side=1, line=3)
  }
}

order_scenario_bp = function(dimension = "", y_range=c(0,28), scenario=""){
  if(scenario == "aesthetic"){
    sumaov <- summary.aov(aov(aesthetic_df[[dimension]] ~ est.ce.votre.premiere..deuxieme.ou.troisieme.experience.., data=aesthetic_df))
    if(round(sumaov[[1]][[1,"Pr(>F)"]] < 1.0)){
      dimensions_df = data.frame(First=aesthetic_first[[dimension]],
                                 Second=aesthetic_second[[dimension]],
                                 Third=aesthetic_third[[dimension]])
      boxplot(dimensions_df, ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
      means <- colMeans(dimensions_df)
      points(means, col = "red", pch = 19, cex=1.5)
      mtext("Score", side=2, line=2, cex=1.75)
 
    }
  }else if(scenario == "narrative"){
    sumaov <- summary.aov(aov(narrative_df[[dimension]] ~ est.ce.votre.premiere..deuxieme.ou.troisieme.experience.., data=narrative_df))
    if(round(sumaov[[1]][[1,"Pr(>F)"]] < 1.0)){
      dimensions_df = data.frame(First=narrative_first[[dimension]],
                                 Second=narrative_second[[dimension]],
                                 Third=narrative_third[[dimension]])
    boxplot(dimensions_df, ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
    means <- colMeans(dimensions_df)
    points(means, col = "red", pch = 19, cex=1.5)
    mtext("Score", side=2, line=2, cex=1.75)
    }
  }else if(scenario == "objectives"){
    sumaov <- summary.aov(aov(objectives_df[[dimension]] ~ est.ce.votre.premiere..deuxieme.ou.troisieme.experience.., data=objectives_df))
    if(round(sumaov[[1]][[1,"Pr(>F)"]] < 1.0)){
      dimensions_df = data.frame(First=objectives_first[[dimension]],
                                 Second=objectives_second[[dimension]],
                                 Third=objectives_third[[dimension]])
      boxplot(dimensions_df, ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
      means <- colMeans(dimensions_df)
      points(means, col = "red", pch = 19, cex=1.5)
      mtext("Score", side=2, line=2, cex=1.75)
    }
  }
  if(round(sumaov[[1]][[1,"Pr(>F)"]] < 1.0)){
    #mtext(paste(str_to_title(dimension), "\n Scenario :" , str_to_title(scenario)), line=1)
    #mtext(paste("P-Value of ANOVA", round(sumaov[[1]][[1,"Pr(>F)"]], digits=3)), side=1, line=3)
  }
}

site_bp = function(dimension = "", y_range=c(0,28)){
  sumaov <- summary.aov(aov(traits_dfs_df[[dimension]] ~ sur.quel.site.allez.vous.participer.a.l.experimentation.., data=traits_dfs_df))
  if(round(sumaov[[1]][[1,"Pr(>F)"]] < 1.0)){
    par(mfrow=c(1, 2))
    boxplot(enise_df[[dimension]], ylim=y_range, data=enise_df, xlab="ENISE")
    boxplot(insa_df[[dimension]], ylim=y_range, data=insa_df, xlab="INSA")
    mtext(paste(str_to_title(dimension), "\n Depending on site"), line=-3, outer=TRUE)
    mtext(paste("P-Value of ANOVA", round(sumaov[[1]][[1,"Pr(>F)"]], digits=3)), side=1, line=-3, outer=TRUE)
  }
}

game_bp = function(dimension = "", y_range=c(0,28)){
  sumaov <- summary.aov(aov(traits_dfs_df[[dimension]] ~ a.quelle.frequence.jouez.vous.aux.jeux.videos.., data=traits_dfs_df))
  if(round(sumaov[[1]][[1,"Pr(>F)"]] < 1.0)){
    par(mfrow=c(1, 4))
    boxplot(nevergame_bf[[dimension]], ylim=y_range, data=enise_df, xlab="Never")
    boxplot(lowgame_bf[[dimension]], ylim=y_range, data=insa_df, xlab="Occasionally")
    boxplot(midgame_bf[[dimension]], ylim=y_range, data=insa_df, xlab="Regularly")
    boxplot(highgame_bf[[dimension]], ylim=y_range, data=insa_df, xlab="Everyday")
    mtext(paste(str_to_title(dimension), "\n Depending on video game frequency"), line=-3, outer=TRUE)
    mtext(paste("P-Value of ANOVA", round(sumaov[[1]][[1,"Pr(>F)"]], digits=3)), side=1, line=-3, outer=TRUE)
  }
}

dimension_corr = function(scenario, scenario_df){
  print(paste("Correlation between dimensions in scenario : ", scenario))
  
  corr_matrix = rcorr(as.matrix(data.frame(challenge_skill_balance=scenario_df[["challenge_skill_balance"]],
                             action_awareness=scenario_df[["action_awareness"]],
                             clear_goals=scenario_df[["clear_goals"]],
                             unambiguous_feedback=scenario_df[["unambiguous_feedback"]],
                             concentration=scenario_df[["concentration"]],
                             sense_of_control=scenario_df[["sense_of_control"]],
                             loss_self_consciousness=scenario_df[["loss_self_consciousness"]],
                             time_transformation=scenario_df[["time_transformation"]],
                             autotelic_experience=scenario_df[["autotelic_experience"]],
                             engagement=scenario_df[["engagement"]]
                             )))
  return(corr_matrix)
}