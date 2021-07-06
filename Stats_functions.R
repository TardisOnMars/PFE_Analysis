library(stringr)
source("LoadData.R",encoding="utf-8")

scenario_bp = function(dimension = "", y_range=c(0,28)){
  sumaov <- summary.aov(aov(traits_dfs_df[[dimension]] ~ qu.avez.vous.vu.lors.de.l.experience..,data=traits_dfs_df))
  if(round(sumaov[[1]][[1,"Pr(>F)"]] < 0.15)){
    boxplot(data.frame(Objectives=objectives_df[[dimension]],
                     Aesthetic=aesthetic_df[[dimension]],
                     Narrative=narrative_df[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
    mtext(paste(str_to_title(dimension), "- All Orders"), line=1)
    mtext(paste("P-Value of ANOVA Analysis", round(sumaov[[1]][[1,"Pr(>F)"]], digits=3)), side=1, line=3)
  }
}

order_bp = function(dimension = "", y_range=c(0,28)){
  sumaov <- summary.aov(aov(traits_dfs_df[[dimension]] ~ est.ce.votre.premiere..deuxieme.ou.troisieme.experience.., data=traits_dfs_df))
  if(round(sumaov[[1]][[1,"Pr(>F)"]] < 0.15)){
    boxplot(data.frame(First=first_df[[dimension]],
                       Second=second_df[[dimension]],
                       Third=third_df[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
    mtext(paste(str_to_title(dimension), " - All Scenario"), line=1)
    mtext(paste("P-Value of ANOVA Analysis", round(sumaov[[1]][[1,"Pr(>F)"]], digits=3)), side=1, line=3)
  }
}

scenario_order_bp = function(dimension = "", y_range=c(0,28), order=1){
  if(order == 1){
    sumaov <- summary.aov(aov(first_df[[dimension]] ~ qu.avez.vous.vu.lors.de.l.experience.., data=first_df))
    if(round(sumaov[[1]][[1,"Pr(>F)"]] < 0.15)){
      boxplot(data.frame(Objectives=objectives_first[[dimension]],
                       Aesthetic=aesthetic_first[[dimension]],
                       Narrative=narrative_first[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
    }
  }else if(order == 2){
    sumaov <- summary.aov(aov(second_df[[dimension]] ~ qu.avez.vous.vu.lors.de.l.experience.., data=second_df))
    if(round(sumaov[[1]][[1,"Pr(>F)"]] < 0.15)){
      boxplot(data.frame(Objectives=objectives_second[[dimension]],
                       Aesthetic=aesthetic_second[[dimension]],
                       Narrative=narrative_second[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
    }
  }else if(order == 3){
    sumaov <- summary.aov(aov(third_df[[dimension]] ~ qu.avez.vous.vu.lors.de.l.experience.., data=third_df))
    if(round(sumaov[[1]][[1,"Pr(>F)"]] < 0.15)){
      boxplot(data.frame(Objectives=objectives_third[[dimension]],
                       Aesthetic=aesthetic_third[[dimension]],
                       Narrative=narrative_third[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
    }
  }
  if(round(sumaov[[1]][[1,"Pr(>F)"]] < 0.15)){
    mtext(paste(str_to_title(dimension), "- Order :", order), line=1)
    mtext(paste("P-Value of ANOVA Analysis", round(sumaov[[1]][[1,"Pr(>F)"]], digits=3)), side=1, line=3)
  }
}

order_scenario_bp = function(dimension = "", y_range=c(0,28), scenario=""){
  if(scenario == "aesthetic"){
    sumaov <- summary.aov(aov(aesthetic_df[[dimension]] ~ est.ce.votre.premiere..deuxieme.ou.troisieme.experience.., data=aesthetic_df))
    if(round(sumaov[[1]][[1,"Pr(>F)"]] < 0.15)){
      boxplot(data.frame(First=aesthetic_first[[dimension]],
                       Second=aesthetic_second[[dimension]],
                       Third=aesthetic_third[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
 
    }
  }else if(scenario == "narrative"){
    sumaov <- summary.aov(aov(narrative_df[[dimension]] ~ est.ce.votre.premiere..deuxieme.ou.troisieme.experience.., data=narrative_df))
    if(round(sumaov[[1]][[1,"Pr(>F)"]] < 0.15)){
    boxplot(data.frame(First=narrative_first[[dimension]],
                       Second=narrative_second[[dimension]],
                       Third=narrative_third[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
    }
  }else if(scenario == "objectives"){
    sumaov <- summary.aov(aov(objectives_df[[dimension]] ~ est.ce.votre.premiere..deuxieme.ou.troisieme.experience.., data=objectives_df))
    if(round(sumaov[[1]][[1,"Pr(>F)"]] < 0.15)){
      boxplot(data.frame(First=objectives_first[[dimension]],
                       Second=objectives_second[[dimension]],
                       Third=objectives_third[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
    }
  }
  if(round(sumaov[[1]][[1,"Pr(>F)"]] < 0.15)){
    mtext(paste(str_to_title(dimension), "- Scenario :" , str_to_title(scenario)), line=1)
    mtext(paste("P-Value of ANOVA Analysis", round(sumaov[[1]][[1,"Pr(>F)"]], digits=3)), side=1, line=3)
  }
}