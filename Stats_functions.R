source("LoadData.R",encoding="utf-8")

scenario_bp = function(dimension = "", y_range=c(0,28)){
  boxplot(data.frame(Objectives=objectives_df[[dimension]],
                     Aesthetic=aesthetic_df[[dimension]],
                     Narrative=narrative_df[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
  mtext(dimension, line=1)
  sumaov <- summary.aov(aov(traits_dfs_df[[dimension]] ~ qu.avez.vous.vu.lors.de.l.experience..,data=traits_dfs_df))
  mtext(paste("P-Value of ANOVA Analysis", round(sumaov[[1]][[1,"Pr(>F)"]], digits=3)), side=1, line=3)
}

order_bp = function(dimension = "", y_range=c(0,28)){
  boxplot(data.frame(First=first_df[[dimension]],
                     Second=second_df[[dimension]],
                     Third=third_df[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
  mtext(dimension, line=1)
  sumaov <- summary.aov(aov(traits_dfs_df[[dimension]] ~ est.ce.votre.premiere..deuxieme.ou.troisieme.experience.., data=traits_dfs_df))
  mtext(paste("P-Value of ANOVA Analysis", round(sumaov[[1]][[1,"Pr(>F)"]], digits=3)), side=1, line=3)
}

scenario_order_bp = function(dimension = "", y_range=c(0,28), order=1){
  if(order == 1){
    boxplot(data.frame(Objectives=objectives_first[[dimension]],
                       Aesthetic=aesthetic_first[[dimension]],
                       Narrative=narrative_first[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
  }else if(order == 2){
    boxplot(data.frame(Objectives=objectives_second[[dimension]],
                       Aesthetic=aesthetic_second[[dimension]],
                       Narrative=narrative_second[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
  }else if(order == 3){
    boxplot(data.frame(Objectives=objectives_third[[dimension]],
                       Aesthetic=aesthetic_third[[dimension]],
                       Narrative=narrative_third[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
  }
  mtext(dimension, line=1)
}

order_scenario_bp = function(dimension = "", y_range=c(0,28), scenario=1){
  if(scenario == 1){
    boxplot(data.frame(First=aesthetic_first[[dimension]],
                       Second=aesthetic_second[[dimension]],
                       Third=aesthetic_third[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
  }else if(order == 2){
    boxplot(data.frame(First=narrative_first[[dimension]],
                       Second=narrative_second[[dimension]],
                       Third=narrative_third[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
  }else if(order == 3){
    boxplot(data.frame(First=objectives_first[[dimension]],
                       Second=objectives_second[[dimension]],
                       Third=objectives_third[[dimension]]), ylab = "score", ylim=y_range, col=c("darkorchid1", "cadetblue1", "darkolivegreen1"))
  }
  mtext(dimension, line=1)
}