library(stringr)
library(ggpubr)
library(rstatix)
source("LoadData.R", encoding = "utf-8")

scenario_ggbp = function(dimension = "",
                         ymax = 28,
                         ylim = c(0, 32),
                         ypos = c(30, 32, 30)) {
  scenario_df = data.frame(Score = traits_dfs_df[[dimension]], Scenario = traits_dfs_df[["qu.avez.vous.vu.lors.de.l.experience.."]])
  scenario_df = mutate(scenario_df,
                       Scenario = str_replace(Scenario, "Des portails", "Aesthetic"))
  scenario_df = mutate(scenario_df, Scenario = str_replace(Scenario, "Des zombies", "Goals"))
  scenario_df = mutate(scenario_df,
                       Scenario = str_replace(Scenario, "Les recherches d'Isidore", "Narrative"))
  scenario_df = mutate(scenario_df, id = rep(1:36, each = 3))
  scenario_df = arrange(scenario_df, Scenario)
  
  scenario_comparison <-
    list(c("Aesthetic", "Narrative"),
         c("Aesthetic", "Goals"),
         c("Narrative", "Goals"))
  
  globalstat.test = friedman_test(scenario_df, Score ~ Scenario |
                                    id)
  print(globalstat.test)
  stat.test = wilcox_test(scenario_df,
                          Score ~ Scenario,
                          paired = TRUE,
                          p.adjust.method = "bonferroni")
  print(stat.test)
  
  print(summary(filter(scenario_df, Scenario=="Aesthetic")))
  print(summary(filter(scenario_df, Scenario=="Goals")))
  print(summary(filter(scenario_df, Scenario=="Narrative")))
  
  return(
    ggboxplot(
      scenario_df,
      x = "Scenario",
      y = "Score",
      fill = "Scenario",
      ylim = ylim
    ) +
      theme(legend.position = "none",
            axis.title.x = element_blank()) +
      stat_pvalue_manual(
        stat.test,
        label = "{p.adj} {p.adj.signif}",
        label.size = 3,
        y.position = ypos,
        bracket.shorten = 0.05
      ) +
      labs(subtitle = get_test_label(globalstat.test)) +
      geom_hline(
        yintercept = ymax,
        color = "black",
        linetype = "dashed"
      ) +
      stat_summary(
        geom = "point",
        fun = "mean",
        col = "red",
        size = 5,
        shape = 20,
      ) +
      ggplot2::annotate(
        "text",
        0.65,
        1.03 * ymax,
        label = paste("Max = ", ymax),
        color = "black",
        size = 2
      )
  )
}

order_ggbp = function(dimension = "",
                      ymax = 28,
                      ylim = c(0, 32),
                      ypos = c(30, 32, 30)) {
  order_df = data.frame(Score = traits_dfs_df[[dimension]], Order = traits_dfs_df[["est.ce.votre.premiere..deuxieme.ou.troisieme.experience.."]])
  order_df = mutate(order_df, Order = str_replace(Order, "Première", "First"))
  order_df = mutate(order_df, Order = str_replace(Order, "Deuxième", "Second"))
  order_df = mutate(order_df, Order = str_replace(Order, "Troisième", "Third"))
  order_df = mutate(order_df, id = rep(1:36, each = 3))
  order_df = arrange(order_df, Order)
  
  order_comparison <-
    list(c("First", "Second"),
         c("First", "Third"),
         c("Second", "Third"))
  
  globalstat.test = friedman_test(order_df, Score ~ Order | id)
  print(globalstat.test)
  stat.test = wilcox_test(order_df,
                          Score ~ Order,
                          paired = TRUE,
                          p.adjust.method = "bonferroni")
  print(stat.test)
  
  print(summary(filter(order_df, Order=="First")))
  print(summary(filter(order_df, Order=="Second")))
  print(summary(filter(order_df, Order=="Third")))
  
  return(
    ggboxplot(
      order_df,
      x = "Order",
      y = "Score",
      fill = "Order",
      ylim = ylim
    ) +
      theme(legend.position = "none",
            axis.title.x = element_blank()) +
      stat_pvalue_manual(
        stat.test,
        label = "{p.adj} {p.adj.signif}",
        label.size = 3,
        y.position = ypos,
        bracket.shorten = 0.05
      ) +
      labs(subtitle = get_test_label(globalstat.test)) +
      geom_hline(
        yintercept = ymax,
        color = "black",
        linetype = "dashed"
      ) +
      stat_summary(
        geom = "point",
        fun = "mean",
        col = "red",
        size = 5,
        shape = 20,
      ) +
      ggplot2::annotate(
        "text",
        0.65,
        1.03 * ymax,
        label = paste("Max = ", ymax),
        color = "black",
        size = 2
      )
  )
}

scenario_order_ggbp = function(dimension = "",
                               order = 1,
                               ymax = 28,
                               ylim = c(0, 32),
                               ypos = c(30, 32, 30)) {
  scenario_order_df = data.frame(Score = traits_dfs_df[[dimension]],
                                 Scenario = traits_dfs_df[["qu.avez.vous.vu.lors.de.l.experience.."]],
                                 Order = traits_dfs_df[["est.ce.votre.premiere..deuxieme.ou.troisieme.experience.."]])
  scenario_order_df = mutate(scenario_order_df,
                             Scenario = str_replace(Scenario, "Des portails", "Aesthetic"))
  scenario_order_df = mutate(scenario_order_df,
                             Scenario = str_replace(Scenario, "Des zombies", "Goals"))
  scenario_order_df = mutate(scenario_order_df,
                             Scenario = str_replace(Scenario, "Les recherches d'Isidore", "Narrative"))
  scenario_order_df = mutate(scenario_order_df, Order = str_replace(Order, "Première", "First"))
  scenario_order_df = mutate(scenario_order_df, Order = str_replace(Order, "Deuxième", "Second"))
  scenario_order_df = mutate(scenario_order_df, Order = str_replace(Order, "Troisième", "Third"))
  scenario_order_df = arrange(scenario_order_df, Scenario)
  
  scenario_comparison <-
    list(c("Aesthetic", "Narrative"),
         c("Aesthetic", "Goals"),
         c("Narrative", "Goals"))
  
  if (order == 1) {
    scenario_order_df <- filter(scenario_order_df, Order == "First")
  } else if (order == 2) {
    scenario_order_df <- filter(scenario_order_df, Order == "Second")
  } else if (order == 3) {
    scenario_order_df <- filter(scenario_order_df, Order == "Third")
  }
  
  globalstat.test = kruskal_test(scenario_order_df, Score ~ Scenario)
  print(globalstat.test)
  stat.test = wilcox_test(scenario_order_df, Score ~ Scenario, p.adjust.method = "bonferroni")
  print(stat.test)
  
  return(
    ggboxplot(
      scenario_order_df,
      x = "Scenario",
      y = "Score",
      fill = "Scenario",
      ylim = ylim
    ) +
      theme(legend.position = "none",
            axis.title.x = element_blank()) +
      stat_pvalue_manual(
        stat.test,
        label = "{p.adj} {p.adj.signif}",
        label.size = 3,
        y.position = ypos,
        bracket.shorten = 0.05
      ) +
      labs(subtitle = get_test_label(globalstat.test)) +
      geom_hline(
        yintercept = ymax,
        color = "black",
        linetype = "dashed"
      ) +
      stat_summary(
        geom = "point",
        fun = "mean",
        col = "red",
        size = 5,
        shape = 20,
      ) +
      ggplot2::annotate(
        "text",
        0.65,
        1.03 * ymax,
        label = paste("Max = ", ymax),
        color = "black",
        size = 2
      )
  )
}

order_scenario_ggbp = function(dimension = "",
                               scenario = "",
                               ymax = 28,
                               ylim = c(0, 32),
                               ypos = c(30, 32, 30)) {
  order_scenario_df = data.frame(Score = traits_dfs_df[[dimension]],
                                 Scenario = traits_dfs_df[["qu.avez.vous.vu.lors.de.l.experience.."]],
                                 Order = traits_dfs_df[["est.ce.votre.premiere..deuxieme.ou.troisieme.experience.."]])
  order_scenario_df = mutate(order_scenario_df,
                             Scenario = str_replace(Scenario, "Des portails", "Aesthetic"))
  order_scenario_df = mutate(order_scenario_df,
                             Scenario = str_replace(Scenario, "Des zombies", "Goals"))
  order_scenario_df = mutate(order_scenario_df,
                             Scenario = str_replace(Scenario, "Les recherches d'Isidore", "Narrative"))
  order_scenario_df = mutate(order_scenario_df, Order = str_replace(Order, "Première", "First"))
  order_scenario_df = mutate(order_scenario_df, Order = str_replace(Order, "Deuxième", "Second"))
  order_scenario_df = mutate(order_scenario_df, Order = str_replace(Order, "Troisième", "Third"))
  order_scenario_df = arrange(order_scenario_df, Order)
  
  if (scenario == "aest") {
    order_scenario_df = filter(order_scenario_df, Scenario == "Aesthetic")
  } else if (scenario == "narr") {
    order_scenario_df = filter(order_scenario_df, Scenario == "Narrative")
  } else if (scenario == "goal") {
    order_scenario_df = filter(order_scenario_df, Scenario == "Goals")
  }
  
  globalstat.test = kruskal_test(order_scenario_df, Score ~ Order)
  print(globalstat.test)
  stat.test = wilcox_test(order_scenario_df, Score ~ Order, p.adjust.method = "bonferroni")
  print(stat.test)
  
  return(
    ggboxplot(
      order_scenario_df,
      x = "Order",
      y = "Score",
      fill = "Order",
      ylim = ylim
    ) +
      theme(legend.position = "none",
            axis.title.x = element_blank()) +
      stat_pvalue_manual(
        stat.test,
        label = "{p.adj} {p.adj.signif}",
        label.size = 3,
        y.position = ypos,
        bracket.shorten = 0.05
      ) +
      labs(subtitle = get_test_label(globalstat.test)) +
      geom_hline(
        yintercept = ymax,
        color = "black",
        linetype = "dashed"
      ) +
      stat_summary(
        geom = "point",
        fun = "mean",
        col = "red",
        size = 5,
        shape = 20,
      ) +
      ggplot2::annotate(
        "text",
        0.6,
        1.03 * ymax,
        label = paste("Max = ", ymax),
        color = "black",
        size = 2
      )
  )
}

site_bp = function(dimension = "",
                   y_range = c(0, 28)) {
  sumaov <-
    summary.aov(
      aov(
        traits_dfs_df[[dimension]] ~ sur.quel.site.allez.vous.participer.a.l.experimentation..,
        data = traits_dfs_df
      )
    )
  if (round(sumaov[[1]][[1, "Pr(>F)"]] < 1.0)) {
    par(mfrow = c(1, 2))
    boxplot(enise_df[[dimension]],
            ylim = y_range,
            data = enise_df,
            xlab = "ENISE")
    boxplot(insa_df[[dimension]],
            ylim = y_range,
            data = insa_df,
            xlab = "INSA")
    mtext(paste(str_to_title(dimension), "\n Depending on site"),
          line = -3,
          outer = TRUE)
    mtext(
      paste("P-Value of ANOVA", round(sumaov[[1]][[1, "Pr(>F)"]], digits = 3)),
      side = 1,
      line = -3,
      outer = TRUE
    )
  }
}

game_bp = function(dimension = "",
                   y_range = c(0, 28)) {
  sumaov <-
    summary.aov(
      aov(
        traits_dfs_df[[dimension]] ~ a.quelle.frequence.jouez.vous.aux.jeux.videos..,
        data = traits_dfs_df
      )
    )
  if (round(sumaov[[1]][[1, "Pr(>F)"]] < 1.0)) {
    par(mfrow = c(1, 4))
    boxplot(nevergame_bf[[dimension]],
            ylim = y_range,
            data = enise_df,
            xlab = "Never")
    boxplot(lowgame_bf[[dimension]],
            ylim = y_range,
            data = insa_df,
            xlab = "Occasionally")
    boxplot(midgame_bf[[dimension]],
            ylim = y_range,
            data = insa_df,
            xlab = "Regularly")
    boxplot(highgame_bf[[dimension]],
            ylim = y_range,
            data = insa_df,
            xlab = "Everyday")
    mtext(
      paste(
        str_to_title(dimension),
        "\n Depending on video game frequency"
      ),
      line = -3,
      outer = TRUE
    )
    mtext(
      paste("P-Value of ANOVA", round(sumaov[[1]][[1, "Pr(>F)"]], digits = 3)),
      side = 1,
      line = -3,
      outer = TRUE
    )
  }
}

dimension_corr = function(scenario, scenario_df) {
  print(paste("Correlation between dimensions in scenario : ", scenario))
  
  corr_matrix = rcorr(as.matrix(
    data.frame(
      challenge_skill_balance = scenario_df[["challenge_skill_balance"]],
      action_awareness = scenario_df[["action_awareness"]],
      clear_goals = scenario_df[["clear_goals"]],
      unambiguous_feedback = scenario_df[["unambiguous_feedback"]],
      concentration = scenario_df[["concentration"]],
      sense_of_control = scenario_df[["sense_of_control"]],
      loss_self_consciousness = scenario_df[["loss_self_consciousness"]],
      time_transformation = scenario_df[["time_transformation"]],
      autotelic_experience = scenario_df[["autotelic_experience"]],
      engagement = scenario_df[["engagement"]]
    )
  ))
  return(corr_matrix)
}

save_ggbp = function(ggbp,
                     condition = "",
                     width = 750,
                     height = 1000) {
  name = paste('./boxplots/bp_', condition, ".png", sep = "")
  png(
    filename = name,
    width = width,
    height = height,
    res = 250
  )
  print(ggbp)
  dev.off()
}