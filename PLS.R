library(plspm)
library(corrplot)
library(RColorBrewer)
source("LoadData.R",encoding="utf-8")

plspm_traits_dfs = function(traits_join_dfs_df, significative_level = 0.05){
  traits_blocks = list(15:19, 25:29, 20:24, 30:34, 10:14)
  dfs_blocks = list()
  for(i in 42:50){
    dfs_blocks[[length(dfs_blocks)+1]] = seq.int(from=i, length.out=4, by=9)
  }
  traits_dfs_blocks = append(traits_blocks, dfs_blocks)
  
  traits_modes = rep("A", 5)
  dfs_modes = rep("A", 9)
  traits_dfs_modes = append(traits_modes, dfs_modes)
  
  traits_dfs_path = read.csv("path_inner_model.csv", header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_join_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[1:5]
  output_vars = colnames(traits_dfs_path)[6:14]
  
  path_coefs = traits_dfs_pls$path_coefs[output_vars, input_vars]
  
  p_values = matrix(nrow=length(input_vars), ncol=length(output_vars))
  rownames(p_values) = input_vars
  colnames(p_values) = output_vars
  p_values = data.frame(p_values)
  
  for(var in output_vars){
    p_values[,var] = traits_dfs_pls$inner_model[[var]][2:(length(input_vars)+1),4]
  }
  
  p_values = t(p_values)
  
  results <- list("path_coefs" = path_coefs, "p_values" = p_values, "traits_dfs_pls" = traits_dfs_pls)
  
  return(results)
}

plspm_traits_engagement = function(traits_join_dfs_df, significative_level = 0.05){
  traits_blocks = list(15:19, 25:29, 20:24, 30:34, 10:14)
  eng_blocks = list(77:83)
  traits_eng_blocks = append(traits_blocks, eng_blocks)
  
  traits_modes = rep("A", 5)
  eng_modes = rep("A", 1)
  traits_eng_modes = append(traits_modes, eng_modes)
  
  traits_eng_path = read.csv("path_inner_model_eng.csv", header=TRUE, sep=";", row.names = 1)
  traits_eng_path = as.matrix(traits_eng_path)
  rownames(traits_eng_path) = make.names(rownames(traits_eng_path))
  colnames(traits_eng_path) = rownames(traits_eng_path)
  
  traits_eng_pls = plspm(traits_join_dfs_df, traits_eng_path, traits_eng_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_eng_path)[1:5]
  output_vars = c(colnames(traits_eng_path)[6])
  
  path_coefs =  as.data.frame(traits_eng_pls$path_coefs)
  path_coefs = filter(path_coefs, Aesthetic != 0)[1:5]
  
  p_values = matrix(nrow=length(input_vars), ncol=length(output_vars))
  rownames(p_values) = input_vars
  colnames(p_values) = output_vars
  p_values = data.frame(p_values)
  
  for(var in output_vars){
    p_values[,var] = traits_eng_pls$inner_model[[var]][2:(length(input_vars)+1),4]
  }
  
  p_values = t(p_values)
  
  results <- list("path_coefs" = as.matrix(path_coefs), "p_values" = p_values, "traits_eng_pls" = traits_eng_pls)
  
  return(results)
}

plot_plspm_traits_dfs = function(path_coefs, p_values, significative_level = 0.05, title = "")
{
  corrplot(path_coefs, title=title, 
           sig.level = significative_level, p.mat = p_values, 
           is.corr = FALSE, cl.lim=c(-1.0, 1.0), col=brewer.pal(n=8, name="PuOr"),
           insig = "blank",  mar=c(0,0,2,0))
}

aesthetic_dfs = plspm_traits_dfs(traits_join_DFS_Aesthetic)
narrative_dfs = plspm_traits_dfs(traits_join_DFS_Narrative)
objective_dfs = plspm_traits_dfs(traits_join_DFS_Objective)

par(mfrow = c(1,3))
plot_plspm_traits_dfs(aesthetic_dfs$path_coefs, aesthetic_dfs$p_values, title = "Aesthetic scenario")
plot_plspm_traits_dfs(narrative_dfs$path_coefs, narrative_dfs$p_values, title = "Narrative scenario")
plot_plspm_traits_dfs(objective_dfs$path_coefs, objective_dfs$p_values, title = "Objectives scenario")
mtext("Significative level = 0.05", line=-6, outer = TRUE)


par(mfrow=c(1,3))
plot_plspm_traits_dfs(aesthetic_dfs$path_coefs, aesthetic_dfs$p_values, significative_level = 0.1 ,title = "Aesthetic scenario")
plot_plspm_traits_dfs(narrative_dfs$path_coefs, narrative_dfs$p_values, significative_level = 0.1, title = "Narrative scenario")
plot_plspm_traits_dfs(objective_dfs$path_coefs, objective_dfs$p_values, significative_level = 0.1, title = "Objectives scenario")
mtext("Significative level = 0.10", line = -6, outer = TRUE)


aesthetic_eng = plspm_traits_engagement(traits_join_DFS_Aesthetic)
narrative_eng = plspm_traits_engagement(traits_join_DFS_Narrative)
objective_eng = plspm_traits_engagement(traits_join_DFS_Objective)

par(mfrow = c(1,3))
plot_plspm_traits_dfs(aesthetic_eng$path_coefs, aesthetic_eng$p_values, title = "Aesthetic scenario")
plot_plspm_traits_dfs(narrative_eng$path_coefs, narrative_eng$p_values, title = "Narrative scenario")
plot_plspm_traits_dfs(objective_eng$path_coefs, objective_eng$p_values, title = "Objectives scenario")
mtext("Significative level = 0.05", line=-6, outer = TRUE)


par(mfrow=c(1,3))
plot_plspm_traits_dfs(aesthetic_eng$path_coefs, aesthetic_eng$p_values, significative_level = 0.1 ,title = "Aesthetic scenario")
plot_plspm_traits_dfs(narrative_eng$path_coefs, narrative_eng$p_values, significative_level = 0.1, title = "Narrative scenario")
plot_plspm_traits_dfs(objective_eng$path_coefs, objective_eng$p_values, significative_level = 0.1, title = "Objectives scenario")
mtext("Significative level = 0.10", line = -6, outer = TRUE)