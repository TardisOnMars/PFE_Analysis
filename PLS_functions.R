library(plspm)
library(corrplot)
library(RColorBrewer)

plspm_traits_dfs = function(traits_dfs_df){
  traits_blocks = 36:40
  dfs_blocks = 86:94
  presence_blocks = 95
  traits_dfs_blocks = append(traits_blocks, dfs_blocks)
  traits_dfs_blocks = as.list(append(traits_dfs_blocks, presence_blocks))
  
  traits_modes = rep("A", 5)
  dfs_modes = rep("A", 9)
  presence_modes = "A"
  traits_dfs_modes = append(traits_modes, dfs_modes)
  traits_dfs_modes = append(traits_dfs_modes, presence_modes)
  
  traits_dfs_path = read.csv("path_inner_model.csv", header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[1:5]
  output_vars = colnames(traits_dfs_path)[6:15]
  
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

plspm_traits_dfs_value = function(traits_dfs_df){
  traits_blocks = list(15:19, 25:29, 20:24, 30:34, 10:14)
  dfs_blocks = list(seq.int(from=42, length.out=4, by=9),
                    seq.int(from=43, length.out=4, by=9),
                    seq.int(from=44, length.out=4, by=9),
                    seq.int(from=45, length.out=4, by=9),
                    seq.int(from=46, length.out=4, by=9),
                    seq.int(from=47, length.out=4, by=9),
                    seq.int(from=48, length.out=4, by=9),
                    seq.int(from=49, length.out=4, by=9),
                    seq.int(from=50, length.out=4, by=9))
  presence_blocks = list(c(78:83))
  traits_dfs_blocks = append(traits_blocks, dfs_blocks)
  traits_dfs_blocks = as.list(append(traits_dfs_blocks, presence_blocks))
  
  traits_modes = rep("A", 5)
  dfs_modes = rep("A", 9)
  presence_modes = "A"
  traits_dfs_modes = append(traits_modes, dfs_modes)
  traits_dfs_modes = append(traits_dfs_modes, presence_modes)
  
  traits_dfs_path = read.csv("path_inner_model.csv", header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[1:5]
  output_vars = colnames(traits_dfs_path)[6:15]
  
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

plspm_traits_dfs_value_adjusted = function(traits_dfs_df){
  traits_blocks = list(c(15,16,18,19), 25:29, 20:24, 30:34, 10:14)
  dfs_blocks = list(seq.int(from=42, length.out=4, by=9),
                    seq.int(from=43, length.out=4, by=9),
                    seq.int(from=44, length.out=4, by=9),
                    seq.int(from=45, length.out=4, by=9),
                    seq.int(from=46, length.out=4, by=9),
                    seq.int(from=47, length.out=4, by=9),
                    seq.int(from=48, length.out=4, by=9),
                    seq.int(from=49, length.out=4, by=9),
                    seq.int(from=50, length.out=4, by=9))
  presence_blocks = list(c(79:81, 83))
  traits_dfs_blocks = append(traits_blocks, dfs_blocks)
  traits_dfs_blocks = as.list(append(traits_dfs_blocks, presence_blocks))
  
  traits_modes = rep("A", 5)
  dfs_modes = rep("A", 9)
  presence_modes = "A"
  traits_dfs_modes = append(traits_modes, dfs_modes)
  traits_dfs_modes = append(traits_dfs_modes, presence_modes)
  
  traits_dfs_path = read.csv("path_inner_model.csv", header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[1:5]
  output_vars = colnames(traits_dfs_path)[6:15]
  
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

plspm_traits_dfs_2 = function(traits_dfs_df){
  traits_blocks = 36:40
  dfs_blocks = list(86:94)
  presence_blocks = 95
  traits_dfs_blocks = append(traits_blocks, dfs_blocks)
  traits_dfs_blocks = as.list(append(traits_dfs_blocks, presence_blocks))
  
  traits_modes = rep("A", 5)
  dfs_modes = "A"
  presence_modes = "A"
  traits_dfs_modes = append(traits_modes, dfs_modes)
  traits_dfs_modes = append(traits_dfs_modes, presence_modes)
  
  traits_dfs_path = read.csv("path_inner_model_2.csv", header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[1:5]
  output_vars = colnames(traits_dfs_path)[6:7]
  
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


plspm_traits_dfs_3 = function(traits_dfs_df){
  traits_blocks = 36:40
  dfs_blocks = c(87, 89, 90, 92, 93, 94)
  presence_blocks = 95
  traits_dfs_blocks = append(traits_blocks, dfs_blocks)
  traits_dfs_blocks = as.list(append(traits_dfs_blocks, presence_blocks))
  
  traits_modes = rep("A", 5)
  dfs_modes = rep("A", 6)
  presence_modes = "A"
  traits_dfs_modes = append(traits_modes, dfs_modes)
  traits_dfs_modes = append(traits_dfs_modes, presence_modes)
  
  traits_dfs_path = read.csv("path_inner_model_3.csv", header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[1:5]
  output_vars = colnames(traits_dfs_path)[6:12]
  
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


plspm_traits_dfs_4 = function(traits_dfs_df){
  traits_blocks = 36:40
  dfs_blocks = list(c(87, 89, 90, 92, 93, 94))
  presence_blocks = 95
  traits_dfs_blocks = append(traits_blocks, dfs_blocks)
  traits_dfs_blocks = as.list(append(traits_dfs_blocks, presence_blocks))
  
  traits_modes = rep("A", 5)
  dfs_modes = "A"
  presence_modes = "A"
  traits_dfs_modes = append(traits_modes, dfs_modes)
  traits_dfs_modes = append(traits_dfs_modes, presence_modes)
  
  traits_dfs_path = read.csv("path_inner_model_2.csv", header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[1:5]
  output_vars = colnames(traits_dfs_path)[6:7]
  
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

plspm_traits_dfs_5 = function(traits_dfs_df){
  traits_blocks = 36:40
  dfs_blocks = c(87, 88, 89, 94)
  presence_blocks = 95
  traits_dfs_blocks = append(traits_blocks, dfs_blocks)
  traits_dfs_blocks = as.list(append(traits_dfs_blocks, presence_blocks))
  
  traits_modes = rep("A", 5)
  dfs_modes = rep("A", 4)
  presence_modes = "A"
  traits_dfs_modes = append(traits_modes, dfs_modes)
  traits_dfs_modes = append(traits_dfs_modes, presence_modes)
  
  traits_dfs_path = read.csv("path_inner_model_4.csv", header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[1:5]
  output_vars = colnames(traits_dfs_path)[6:10]
  
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

plspm_traits_dfs_6 = function(traits_dfs_df){
  traits_blocks = 36:40
  dfs_blocks = list(c(87, 88, 89, 94))
  presence_blocks = 95
  traits_dfs_blocks = append(traits_blocks, dfs_blocks)
  traits_dfs_blocks = as.list(append(traits_dfs_blocks, presence_blocks))
  
  traits_modes = rep("A", 5)
  dfs_modes = "A"
  presence_modes = "A"
  traits_dfs_modes = append(traits_modes, dfs_modes)
  traits_dfs_modes = append(traits_dfs_modes, presence_modes)
  
  traits_dfs_path = read.csv("path_inner_model_2.csv", header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[1:5]
  output_vars = colnames(traits_dfs_path)[6:7]
  
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

plot_plspm_traits_dfs = function(path_coefs, p_values, significative_level = 0.05, title = "", color_lim)
{
  corrplot(path_coefs, title=title, method="number",
           sig.level = significative_level, p.mat = p_values, 
           is.corr = FALSE, cl.lim = color_lim, col=brewer.pal(n=8, name="PuOr"),
           insig = "blank",  mar=c(0,0,1,0))
}

pls_analysis = function(first_df, second_df, third_df, first_title, second_title, third_title, type=1, color_lim = c(-1, 1)){
  if(type==1){
    first_pls = plspm_traits_dfs(first_df)
    second_pls = plspm_traits_dfs(second_df)
    third_pls = plspm_traits_dfs(third_df)
  }else if(type==2){
    first_pls = plspm_traits_dfs_2(first_df)
    second_pls = plspm_traits_dfs_2(second_df)
    third_pls = plspm_traits_dfs_2(third_df)
  }else if(type==3){
    first_pls = plspm_traits_dfs_3(first_df)
    second_pls = plspm_traits_dfs_3(second_df)
    third_pls = plspm_traits_dfs_3(third_df)
  }else if(type==4){
    first_pls = plspm_traits_dfs_4(first_df)
    second_pls = plspm_traits_dfs_4(second_df)
    third_pls = plspm_traits_dfs_4(third_df)
  }else if(type==5){
    first_pls = plspm_traits_dfs_5(first_df)
    second_pls = plspm_traits_dfs_5(second_df)
    third_pls = plspm_traits_dfs_5(third_df)
  }else if(type==6){
    first_pls = plspm_traits_dfs_6(first_df)
    second_pls = plspm_traits_dfs_6(second_df)
    third_pls = plspm_traits_dfs_6(third_df)
  }else if(type==7){
    first_pls = plspm_traits_dfs_value(first_df)
    second_pls = plspm_traits_dfs_value(second_df)
    third_pls = plspm_traits_dfs_value(third_df)
  }
  
  par(mfcol = c(3,2))
  plot_plspm_traits_dfs(first_pls$path_coefs, first_pls$p_values, title = first_title, color_lim = color_lim)
  plot_plspm_traits_dfs(second_pls$path_coefs, second_pls$p_values, title = second_title, color_lim = color_lim)
  plot_plspm_traits_dfs(third_pls$path_coefs, third_pls$p_values, title = third_title, color_lim = color_lim)
  mtext("Significative level = 0.05", line=-2, side=2, outer = TRUE)
  plot_plspm_traits_dfs(first_pls$path_coefs, first_pls$p_values, significative_level = 0.1 ,title = first_title, color_lim = color_lim)
  print(first_pls$p_values)
  print(first_pls$path_coefs)
  plot_plspm_traits_dfs(second_pls$path_coefs, second_pls$p_values, significative_level = 0.1, title = second_title, color_lim = color_lim)
  print(second_pls$p_values)
  print(second_pls$path_coefs)
  plot_plspm_traits_dfs(third_pls$path_coefs, third_pls$p_values, significative_level = 0.1, title = third_title, color_lim = color_lim)
  print(third_pls$p_values)
  print(third_pls$path_coefs)
  mtext("Significative level = 0.10", line = -1, side=4, outer = TRUE)
  tab = list(first_pls, second_pls, third_pls)
  return(tab)
}

pls_analysis_check = function(plspm_analysis){
  # Unidimsensionality
  unidim_rows = rownames(plspm_analysis[["unidim"]])
  c_alpha = plspm_analysis[["unidim"]][["C.alpha"]]
  c_alpha_check = data.frame(block=unidim_rows[c_alpha<0.7], value=c_alpha[c_alpha<0.7])
  
  dg_rho = plspm_analysis[["unidim"]][["DG.rho"]]
  dg_rho_check = data.frame(block=unidim_rows[dg_rho<0.7], value=dg_rho[dg_rho<0.7])
  
  eig_f = plspm_analysis[["unidim"]][["eig.1st"]]
  eig_f_check = data.frame(block=unidim_rows[eig_f<1.0], value=eig_f[eig_f<1.0])
  
  eig_s = plspm_analysis[["unidim"]][["eig.2nd"]]
  eig_s_check = data.frame(block=unidim_rows[eig_s>1.0], value=eig_s[eig_s>1.0])
  
  unidim_checks = list(c_alpha = c_alpha_check, dg_rho = dg_rho_check, eig_f = eig_f_check, eig_s = eig_s_check)
  
  # Checking loadings and communalities
  outer_model_item = plspm_analysis[["outer_model"]][["name"]]
  outer_model_block = plspm_analysis[["outer_model"]][["block"]]
  
  loadings = plspm_analysis[["outer_model"]][["loading"]]
  loadings_check = data.frame(item=outer_model_item[loadings<0.7], block=outer_model_block[loadings<0.7], value=loadings[loadings<0.7])
  
  communalities = plspm_analysis[["outer_model"]][["communality"]]
  communalities_check = data.frame(item=outer_model_item[loadings<0.7], block=outer_model_block[loadings<0.7], value=communalities[loadings<0.7])

  outer_model_checks = list(loadings=loadings_check, communalities=communalities_check)
    
  # Coherence of crossloadings
  crossloadings = plspm_analysis[["crossloadings"]]
  crossloadings_check = NULL
  for(i in 1:length(crossloadings[[1]])){
    block = toString(crossloadings[i,2])
    block_crossloading = crossloadings[i,block]
    max_crossloading = max(crossloadings[i,3:17])
    if(block_crossloading != max_crossloading){ 
      if(is.null(crossloadings_check)){
        crossloadings_check = data.frame(item=crossloadings[i,"name"], block=crossloadings[i,"block"], value=crossloadings[i,block])
      }else{
        crossloadings_check[nrow(crossloadings_check) + 1,] = list(crossloadings[i,"name"], crossloadings[i,"block"], crossloadings[i,block])
      }
    }
  }

  # Cohesion of inner summary
  inner_summary_rows = rownames(plspm_analysis[["inner_summary"]])
  
  block_com = plspm_analysis[["inner_summary"]][["Block_Communality"]]
  block_com_check = data.frame(block=inner_summary_rows[block_com<0.5], value=block_com[block_com<0.5])
  
  ave = plspm_analysis[["inner_summary"]][["AVE"]]
  ave_check = data.frame(block=inner_summary_rows[ave<0.5], value=block_com[ave<0.5])
  
  inner_summary_checks=list(block_com=block_com_check, ave=ave_check)
  
  # Pseudo Goodness of fit
  return(list(unidim=unidim_checks, outer_model=outer_model_checks, crossloadings=crossloadings_check, inner_summary=inner_summary_checks, gof=plspm_analysis[["gof"]]))
}
