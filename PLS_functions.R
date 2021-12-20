library(plspm)
library(corrplot)
library(RColorBrewer)

model_path = "./model/"

#PLSPM precalculted mean
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
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model.csv", sep=''), header=TRUE, sep=";", row.names = 1)
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
  ### CODE TO COMPUTE MEAN AT RUNTIME, EASIER TO MODIFY
  # traits_values_df = data.frame(traits_dfs_df[15:19], traits_dfs_df[25:29], traits_dfs_df[20:24], traits_dfs_df[30:34], traits_dfs_df[10:14])
  # 
  # flow_values_df = data.frame(traits_dfs_df[seq.int(from=42, length.out=4, by=9)],
  #                             traits_dfs_df[seq.int(from=43, length.out=4, by=9)],
  #                             traits_dfs_df[seq.int(from=44, length.out=4, by=9)],
  #                             traits_dfs_df[seq.int(from=45, length.out=4, by=9)],
  #                             traits_dfs_df[seq.int(from=46, length.out=4, by=9)],
  #                             traits_dfs_df[seq.int(from=47, length.out=4, by=9)],
  #                             traits_dfs_df[seq.int(from=48, length.out=4, by=9)],
  #                             traits_dfs_df[seq.int(from=49, length.out=4, by=9)],
  #                             traits_dfs_df[seq.int(from=50, length.out=4, by=9)])
  ###
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
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model.csv", sep=''), header=TRUE, sep=";", row.names = 1)
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
  traits_blocks = list(25:29, 20:24, 30:34, 10:14)
  dfs_blocks = list(seq.int(from=42, length.out=4, by=9),
                    seq.int(from=43, length.out=4, by=9),
                    seq.int(from=44, length.out=4, by=9),
                    seq.int(from=45, length.out=4, by=9),
                    seq.int(from=46, length.out=4, by=9),
                    seq.int(from=47, length.out=4, by=9),
                    seq.int(from=48, length.out=4, by=9),
                    seq.int(from=49, length.out=4, by=9),
                    seq.int(from=50, length.out=4, by=9))
  traits_dfs_blocks = as.list(append(traits_blocks, dfs_blocks))
  
  traits_modes = rep("A", 4)
  dfs_modes = rep("A", 9)
  traits_dfs_modes = append(traits_modes, dfs_modes)
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_adjusted.csv", sep=''), header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[1:4]
  output_vars = colnames(traits_dfs_path)[5:13]
  
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

plspm_traits_dfs_value_adjusted_2 = function(traits_dfs_df){
  
  traits_blocks = list(c(15,16,19), 25:29, 20:24, 30:33, 10:14)
  dfs_blocks = list(seq.int(from=42, length.out=4, by=9),
                    seq.int(from=43, length.out=4, by=9),
                    seq.int(from=44, length.out=4, by=9),
                    seq.int(from=45, length.out=4, by=9),
                    seq.int(from=46, length.out=4, by=9),
                    seq.int(from=47, length.out=4, by=9),
                    seq.int(from=48, length.out=4, by=9),
                    seq.int(from=49, length.out=4, by=9),
                    seq.int(from=50, length.out=4, by=9))

  traits_dfs_blocks = as.list(append(traits_blocks, dfs_blocks))
  
  traits_modes = rep("A", 5)
  dfs_modes = rep("A", 9)
  traits_dfs_modes = append(traits_modes, dfs_modes)
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_adjusted_eng.csv", sep=''), header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
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

plspm_traits_dfs_value_adjusted_3 = function(traits_dfs_df){
  traits_blocks = list(c(25,27,28,29), c(20,22,23,24), 30:33, 10:13)
  dfs_blocks = list(seq.int(from=42, length.out=4, by=9),
                    seq.int(from=43, length.out=4, by=9),
                    seq.int(from=44, length.out=4, by=9),
                    seq.int(from=45, length.out=4, by=9),
                    seq.int(from=46, length.out=4, by=9),
                    seq.int(from=47, length.out=4, by=9),
                    seq.int(from=48, length.out=4, by=9),
                    seq.int(from=49, length.out=4, by=9),
                    seq.int(from=50, length.out=4, by=9))
  traits_dfs_blocks = append(traits_blocks, dfs_blocks)
  
  traits_modes = rep("A", 4)
  dfs_modes = rep("A", 9)
  traits_dfs_modes = append(traits_modes, dfs_modes)
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_adjusted_aest_eng.csv", sep=''), header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[1:4]
  output_vars = colnames(traits_dfs_path)[5:13]
  
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

pls_traits = function(traits_dfs_df){
  traits_blocks = list(15:19, 25:29, 20:24, 30:34, 10:14)
  
  traits_modes = rep("A", 5)
  
  Aest = c(1,0,0,0,0)
  Chal = c(1,1,0,0,0)
  Narr = c(1,1,1,0,0)
  Goal = c(1,1,1,1,0)
  Soci = c(1,1,1,1,1)
  
  traits_path = rbind(Aest, Chal, Narr, Goal, Soci)
  
  traits_pls = plspm(Data=traits_dfs_df, path_matrix=traits_path, blocks=traits_blocks, modes=traits_modes, scaled=FALSE)
  
  return(traits_pls)
}

pls_dfs = function(traits_dfs_df){
  dfs_blocks = list(seq.int(from=42, length.out=4, by=9),
                    seq.int(from=43, length.out=4, by=9),
                    seq.int(from=44, length.out=4, by=9),
                    seq.int(from=45, length.out=4, by=9),
                    seq.int(from=46, length.out=4, by=9),
                    seq.int(from=47, length.out=4, by=9),
                    seq.int(from=48, length.out=4, by=9),
                    seq.int(from=49, length.out=4, by=9),
                    seq.int(from=50, length.out=4, by=9))
  
  dfs_modes = rep("A", 9)
  
  csb = c(1,0,0,0,0,0,0,0,0)
  aa = c(1,1,0,0,0,0,0,0,0)
  cg = c(1,1,1,0,0,0,0,0,0)
  uf = c(1,1,1,1,0,0,0,0,0)
  cnt = c(1,1,1,1,1,0,0,0,0)
  soc = c(1,1,1,1,1,1,0,0,0)
  lsc = c(1,1,1,1,1,1,1,0,0)
  tt = c(1,1,1,1,1,1,1,1,0)
  ae = c(1,1,1,1,1,1,1,1,1)
  
  dfs_path = rbind(csb, aa, cg, uf, cnt, soc, lsc, tt, ae)
  
  dfs_pls = plspm(Data=traits_dfs_df, path_matrix=dfs_path, blocks=dfs_blocks, modes=dfs_modes, scaled=FALSE)
  
  return(dfs_pls)
}

pls_dfs_eng = function(traits_dfs_df){
  dfs_eng_blocks = list(seq.int(from=42, length.out=4, by=9),
                    seq.int(from=43, length.out=4, by=9),
                    seq.int(from=44, length.out=4, by=9),
                    seq.int(from=45, length.out=4, by=9),
                    seq.int(from=46, length.out=4, by=9),
                    seq.int(from=47, length.out=4, by=9),
                    seq.int(from=48, length.out=4, by=9),
                    seq.int(from=49, length.out=4, by=9),
                    seq.int(from=50, length.out=4, by=9),
                    c(78:81),
                    c(82,83))
  
  dfs_eng_modes = rep("A", 11)
  
  csb   = c(1,0,0,0,0,0,0,0,0,0,0)
  aa    = c(1,1,0,0,0,0,0,0,0,0,0)
  cg    = c(1,1,1,0,0,0,0,0,0,0,0)
  uf    = c(1,1,1,1,0,0,0,0,0,0,0)
  cnt   = c(1,1,1,1,1,0,0,0,0,0,0)
  soc   = c(1,1,1,1,1,1,0,0,0,0,0)
  lsc   = c(1,1,1,1,1,1,1,0,0,0,0)
  tt    = c(1,1,1,1,1,1,1,1,0,0,0)
  ae    = c(1,1,1,1,1,1,1,1,1,0,0)
  eng_1 = c(1,1,1,1,1,1,1,1,1,1,0)
  eng_2 = c(1,1,1,1,1,1,1,1,1,1,1)
  
  dfs_eng_path = rbind(csb, aa, cg, uf, cnt, soc, lsc, tt, ae, eng_1, eng_2)
  
  dfs_eng_pls = plspm(Data=traits_dfs_df, path_matrix=dfs_eng_path, blocks=dfs_eng_blocks, modes=dfs_eng_modes, scaled=FALSE)
  
  return(dfs_eng_pls)
}

pls_traits_dfs = function(traits_dfs_df){
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
  
  traits_modes = rep("A", 5)
  
  dfs_modes = rep("A", 9)
  
  Aest = c(1,0,0,0,0)
  Chal = c(1,1,0,0,0)
  Narr = c(1,1,1,0,0)
  Goal = c(1,1,1,1,0)
  Soci = c(1,1,1,1,1)
  
  csb = c(1,0,0,0,0,0,0,0,0)
  aa = c(1,1,0,0,0,0,0,0,0)
  cg = c(1,1,1,0,0,0,0,0,0)
  uf = c(1,1,1,1,0,0,0,0,0)
  cnt = c(1,1,1,1,1,0,0,0,0)
  soc = c(1,1,1,1,1,1,0,0,0)
  lsc = c(1,1,1,1,1,1,1,0,0)
  tt = c(1,1,1,1,1,1,1,1,0)
  ae = c(1,1,1,1,1,1,1,1,1)
  
  dfs_path = rbind(Aest, Chal, Narr, Goal, Soci, csb, aa, cg, uf, cnt, soc, lsc, tt, ae)
  
  traits_pls = plspm(Data=traits_dfs_df, path_matrix=traits_path, blocks=traits_blocks, modes=traits_modes, scaled=FALSE)
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
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_2.csv", sep=''), header=TRUE, sep=";", row.names = 1)
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
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_3.csv", sep=''), header=TRUE, sep=";", row.names = 1)
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
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_2.csv", sep=''), header=TRUE, sep=";", row.names = 1)
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
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_4.csv", sep=''), header=TRUE, sep=";", row.names = 1)
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
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_2.csv", sep=''), header=TRUE, sep=";", row.names = 1)
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

# PLSPM Adjusted Without Aesthetic and Engagement
plspm_traits_dfs_adjusted_aest_eng = function(traits_dfs_df){
  traits_blocks = 37:40
  dfs_blocks = 86:94
  traits_dfs_blocks = as.list(append(traits_blocks, dfs_blocks))
  
  traits_modes = rep("A", 4)
  dfs_modes = rep("A", 9)
  traits_dfs_modes = append(traits_modes, dfs_modes)
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_adjusted_aest_eng.csv", sep=''), header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[1:4]
  output_vars = colnames(traits_dfs_path)[5:13]
  
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

# PLSPM Without Engagement and Adjustments on Aesthetic
plspm_traits_dfs_adjusted_aest_eng_2 = function(traits_dfs_df){
  traits_blocks = 36:40
  dfs_blocks = 86:94
  traits_dfs_blocks = as.list(append(traits_blocks, dfs_blocks))
  
  traits_modes = rep("A", 4)
  dfs_modes = rep("A", 9)
  traits_dfs_modes = append(traits_modes, dfs_modes)
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_adjusted_aest_eng.csv", sep=''), header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[1:4]
  output_vars = colnames(traits_dfs_path)[5:13]
  
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

plspm_traits_dfs_mean = function(traits_dfs_df){
  traits_mean_df = data.frame(transmute(rowwise(traits_dfs_df), aesthetic = mean(c_across(15:19))),
             transmute(rowwise(traits_dfs_df), challenge = mean(c_across(25:29))),
             transmute(rowwise(traits_dfs_df), narrative = mean(c_across(20:24))),
             transmute(rowwise(traits_dfs_df), goals = mean(c_across(30:34))),
             transmute(rowwise(traits_dfs_df), social = mean(c_across(10:14))))
  
  flow_mean_df = data.frame(transmute(rowwise(traits_dfs_df), challenge.skill.balance = mean(c_across(seq.int(from=42, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), action.awareness = mean(c_across(seq.int(from=43, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), clear.goals = mean(c_across(seq.int(from=44, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), unambiguous.feedback = mean(c_across(seq.int(from=45, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), concentration = mean(c_across(seq.int(from=46, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), sense.of.control = mean(c_across(seq.int(from=47, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), loss.of.self.consciousness = mean(c_across(seq.int(from=48, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), time.transformation = mean(c_across(seq.int(from=49, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), autotelic.experience = mean(c_across(seq.int(from=50, length.out=4, by=9)))))
  
  traits_dfs_df = data.frame(traits_mean_df, flow_mean_df)
  
  traits_blocks = 1:5
  dfs_blocks = 6:14
  traits_dfs_blocks = as.list(append(traits_blocks, dfs_blocks))
  
  traits_modes = rep("A", 5)
  dfs_modes = rep("A", 9)

  traits_dfs_modes = append(traits_modes, dfs_modes)
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_adjusted_eng.csv", sep=''), header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
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

plspm_traits_dfs_mean_adj_aest = function(traits_dfs_df){
  traits_mean_df = data.frame(transmute(rowwise(traits_dfs_df), challenge = mean(c_across(25:29))),
                              transmute(rowwise(traits_dfs_df), narrative = mean(c_across(20:24))),
                              transmute(rowwise(traits_dfs_df), goals = mean(c_across(30:34))),
                              transmute(rowwise(traits_dfs_df), social = mean(c_across(10:14))))
  
  flow_mean_df = data.frame(transmute(rowwise(traits_dfs_df), challenge.skill.balance = mean(c_across(seq.int(from=42, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), action.awareness = mean(c_across(seq.int(from=43, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), clear.goals = mean(c_across(seq.int(from=44, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), unambiguous.feedback = mean(c_across(seq.int(from=45, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), concentration = mean(c_across(seq.int(from=46, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), sense.of.control = mean(c_across(seq.int(from=47, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), loss.of.self.consciousness = mean(c_across(seq.int(from=48, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), time.transformation = mean(c_across(seq.int(from=49, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), autotelic.experience = mean(c_across(seq.int(from=50, length.out=4, by=9)))))
  
  traits_dfs_df = data.frame(traits_mean_df, flow_mean_df)
  
  traits_blocks = 1:4
  dfs_blocks = 5:13
  traits_dfs_blocks = as.list(append(traits_blocks, dfs_blocks))
  
  traits_modes = rep("A", 4)
  dfs_modes = rep("A", 9)
  
  traits_dfs_modes = append(traits_modes, dfs_modes)
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_adjusted_aest_eng.csv", sep=''), header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[traits_blocks]
  output_vars = colnames(traits_dfs_path)[dfs_blocks]
  
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

plspm_traits_dfs_mean_adj_part_aest_1 = function(traits_dfs_df){
  traits_mean_df = data.frame(transmute(rowwise(traits_dfs_df), aesthetic = mean(c_across(c(15,16,17,19)))),
                              transmute(rowwise(traits_dfs_df), challenge = mean(c_across(25:29))),
                              transmute(rowwise(traits_dfs_df), narrative = mean(c_across(20:24))),
                              transmute(rowwise(traits_dfs_df), goals = mean(c_across(30:34))),
                              transmute(rowwise(traits_dfs_df), social = mean(c_across(10:14))))
  
  flow_mean_df = data.frame(transmute(rowwise(traits_dfs_df), challenge.skill.balance = mean(c_across(seq.int(from=42, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), action.awareness = mean(c_across(seq.int(from=43, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), clear.goals = mean(c_across(seq.int(from=44, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), unambiguous.feedback = mean(c_across(seq.int(from=45, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), concentration = mean(c_across(seq.int(from=46, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), sense.of.control = mean(c_across(seq.int(from=47, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), loss.of.self.consciousness = mean(c_across(seq.int(from=48, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), time.transformation = mean(c_across(seq.int(from=49, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), autotelic.experience = mean(c_across(seq.int(from=50, length.out=4, by=9)))))
  
  traits_dfs_df = data.frame(traits_mean_df, flow_mean_df)
  
  traits_blocks = 1:5
  dfs_blocks = 6:14
  traits_dfs_blocks = as.list(append(traits_blocks, dfs_blocks))
  
  traits_modes = rep("A", 5)
  dfs_modes = rep("A", 9)
  
  traits_dfs_modes = append(traits_modes, dfs_modes)
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_adjusted_eng.csv", sep=''), header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
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

plspm_traits_dfs_mean_adj_part_aest_2 = function(traits_dfs_df){
  traits_mean_df = data.frame(transmute(rowwise(traits_dfs_df), aesthetic = mean(c_across(c(15,16,19)))),
                              transmute(rowwise(traits_dfs_df), challenge = mean(c_across(25:29))),
                              transmute(rowwise(traits_dfs_df), narrative = mean(c_across(20:24))),
                              transmute(rowwise(traits_dfs_df), goals = mean(c_across(30:34))),
                              transmute(rowwise(traits_dfs_df), social = mean(c_across(10:14))))
  
  flow_mean_df = data.frame(transmute(rowwise(traits_dfs_df), challenge.skill.balance = mean(c_across(seq.int(from=42, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), action.awareness = mean(c_across(seq.int(from=43, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), clear.goals = mean(c_across(seq.int(from=44, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), unambiguous.feedback = mean(c_across(seq.int(from=45, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), concentration = mean(c_across(seq.int(from=46, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), sense.of.control = mean(c_across(seq.int(from=47, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), loss.of.self.consciousness = mean(c_across(seq.int(from=48, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), time.transformation = mean(c_across(seq.int(from=49, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), autotelic.experience = mean(c_across(seq.int(from=50, length.out=4, by=9)))))
  
  traits_dfs_df = data.frame(traits_mean_df, flow_mean_df)
  
  traits_blocks = 1:5
  dfs_blocks = 6:14
  traits_dfs_blocks = as.list(append(traits_blocks, dfs_blocks))
  
  traits_modes = rep("A", 5)
  dfs_modes = rep("A", 9)
  
  traits_dfs_modes = append(traits_modes, dfs_modes)
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_adjusted_eng.csv", sep=''), header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
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

plspm_traits_dfs_mean_adj_part_all = function(traits_dfs_df){
  traits_mean_df = data.frame(transmute(rowwise(traits_dfs_df), aesthetic = mean(c_across(c(15,16,19)))),
                              transmute(rowwise(traits_dfs_df), challenge = mean(c_across(c(25,27,28,29)))),
                              transmute(rowwise(traits_dfs_df), narrative = mean(c_across(c(20,22,23,24)))),
                              transmute(rowwise(traits_dfs_df), goals = mean(c_across(30:33))),
                              transmute(rowwise(traits_dfs_df), social = mean(c_across(10:13))))
  
  flow_mean_df = data.frame(transmute(rowwise(traits_dfs_df), challenge.skill.balance = mean(c_across(seq.int(from=42, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), action.awareness = mean(c_across(seq.int(from=43, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), clear.goals = mean(c_across(seq.int(from=44, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), unambiguous.feedback = mean(c_across(seq.int(from=45, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), concentration = mean(c_across(seq.int(from=46, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), sense.of.control = mean(c_across(seq.int(from=47, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), loss.of.self.consciousness = mean(c_across(seq.int(from=48, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), time.transformation = mean(c_across(seq.int(from=49, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), autotelic.experience = mean(c_across(seq.int(from=50, length.out=4, by=9)))))
  
  traits_dfs_df = data.frame(traits_mean_df, flow_mean_df)
  
  traits_blocks = 1:5
  dfs_blocks = 6:14
  traits_dfs_blocks = as.list(append(traits_blocks, dfs_blocks))
  
  traits_modes = rep("A", 5)
  dfs_modes = rep("A", 9)
  
  traits_dfs_modes = append(traits_modes, dfs_modes)
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_adjusted_eng.csv", sep=''), header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
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

plspm_traits_dfs_mean_adj_aest_part_all = function(traits_dfs_df){
  traits_mean_df = data.frame(transmute(rowwise(traits_dfs_df), challenge = mean(c_across(c(25,27,28,29)))),
                              transmute(rowwise(traits_dfs_df), narrative = mean(c_across(c(20,22,23,24)))),
                              transmute(rowwise(traits_dfs_df), goals = mean(c_across(30:33))),
                              transmute(rowwise(traits_dfs_df), social = mean(c_across(10:13))))
  
  flow_mean_df = data.frame(transmute(rowwise(traits_dfs_df), challenge.skill.balance = mean(c_across(seq.int(from=42, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), action.awareness = mean(c_across(seq.int(from=43, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), clear.goals = mean(c_across(seq.int(from=44, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), unambiguous.feedback = mean(c_across(seq.int(from=45, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), concentration = mean(c_across(seq.int(from=46, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), sense.of.control = mean(c_across(seq.int(from=47, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), loss.of.self.consciousness = mean(c_across(seq.int(from=48, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), time.transformation = mean(c_across(seq.int(from=49, length.out=4, by=9)))),
                            transmute(rowwise(traits_dfs_df), autotelic.experience = mean(c_across(seq.int(from=50, length.out=4, by=9)))))
  
  traits_dfs_df = data.frame(traits_mean_df, flow_mean_df)
  
  traits_blocks = 1:4
  dfs_blocks = 5:13
  traits_dfs_blocks = as.list(append(traits_blocks, dfs_blocks))
  
  traits_modes = rep("A", length(traits_blocks))
  dfs_modes = rep("A", length(dfs_blocks))
  
  traits_dfs_modes = append(traits_modes, dfs_modes)
  
  traits_dfs_path = read.csv(paste(model_path, "path_inner_model_adjusted_aest_eng.csv", sep=''), header=TRUE, sep=";", row.names = 1)
  traits_dfs_path = as.matrix(traits_dfs_path)
  rownames(traits_dfs_path) = make.names(rownames(traits_dfs_path))
  colnames(traits_dfs_path) = rownames(traits_dfs_path)
  
  traits_dfs_pls = plspm(traits_dfs_df, traits_dfs_path, traits_dfs_blocks, scaled = FALSE )
  
  input_vars = colnames(traits_dfs_path)[traits_blocks]
  output_vars = colnames(traits_dfs_path)[dfs_blocks]
  
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

plot_plspm_traits_dfs = function(path_coefs, p_values, significative_level = 0.05, title = "", color_lim){
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
  }else if(type==8){
    first_pls = plspm_traits_dfs_value_adjusted(first_df)
    second_pls = plspm_traits_dfs_value_adjusted(second_df)
    third_pls = plspm_traits_dfs_value_adjusted(third_df)
  }else if(type==9){
    first_pls = plspm_traits_dfs_value_adjusted_2(first_df)
    second_pls = plspm_traits_dfs_value_adjusted_2(second_df)
    third_pls = plspm_traits_dfs_value_adjusted_2(third_df)
  }else if(type==10){
    first_pls = plspm_traits_dfs_value_adjusted_3(first_df)
    second_pls = plspm_traits_dfs_value_adjusted_3(second_df)
    third_pls = plspm_traits_dfs_value_adjusted_3(third_df)
  }else if(type==11){
    first_pls = plspm_traits_dfs_adjusted_aest_eng(first_df)
    second_pls = plspm_traits_dfs_adjusted_aest_eng(second_df)
    third_pls = plspm_traits_dfs_adjusted_aest_eng(third_df)
  }else if(type==12){
    first_pls = plspm_traits_dfs_adjusted_eng_part_aest(first_df)
    second_pls = plspm_traits_dfs_adjusted_eng_part_aest(second_df)
    third_pls = plspm_traits_dfs_adjusted_eng_part_aest(third_df)
  }else if(type==13){
    first_pls = plspm_traits_dfs_mean(first_df)
    second_pls = plspm_traits_dfs_mean(second_df)
    third_pls = plspm_traits_dfs_mean(third_df)
  }else if(type==14){
    first_pls = plspm_traits_dfs_mean_adj_aest(first_df)
    second_pls = plspm_traits_dfs_mean_adj_aest(second_df)
    third_pls = plspm_traits_dfs_mean_adj_aest(third_df)
  }else if(type==15){
    first_pls = plspm_traits_dfs_mean_adj_part_aest_1(first_df)
    second_pls = plspm_traits_dfs_mean_adj_part_aest_1(second_df)
    third_pls = plspm_traits_dfs_mean_adj_part_aest_1(third_df)
  }else if(type==16){
    first_pls = plspm_traits_dfs_mean_adj_part_aest_2(first_df)
    second_pls = plspm_traits_dfs_mean_adj_part_aest_2(second_df)
    third_pls = plspm_traits_dfs_mean_adj_part_aest_2(third_df)
  }else if(type==17){
    first_pls = plspm_traits_dfs_mean_adj_part_all(first_df)
    second_pls = plspm_traits_dfs_mean_adj_part_all(second_df)
    third_pls = plspm_traits_dfs_mean_adj_part_all(third_df)
  }else if(type==18){
    first_pls = plspm_traits_dfs_mean_adj_aest_part_all(first_df)
    second_pls = plspm_traits_dfs_mean_adj_aest_part_all(second_df)
    third_pls = plspm_traits_dfs_mean_adj_aest_part_all(third_df)
  }
  
  par(mfcol = c(3,2))
  plot_plspm_traits_dfs(first_pls$path_coefs, first_pls$p_values, title = first_title, color_lim = color_lim)
  plot_plspm_traits_dfs(second_pls$path_coefs, second_pls$p_values, title = second_title, color_lim = color_lim)
  plot_plspm_traits_dfs(third_pls$path_coefs, third_pls$p_values, title = third_title, color_lim = color_lim)
  mtext("Significative level = 0.05", line=-2, side=2, outer = TRUE)
  
  plot_plspm_traits_dfs(first_pls$path_coefs, first_pls$p_values, significative_level = 0.1 ,title = first_title, color_lim = color_lim)
  plot_plspm_traits_dfs(second_pls$path_coefs, second_pls$p_values, significative_level = 0.1, title = second_title, color_lim = color_lim)
  plot_plspm_traits_dfs(third_pls$path_coefs, third_pls$p_values, significative_level = 0.1, title = third_title, color_lim = color_lim)
  mtext("Significative level = 0.10", line = -1, side=4, outer = TRUE)
  tab = list(first_pls, second_pls, third_pls)
  return(tab)
}

pls_analysis_values = function(plspm_analysis){
  # Unidimsensionality
  print("Unidimensionality")
  unidim_rows = rownames(plspm_analysis[["unidim"]])
  c_alpha = plspm_analysis[["unidim"]][["C.alpha"]]
  c_alpha_check = data.frame(block=unidim_rows, value=c_alpha)
  
  dg_rho = plspm_analysis[["unidim"]][["DG.rho"]]
  dg_rho_check = data.frame(block=unidim_rows, value=dg_rho)
  
  eig_f = plspm_analysis[["unidim"]][["eig.1st"]]
  eig_f_check = data.frame(block=unidim_rows, value=eig_f)
  
  eig_s = plspm_analysis[["unidim"]][["eig.2nd"]]
  eig_s_check = data.frame(block=unidim_rows, value=eig_s)
  
  unidim_values = data.frame(block=unidim_rows, c_alpha=c_alpha, dg_rho=dg_rho, eig_f=eig_f, eig_s=eig_s)
  
  # Checking loadings and communalities
  print("Outer Model - Loadings and Communalities")
  outer_model_item = plspm_analysis[["outer_model"]][["name"]]
  outer_model_block = plspm_analysis[["outer_model"]][["block"]]
  
  loadings = plspm_analysis[["outer_model"]][["loading"]]
  loadings_check = data.frame(item=outer_model_item, block=outer_model_block, loadings=loadings)
  loadings_check = mutate(loadings_check, across(everything(), str_sub, 1, 50))
  
  communalities = plspm_analysis[["outer_model"]][["communality"]]
  communalities_check = data.frame(item=outer_model_item, block=outer_model_block, communalities=communalities)
  communalities_check = mutate(communalities_check, across(everything(), str_sub, 1, 50))
  
  # Coherence of crossloadings
  print("Crossloadings")
  crossloadings = plspm_analysis[["crossloadings"]]
  
  # Cohesion of inner summary
  print("Inner Summary - R2, AVE and Block Communality")
  inner_summary_rows = rownames(plspm_analysis[["inner_summary"]])
  
  block_com = plspm_analysis[["inner_summary"]][["Block_Communality"]]
  ave = plspm_analysis[["inner_summary"]][["AVE"]]
  r2 = plspm_analysis[["inner_summary"]][["R2"]]
  
  innersummary_check = data.frame(block=inner_summary_rows, AVE=ave, Block_Communality=block_com, R2=r2)
  
  # Pseudo Goodness of fit
  print("Pseudo Goodness of fit (> 0.7 is considered great)")
  gof = data.table(Goodness_of_fit=plspm_analysis[["gof"]])
  #crossloadings=crossloadings
  return(list(unidim=unidim_values, loadings=loadings_check, communalities=communalities_check, inner_summary=innersummary_check, gof=gof))
}

pls_analysis_check = function(plspm_analysis){
  # Unidimsensionality
  print("Unidimensionality")
  unidim_rows = rownames(plspm_analysis[["unidim"]])
  c_alpha = plspm_analysis[["unidim"]][["C.alpha"]]
  c_alpha_check = data.frame(block=unidim_rows[c_alpha<0.7], c_alpha=c_alpha[c_alpha<0.7])
  
  dg_rho = plspm_analysis[["unidim"]][["DG.rho"]]
  dg_rho_check = data.frame(block=unidim_rows[dg_rho<0.7], dg_rho=dg_rho[dg_rho<0.7])
  
  eig_f = plspm_analysis[["unidim"]][["eig.1st"]]
  eig_f_check = data.frame(block=unidim_rows[eig_f<1.0], eig_f=eig_f[eig_f<1.0])
  
  eig_s = plspm_analysis[["unidim"]][["eig.2nd"]]
  eig_s_check = data.frame(block=unidim_rows[eig_s>1.0], eig_s=eig_s[eig_s>1.0])
  
  unidim_checks = list(c_alpha = c_alpha_check, dg_rho = dg_rho_check, eig_f = eig_f_check, eig_s = eig_s_check)
  
  # Checking loadings and communalities
  print("Outer Model - Loadings and Communalities")
  outer_model_item = plspm_analysis[["outer_model"]][["name"]]
  outer_model_block = plspm_analysis[["outer_model"]][["block"]]
  
  loadings = plspm_analysis[["outer_model"]][["loading"]]
  loadings_check = data.frame(item=outer_model_item[loadings<0.7], block=outer_model_block[loadings<0.7], loadings=loadings[loadings<0.7])
  loadings_check = mutate(loadings_check, across(everything(), str_sub, 1, 50))
  
  communalities = plspm_analysis[["outer_model"]][["communality"]]
  communalities_check = data.frame(item=outer_model_item[communalities<0.49], block=outer_model_block[communalities<0.49], communalities=communalities[communalities<0.49])
  communalities_check = mutate(communalities_check, across(everything(), str_sub, 1, 50))
  
  outer_model_checks = list(loadings=loadings_check, communalities=communalities_check)
    
  # Coherence of crossloadings
  print("Crossloadings")
  crossloadings = plspm_analysis[["crossloadings"]]
  crossloadings_check = NULL
  for(i in 1:length(crossloadings[[1]])){
    block = toString(crossloadings[i,2])
    block_crossloading = crossloadings[i,block]
    max_crossloading = max(crossloadings[i,3:length(crossloadings)])
    if(block_crossloading != max_crossloading){ 
      if(is.null(crossloadings_check)){
        crossloadings_check = data.frame(item=crossloadings[i,"name"], block=crossloadings[i,"block"], crossloadings=crossloadings[i,block])
      }else{
        crossloadings_check[nrow(crossloadings_check) + 1,] = list(crossloadings[i,"name"], crossloadings[i,"block"], crossloadings[i,block])
      }
    }
  }

  # Cohesion of inner summary
  print("Inner Summary - R2, AVE and Block Communality")
  inner_summary_rows = rownames(plspm_analysis[["inner_summary"]])
  
  r2 = plspm_analysis[["inner_summary"]][["R2"]]
  r2_check = data.frame(block=inner_summary_rows[r2<0.5], R2=r2[r2<0.5])
  
  ave = plspm_analysis[["inner_summary"]][["AVE"]]
  ave_check = data.frame(block=inner_summary_rows[ave<0.5], AVE=ave[ave<0.5])
  
  block_com = plspm_analysis[["inner_summary"]][["Block_Communality"]]
  block_com_check = data.frame(block=inner_summary_rows[block_com<0.5], block_communality=block_com[block_com<0.5])
  
  inner_summary_checks=list(r2=r2_check, block_com=block_com_check, ave=ave_check)
  
  # Pseudo Goodness of fit
  gof = data.table(Goodness_of_fit=plspm_analysis[["gof"]])
  
  return(list(unidim=unidim_checks, outer_model=outer_model_checks, crossloadings=crossloadings_check, inner_summary=inner_summary_checks, gof=gof))
}

knit_plspm_results = function(plspm_analysis, scenario = "Scenario", latex = FALSE){
  print(paste("--------------- Valeurs pour le scenario", scenario))
  plspm_values = pls_analysis_values(plspm_analysis)
  print(knitr::kable(plspm_values, "simple"))
  
  print(paste("--------------- Valeurs a verifier pour le scenario", scenario))
  plspm_values_check = pls_analysis_check(plspm_analysis)
  print(knitr::kable(plspm_values_check, "simple"))
  
  latex_list = list()
  if(latex){
    latex_list.append(knitr::kable(plspm_values, "latex"))
    latex_list.append(knitr::kable(plspm_values_check, "latex"))
  }
  
  return(latex_list)
}
