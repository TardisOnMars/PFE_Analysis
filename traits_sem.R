library(lavaan)

# aesthetic = aesthetic_df[15:19]
# colnames(aesthetic) = c("AEST1", "AEST2", "AEST3", "AEST4", "AEST5")
# 
# challenge = aesthetic_df[25:29]
# colnames(challenge) = c("CHAL1", "CHAL2", "CHAL3", "CHAL4", "CHAL5")
# 
# narrative = aesthetic_df[20:24]
# colnames(narrative) = c("NARR1", "NARR2", "NARR3", "NARR4", "NARR5")
# 
# goals = aesthetic_df[30:34]
# colnames(goals) = c("GOAL1", "GOAL2", "GOAL3", "GOAL4", "GOAL5")
# 
# social = aesthetic_df[10:14]
# colnames(social) = c("SOCI1", "SOCI2", "SOCI3", "SOCI4", "SOCI5")
# 
# traits_data = cbind(aesthetic, challenge, narrative, goals, social)
# 
# cov(traits_data)
# 
# traits_model <- '
# # measurement model
# Aesthetic =~ AEST1 + AEST2 + AEST3 + AEST4 + AEST5
# Challenge =~ CHAL1 + CHAL2 + CHAL3 + CHAL4 + CHAL5
# Goals =~ GOAL1 + GOAL2 + GOAL3 + GOAL4 + GOAL5
# Narrative =~ NARR1 + NARR2 + NARR3 + NARR4 + NARR5
# Social =~ SOCI1 + SOCI2 + SOCI3 + SOCI4 + SOCI5
# '
# 
# fit_traits_model <- sem(traits_model, data=traits_data)
# 
# summary(fit_traits_model, standardized=TRUE, fit.measures=TRUE)


sem_flow_data = function(scenario_df){
  csb = scenario_df[seq.int(from=42, length.out=4, by=9)]
  colnames(csb) = c("CSB1", "CSB2", "CSB3", "CSB4")
  
  aa = scenario_df[seq.int(from=43, length.out=4, by=9)]
  colnames(aa) = c("AA1", "AA2", "AA3", "AA4")
  
  cg = scenario_df[seq.int(from=44, length.out=4, by=9)]
  colnames(cg) = c("CG1", "CG2", "CG3", "CG4")
  
  uf = scenario_df[seq.int(from=45, length.out=4, by=9)]
  colnames(uf) = c("UF1", "UF2", "UF3", "UF4")
  
  cnt = scenario_df[seq.int(from=46, length.out=4, by=9)]
  colnames(cnt) = c("CNT1", "CNT2", "CNT3", "CNT4")
  
  soc = scenario_df[seq.int(from=47, length.out=4, by=9)]
  colnames(soc) = c("SOC1", "SOC2", "SOC3", "SOC4")
  
  lsc = scenario_df[seq.int(from=48, length.out=4, by=9)]
  colnames(lsc) = c("LSC1", "LSC2", "LSC3", "LSC4")
  
  tt = scenario_df[seq.int(from=49, length.out=4, by=9)]
  colnames(tt) = c("TT1", "TT2", "TT3", "TT4")
  
  ae = scenario_df[seq.int(from=50, length.out=4, by=9)]
  colnames(ae) = c("AE1", "AE2", "AE3", "AE4")
  
  return(cbind(csb, aa, cg, uf, cnt, soc, lsc, tt, ae))
}
