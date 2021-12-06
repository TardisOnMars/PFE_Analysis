sem_traits_data = function(scenario_df)
{
  aesthetic = scenario_df[15:19]
  colnames(aesthetic) = c("AEST1", "AEST2", "AEST3", "AEST4", "AEST5")
  
  challenge = scenario_df[25:29]
  colnames(challenge) = c("CHAL1", "CHAL2", "CHAL3", "CHAL4", "CHAL5")
  
  narrative = scenario_df[20:24]
  colnames(narrative) = c("NARR1", "NARR2", "NARR3", "NARR4", "NARR5")
  
  goals = scenario_df[30:34]
  colnames(goals) = c("GOAL1", "GOAL2", "GOAL3", "GOAL4", "GOAL5")
  
  social = scenario_df[10:14]
  colnames(social) = c("SOCI1", "SOCI2", "SOCI3", "SOCI4", "SOCI5")
  
  return (cbind(aesthetic, challenge, narrative, goals, social))
}


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

sem_engagement_data = function(scenario_df){
  engagement = scenario_df[78:83]
  colnames(engagement) = c("ENG1", "ENG2", "ENG3", "ENG4", "ENG5", "ENG6")
  
  return(engagement)
}
