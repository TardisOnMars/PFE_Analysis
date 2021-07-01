source("LoadData.R",encoding="utf-8")
source("PLS_functions.R")

aesthetic_first = filter(aesthetic_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Première")
aesthetic_second = filter(aesthetic_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Deuxième")
aesthetic_third = filter(aesthetic_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Troisième")

narrative_first = filter(narrative_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Première")
narrative_second = filter(narrative_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Deuxième")
narrative_third = filter(narrative_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Troisième")

objectives_first = filter(objectives_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Première")
objectives_second = filter(objectives_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Deuxième")
objectives_third = filter(objectives_df, est.ce.votre.premiere..deuxieme.ou.troisieme.experience.. == "Troisième")

pls_analysis(aesthetic_first, narrative_first, objectives_first)
pls_analysis(aesthetic_second, narrative_second, objectives_second)
pls_analysis(aesthetic_third, narrative_third, objectives_third)
