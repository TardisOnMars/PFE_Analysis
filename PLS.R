source("LoadData.R",encoding="utf-8")
source("PLS_functions.R")

pls_analysis(aesthetic_df, narrative_df, objectives_df)

pls_analysis(bind_rows(aesthetic_second, aesthetic_third),
             bind_rows(narrative_second, narrative_third),
             bind_rows(objectives_second, objectives_third), type=5)

pls_analysis(first_df, second_df, third_df)
