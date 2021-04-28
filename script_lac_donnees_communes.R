################################################## SCRIPT DATA ENV #############################################################

############## PHYSICO-CHIMIE ##############

table_physicochimie <- read_csv("/home/theo/Bureau/Data/Données finales/PC/pc_toutes_annees.csv") %>% 
  column_to_rownames() %>% 
  missForest() %>% 
  .$ximp %>% 
  setnames(c("demande_biochimique_en_oxygene_en_5_jours_dbo5",
             "matieres_en_suspension","potentiel_en_hydrogene_ph",
             "orthophosphates_po4","taux_de_saturation_en_oxygene",
             "turbidite_formazine_nephelometrique"),
           c("dbo5","mes","ph","orthophosphates","sat_ox","turbidite_forma")) %>% 
  tibble::rownames_to_column(var="code_lac")


write.csv(df,"/home/theo/Bureau/Data/DATA/PHYSICOCHIMIE/env_physicochimie.csv", row.names = FALSE)

df <- table_physicochimie %>% column_to_rownames() %>% mutate_all(log) 
com_pc <- fun_comp(df,site = table_physicochimie$code_lac,nom = "PC_",seuil=80)



################################################### LAC ################################################################

table_lac <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES TRAVAIL/lacs/df_lac.csv")

table_typo_dce <- dbGetQuery(con,"SELECT typo_dce, code_lac FROM plan_eau") %>% mutate(typo_dce = str_sub(typo_dce,1,1)) %>% na.omit()

table_lac_typo <- inner_join(table_lac,table_typo_dce) %>% 
  dplyr::select(-c(cd_mefm,mea,lac_commun.x,lac_commun.y,geologie,geologie_class,typo_pla)) %>% 
  mutate(marnage = case_when(marnage_pla == ">"~1500,
                             marnage_pla =="<" ~0.01258,
                             TRUE ~ as.numeric(marnage_pla)),
         marnage = case_when(marnage > 3 ~ ">3m",
                             marnage < 3 ~ "<3m"),
         prof_moy_pla = ifelse(is.na(prof_moy_pla)&!is.na(prof_moy_pla_calculee),prof_moy_pla_calculee,prof_moy_pla),
         tps_sejour = case_when(marnage_pla == ">"~ NA_real_,
                                marnage_pla =="<" ~NA_real_,
                                TRUE ~ as.numeric(marnage_pla)),
         tps_sejour = ifelse(is.na(tps_sejour)&!is.na(tps_sejour_moy_calc),tps_sejour_moy_calc,tps_sejour),
         prof_max_pla = ifelse(prof_max_pla %in%c(">","<"),NA,as.numeric(prof_max_pla)),
         alcalinite = ifelse(alcalinite %in% c(">","<"), NA, as.numeric(alcalinite)),
         lake_order = factor(lake_order,ordered=TRUE)
         ) %>% 
  dplyr::select(-c(marnage_pla,prof_moy_pla_calculee,tps_sejour_moy_calc,
                   tps_sejour_moy_pla,longueur_max_pla,largeur_max_pla,
                   surf_litto,cd_proj,kieth,typo_prof_moy,her1,her2,annee_mise_en_eau)) %>% 
  mutate_if(is.character,as.factor) %>% 
  column_to_rownames() %>% 
  missForest() %>% 
  .$ximp %>% 
  tibble::rownames_to_column(var="code_lac") %>% 
  mutate(sinuosite = perimetre_pla/(2*sqrt(pi*superficie_pla)))




lac_log <- table_lac_typo %>% select_if(is.numeric) %>% dplyr::select(-c(lat_pla,long_pla)) %>% mutate_all(~log(.+1))

table_lac_finale <- table_lac_typo %>% dplyr::select(code_lac,mefm,forme_cuvette,lake_order,typo_dce,marnage) %>% 
  bind_cols(lac_log) 

write.csv(table_lac_finale,"/home/theo/Bureau/Data/DATA/LAC/env_lac.csv")

