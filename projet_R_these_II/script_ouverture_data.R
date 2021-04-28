########################################################################################################################
#######
#######     DONNEES COMMUNES : traitement et imputation des données manquantes
#######
########################################################################################################################

df_lac <-
  read_csv("/home/theo/Bureau/Data/Données finales/lacs/df_lac.csv") %>%
  mutate(
    alcalinite = ifelse(alcalinite %in% c(">", "<"), NA, alcalinite) %>% as.numeric(),
    marnage_pla = ifelse(marnage_pla %in% c(">", "<"), NA, marnage_pla) %>% as.numeric(),
    prof_max_pla = ifelse(prof_max_pla %in% c(">", "<"), NA, prof_max_pla) %>% as.numeric(),
    tps_sejour_moy_pla = ifelse(tps_sejour_moy_pla %in% c(">", "<"), NA, tps_sejour_moy_pla) %>% as.numeric(),
    prof_moy_pla = ifelse(is.na(prof_moy_pla) &!is.na(prof_moy_pla_calculee),prof_moy_pla_calculee,prof_moy_pla),
    tps_sejour_moy_pla = ifelse(is.na(tps_sejour_moy_pla) &!is.na(tps_sejour_moy_calc),tps_sejour_moy_calc,tps_sejour_moy_pla)
    
  ) %>%
  dplyr::select(
    -c(
      lac_commun.y,
      lac_commun.x,
      geologie,
      geologie_class,
      longueur_max_pla,
      largeur_max_pla,
      surf_litto,
      prof_moy_pla_calculee,
      tps_sejour_moy_calc,
      cd_proj,
      annee_mise_en_eau
    )
  ) %>%
  setnames(colnames(.)[-1], paste0("LAC_", colnames(.)[-1])) %>% 
  mutate_if(is.character,as.factor) %>% 
  as.data.frame() 



df_pc_lac <-
  read_csv("/home/theo/Bureau/Data/Données finales/PC/pc_toutes_annees.csv") %>%
  setnames(colnames(.)[-1], paste0("PC_", colnames(.)[-1])) %>% 
  setnames(c("PC_demande_biochimique_en_oxygene_en_5_jours_dbo5","PC_taux_de_saturation_en_oxygene","PC_potentiel_en_hydrogene_ph"),
           c("PC_dbo5","PC_sat_ox","PC_ph")) %>% 
  as.data.frame()

coord_lac <-
  df_lac %>% dplyr::select(code_lac, LAC_lat_pla, LAC_long_pla) %>% setnames(colnames(.)[-1], paste0("XY_", colnames(.)[-1]))

df_bv <- read_csv("/home/theo/Bureau/Data/Données finales/bassins versans/table_bv.csv") %>% 
  mutate_if(is.character,as.factor) %>% 
  as.data.frame()

df_bv_occsol <- read_csv("/home/theo/Bureau/Data/Données finales/bassins versans/table_bv_occ_sol_all_years.csv") %>% 
  dplyr::filter(annee==2018) %>% 
  dplyr::select(-annee)%>% 
  setnames(colnames(.)[-1],paste0("BVO_",colnames(.)[-1]))


table_env_tot <- join_all(list(
  df_lac,
  df_pc_lac,
  coord_lac,
  df_bv,
  df_bv_occsol
  ),type="full",by="code_lac") %>% 
  column_to_rownames() %>%
  missForest() %>% 
  .$ximp
table_env_tot %>% dplyr::select(starts_with("PC_")) %>% mutate_all(~log(.)) %>% PerformanceAnalytics::chart.Correlation()
log(-2)
df_lac %>% select_if(is.numeric) %>% cor()
