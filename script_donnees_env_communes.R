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
  tibble::rownames_to_column(var="code_lac") %>% 
  mutate_if(is.numeric,log)

write.csv(table_physicochimie,"/home/theo/Bureau/Data/DATA/ENV/PHYSICOCHIMIE/env_physicochimie.csv", row.names = FALSE)

df <- table_physicochimie %>% column_to_rownames() %>% PCA()
com_pc <- fun_comp(df,site = table_physicochimie$code_lac,nom = "PC_",seuil=80)



################################################### LAC ################################################################
table_lac <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES TRAVAIL/lacs/df_lac.csv")
table_typo_dce <- dbGetQuery(con,"SELECT typo_dce, code_lac FROM plan_eau") %>% 
  mutate(typo_dce = case_when(str_detect(typo_dce,"N")~"N",
                              typo_dce %in% c("A13a","A13b","A14","A15","A16") ~ "MEAB",
                              TRUE ~ "MEA")) %>%
  na.omit()
save(table_lac,table_typ)
table_lac_typo <- inner_join(table_lac,table_typo_dce) %>% 
  dplyr::select(-c(cd_mefm,mea,lac_commun.x,lac_commun.y,geologie,geologie_class)) %>% 
  mutate(marnage = case_when(marnage_pla == ">"~1500,
                             marnage_pla =="<" ~0.01258,
                             TRUE ~ as.numeric(marnage_pla)),
         marnage = case_when(marnage > 3 ~ ">3m",
                             marnage < 3 ~ "<3m"),
         prof_moy_pla = ifelse(is.na(prof_moy_pla)&!is.na(prof_moy_pla_calculee),prof_moy_pla_calculee,prof_moy_pla),
         tps_sejour = case_when(tps_sejour_moy_pla == ">"~ NA_real_,
                                tps_sejour_moy_pla =="<" ~ NA_real_,
                                TRUE ~ as.numeric(tps_sejour_moy_pla)),
         tps_sejour = ifelse(is.na(tps_sejour) & !is.na(tps_sejour_moy_calc),tps_sejour_moy_calc,tps_sejour),
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

table_lac_finale <- table_lac_typo %>% dplyr::select(code_lac,mefm,forme_cuvette,lake_order,typo_pla,typo_dce,marnage) %>% 
  bind_cols(lac_log) 

write.csv(table_lac_finale,"/home/theo/Bureau/Data/DATA/ENV/LAC/env_lac.csv",row.names = FALSE)


################################################### Bassins versans ################################################################

######################### CLC
table_bv_occ <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES TRAVAIL/bassins versans/table_bv_occ_sol_all_years.csv") %>% 
  dplyr::filter(annee==2018) %>% dplyr::select(-annee) %>% 
  gather(var,val,-code_lac) %>% 
  mutate(var=str_sub(var,1,1),
          var=case_when(var==1 ~ "ARS",
                        var==2 ~ "AGS",
                        var ==3 ~"FSS",
                        var==4 ~ "WLS",
                        TRUE ~ "WBS")) %>% 
  group_by(code_lac,var) %>% 
  dplyr::summarise(m=sum(val)) %>% 
  ungroup() %>% 
  spread(var,m) %>% 
  column_to_rownames() %>% 
  decostand(MARGIN=1,method="total") %>% 
  tibble::rownames_to_column(var="code_lac")

write.csv(table_bv_occ,"/home/theo/Bureau/Data/DATA/ENV/BV_CLC/env_bv_clc.csv",row.names = FALSE)


##################### BV
table_bv <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES TRAVAIL/bassins versans/table_bv.csv") %>% 
  dplyr::select(-roche_predom) %>% 
  column_to_rownames() %>% 
  missForest() %>% 
  .$ximp %>% 
  tibble::rownames_to_column(var="code_lac") %>% 
  dplyr::select(code_lac,pente_moy,perim_km,p100_silice,p100_calcaire,
                p100_autre,battance_pmoy,densite_pop_moy,alea_erosion_annu,
                r_bv_retenu_cumu,tps_res_cumu)

table_bv %>% dplyr::select_if(is.numeric) %>% PCA()
table_bv %>% dplyr::select_if(is.numeric) %>% chart.Correlation()
table_bv %>% vis_dat

write.csv(table_bv,"/home/theo/Bureau/Data/DATA/BV/env_bv.csv",row.names = FALSE)



############################################ METEO ################################################

table_meteo <- dbGetQuery(con,"SELECT * FROM climat_pla")
meteo %>% select_if(is.numeric) %>% PCA()
meteo %>% select_if(is.numeric) %>% chart.Correlation()
write.csv(table_meteo,"/home/theo/Bureau/Data/DATA/ENV/METEO/env_meteo.csv",row.names = FALSE)




########################################### SPATIAL ###############################################
table_spatial <- dbGetQuery(con,"SELECT code_lac, lat_pla, long_pla FROM plan_eau") %>% na.omit()
write.csv(table_spatial,"/home/theo/Bureau/Data/DATA/ENV/SPATIAL/env_spatial.csv",row.names = FALSE)





############################################ Alber et Charli ################################################
perimetre_lac <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES TRAVAIL/lacs/df_lac.csv") %>%
  dplyr::select(code_lac,perimetre_pla) %>% 
  arrange(code_lac)

######### Alber : alteration des berge
# proportion d'alteration de lar berge (sum(linéaire_alter)/total linéaire)
# richesse d'alteration
# proportion d'alteration /type d'alteration (sum(alter1)/total linéaire)

table_alber <- dbGetQuery(con_ac,"SELECT id_camp, code_lac,code1,code2,code3,obs,shp_length,pkey,id  FROM alber") %>%
  mutate_at(vars(matches("code")),~ifelse(. == "NA","NAC",.))

df_alber_type <-  table_alber %>%
  dplyr::select(code_lac,shp_length,code1,code2,code3) %>%
  gather(var,val,-c(code_lac,shp_length)) %>% 
  mutate(val = 
           case_when(val %in% c("RMa","RMs","E") ~ "REN",
                     val %in% c("ASa","AGr","DS") ~ "AMO",
                     val %in% c("ESe","ESa","EGr","EB") ~ "EXM",
                     val %in% c("Cm","ER") ~ "COE",
                     val %in% c("Pe","Re","AMo","Can") ~ "HYD",
                     val %in% c("Pt","Ba","Po","J","Mae","Pla") ~ "EQU",
                     val %in% c("Co","Gv","Ei") ~ "VEG",
                     TRUE ~ val
           )
  ) %>% na.omit()

### proportion total d'alteration des berges
df_contingence_alber_total <- table_alber %>% 
  group_by(code_lac,code1) %>% 
  dplyr::summarise(som=sum(shp_length)) %>%
  na.omit() %>% 
  spread(code1,som,fill=0) %>% 
  column_to_rownames() %>% 
  rowSums() %>%
  data.frame(total=.)%>%
  tibble::rownames_to_column(var="code_lac")

df_contingence_alber_alter <- table_alber %>% 
  group_by(code_lac,code1) %>% 
  dplyr::summarise(som=sum(shp_length)) %>%
  na.omit() %>% 
  spread(code1,som,fill=0) %>% 
  dplyr::select(-c(ABSENT,NAC)) %>% 
  column_to_rownames() %>% 
  rowSums() %>%
  data.frame(alter=.) %>%
  tibble::rownames_to_column(var="code_lac") %>% 
  inner_join(df_contingence_alber_total) %>% 
  mutate(p_alter = alter/total*100) %>% 
  dplyr::select(code_lac,p_alter) %>% 
  ungroup() %>% as_tibble()
  
### richesse d'alteration (nombre d'alteration différentes par lac)
df_contingence_alber_cat_ric <- table_alber %>% 
  dplyr::select(code_lac,code1,code2,code3) %>% 
  gather(var,val,-code_lac) %>% 
  na.omit() %>% 
  dplyr::filter(!val %in%c("ABSENT","NAC")) %>% 
  dplyr::select(-var) %>% 
  distinct() %>% 
  group_by(code_lac) %>% 
  dplyr::summarise(n_alter_cat = n()) %>% 
  ungroup()
  
df_contingence_alber_type_ric <- df_alber_type %>% 
  dplyr::select(-c(shp_length,var)) %>% 
  na.omit() %>% 
  dplyr::filter(!val %in%c("ABSENT","NAC")) %>% 
  distinct() %>% 
  group_by(code_lac) %>% 
  dplyr::summarise(n_alter_type = n()) %>% 
  ungroup()
### pourcentage d'alteration par type (nombre d'alteration différentes par lac)


## version sans pondération ==> les codes qui s'overlappent donnent augmente la surface total et du coup diminue la proportion réelle 
# des codes qui n'overlappe pas
df_alber_type_total <- df_alber_type%>% 
    dplyr::select(code_lac,shp_length) %>% 
    group_by(code_lac) %>% 
    dplyr::summarise(total=sum(shp_length,na.rm=TRUE))

df_alber_type_prop <- df_alber_type %>% 
  dplyr::select(code_lac,val,shp_length) %>% 
  group_by(code_lac,val) %>% 
  dplyr::summarise(som= sum(shp_length,na.rm=TRUE)) %>% 
  inner_join(df_alber_type_total) %>%
  arrange(code_lac) %>% 
  mutate(prop_type = som/total*100) %>% 
  dplyr::select(-c(som,total)) %>% 
  spread(val,prop_type,fill=0)%>% 
  setnames(colnames(.)[-1],paste0("TYP_",colnames(.)[-1])) %>% 
  ungroup()




## version sans pondération : plus de poids pour les zones de superposition d'alteration par catégorie
df_alber_cat <- table_alber %>% 
  dplyr::select(code_lac,shp_length,code1,code2,code3) %>%
  gather(var,val,-c(code_lac,shp_length)) %>% 
  na.omit()

df_alber_cat_total <- df_alber_cat %>% 
  dplyr::select(code_lac,shp_length) %>% 
  group_by(code_lac) %>% 
  dplyr::summarise(total=sum(shp_length,na.rm=TRUE))
  
df_alber_cat_prop <- df_alber_cat %>% 
  dplyr::select(code_lac,val,shp_length) %>% 
  group_by(code_lac,val) %>% 
  dplyr::summarise(som= sum(shp_length,na.rm=TRUE)) %>% 
  inner_join(df_alber_cat_total) %>% 
  arrange(code_lac) %>% 
  mutate(prop_type = som/total*100) %>% 
  dplyr::select(-c(som,total)) %>% 
  spread(val,prop_type,fill=0) %>% dplyr::select(-c(ABSENT,NAC)) %>% 
  setnames(colnames(.)[-1],paste0("CAT_",colnames(.)[-1])) %>% 
  ungroup()

df_alber_cat_prop$TMo
df_alber_final <- join_all(list(df_alber_cat_prop,
                                df_alber_type_prop,
                                df_contingence_alber_type_ric,
                                df_contingence_alber_cat_ric,
                                df_contingence_alber_alter),
                           type="inner",by="code_lac")%>% 
  column_to_rownames() %>% 
  select_if(colSums(.)!=0) %>% .[which(rowSums(.)!=0),] %>% 
  tibble::rownames_to_column()

write.csv(df_alber_final, "/home/theo/Bureau/Data/DATA/ENV/ALBER & CHARLI/df_alber.csv",row.names = FALSE)


######### Charli : caractérisation des habitats des rives et du littoral
# proportion d'alteration de lar berge (sum(linéaire_alter)/total linéaire)
# richesse d'alteration par catégorie et par type
# proportion d'alteration /type d'alteration (sum(alter1)/total linéaire) par catégorie et par type
# un type rassemble plusieurs catégorie
nom_table_veg <- paste("SELECT id_camp, code_lac, code, largeur, shp_length, pkey FROM",(dbListTables(con_ac)[7:16])[-10])

# hydrologie
table_hydro <- dbGetQuery(con_ac,"SELECT id_camp, code_lac, code,  pkey FROM hydrologie") %>% 
  mutate(code = case_when(code == "TR"~ "AF",
                          code == "EM" ~ "EF",
                          TRUE ~ code)) %>% 
  group_by(code_lac,id_camp,code) %>% 
  dplyr::summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(code_lac,code) %>% 
  dplyr::summarise(mean = mean(n)) %>% 
  spread(code,mean,fill=0) %>% 
  setnames(colnames(.)[-1],paste0("hydro_",colnames(.)[-1]))

# substrat DA = dalle ? ; GR = gravier ; Sl = 
table_substrat <- dbGetQuery(con_ac,"SELECT id_camp, code_lac,dominant, acc, shp_length FROM substrat") %>% 
  dplyr::filter(!dominant  %in% c("V","LA","S","G","C","P","B","R","D"))  
table_substrat_fusion <- table_substrat %>% # on récupère que le anciens codes (90% des données, car recoupement sur les autres)
  mutate(substrat = ifelse(is.na(acc)|str_detect(acc,"ABSENT")|acc == dominant,dominant,paste0(dominant,"_",acc)))%>% 
  dplyr::filter(!str_detect(substrat,"NA")) %>% # on utilise les catégories dominant et principale pour faire une variable combiné
  dplyr::select(-c(dominant,acc)) %>% 
  na.omit() %>% 
  group_by(code_lac,id_camp,substrat) %>% 
  dplyr::summarise(shp_length = sum(shp_length,na.rm=TRUE)) %>% 
  group_by(code_lac, substrat) %>% 
  dplyr::summarise(shp_length = mean(shp_length,na.rm=TRUE)) %>% 
  dplyr::filter(substrat != "Absent") %>% 
  group_by(code_lac) %>% 
  nest() %>% 
  mutate(total = map_dbl(data, ~sum(.$shp_length))) %>% 
  unnest(data) %>% 
  mutate( prop = shp_length / total * 100) %>% 
  dplyr::select(code_lac,substrat,prop) %>% 
  spread(substrat,prop,fill=0) %>%  
  bind_rows(., fun_div(.))
table_substrat_dom <- 
  table_substrat_fusion %>% 
  dplyr::select(-c(shannon,simpson,richesse)) %>% 
  gather(var,val,-code_lac) %>% 
  mutate(var = str_extract(var,"[:alpha:]{1,2}")) %>% 
  group_by(code_lac, var) %>% 
  dplyr::summarise(val = sum(val,na.rm=TRUE)) %>% 
  group_by(code_lac) %>% 
  spread(var,val,fill=0) %>% 
  as.data.frame() %>% 
  inner_join(.,fun_div(.)) %>% 
  setnames(colnames(.)[-1],paste0("substrat_",colnames(.)[-1]))

# berges 
table_berges <- dbGetQuery(con_ac,"SELECT id_camp, code_lac,code, largeur, shp_length FROM sberge") 
table_berges_tot <- table_berges %>% 
  mutate(code = ifelse(is.na(code),"NA",code)) %>% 
  dplyr::filter(code != "NA") %>% 
  dplyr::group_by(code_lac) %>% 
  dplyr::summarise(som =sum(shp_length,na.rm=TRUE))

table_berges_prop <- table_berges  %>% 
  mutate(code = ifelse(is.na(code),"NA",code)) %>%
  dplyr::filter(code != "NA") %>% 
  dplyr::group_by(code_lac,code) %>% 
  dplyr::summarise(som =sum(shp_length,na.rm=TRUE)) %>% 
  inner_join(table_berges_tot,by="code_lac") %>% 
  mutate(prop = som.x /som.y * 100) %>% 
  dplyr::select(code_lac,code,prop) %>% 
  spread(code,prop,fill=0) %>%
  column_to_rownames() %>% 
  .[which(rowSums(.)!=0),] %>% 
  tibble::rownames_to_column(var="code_lac") %>% 
  dplyr::select(code_lac,SB) %>% 
  setnames(colnames(.)[-1],"sous_berge")

  
  
# vegetation
## proportion du lac prospecté occuppé par un type de végétation
table_veg <- map(nom_table_veg,~dbGetQuery(con_ac,.) %>% na.omit() %>% fun_veg()) %>%
  join_all(by="code_lac",type="full") %>% 
  full_join(.,fun_div(na.omit(.)),by="code_lac") %>% 
  dplyr::filter(richesse > 0) %>% 
  setnames(colnames(.)[-1],paste0("veg_",colnames(.)[-1]))

  

fun_veg_prop <- function(df){df %>% 
  mutate(code = ifelse(is.na(code),"NA",code)) %>% 
  dplyr::filter(code != "NA") %>% 
  group_by(code_lac,id_camp,code) %>% 
  dplyr::summarise(m = sum(shp_length,na.rm=TRUE)) %>% 
  group_by(code_lac,code) %>% 
  dplyr::summarise(m2 = mean(m)) %>% 
  group_by(code_lac) %>% 
  nest() %>% 
  mutate(total = map_dbl(data,~sum(.$m2))) %>% 
  unnest() %>% 
  ungroup() %>% 
  mutate(prop = m2/total*100) %>% 
  dplyr::select(code_lac,code,prop) %>% 
  spread(code,prop, fill = 0) %>% 
  dplyr::select(-ABSENT) %>% 
    return()}

df_charli_final <- join_all(list(table_berges_prop,table_veg,table_substrat_dom,table_hydro),type = "full",by = "code_lac")

df_charli_final %>% column_to_rownames() %>%  PCA() %>% .$call

write.csv(df_charli_final, "/home/theo/Bureau/Data/DATA/ENV/ALBER & CHARLI/df_charli.csv",row.names = FALSE)
