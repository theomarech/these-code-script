

# Données biologiques -----------------------------------------------------


setwd("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_CWM/traits_interactions")

nom <- c("iml","izl","izp","mac","phy","poi")
nom_tax <- c("sha","sim","ric","pie")
nom_fun <- c("fri","fev","fdi","fdis","rao")

list_tax <- map2(dir(pattern="tax"),nom,~read_csv(.x) %>% setnames(colnames(.)[-1],paste0(.y,"_",nom_tax)) %>% mutate(tax = .y))
list_fun <- map2(dir(pattern="fun"),nom,~read_csv(.) %>% dplyr::select(code_lac,3:7) %>% setnames(colnames(.)[-1],paste0(.y,"_",nom_fun)) %>% mutate(tax = .y))

names(list_fun) <- nom
names(list_tax) <- nom



table_pmp_tax <- join_all(list_tax[c("poi","mac","phy")],type="inner",by="code_lac") %>% dplyr::select(-tax)
table_pmp_fun <- join_all(list_fun[c("poi","mac","phy")],type="inner",by="code_lac") %>% dplyr::select(-tax)




# Données environnementales -----------------------------------------------------
env_pc <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/ENV/ENV/env_physicochimie_peh.csv") %>% 
  dplyr::select(code_lac,
                ammo = ammonium,
                azote = azote_kjeldahl,
                carb = carbone_organique,
                chla = chlorophylle_a,
                cond = conductivite,
                dbo5 = demande_biochimique_en_oxygene_en_5_jours_dbo5,
                mes = matieres_en_suspension,
                nitra = nitrates,
                nitri = nitrites,
                ortho = orthophosphates_po4,
                oxy = oxygene_dissous,
                pheo = pheopigments,
                phos = phosphore_total,
                ph = potentiel_en_hydrogene_ph,
                satox = taux_de_saturation_en_oxygene,
                tpe = temperature_de_leau,
                turb = turbidite_formazine_nephelometrique)

env_bv2 <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/ENV/ENV/env_bv_clc.csv")

env_morpho <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/Données finales/lacs/df_lac.csv") %>%
  dplyr::select(code_lac,
                superficie_pla,
                altitude_pla,
                perimetre_pla,
                volume_pla,
                prof_max_pla,
                prof_moy_pla_calculee,
                prof_moy_pla,
                alcalinite,
                tps_sejour_moy_pla,
                tps_sejour_moy_calc,
                lat_pla,
                long_pla)%>% 
  mutate(prof_moy_pla = ifelse(is.na(prof_moy_pla) & !is.na(prof_moy_pla_calculee),prof_moy_pla_calculee,prof_moy_pla),
         prof_max_pla = ifelse( prof_max_pla  == "<",NA,prof_max_pla) %>% as.numeric,
         tps_sej_moy = ifelse(is.na(tps_sejour_moy_pla) & !is.na(tps_sejour_moy_calc),tps_sejour_moy_calc,tps_sejour_moy_pla),
         tps_sej_moy = case_when(tps_sej_moy == ">" ~ as.character(5*365),
                                 tps_sej_moy == "<" ~ as.character(365/2),
                                 tps_sej_moy == 'alimentation par nappe' ~ NA_character_,
                                 tps_sej_moy == '> 150' ~ "150",
                                 TRUE ~ tps_sej_moy) %>% as.numeric(),
         alcalinite = case_when(alcalinite == "<" ~ "0.5",
                                alcalinite == ">" ~ "2.5",
                                TRUE ~ alcalinite) %>% as.numeric,
         altitude = ifelse(altitude_pla == 0,1,altitude_pla)) %>% 
  dplyr::select(-c(tps_sejour_moy_calc,tps_sejour_moy_pla,prof_moy_pla_calculee,altitude_pla))

env_meteo <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/ENV/ENV/env_meteo.csv") %>%
  mutate(temp_ampli = abs(temp_moy01 - temp_moy07)) %>% dplyr::select(code_lac,temp_moy_annuelle,temp_ampli)

env_alter <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/ENV/ENV/df_alber.csv") %>% dplyr::select(code_lac=rowname,p_alter,n_alter_cat)

env_charli <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/ENV/ENV/df_charli.csv") %>% dplyr::select(code_lac,substrat_shannon)


table_env <- join_all(list(env_pc,env_morpho,env_alter,env_charli),type="full") %>%
  inner_join(env_meteo) %>%
  inner_join(env_bv2) %>% 
  mutate(alter = n_alter_cat/perimetre_pla,
         p_alter = p_alter/100,
         chla = ifelse(chla == 0,NA,chla),
         oxy = ifelse(oxy < 0,0,oxy)) %>% 
  as.data.frame()

table_env_boxcox <- table_env %>% dplyr::select(code_lac,
                                                ammo:perimetre_pla,
                                                prof_max_pla:alcalinite,
                                                tps_sej_moy:altitude,
                                                substrat_shannon,temp_ampli) %>% mutate_if(is.numeric,~fun_boxcox(.+0.1))

table_env_pourc <- table_env %>% dplyr::select(AGS:WLS,p_alter) %>% mutate_if(is.numeric,~asin(sqrt(.)))

table_volume <- table_env %>% dplyr::select(volume_pla) %>% mutate(volume_pla = log(log(volume_pla)))

table_env <- bind_cols(table_env_boxcox,table_env_pourc,table_volume)

table_env %>% select_if(is.numeric) %>% PCA()
table_env %>% select_if(is.numeric) %>% chart.Correlation()


table_env_impute <- table_env %>% column_to_rownames() %>% missForest() %>% .$ximp

table_env <- table_env_impute %>% tibble::rownames_to_column(var="code_lac") %>% mutate_if(is.numeric,scale)

table_env %>% select_if(is.numeric) %>% chart.Correlation()



# jointure env - bio ------------------------------------------------------

data_tax <- list_tax %>%  map(~ mutate_if(.,is.numeric,~fun_boxcox(.+1)) %>% inner_join(.,table_env))
data_fun <- list_fun %>%  map(~inner_join(.,table_env))

data_fun$izl %>% dplyr::select(ind=izl_fri,ammo:volume_pla) %>% gather(var,val,-ind) %>%
  ggplot(aes(val,ind))+geom_point()+facet_wrap(~var,scale="free")+geom_smooth()
