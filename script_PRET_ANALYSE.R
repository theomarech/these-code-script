source("/home/theo/Bureau/scriptR/librairies.R")

setwd("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_CWM/")
dir()
name <- c("inv_iml","inv","mac","phy","poi")

############################## LISTE CONTENANT TOUS LES INDICES POUR TOUS LES MAILLONS ##############################
# diversité fonctionnelle
list_tab_fun<- map2(dir(),
                    name,
                    ~read_csv(.x) %>% 
                      setnames(colnames(.)[-ncol(.)],
                               paste0(.y,"_",str_remove_all(colnames(.)[-ncol(.)],"[:upper:]{1,}_(?=.)"))) %>% 
                      dplyr::select(-c(1,2)) %>% 
                      dplyr::select(code_lac,c(1,2,3,5)))


# CWM par groupe taxo

list_tab_CWM<- map2(dir(),
                    name,
                    ~read_csv(.x) %>% dplyr::select(code_lac,starts_with("CWM")))

# diversité taxo
setwd("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_TAX/")
dir()
list_tab_tax <- map2(dir(),
                     name,
                     ~read_csv(.x) %>% 
                       setnames(colnames(.)[-1],paste0(.y,'_',colnames(.)[-1]))) 

########################################## environnement ###############################################
setwd("/home/theo/Bureau/Data/DATA/ENV/ENV")
dir()
env <- map(dir(),~read_csv(.))  %>% modify_at(4,~ dplyr::select(.,code_lac,temp_moy_annuelle,precip_tot))
env_pur <- env[-6]
spatial <- env[[6]]
env_alber <- read_csv("/home/theo/Bureau/Data/DATA/ENV/ALBER & CHARLI/df_alber.csv") %>% 
  dplyr::select(code_lac = rowname,n_alter_cat,p_alter)
env_pur[[6]]<-env_alber

env_charli <- read_csv("/home/theo/Bureau/Data/DATA/ENV/ALBER & CHARLI/df_charli.csv") %>% 
  select_at(vars(matches("code_lac|shannon"))) %>% na.omit() %>% dplyr::select_at(vars(matches("code_lac|substrat")))
env_pur[[7]]<-env_charli
data_env <- join_all(list(env[[2]],env[[3]],env[[5]]),type="full") %>%
  inner_join(env[[1]]) %>% 
  inner_join(env[[6]]) %>% 
  inner_join(env[[4]]) %>% 
  left_join(env_charli) %>% 
  left_join(env_alber)

impute_env <- data_env %>%
  mutate_if(is.character,as.factor) %>% 
  mutate(lake_order = factor(lake_order,ordered=TRUE)) %>% 
  column_to_rownames() %>% 
  as.data.frame() %>%
  missForest::missForest()

table_env_tot <- impute_env %>% .$ximp

table_env_filtre <-table_env_tot %>%dplyr::select(-c(calcium, durete_totale, oxygene_dissous,
                                                     chlorures, module,  prof_max_pla,r_bv_retenu_cumu,alea_erosion_annu,
                                                     sinuosite,dbo5,turbidite_forma,pheopigments,chlorophylle_a,superficie_pla,magnesium,
                                                     volume_pla, AGS,p100_autre,typo_pla,forme_cuvette,mefm,tps_res_cumu,p100_calcaire,p100_silice,precip_tot,alcalinite,sulfates,nitrites))%>% 
  mutate(lake_order = factor(lake_order,ordered=TRUE)) %>% 
  mutate_if(is.numeric,~scale(.)) %>% 
  mutate(code_lac = data_env$code_lac) %>% 
  mutate_if(is.character,as.factor)

env_comp <- fun_comp(table_env_filtre %>% dplyr::select_if(is.numeric),
                     site = table_env_filtre$code_lac,
                     seuil = 50)
env <- table_env_filtre
########################################### Jointure entre les tables ###############################################


########## macroinv iml###########
iml_tax <- list_tab_tax[[1]]
iml_fun <- list_tab_fun[[1]]
iml_all <- join_all(list(iml_tax,iml_fun,env,env_comp),by="code_lac",type="inner")

########## macroinv ###########
inv_tax <- list_tab_tax[[2]]
inv_fun <- list_tab_fun[[2]]
inv_all <- join_all(list(inv_tax,inv_fun,env,env_comp),by="code_lac",type="inner")

########## macrophytes ###########
mac_tax <- list_tab_tax[[3]]
mac_fun <- list_tab_fun[[3]]
mac_all <- join_all(list(mac_tax,mac_fun,env,env_comp),by="code_lac",type="inner")

########## phytoplancton ###########
phy_tax <- list_tab_tax[[4]]
phy_fun <- list_tab_fun[[4]]
phy_all <- join_all(list(phy_tax,phy_fun,env,env_comp),by="code_lac",type="inner")

########## poisson ###########
poi_tax <- list_tab_tax[[5]]
poi_fun <- list_tab_fun[[5]]
poi_all <- join_all(list(poi_tax,poi_fun,env,env_comp),by="code_lac",type="inner")
get("poi_all")


chem <- "/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/PRET_ANALYSE/"
list_tab <- list("iml_all","inv_all","mac_all","phy_all","poi_all")
map(list_tab, ~write.csv(get(.),paste0(chem,.,".csv"),row.names = FALSE))



