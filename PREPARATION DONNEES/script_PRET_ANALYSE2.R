
source("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/scriptR/librairies.R",
       encoding = "utf8")

dir()
setwd("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/PRET_ANALYSE/")
table <- map(dir(),~read_csv(.) %>%
               dplyr::select(!matches("simpson")) %>% 
               mutate_if(is.character,as.factor)) 
var_quali <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/ENV/ENV/env_lac.csv") %>% 
  dplyr::select(code_lac,forme_cuvette)
names(table) <- dir()
dir()

name_indice <- str_extract(colnames(table$poi_all.csv)[2:7],"(?<=_)[:alpha:]{1,}(?!_|[:alpha:])") %>%
  str_sub(start = 1,end = 3) %>% 
  str_to_lower()

table_poi <- table$poi_all.csv  %>% 
  setnames(colnames(.)[2:7],name_indice)
table_mac <- table$mac_all.csv  %>% 
  setnames(colnames(.)[2:7],name_indice)
table_iml <- table$iml_all.csv  %>% 
  setnames(colnames(.)[2:7],name_indice)
table_inv <- table$inv_all.csv  %>% 
  setnames(colnames(.)[2:7],name_indice)
table_phy <- table$phy_all.csv  %>% 
  setnames(colnames(.)[2:7],name_indice)


#####################################################################################
########################## Préparation des données ##################################
#####################################################################################

#####################################################################################
##### TAXONS
setwd("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/PRET_ANALYSE/")
table <- purrr::map(dir(pattern="_all"),~read_csv(.) %>%
               dplyr::select(!matches("simpson")) %>% 
               mutate_if(is.character,as.factor)) 
names(table) <- dir(pattern="_all")
dir()
table <- table[-2]
name_indice <- str_extract(colnames(table$poi_all.csv)[2:8],"(?<=_)[:alpha:]{1,}(?!_|[:alpha:])") %>%
  str_sub(start = 1,end = 3) %>% 
  str_to_lower()

table_poi <- table$poi_all.csv  %>% 
  setnames(colnames(.)[2:8],name_indice) %>% 
  dplyr::select(1:8)
table_mac <- table$mac_all.csv  %>% 
  setnames(colnames(.)[2:8],name_indice)%>% 
  dplyr::select(1:8)
table_iml <- table$iml_all.csv  %>% 
  setnames(colnames(.)[2:8],name_indice)%>% 
  dplyr::select(1:8)
table_inv_zl <- table$inv_all_zl.csv  %>% 
  setnames(colnames(.)[2:8],name_indice)%>% 
  dplyr::select(1:8)
table_inv_zp <- table$inv_all_zp.csv  %>% 
  setnames(colnames(.)[2:8],name_indice)%>% 
  dplyr::select(1:8)
table_phy <- table$phy_all.csv  %>% 
  setnames(colnames(.)[2:8],name_indice)%>% 
  dplyr::select(1:8)


################################################################################
###### ENV 
setwd("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/ENV/ENV")

###################################### PAR MIXTES ##############################
#### physico_chimie
env_pc <- read_csv("env_physicochimie_peh.csv")

pca_env <- env_pc %>%
  dplyr::select_if(is.numeric) %>% PCA()

comp_pc <- env_pc %>%
  dplyr::select_if(is.numeric) %>% 
  fun_comp(site = env_pc$code_lac,nom = "PC") %>% 
  dplyr::select(code_lac,2:3)

pc_var <- env_pc %>% dplyr::select(code_lac,
                                   conductivite,
                                   # ammonium, 
                                   # azote_kjeldahl,
                                   carbone_organique,
                                   phosphore_total,
                                   oxygene_dissous,
                                   # ph,
                                   # orthophosphates,
                                   nitrates
                                   # mes,
                                   # potassium
)
pc_var %>% select_if(is.numeric) %>% chart.Correlation()
pc_var %>% select_if(is.numeric) %>% PCA()

#### bassins versants
env_bv1 <-read_csv("env_bv.csv")  %>%
  # mutate_at(4:6,~case_when(.==0~logit((./100)+0.01),
  #                          .==100~logit((./100)-0.01),
  #                          TRUE ~ logit((./100)))) %>% 
  mutate_at(-c(1,4:6),~log(.+1))

env_bv2 <- read_csv("env_bv_clc.csv")

env_bv3 <- inner_join(env_bv1,env_bv2,by="code_lac") %>%
  na.omit()
# %>% 
#   dplyr::select(-c(alea_erosion_annu,p100_autre,p100_calcaire,p100_silice,battance_pmoy))
map_lgl(env_bv3,~names(.)=="code_lac")
env_bv <- env_bv3%>%
  dplyr::select_if(function(col)is.numeric(col)|all(col == .$code_lac))
acp_bv <-  fun_comp(env_bv %>% select_if(is.numeric),site = env_bv3$code_lac,nom = "BV")
comp_bv <- acp_bv[[1]] %>% dplyr::select(code_lac,2:3)
acp_bv[[2]]
pca_bv <- env_bv %>% select_if(is.numeric) %>% PCA()

env_bv %>% select_if(is.numeric) %>% chart.Correlation()
env_bv %>% vis_dat



#### lac : perimetre_pla ; prof_moy ; alca; alitude ; tps_sejour
env_lac <- read_csv("env_lac.csv") %>%
  dplyr::select(code_lac,which(map_lgl(., ~ is.numeric(.)))) %>% 
  dplyr::select(-c(module,typo_lac_clust)) %>% 
  mutate(superficie_pla = log(superficie_pla)) %>% 
  mutate_if(is.numeric,~scale(.)) 

pca <- PCA(select_if(env_lac,is.numeric))                 

env_lac %>% fviz_nbclust( kmeans,method = "silhouette")
env_lac %>% fviz_nbclust( hcut,method = "silhouette")
env_lac %>% fviz_nbclust( kmeans,method = "wss")
env_lac %>% fviz_nbclust( hcut,method = "wss")

clust_2 <- kmeans(env_lac,centers = 2)  
env_lac_clust2 <- env_lac %>% mutate(clust=clust_2$cluster)                 
fviz_pca_biplot(pca, geom="text",col.ind = as.factor(env_lac_clust2$clust),palette = col)
col <- RColorBrewer::brewer.pal(2,"Set1")

  #                ****$names(col)=="code_lac") %>%
  # mutate(superficie_pla = log(superficie_pla)) %>% 
  # dplyr::select(-module)

comp_lac <- env_lac %>% 
  fun_comp(site=env_lac$code_lac,nom = "LAC")

pca_lac <- env_lac %>%
  dplyr::select_if(is.numeric) %>%
  mutate(superficie_pla = log(superficie_pla)) %>% 
  dplyr::select(-module) %>% PCA()
lac_var <- env_lac %>%
  dplyr::select(code_lac,prof_moy_pla,sinuosite) 

####meteo : var brute : ampli/ moyenne
env_meteo <- read_csv("env_meteo.csv") %>% mutate(temp_ampli = abs(temp_moy01 - temp_moy07)) %>% dplyr::select(code_lac,temp_moy_annuelle,temp_ampli)

#### spatial : var brute moyenne
env_spatial <- read_csv("env_spatial.csv")

#### alber : proportion d'alteration et nombre d'alteration
env_alter <- read_csv("df_alber.csv") %>% dplyr::select(code_lac=rowname,p_alter,n_alter_cat)

#### diversité de substrat
env_charli <- read_csv("df_charli.csv") %>% dplyr::select(code_lac,substrat_shannon)

names_var_mixte <- c("tpm","tpa","lat","lon","pal","nal","sbs")

table_env_mixte <- join_all(list(comp_pc,comp_bv,comp_lac,env_meteo,env_spatial,env_alter,env_charli),
                            by="code_lac",type="full") %>% 
  as_tibble() %>% 
  mutate_if(is.numeric,~scale(.) %>% as.numeric()) %>% 
  as.data.frame() %>% 
  column_to_rownames() %>% 
  missForest::missForest() %>% 
  .$ximp %>% 
  tibble::rownames_to_column(var="code_lac") %>% 
  setnames(colnames(.)[-c(1:7)],names_var_mixte)


  setnames(colnames(.)[-c(1:7)],names_var_mixte)

iml_mixte <- inner_join(table_iml,table_env_mixte)
plot_iml <- fun_aic(iml_mixte)

poi_mixte <- inner_join(table_poi,table_env_mixte)
plot_poi <- fun_aic(poi_mixte)

inv_mixte <- inner_join(table_inv,table_env_mixte)
plot_inv <- fun_aic(inv_mixte)

phy_mixte <- inner_join(table_phy,table_env_mixte)
plot_phy <- fun_aic(phy_mixte)

mac_mixte <- inner_join(table_mac,table_env_mixte)
plot_mac <- fun_aic(mac_mixte)




names_var_var <- c("cond","carb","phos","oxy","nitr","BV1","BV2","prof","sinu","tpm","tpa","pal","nal","sbs")

table_env_var <-  join_all(list(pc_var,env_bv,lac_var,env_meteo,env_alter,env_charli),
                           by="code_lac",type="full") %>% 
  as_tibble() %>% 
  mutate_if(is.numeric,~scale(.) %>% as.numeric()) %>% 
  as.data.frame() %>% 
  column_to_rownames() %>% 
  missForest::missForest() %>% 
  .$ximp %>% 
  tibble::rownames_to_column(var="code_lac") %>% 
  setnames(colnames(.)[-1],names_var_var)

table_env_tot <- join_all(list(env_pc,env_bv,env_lac,env_alter,env_charli),
                          by="code_lac",type="full") %>% 
  inner_join(env_spatial) %>% 
  inner_join(env_meteo) %>% 
  as_tibble() %>% 
  mutate_if(is.numeric,~scale(.) %>% as.numeric()) %>% 
  mutate_if(is.character,as.factor) %>% 
  as.data.frame() %>% 
  tibble::column_to_rownames(var = "code_lac") %>% 
  missForest::missForest() %>% 
  .$ximp %>% 
  tibble::rownames_to_column(var="code_lac")

table_env_var <- table_env_tot  %>% 
  dplyr::select(code_lac,
                cond=conductivite,
                carb =carbone_organique,
                phos=phosphore_total,
                oxy=oxygene_dissous,
                nitr=nitrates,
                tpa=temp_ampli,
                tpm=temp_moy_annuelle,
                BV1= BV1,
                BV2= BV2,
                sin = sinuosite,
                per = perimetre_pla,
                prof=prof_moy_pla,
                sbs=substrat_shannon,
                pal=p_alter)


iml_var <- inner_join(table_iml,table_env_tot)

poi_var <- inner_join(table_poi,table_env_tot)

inv_var_zl <- inner_join(table_inv_zl,table_env_tot)

inv_var_zp <- inner_join(table_inv_zp,table_env_tot)

phy_var <- inner_join(table_phy,table_env_tot)

mac_var <- inner_join(table_mac,table_env_tot)

phy_var %>% select_if(is.numeric) %>% chart.Correlation()








setwd("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/PRET_ANALYSE/")

table_mixte <- bind_rows(poi_mixte %>% mutate(libelle = "poi"),
                         inv_mixte %>% mutate(libelle = "inv"),
                         mac_mixte %>% mutate(libelle = "mac"),
                         phy_mixte %>% mutate(libelle = "phy"),
                         iml_mixte %>% mutate(libelle = "iml")) %>% mutate(libelle = as.factor(libelle))
table_var <- bind_rows(poi_var %>% mutate(libelle = "poi"),
                       inv_var_zl %>% mutate(libelle = "izl"),
                       inv_var_zp %>% mutate(libelle = "izp"),
                       mac_var %>% mutate(libelle = "mac"),
                       phy_var %>% mutate(libelle = "phy"),
                       iml_var %>% mutate(libelle = "iml")) %>% mutate(libelle = as.factor(libelle))
dir()
write.csv(table_mixte,"table_mixte.csv",row.names = F)
write.csv(table_var,"table_complet.csv",row.names = F)
dir()

############################################################################################
#######
#######                       CLUSTERING TYPO LAC
#######
############################################################################################
poi_var <- data_var_2$poi
poi_var %>% dplyr::select(-c(fri,sha,ric,pie,fev,code_lac)) %>% 
  gather(var,val,-c(fdi,typo,typo2)) %>% 
  ggplot(aes(x=val,y=fdi))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth(method="lm")







setwd("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/ENV/ENV")

env_lac <- read_csv("env_lac.csv") 

comp_pc <- env_lac %>%
  dplyr::select_if(is.numeric) %>%
  mutate(superficie_pla = log(superficie_pla)) %>% 
  dplyr::select(-c(module,typo_lac_clust)) %>% 
  fun_comp(site = env_lac$code_lac,nom = "lac") %>% 
  dplyr::select(code_lac,2:3) %>% 
  column_to_rownames()
x11();comp_pc %>% PCA() 


fviz_nbclust(comp_pc, hcut, method = "silhouette") # methode des ilhouettes
fviz_nbclust(comp_pc, kmeans, method = "silhouette") # methode des ilhouettes
fviz_nbclust(comp_pc, hcut, method = "wss") # methode des ilhouettes
fviz_nbclust(comp_pc, kmeans, method = "wss") # methode des ilhouettes

hc <- hclust(dist(comp_pc)) %>% cutree(.,k=3)
km <-
  kmeans(comp_pc,
         centers = 3,
         nstart = 100)

fviz_cluster(km,data=comp_pc)
fviz_cluster(hc,data=dist(comp_pc))


env_lac2 <- env_lac %>% mutate(typo_lac_clust = km$cluster)
table(env_lac2$typo_clust_km,env_lac2$forme_cuvette)
#•
write.csv(env_lac2,"env_lac.csv",row.names = FALSE)
