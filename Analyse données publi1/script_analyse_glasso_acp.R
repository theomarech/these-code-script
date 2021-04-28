######################################################################################
###
###               SCRIPT ANALYSE VIA GLASSO MULTITAXO et composantes
###
######################################################################################

# L'objectif est de regarder quelles sont les types de variables participant le plus à la diversité fonctionnelle
# sous ces différentes composantes : équitabilité, divergence, richesse
# les variables seront des composantes principales
# les autres variables seront les autres indices biotiques
# l'hypothèse est que ces bien l'environnement qui structure la diversité fonctionnel principalement
# les convergences (corrélation des réponses) soont principalement due à l'environnement
# ensuite pour chaque taxons suivant l'endroit (benthique/ pelagique/littoral)
# taille et niveau trophique des individus

# un glasso pour chaque indicateur avec : tous les taxons ; les composantes ; et les variables
# sans les inv
# avec chaque inv 

#####################################################################################
########################## Préparation des données ##################################
#####################################################################################

#####################################################################################
##### TAXONS
setwd("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/PRET_ANALYSE/")
table <- map(dir(),~read_csv(.) %>%
               dplyr::select(!matches("simpson")) %>% 
               dplyr::select(-c(perim_km,battance_pmoy))%>% 
               mutate_if(is.character,as.factor)) 
names(table) <- dir()
dir()

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
table_inv <- table$inv_all.csv  %>% 
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
env_pc <- read_csv("env_physicochimie.csv")
comp_pc <- env_pc %>%
  dplyr::select_if(is.numeric) %>% 
  fun_comp(site = env_pc$code_lac,nom = "PC") %>% 
  dplyr::select(code_lac,2:3)

#### bassins versants
env_bv1 <-read_csv("env_bv.csv")
env_bv2 <- read_csv("env_bv_clc.csv")
env_bv3 <- inner_join(env_bv1,env_bv2,by="code_lac") %>% na.omit()
env_bv <- env_bv3%>%
  dplyr::select_if(is.numeric) %>%
  mutate_all(~log(.+1))
comp_bv <-  fun_comp(env_bv,site = env_bv3$code_lac,nom = "BV") %>% dplyr::select(code_lac,2:3)

env_bv %>% select_if(is.numeric) %>% PCA()
#### lac : perimetre_pla ; prof_moy ; alca; alitude ; tps_sejour
env_lac <- read_csv("env_lac.csv") 
comp_lac <- env_lac %>%
  dplyr::select_if(is.numeric) %>%
  mutate(superficie_pla = log(superficie_pla)) %>% 
  dplyr::select(-module) %>% 
  fun_comp(site=env_lac$code_lac,nom = "LAC")
env_lac %>% dplyr::select_if(is.numeric) %>% PCA()


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
colnames(table_env_mixte)

iml_mixte <- inner_join(table_iml,table_env_mixte)
poi_mixte <- inner_join(table_poi,table_env_mixte)
inv_mixte <- inner_join(table_inv,table_env_mixte)
phy_mixte <- inner_join(table_phy,table_env_mixte)
mac_mixte <- inner_join(table_mac,table_env_mixte)

###################################### ANALYSE MULTI ################################

multi_sans_inv <- join_all(list(poi_mixte %>% dplyr::select(code_lac,pofri=fri,poric=ric),
                             phy_mixte %>% dplyr::select(code_lac,phfri=fri,phric=ric),
                           mac_mixte %>% dplyr::select(-c(sha,fev,rao,pie,fdi))),
                           type = "inner", by="code_lac") %>% 
  select_if(is.numeric) %>% 
  mutate_all(~scale(.)) %>%
  na.omit()

glasso_poi <-   multi_sans_inv %>%
  dplyr::select_if(is.numeric) %>% 
  qgraph::cor_auto() %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(multi_sans_inv),
                 threshold = TRUE, layout = "spring",
                 cut = 0)

multi_iml <- join_all(list(poi_mixte %>% dplyr::select(code_lac,POfri=fri,POric=ric),
                                phy_mixte %>% dplyr::select(code_lac,PHfri=fri,PHric=ric),
                                iml_mixte %>% dplyr::select(code_lac,IMfri=fri,IMric=ric),
                                mac_mixte %>% dplyr::select(-c(sha,fev,rao,pie,fdi))),
                           type = "inner", by="code_lac") %>% 
  select_if(is.numeric) %>% 
  mutate_all(~scale(.)) %>%
  na.omit()

glasso_poi <-   multi_iml %>%
  dplyr::select_if(is.numeric) %>% 
  qgraph::cor_auto() %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(multi_iml),
                 threshold = FALSE, layout = "spring",
                 gamma = 0,
                 cut = 0)
multi_iml_inv <- join_all(list(poi_mixte %>% dplyr::select(code_lac,POfri=fri,POric=ric),
                           phy_mixte %>% dplyr::select(code_lac,PHfri=fri,PHric=ric),
                           inv_mixte %>% dplyr::select(code_lac,INfri=fri,INric=ric),
                           mac_mixte %>% dplyr::select(-c(sha,fev,rao,pie,fdi))),
                      type = "inner", by="code_lac") %>% 
  select_if(is.numeric) %>%
  mutate(INfri = log(INfri),
         INric = log(INric)) %>% 
  mutate_all(~scale(.)) %>%
  na.omit() 

glasso_poi <-   multi_iml_inv %>%
  dplyr::select_if(is.numeric) %>% 
  qgraph::cor_auto() %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(multi_iml_inv),
                 threshold = TRUE, layout = "spring",
                 gamma = 0,
                 cut = 0)
