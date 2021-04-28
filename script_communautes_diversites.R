############################################################# DONNNES DIVERSITES AU PROPRE ###############################################
## OBJECTIF : tableau de contingence pour chaque groupe taxonomique
## se font simplement à partir des data brutes
## sauf pour macrophytes et phytoplanctons
## pour les macrophytes : suppression des taxons qui ont des indices d'aqua > 4
## pour les phytos : suppresion des espèces trsè rare (à définir, choix arbitraire)
source("/home/theo/Bureau/scriptR/librairies.R")

setwd("/home/theo/Bureau/Data/DATA/DONNEES TRAVAIL/communautés")

############### MACROPHYTES #################
table_macrophytes <- read_csv("df_macrophytes.csv") # table contenant les données issues de la base
aquaticite %>% dplyr::filter(nom_taxon == "Agrostis stolonifera")
aquaticite <- read_csv("/home/theo/Bureau/Data/Données traits/Synthèse traits/macrophytes/aquaticite_complet.csv") %>% distinct()

jointure_macro_aqua <- inner_join(table_macrophytes,aquaticite, by=c("nom_taxo"="nom_taxon")) %>% dplyr::filter(aquaticite %in% c(1,2,4))

anti_nom <- anti_join(table_macrophytes,aquaticite, by=c("nom_taxo"="nom_taxon")) %>% 
  dplyr::select(nom_taxo,aquaticite) %>% 
  distinct()

table_contingence_macro <-jointure_macro_aqua %>%
  group_by(code_lac,nom_taxo) %>%
  dplyr::summarise(abondance=mean(abondance,na.rm=TRUE)) %>%
  ungroup() %>% 
  mutate(nom_taxo = str_replace_all(nom_taxo," ","_")) %>% 
  spread(nom_taxo,abondance,fill=0)

write.csv(table_contingence_macro,"/home/theo/Bureau/Data/DATA/DONNEES DIVERSITES/macrophytes.csv",row.names = FALSE)


############### PHYTOPLANCTON #################
#### On ne garde que les taxons ayant au moins 10 prèlevement et présent dans plus de 1 lac
table_phytoplancton <- read_csv("df_phytoplanctons.csv") # table contenant les données issues de la base

occ_prelev <- table_phytoplancton %>%
  group_by(nom_ref) %>%
  dplyr::summarise(n=n()) %>%
  arrange(desc(n)) %>% 
  dplyr::filter(n>10)

occ_lac <- table_phytoplancton %>%
  group_by(nom_ref,code_lac) %>%
  dplyr::summarise(n=1) %>%
  ungroup() %>% 
  spread(nom_ref,n,fill=0) %>% 
  dplyr::select(-code_lac) %>% 
  dplyr::summarise_all(sum) %>% dplyr::select_if(.>1)

nom_select <- inner_join(
  data.frame(nom_taxo=occ_prelev$nom_ref),
  data.frame(nom_taxo=colnames(occ_lac)),
  by="nom_taxo")

table_contingence_phyto <- table_phytoplancton %>% 
  dplyr::filter(nom_ref %in% nom_select$nom_taxo, confer== FALSE) %>% 
  mutate(nom_ref = str_replace_all(nom_ref," ","_")) %>% 
  group_by(code_lac,nom_ref) %>% 
  dplyr::summarise(abondance=mean(abondance,na.rm=TRUE)) %>% 
  spread(nom_ref,abondance,fill=0)

write.csv(table_contingence_phyto,"/home/theo/Bureau/Data/DATA/DONNEES DIVERSITES/phytoplancton.csv",row.names = FALSE)



############### MACROINVERTEBRES #################

table_macroinvertebres <- read_csv("df_macroinvertebres.csv") # table contenant les données issues de la base

table_macroinvertebres_zl <- table_macroinvertebres  %>% dplyr::filter(zone_inv=="ZL") # juste les données en zone littorale

table_contingence_macroinvertebres <- table_macroinvertebres %>% 
  mutate(nom_taxo = str_replace_all(nom_taxo," ","_")) %>% 
  group_by(nom_taxo,code_lac) %>% 
  dplyr::summarise(abondance=mean(abondance,na.rm=TRUE)) %>% 
  ungroup() %>% 
  spread(nom_taxo,abondance,fill=0) %>% 
  na.omit()

write.csv(table_contingence_macroinvertebres,"/home/theo/Bureau/Data/DATA/DONNEES DIVERSITES/macroinvertebres.csv",row.names = FALSE)



############### POISSONS #################

table_poissons <- read_csv("df_poissons.csv")

table_contingence_poissons <- table_poissons %>% 
  dplyr::filter(str_detect(nom_taxo," "),!str_detect(nom_taxo,"Hybride")) %>% 
  mutate(nom_taxo = str_replace_all(nom_taxo," ","_")) %>% 
  group_by(nom_taxo,code_lac) %>% 
  dplyr::summarise(abondance=mean(biomasse,na.rm=TRUE)) %>% 
  ungroup() %>% 
  spread(nom_taxo,abondance,fill=0)


write.csv(table_contingence_poissons,"/home/theo/Bureau/Data/DATA/DONNEES DIVERSITES/poissons.csv",row.names = FALSE)





############################################################# DONNNES FONCTIONNELLE AU PROPRE ###############################################
## OBJECTIF : tableau de traits pour chaque maillons

############### MACROPHYTES #################
com_macrophytes <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES DIVERSITES/macrophytes.csv")
traits_macrophytes <- read_ods("/home/theo/Bureau/Data/DATA/DONNEES TRAVAIL/traits/traits_macrophytes_travail.ods") %>% 
  as_tibble() %>% mutate(nom_taxo =str_replace_all(nom_taxo," ","_")) %>% 
  .[which((!is.na(.)) %>% rowSums()>1),] %>% 
  inner_join(
    read_csv("/home/theo/Bureau/Data/DATA/DONNEES TRAVAIL/traits/traits_macrophytes_final.csv") %>%
      dplyr::select(nom_taxo,SLA,CAHE)%>% 
      mutate(nom_taxo =str_replace_all(nom_taxo," ","_")),
    by="nom_taxo")
  traits_macrophytes %>% dplyr::filter(nom_taxo == "Potamogeton_lucens")
nom_com_macro <- com_macrophytes %>% 
  gather(nom_taxo,abondance,-code_lac) %>%
  dplyr::select(nom_taxo) %>% 
  distinct()

traits_macro_impute <- traits_macrophytes %>%
  mutate_if(is.character,as.factor) %>% 
  column_to_rownames() %>% 
  as.data.frame() %>% 
  missForest() %>% 
  .$ximp %>% 
  tibble::rownames_to_column(var="nom_taxo")
acp <-  traits_macro_impute %>% column_to_rownames() %>% mutate_if(is.character,as.factor) %>% dudi.hillsmith()
a <- traits_macro_impute %>% mutate_if(is.character,as.factor)  %>% column_to_rownames()%>% gowdis() %>% sqrt() %>% dudi.pco()

nom_commun_macro <-  inner_join(traits_macro_impute,nom_com_macro) %>% 
  as_tibble() %>% 
  .$nom_taxo

com_macrophytes_jointure <-  com_macrophytes %>%
  gather(nom_taxo,abondance,-code_lac) %>% 
  dplyr::filter(nom_taxo %in%nom_commun_macro) %>% 
  spread(nom_taxo,abondance) %>% 
  column_to_rownames() %>% 
  tibble::rownames_to_column(var="code_lac") %>% 
  as_tibble()

tra_macrophytes_jointure <-  traits_macro_impute %>%
  dplyr::filter(nom_taxo %in%nom_commun_macro) %>% as_tibble()

write.csv(com_macrophytes_jointure,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macrophytes.csv",row.names = FALSE)
write.csv(tra_macrophytes_jointure,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macrophytes.csv",row.names = FALSE)


############### POISSONS #################
com_poissons <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES DIVERSITES/poissons.csv")
traits_poissons <- read_ods("/home/theo/Bureau/Data/Données finales/traits/traits_poissons_homo.ods") %>%
  dplyr::select(-nom_taxo_phylo) %>% setnames("nom_taxo_lien_com","nom_taxo") %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(female_mat = factor(female_mat,ordered = TRUE),
         nom_taxo = str_replace_all(nom_taxo," ", "_")) %>% 
  as_tibble() %>% 
  column_to_rownames() %>% 
  missForest() %>% 
  .$ximp %>% 
  tibble::rownames_to_column(var="nom_taxo")
  
  
nom_commun_poisson <- inner_join(
    data.frame(nom_taxo=colnames(com_poissons)[-1]),
    traits_poissons %>% dplyr::select(nom_taxo)
    )

com_poisson_filtre <-  com_poissons %>% 
  gather(nom_taxo,abondance,-code_lac) %>% 
  dplyr::filter(nom_taxo %in% nom_commun_poisson$nom_taxo) %>% 
  spread(nom_taxo,abondance) %>% 
  column_to_rownames() %>% 
  tibble::rownames_to_column(var="code_lac")

tra_poisson_filtre <- traits_poissons %>% 
  dplyr::filter(nom_taxo %in% nom_commun_poisson$nom_taxo)

dim(com_poisson_filtre)-1 ; dim(tra_poisson_filtre)



tra_poi_ratio <- read_ods("/home/theo/Bureau/Data/DATA/DONNEES TRAVAIL/traits/traits_poissons_ratio.ods") %>% 
  column_to_rownames() %>% 
  missForest() %>% 
  .$ximp %>% 
  PCA()



write.csv(com_poisson_filtre,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_poissons.csv",row.names = FALSE)
write.csv(tra_poisson_filtre,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_poissons.csv",row.names = FALSE)




############### MACROINV #################

com_macroinvertebres <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES DIVERSITES/macroinvertebres.csv")
tra_macroinvertebres <- read_csv("/home/theo/Bureau/Data/Données finales/traits/traits_macroinvertebres.csv") %>% 
  mutate(nom_taxo=str_replace_all(nom_taxo," ","_")) %>% 
  column_to_rownames() %>% 
  missForest() %>% 
  .$ximp %>% 
  tibble::rownames_to_column(var="nom_taxo")
  
  

nom_commun_macroinvertebres <- inner_join(
  data.frame(nom_taxo=colnames(com_macroinvertebres)[-1]),
  tra_macroinvertebres %>% dplyr::select(nom_taxo)
)

com_macroinvertebres_filtre <-  com_macroinvertebres %>% 
  gather(nom_taxo,abondance,-code_lac) %>% 
  dplyr::filter(nom_taxo %in%nom_commun_macroinvertebres$nom_taxo) %>% 
  spread(nom_taxo,abondance) %>% 
  column_to_rownames() %>% 
  tibble::rownames_to_column(var="code_lac")

tra_macroinvertebres_filtre <- tra_macroinvertebres %>% 
  dplyr::filter(nom_taxo %in% nom_commun_macroinvertebres$nom_taxo)

dim(com_macroinvertebres_filtre)-1 ; dim(tra_macroinvertebres_filtre)

write.csv(com_macroinvertebres_filtre,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres.csv",row.names = FALSE)
write.csv(tra_macroinvertebres_filtre,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macroinvertebres.csv",row.names = FALSE)

############### PHYTOPLANCTON #################

com_phytoplancton <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES DIVERSITES/phytoplancton.csv")
tra_phytoplancton <- read_ods("/home/theo/Bureau/Data/Données finales/traits/traits_phytoplancton_lastversion.ods") %>% 
  mutate(nom_taxo = str_replace_all(nom_taxo," ","_")) 

tra_phytoplancton_typevar <- tra_phytoplancton %>% 
  as_tibble() %>% 
  mutate_at(1:10,as.factor) %>% 
  mutate_at(11:17,factor,ordered=TRUE) %>% 
  mutate_at(18:38,as.logical) 

tra_phytoplancton_impute <- tra_phytoplancton_typevar %>% 
  column_to_rownames() %>% 
  as.data.frame() %>% 
  missForest() %>% 
  .$ximp %>% 
  tibble::rownames_to_column(var="nom_taxo")

a <- tra_phytoplancton_impute %>% mutate_if(is.logical,as.numeric) %>% column_to_rownames() %>% vegdist(method = "gower")
b <- dudi.pco(a)
nom_commun_phytoplancton<-inner_join(data.frame(nom_taxo=colnames(com_phytoplancton)[-1]),
           tra_phytoplancton_impute %>% dplyr::select(nom_taxo)) %>% as_tibble()

com_phytoplancton_filtre <-  com_phytoplancton %>% 
  gather(nom_taxo,abondance,-code_lac) %>% 
  dplyr::filter(nom_taxo %in% nom_commun_phytoplancton$nom_taxo) %>% 
  spread(nom_taxo,abondance) %>% 
  column_to_rownames() %>% 
  tibble::rownames_to_column(var="code_lac") %>% 
  as_tibble()


tra_phytoplancton_filtre <- tra_phytoplancton_impute %>% 
  dplyr::filter(nom_taxo %in% nom_commun_phytoplancton$nom_taxo)

dim(com_phytoplancton_filtre)-1 ; dim(tra_phytoplancton_filtre)

write.csv(com_phytoplancton_filtre,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_phytoplancton.csv",row.names = FALSE)
write.csv(tra_phytoplancton_filtre,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_phytoplancton.csv",row.names = FALSE)
#



























































com_macrophytes_jointure %>% dplyr::select(-code_lac) %>% rowSums()
df_c <- com_macrophytes_jointure %>% 
  column_to_rownames()
df_t <-tra_macrophytes_jointure %>% arrange(nom_taxo) %>% column_to_rownames()
fun_div <- function(df_c,df_t,func = TRUE){
  
  df_trait <- df_t
  df_c <- df_c %>% .[which(!rowSums(.)==0),]
  
  esp_0 <- df_c %>% as.data.frame()%>%dplyr::select_if( colSums(.)==0) %>% colnames()
  df_com <- df_c%>% dplyr::select(-esp_0)
  df_trait <- df_trait %>% as.data.frame() %>% dplyr::filter(!nom_taxo%in%esp_0) %>% column_to_rownames(ncol(.))
  df_com_rich <- df_com %>% mutate_if(is.numeric,~ifelse(.==0,0,1)) %>% rowSums() %>% data.frame(DIV_richesse=.) %>% tibble::rownames_to_column(var="code_lac")
  diversite_spe <- map_dfc(c("shannon","simpson"),
                           ~df_com %>%
                             dplyr::select(-code_lac) %>%
                             diversity(index=.x)) %>% setnames(colnames(.),c("DIV_shannon","DIV_simpson"))
  if(func==TRUE){
    
    diversite_fonct1 <- FD::dbFD(df_trait,df_com,corr = "cailliez")
    diversite_fonct <- diversite_fonct1[diversite_fonct1 %>% names() %>% .[-c(length(.),4)]] %>% bind_cols()
    table <- bind_cols(diversite_fonct,diversite_spe) %>%
      mutate(code_lac=df_com$code_lac) %>% 
      setnames(colnames(.)[-ncol(.)],paste0("DIV_",colnames(.)[-ncol(.)])) %>% 
      bind_cols(diversite_fonct1$CWM %>% setnames(colnames(.),paste0("CWM_",colnames(.)))) %>% 
      return()
  }else{
    table <- bind_cols(diversite_spe,df_com_rich)
  }
}
diversite_fonct1$CWM %>% tibble::rownames_to_column(var="code_lac") %>% dplyr::filter(code_lac != "VEN06") %>%  column_to_rownames() %>% dudi.hillsmith() %>% scatter()
mat_div <- fun_div(df_c=df_c_macro,df_t=df_t_macro ,func=TRUE)
