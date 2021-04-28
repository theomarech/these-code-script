################################################# TABLEAU DIVERSITE FONCTIONNELLE ##########################################################
rm(list=ls())
gc()
library("PCPS")
library("otuSummary")
################ Poissons -----------------------------------------------------------------------------------------------------------
### Spécifique et fonctionnelle

com_poi <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_poissons.csv")
tra_poi <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_poissons.csv") %>%
  mutate(prot_oeu = as.numeric(prot_oeu)) %>% 
  mutate(zone_ali = ifelse(zone_ali=="ben",0,1))

# traits à sélectionner ---------------------------------------------------

tra_interact_poi <- c("nom_taxo","zone_ali","zone_rep","pref_hab","typealimenttion","biomasse_med","taille_med") 

# filtrage des données  ---------------------------------------------------

df_c <- com_poi
df_t <- tra_poi %>% dplyr::select(all_of(tra_interact_poi)) %>% mutate_if(is.character,as.factor)
df_t <- tra_poi %>% dplyr::select(-c(period_incub,diam_oeu,fec,tps_eclo,female_mat,taille_larves,reprob)) %>% mutate_if(is.character,as.factor)


pco_poi <- df_t %>% column_to_rownames() %>% gowdis() %>% sqrt() %>% dudi.pco(scannf = F,nf=5)
pco_poi$eig/sum(pco_poi$eig)
scatter(pco_poi)
nax = 4 

poi_filtre <- df_c %>% 
  mutate_if(is.numeric,~dplyr::if_else(.>0,1,0)) %>% 
  as.data.frame() %>% 
  column_to_rownames("code_lac")%>% 
  t() %>%
  as.data.frame() %>% 
  dplyr::select(where(~sum(.)>(nax+1))) %>% 
  t() %>% 
  as.data.frame() %>%
  dplyr::select_if(~sum(.)!=0) %>% 
  tibble::rownames_to_column("code_lac") 

nom_lac_poi <- poi_filtre$code_lac
nom_esp_poi <- colnames(poi_filtre)

df_c <- df_c %>% dplyr::filter(code_lac %in% nom_lac_poi) %>% dplyr::select(code_lac,all_of(nom_esp_poi))
df_t <- df_t %>% dplyr::filter(nom_taxo %in% nom_esp_poi)
table_fun_poi <- fun_div(df_c=df_c,df_t=df_t ,m=5,corr="sqrt",func = TRUE,stand.x = FALSE)

table_tax_poi <- fun_div(df_c=df_c,df_t=df_t ,func=FALSE)

### Beta spécifique et fonctionnelle
# beta_fonctionnelle poisson
trait_poi_beta <- df_t %>% column_to_rownames() %>% gowdis() %>% sqrt() %>% dudi.pco(scannf = F,nf=5) 
biplot(trait_poi_beta)
(trait_poi_beta$eig/sum(trait_poi_beta$eig))*100
com <- df_c %>% 
  mutate_if(is.numeric,~ifelse(.>0,1,0)) %>% 
  dplyr::filter(!code_lac %in% c("PRA66","ALE2B","CHA38","RMN29","ALZ81","POR74","ANT74","GAS14")) %>% 
  column_to_rownames()

traits = trait_poi_beta$li[,1:4]

beta_fun_poi <- functional.beta.pair(x = com,
                                     traits = traits,
                                     index.family = "jaccard")


trait_poi_beta <- df_t %>% column_to_rownames() %>% gowdis() %>% sqrt() %>% hclust() 
plot(trait_poi_beta)


beta_div <- betapart::beta.pair(df_c %>% column_to_rownames() %>% mutate_all(~ifelse(.>0,1,0)),index.family = "jaccard")
beta_div_abun <- betapart::beta.pair.abund(df_c %>% column_to_rownames(),index.family = "bray")

beta <- beta_div$beta.jac %>% as.matrix() %>% as.data.frame()

names(beta) <- df_c$code_lac
rownames(beta) <- df_c$code_lac

beta_poi_com <- otuSummary::matrixConvert(beta)


################ Phytoplancton ###############
com_phyto_ab <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_phytoplancton_abondance.csv"
  )

data <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DONNEES TRAITS PHYTO/BDD_TRAVAIL/tableau_phyto_impute.csv"
  ) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.binaire, as.logical) %>%
  mutate(
    Nb_cells_colony = as.integer(Nb_cells_colony),
    Nb_Flagellum = factor(Nb_Flagellum, order = TRUE),
    Maximal_length_algal_object = as.numeric(Maximal_length_algal_object)
  ) %>%
  mutate_at(c(16, 17, 18, 21, 29, 39, 40, 74), factor, order = TRUE) %>%
  column_to_rownames() %>%
  as.data.frame() %>%
  missForest(maxiter = 40,
             ntree = 100,
             variablewise = TRUE) %>%
  .$ximp %>%
  tibble::rownames_to_column(var = "nom_taxo") %>%
  dplyr::select(nom_taxo, Cell_Biovolume, Ind_Biovolume, Life_Form)







com_phy <- com_phyto_ab %>%
  gather(nom_taxo, abondance,-code_lac) %>%
  inner_join(data) %>%
  dplyr::transmute(code_lac,
                   nom_taxo,
                   biovolume = ifelse(
                     Life_Form == "Cel.",
                     (abondance * Cell_Biovolume) / 1000000,
                     (abondance * Ind_Biovolume) / 1000000
                   )) %>%
  tidyr::spread(nom_taxo, biovolume, fill = 0)

dist_phy_ratio <- tra_phy_ratio  %>%
  arrange(nom_taxo) %>%
  mutate_if(is.character, as.factor) %>%
  mutate_if(is.binaire, as.numeric) %>%
  dplyr::select(-Organic_carbon_ratio)

df_t_phy <-
  dist_phy_ratio %>% dplyr::filter(nom_taxo %in% colnames(com_phy))

df_c_phy <- com_phy %>% dplyr::select(code_lac, df_t_phy$nom_taxo)

nax = 4

phy_filtre <- df_c_phy %>%
  mutate_if(is.numeric,  ~ dplyr::if_else(. > 0, 1, 0)) %>%
  as.data.frame() %>%
  column_to_rownames() %>%
  t() %>%
  as.data.frame() %>%
  dplyr::select(where( ~ sum(.) > (nax + 1))) %>%
  t() %>%
  as.data.frame() %>%
  dplyr::select_if( ~ sum(.) != 0) %>%
  tibble::rownames_to_column(var = "code_lac")

nom_lac_phy <- phy_filtre$code_lac
nom_esp_phy <- colnames(phy_filtre)

df_c_phy <-
  df_c_phy %>% dplyr::filter(code_lac %in% nom_lac_phy) %>% dplyr::select(code_lac, all_of(nom_esp_phy))
df_t_phy <-
  df_t_phy %>% dplyr::filter(nom_taxo %in% nom_esp_phy) %>%
  mutate(
    Reproduction = case_when(
      Reproduction  %in% c(
        "Asexual-binary fission",
        "Asexual-binary fission or akinete",
        "Asexual"
      ) ~ "Asexual",
      Reproduction %in% "Sexual and asexual" ~ "SexualAsexual",
      TRUE ~ "Sexual"
    )
  ) 



df_com <- df_c_phy %>% column_to_rownames()
df_trait <- df_t_phy %>% column_to_rownames() 
mat_div_phy <- FD::dbFD(x = df_trait,a = df_com, corr = "sqrt", m = 2)

table_fun_phy <- fun_div(df_c=df_c_phy,df_t=df_t_phy ,m=2,corr="sqrt",func = TRUE,stand.x = FALSE)

table_tax_phy <- fun_div(df_c=df_c_phy,df_t=df_t_phy ,func=FALSE)


### Beta spécifique et fonctionnelle
trait_phy_beta <- df_trait  %>% gowdis() %>% sqrt() %>% dudi.pco(scannf = F,nf=5) 
trait_phy_beta$eig/sum(trait_phy_beta$eig)
biplot(trait_phy_beta)


com <- df_c %>% 
  mutate_if(is.numeric,~ifelse(.>0,1,0)) %>% 
  dplyr::filter(!code_lac %in% c("EYC05")) %>% 
  column_to_rownames()

traits = trait_phy_beta$li[,1:4]


beta_fun_phy <- functional.beta.pair(x = com,
                                     traits = traits,
                                     index.family = "jaccard")


trait_poi_beta <- df_t %>% column_to_rownames() %>% gowdis() %>% sqrt() %>% hclust() 
plot(trait_poi_beta)


beta_div <- betapart::beta.pair(df_c %>% column_to_rownames() %>% mutate_all(~ifelse(.>0,1,0)),index.family = "jaccard")
beta_div_abun <- betapart::beta.pair.abund(df_c %>% column_to_rownames(),index.family = "bray")

beta <- beta_div$beta.jac %>% as.matrix() %>% as.data.frame()

names(beta) <- df_c$code_lac
rownames(beta) <- df_c$code_lac

beta_phy_com <- otuSummary::matrixConvert(beta)


################ Macroinvertebres ###############
com_inv <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres_inv_zp.csv")
tra_inv <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macroinvertebres_inv_zp.csv")

df_c <- com_inv
df_t <- tra_inv %>% arrange(nom_taxo)

mat_div_inv <- fun_div(df_c=df_c,df_t=df_t ,corr="none",func = TRUE,stand.x = FALSE)
table_fun_inv <- mat_div_inv %>% mutate(code_lac=com_inv$code_lac)

table_tax_inv <- fun_div(df_c=df_c,df_t=df_t ,func=FALSE)


beta_fun_poi <- betapart::functional.beta.pair(x = df_c %>%  mutate_if(is.numeric,~ifelse(.>0,1,0))  %>% column_to_rownames(),
                                               traits = df_t %>% column_to_rownames())

com_inv_iml <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres_iml.csv")
tra_inv_iml <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macroinvertebres_iml.csv")

df_c <- com_inv_iml
df_t <- tra_inv_iml %>% arrange(nom_taxo)

mat_div_inv <- fun_div(df_t=df_t,df_c=df_c ,corr="none",func = TRUE,stand.x = FALSE)
table_fun_inv_iml <- mat_div_inv %>% mutate(code_lac=com_inv_iml$code_lac)

table_tax_inv_iml <- fun_div(df_c=df_c,df_t=df_t ,func=FALSE)

dplyr::filter(table_fun_inv,code_lac %in% table_fun_inv_iml$code_lac)


### Beta spécifique et fonctionnelle
nax = 5 

com <- df_c %>% 
  mutate_if(is.numeric,~ifelse(.>0,1,0)) %>% 
  column_to_rownames()%>% 
  t() %>%
  as.data.frame() %>% 
  dplyr::select(where(~sum(.)>(nax+1))) %>% 
  t() %>% 
  as.data.frame()

trait_izp_beta <- df_t %>% dplyr::filter(nom_taxo %in% colnames(com)) %>% 
  column_to_rownames() %>% gowdis() %>% sqrt() %>% dudi.pco(scannf = F,nf=5) 
(trait_izp_beta$eig/sum(trait_izp_beta$eig))*100
biplot(trait_izp_beta)



traits = trait_izp_beta$li[,1:nax]

dim(com)
dim(traits)
traits <- 
  
  beta_fun_mac <- functional.beta.pair(x = com,
                                       traits = traits,
                                       index.family = "jaccard")



trait_mac_beta <- df_t %>% column_to_rownames() %>% gowdis() %>% sqrt() %>% hclust() 
plot(trait_poi_beta)


beta_div <- betapart::beta.pair(df_c %>% column_to_rownames() %>% mutate_all(~ifelse(.>0,1,0)),index.family = "jaccard")
beta_div_abun <- betapart::beta.pair.abund(df_c %>% column_to_rownames(),index.family = "bray")

beta <- beta_div$beta.jac %>% as.matrix() %>% as.data.frame()

names(beta) <- df_c$code_lac
rownames(beta) <- df_c$code_lac

beta_izp_com <- otuSummary::matrixConvert(beta)




############## PHYTOPLANCTON BIOVOLUME -------------------------------------------------------------------
tra_phyto_bio <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_phytoplancton_biovolume.csv")
tra_phyto_ab <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_phytoplancton_abondance.csv")
com_phyto_bio <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_phytoplancton_biovolume.csv")
com_phyto_ab <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_phytoplancton_abondance.csv")


data <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DONNEES TRAITS PHYTO/BDD_TRAVAIL/tableau_phyto_impute.csv") %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.binaire,as.logical) %>%
  mutate(Nb_cells_colony = as.integer(Nb_cells_colony),
         Nb_Flagellum = factor(Nb_Flagellum,order=TRUE),
         Maximal_length_algal_object = as.numeric(Maximal_length_algal_object)) %>%
  mutate_at(c(16,17,18,21,29,39,40,74),factor,order=TRUE) %>% 
  tibble::column_to_rownames(var="Taxa_Name") %>% 
  as.data.frame() %>% 
  missForest(maxiter = 40, 
             ntree = 100,
             variablewise = TRUE)

biovol_ab <- data$ximp %>% tibble::rownames_to_column(var="nom_taxo") %>% dplyr::select(nom_taxo, Cell_Biovolume,Ind_Biovolume,Life_Form)

com_phy <- com_phyto_ab %>% 
  gather(nom_taxo,abondance,-code_lac) %>% 
  inner_join(biovol_ab) %>%
  dplyr::transmute(code_lac,
                   nom_taxo,
                   biovolume = ifelse(Life_Form == "Cel.",
                                      (abondance*Cell_Biovolume)/1000000,
                                      (abondance*Ind_Biovolume)/1000000)) %>% 
  tidyr::spread(nom_taxo,biovolume,fill=0)
write.csv(com_phy,
          "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_phytoplancton_biovolume.csv",
          row.names=FALSE)

tra_phy_ratio <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_phyto_ratio.csv")

dist_phy_ratio <- tra_phy_ratio  %>%
  arrange(nom_taxo) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.binaire,as.numeric) %>% 
  dplyr::select(-Organic_carbon_ratio)

df_c <- com_phy %>% select_if(is.numeric)%>% select_if(~sum(.)!=0)
df_t <- dist_phy_ratio %>% dplyr::filter(nom_taxo %in% colnames(com_phy))
df_c <- com_phy %>% dplyr::select(code_lac,df_t$nom_taxo)

df_com <- df_c %>% column_to_rownames()
df_trait <- df_t %>% column_to_rownames() %>% gowdis() %>% sqrt()


mat_div_phy <- fun_div(df_c=df_c,df_t=df_t ,corr="sqrt",func = TRUE,stand.x = FALSE)
diversite_fonct <- mat_div_phy[mat_div_phy %>% names() %>% .[-c(length(.),4)]] %>% bind_cols()
table <- diversite_fonct %>%
  mutate(code_lac=df_com$code_lac) %>% 
  setnames(colnames(.)[-ncol(.)],paste0("DIV_",colnames(.)[-ncol(.)]))

table_fun_phy <- table %>% mutate(code_lac=df_c$code_lac)

table_tax_phy <- fun_div(df_c=df_c,df_t=df_t ,func=FALSE)







################ Macrophytes ###############
com_mac <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macrophytes.csv")
tra_mac <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macrophytes.csv") %>% 
  dplyr::select(-c(FRU,DIS,SES,OCC,FDF))

df_c <- com_mac %>% column_to_rownames() %>% .[which(rowSums(.)!=0),] %>% tibble::rownames_to_column(var="code_lac")
df_t <- tra_mac %>% arrange(nom_taxo) %>% mutate(BRA = ifelse(BRA=="yes",1,0)) %>% mutate_if(is.character,as.factor)

table_fun_mac <- fun_div(df_c=df_c_mac,df_t=df_t_mac ,corr="sqrt",func=TRUE)
table_tax_mac <- fun_div(df_c=df_c_mac,df_t=df_t_mac ,func=FALSE)

table_tax_mac %>% select_if(is.numeric)  %>% chart.Correlation()
### Beta spécifique et fonctionnelle
nax = 4

com <- df_c %>% 
  mutate_if(is.numeric,~dplyr::if_else(.>0,1,0)) %>% 
  as.data.frame() %>% 
  column_to_rownames()%>% 
  t() %>%
  as.data.frame() %>% 
  dplyr::select(where(~sum(.)>(nax+1))) %>% 
  t() %>% 
  as.data.frame() %>%
  dplyr::select_if(~sum(.)!=0) %>% 
  tibble::rownames_to_column(var="code_lac") 
df_t_mac <- df_t %>%  dplyr::filter(nom_taxo %in% colnames(com)[-1]) 
df_c_mac <- df_c %>% dplyr::filter(code_lac%in%com$code_lac) %>% dplyr::select(code_lac,all_of(df_t_mac$nom_taxo))

table_fun_mac <- fun_div(df_c=df_c_mac,df_t=df_t_mac , m = "max", corr="sqrt",func=TRUE)
table_tax_mac <- fun_div(df_c=df_c_mac,df_t=df_t_mac ,func=FALSE)

trait_mac_beta <- df_t %>% dplyr::filter(nom_taxo %in% nom_esp_mac) %>% 
  column_to_rownames() %>% gowdis() %>% sqrt() %>% dudi.pco(scannf = F,nf=5) 

(trait_mac_beta$eig/sum(trait_mac_beta$eig))*100

biplot(trait_mac_beta)


 
traits = trait_mac_beta$li[,1:nax]

dim(com)
dim(traits)
traits <- 

beta_fun_mac <- functional.beta.pair(x = com,
                                     traits = traits,
                                     index.family = "jaccard")


trait_mac_beta <- df_t %>% column_to_rownames() %>% gowdis() %>% sqrt() %>% hclust() 
plot(trait_poi_beta)


beta_div <- betapart::beta.pair(df_c %>% column_to_rownames() %>% mutate_all(~ifelse(.>0,1,0)),index.family = "jaccard")
beta_div_abun <- betapart::beta.pair.abund(df_c %>% column_to_rownames(),index.family = "bray")

beta <- beta_div$beta.jac %>% as.matrix() %>% as.data.frame()

names(beta) <- df_c$code_lac
rownames(beta) <- df_c$code_lac

beta_mac_com <- otuSummary::matrixConvert(beta)
hist(table_fun_poi$DIV_FRic) == hist(data_var2$poi$fri)

####################################################################"""

write.csv(table_fun_poi,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_CWM/traits_interactions/indice_fun_poissons.csv",row.names = FALSE)
write.csv(table_fun_mac,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_CWM/traits_interactions/indice_fun_macrophytes.csv",row.names = FALSE)
write.csv(table_fun_inv_iml,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_CWM/indice_fun_macroinvertebres_iml.csv",row.names = FALSE)
write.csv(table_fun_inv,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_CWM/indice_fun_macroinvertebres_zp.csv",row.names = FALSE)
write.csv(table_fun_phy,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_CWM/indice_fun_phytoplancton.csv",row.names = FALSE)

write.csv(table_tax_poi,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_CWM/traits_interactions/indice_tax_poissons.csv",row.names = FALSE)
write.csv(table_tax_mac,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_CWM/traits_interactions/indice_tax_macrophytes.csv",row.names = FALSE)
write.csv(table_tax_inv_iml,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_TAX/indice_tax_macroinvertebres_iml.csv",row.names = FALSE)
write.csv(table_tax_inv,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_TAX/indice_tax_macroinvertebres_zp.csv",row.names = FALSE)
write.csv(table_tax_phy,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_TAX/indice_tax_phytoplancton.csv",row.names = FALSE)



fun_div <- function(df_c,df_t,corr="sqrt",m = 3,func = TRUE,stand.x = FALSE){
  df_com <- df_c %>% column_to_rownames()
  
  df_trait <- df_t %>% column_to_rownames()
  
  df_com_rich <- df_com %>% 
    mutate_all(~ifelse(.==0,0,1)) %>% 
    rowSums() %>% 
    data.frame(DIV_richesse=.) 
  
  diversite_spe <- map_dfc(c("shannon","simpson"),
                           ~df_com %>%
                             vegan::diversity(index=.x)) %>% setnames(colnames(.),c("DIV_shannon","DIV_simpson"))
  if(func==TRUE){
    
    diversite_fonct1 <- FD::dbFD(df_trait,df_com,corr = corr,m = m ,stand.x = stand.x)
    
    diversite_fonct <- diversite_fonct1[diversite_fonct1 %>% names() %>% .[-c(length(.),4)]] %>% bind_cols()
     table <- diversite_fonct %>%
      setnames(colnames(.),paste0("DIV_",colnames(.))) %>% 
      bind_cols(diversite_fonct1$CWM %>% setnames(colnames(.),paste0("CWM_",colnames(.)))) %>% 
       mutate(code_lac=df_c$code_lac) %>% 
      return()
     
  }else{
    table <- bind_cols(code_lac=df_com %>% tibble::rownames_to_column(var='code_lac') %>% 
                         pull(code_lac),diversite_spe,df_com_rich) %>% 
      mutate(DIV_pielou = DIV_shannon / log(DIV_richesse)) %>% 
      return()
  }
}


fun_beta<-function(df_c,df_t,nax=4){
  
  
  trait_izp_beta <- df_t %>% 
    column_to_rownames() %>% gowdis() %>% sqrt() %>% dudi.pco(scannf = F,nf=10) 
  
  trait_izp_beta$eig/sum(trait_izp_beta$eig)
  
  traits <- trait_izp_beta$li[,1:nax]
  
  com <- df_c  %>% mutate_if(is.numeric,~ifelse(.>0,1,0))%>% column_to_rownames()
  
  beta_fun_mac <- functional.beta.pair(x = com,
                                       traits = traits,
                                       index.family = "jaccard") %>% return()
}

a <- fun_beta(list_com_test[[3]],list_tra_test[[3]],nax=3)
rm(list=ls())
gc()
