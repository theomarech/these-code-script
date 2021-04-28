
rm(list=ls())
gc()

#note : avec les comp des var selectionnée
source("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/scriptR/librairies.R",
       encoding = "utf8")


setwd("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/PRET_ANALYSE/")

var_quali <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/ENV/ENV/env_lac.csv") %>% 
  dplyr::select(code_lac,forme_cuvette = typo_lac_clust)

data_var2 <- read_csv("table_mixte.csv") %>%
  split(.$libelle) %>% 
  map(~dplyr::select(.,-libelle))

data_var2 <- read_csv("table_var_2.csv") %>%
  dplyr::select(-c(sin)) %>% 
  split(.$libelle) %>% 
  purrr::map(~dplyr::select(.,-libelle))

data_var2 <- read_csv("table_complet.csv") %>%
  split(.$libelle) %>% 
  purrr::map(~dplyr::select(.,-libelle))

####### poisson - phyto - macrophytes ---------------------------------------------------------------------------
data_poi_multi <- join_all(list(
  data_var2$poi,
  data_var2$phy %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("phy","_",colnames(.)[-1])),
  data_var2$mac %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("mac","_",colnames(.)[-1]))),
  type = "inner", by = "code_lac")
####### poi - phy - mac - inv zone lito -------------------------------------------------------------------------
data_izl_multi <- join_all(list(
  data_var2$izl,
  data_var2$mac %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("mac","_",colnames(.)[-1])),
  data_var2$poi %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("poi","_",colnames(.)[-1])),
  data_var2$phy %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("phy","_",colnames(.)[-1]))),
  type = "inner", by = "code_lac")
####### poi - phy - mac - inv zone pel-------------------------------------------------------------------------
data_izp_multi <- join_all(list(
  data_var2$izp,
  data_var2$mac %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("mac","_",colnames(.)[-1])),
  data_var2$poi %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("poi","_",colnames(.)[-1])),
  data_var2$phy %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("phy","_",colnames(.)[-1]))),
  type = "inner", by = "code_lac")
####### poi - phy - mac - inv zone pel & lito -------------------------------------------------------------------------
data_inv_multi <- join_all(list(
  data_var2$izp,
  data_var2$izl%>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("izl","_",colnames(.)[-1])),
  data_var2$mac %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("mac","_",colnames(.)[-1])),
  data_var2$poi %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("poi","_",colnames(.)[-1])),
  data_var2$phy %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("phy","_",colnames(.)[-1]))),
  type = "inner", by = "code_lac")
####### poi - phy - mac - inv zone pel & lito -------------------------------------------------------------------------
data_izl_izp <- join_all(list(
  data_var2$izp,
  data_var2$izl%>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("izl","_",colnames(.)[-1]))),
  type = "inner", by = "code_lac")

####### poi - phy - mac - iml -------------------------------------------------------------------------
data_iml_multi <- join_all(list(
  data_var2$iml,
  data_var2$mac %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("mac","_",colnames(.)[-1])),
  data_var2$poi %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("poi","_",colnames(.)[-1])),
  data_var2$phy %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("phy","_",colnames(.)[-1]))),
  type = "inner", by = "code_lac")


####### macroinvertébrés ----------------------------------------------------------------------------------------
####### IML ----------------------------------------------------------------------------------------

data_iml_mac <- inner_join(data_var2$iml,
                           data_var2$mac %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("mac","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")
data_iml_poi <- inner_join(data_var2$iml,
                           data_var2$poi %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("poi","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")
data_iml_phy <- inner_join(data_var2$iml,
                           data_var2$phy %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("phy","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")
data_iml_inv <- inner_join(data_var2$iml,
                           data_var2$inv %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("inv","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")
####### IZL ----------------------------------------------------------------------------------------
  data_izl_mac <- inner_join(data_var2$izl,
                           data_var2$mac %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("mac","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")
data_izl_poi <- inner_join(data_var2$izl,
                           data_var2$poi %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("poi","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")
data_izl_phy <- inner_join(data_var2$izl,
                           data_var2$phy %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("phy","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")
data_izl_izp <- inner_join(data_var2$izl,
                           data_var2$izp %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("iml","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")
####### IZP ----------------------------------------------------------------------------------------
data_izp_mac <- inner_join(data_var2$izp,
                           data_var2$mac %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("mac","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")
data_izp_poi <- inner_join(data_var2$izp,
                           data_var2$poi %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("poi","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")
data_izp_phy <- inner_join(data_var2$izp,
                           data_var2$phy %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("phy","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")
data_izl_izp <- inner_join(data_var2$izl,
                           data_var2$izp %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("iml","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")

####### par paires ---------------------------------------------------------------------------
data_poi_mac <- inner_join(data_var2$poi,
                           data_var2$mac %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("mac","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")
data_phy_mac <- inner_join(data_var2$phy,
                           data_var2$mac %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("mac","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")
data_poi_phy <- inner_join(data_var2$poi,
                           data_var2$phy %>% dplyr::select(code_lac:rao) %>% setnames(colnames(.)[-1],paste0("phy","_",colnames(.)[-1])),
                           type= "inner", by ="code_lac")

