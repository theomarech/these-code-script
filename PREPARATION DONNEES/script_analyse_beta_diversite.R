

rm(list = ls())
gc()
###### Poissons ----------------------------------------------------------------------------------------------------------------------
com_poi <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_poissons.csv"
  )
tra_poi <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_poissons.csv"
  ) %>%
  mutate(prot_oeu = as.numeric(prot_oeu)) %>%
  mutate(zone_ali = ifelse(zone_ali == "ben", 0, 1))

df_c_poi <- com_poi
df_t_poi <-
  tra_poi %>% dplyr::select(-c(period_incub, diam_oeu, fec, tps_eclo, female_mat)) %>% mutate_if(is.character, as.factor)


nax = 5

poi_filtre <- df_c_poi %>%
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

nom_lac_poi <- poi_filtre$code_lac
nom_esp_poi <- colnames(poi_filtre)

df_c_poi <-
  df_c_poi %>% dplyr::filter(code_lac %in% nom_lac_poi) %>% dplyr::select(code_lac, all_of(nom_esp_poi))
df_t_poi <- df_t_poi %>% dplyr::filter(nom_taxo %in% nom_esp_poi)
###### Phytoplancton ----------------------------------------------------------------------------------------------------------------------
tra_phy_ratio <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_phyto_ratio.csv"
  )
com_phyto_ab <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_phytoplancton_abondance.csv"
  )

data <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_phytoplancton_impute.csv"
  ) 

# data <-
#   read_csv(
#     "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DONNEES TRAITS PHYTO/BDD_TRAVAIL/tableau_phyto_impute.csv"
#   ) %>%
#   mutate_if(is.character, as.factor) %>%
#   mutate_if(is.binaire, as.logical) %>%
#   mutate(
#     Nb_cells_colony = as.integer(Nb_cells_colony),
#     Nb_Flagellum = factor(Nb_Flagellum, order = TRUE),
#     Maximal_length_algal_object = as.numeric(Maximal_length_algal_object)
#   ) %>%
#   mutate_at(c(16, 17, 18, 21, 29, 39, 40, 74), factor, order = TRUE) %>%
#   column_to_rownames() %>%
#   as.data.frame() %>%
#   missForest(maxiter = 40,
#              ntree = 100,
#              variablewise = TRUE) %>%
#   .$ximp %>%
#   tibble::rownames_to_column(var = "nom_taxo") %>%
#   dplyr::select(nom_taxo, Cell_Biovolume, Ind_Biovolume, Life_Form)


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
nax = 3

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


###### Macrophytes ----------------------------------------------------------------------------------------------------------------------


com_mac <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macrophytes.csv"
  )
tra_mac <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macrophytes.csv"
  )


df_c_mac <-
  com_mac %>% column_to_rownames() %>% .[which(rowSums(.) != 0),] %>% tibble::rownames_to_column(var =
                                                                                                   "code_lac")
df_t_mac <-
  tra_mac %>% arrange(nom_taxo) %>% mutate(BRA = ifelse(BRA == "yes", 1, 0)) %>% mutate_if(is.character, as.factor)


nax = 5

mac_filtre <- df_c_mac %>%
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

nom_lac_mac <- mac_filtre$code_lac
nom_esp_mac <- colnames(mac_filtre)

df_c_mac <-
  df_c_mac %>% dplyr::filter(code_lac %in% nom_lac_mac) %>% dplyr::select(code_lac, all_of(nom_esp_mac))
df_t_mac <- df_t_mac %>% dplyr::filter(nom_taxo %in% nom_esp_mac)
###### Invertébrés IZL ----------------------------------------------------------------------------------------------------------------------

com_inv_zl <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres_inv_zl.csv"
  )
tra_inv_zl <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macroinvertebres_inv_zl.csv"
  )

df_c_zl <- com_inv_zl
df_t_zl <- tra_inv_zl %>% arrange(nom_taxo)

nax = 5

zl_filtre <- df_c_zl %>%
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

nom_lac_zl <- zl_filtre$code_lac
nom_esp_zl <- colnames(zl_filtre)

df_c_zl <-
  df_c_zl %>% dplyr::filter(code_lac %in% nom_lac_zl) %>% dplyr::select(code_lac, all_of(nom_esp_zl))
df_t_zl <- df_t_zl %>% dplyr::filter(nom_taxo %in% nom_esp_zl)
###### Invertébrés IZP ----------------------------------------------------------------------------------------------------------------------

com_inv_zp <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres_inv_zp.csv"
  )
tra_inv_zp <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macroinvertebres_inv_zp.csv"
  )

df_c_zp <- com_inv_zp
df_t_zp <- tra_inv_zp %>% arrange(nom_taxo)
nax = 5

zp_filtre <- df_c_zp %>%
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

nom_lac_zp <- zp_filtre$code_lac
nom_esp_zp <- colnames(zp_filtre)

df_c_zp <-
  df_c_zp %>% dplyr::filter(code_lac %in% nom_lac_zp) %>% dplyr::select(code_lac, all_of(nom_esp_zp))
df_t_zp <- df_t_zp %>% dplyr::filter(nom_taxo %in% nom_esp_zp)
###### Invertébrés IML ----------------------------------------------------------------------------------------------------------------------

com_inv_iml <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres_iml.csv"
  )
tra_inv_iml <-
  read_csv(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macroinvertebres_iml.csv"
  )

df_c_iml <- com_inv_iml
df_t_iml <- tra_inv_iml %>% arrange(nom_taxo)
nax = 5

iml_filtre <- df_c_iml %>%
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

nom_lac_iml <- iml_filtre$code_lac
nom_esp_iml <- colnames(iml_filtre)

df_c_iml <-
  df_c_iml %>% dplyr::filter(code_lac %in% nom_lac_iml) %>% dplyr::select(code_lac, all_of(nom_esp_iml))
df_t_iml <- df_t_iml %>% dplyr::filter(nom_taxo %in% nom_esp_iml)



list_com <- purrr::map(ls(pattern = "df_c_"),  ~ get(.))
list_tra <- purrr::map(ls(pattern = "df_t_"),  ~ get(.))
names(list_com) <- ls(pattern = "df_c_")
names(list_tra) <- ls(pattern = "df_t_")

list_com %>% length()
list_tra %>% length()

list_beta <-
  furrr::future_map2(list_com_test,
                     list_tra_test,
                     ~ fun_beta(df_c = .x, df_t = .y, nax = 3))

list_tra_test <- map(list_tra,  ~ .[1:10, 1:4])

###----------------------------------------------------------------------------------------------------------------------------------
library(doParallel)
library(foreach)
list_tra
registerDoParallel(length(list_com))
list_beta <- list()
list_com_test <- list_com_test %>% map( ~ mutate_if(., is.numeric, abs))

list_com <- list_com[-c(1,3)]
list_tra <- list_tra[-c(1,3)]
list_beta <- foreach(
  i = 1:length(list_com),
  .packages = c("dplyr", "ade4", "vegan", "FD", "textshape", "betapart"),
  .multicombine = TRUE
) %dopar% {
  fun_beta(df_c = list_com[[i]],
           df_t = list_tra[[i]],
           nax = 4)
}

list_beta %>% unlist(recursive = F)
list.save(
  list_beta,
  "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Resultats/diversite_fonctionnelle/diversite_beta_3axes/div_beta_phy2.RData"
)
list_beta <-
  list.load(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Resultats/diversite_fonctionnelle/diversite_beta_3axes/div_beta_phy2.RData"
  )
list_beta_4 <-
  list.load(
    "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Resultats/diversite_fonctionnelle/diversite_beta_3axes/div_beta_4.RData"
  )



list_beta_3 <- list_beta %>% unlist(recursive = F)
ind <- names(list_beta_3) %>% str_sub(-3)
nom <-
  names(list_com) %>% str_sub(-3)  %>% str_replace("_", "i") %>% rep(each = 3) %>% paste0(ind, "_", .)


############### fonction pour créer les tableaux de diversités beta #################

library(otuSummary)

# data_indice <- map2(
#   list_beta_3,
#   nom,
#   ~ matrixConvert(.x, colname = c("code_lac1", "code_lac2", .y)) %>%
#     mutate(ind = .y) %>% separate(ind, c("indice", "taxon"), sep =
#                                     "_") %>% rename(beta = .y)
# ) %>%
#   bind_rows()
# data_split <-
#   data_indice %>% dplyr::filter(taxon %in% c("mac", "phy", "poi")) %>% split(.$indice) %>% join_all(type="inner",by=c("code_lac1" = "code_lac1","code_lac2"="code_lac2"))
# a <- data_split$jne
# 
# nax_pca = 5
# env1 <-
#   data_var2$phy[, -c(2:8)] %>%  dplyr::filter(code_lac %in% unique(a$code_lac1))
# env <-
#   env1[, -1] %>% fun_comp(site = env1$code_lac, nax = nax_pca) %>% .[[1]]
# pc1 <-
#   env[, 1:2] %>% column_to_rownames() %>% dist() %>% matrixConvert(colname = c("code_lac1", "code_lac2", "pc1"))
# pc2 <-
#   env[, c(1, 3)] %>% dist()  %>% matrixConvert(colname = c("code_lac1", "code_lac2", "pc2"))
# pc3 <-
#   env[, c(1, 4)] %>% dist()  %>% matrixConvert(colname = c("code_lac1", "code_lac2", "pc3"))
# pc4 <-
#   env[, c(1, 5)] %>% dist()  %>% matrixConvert(colname = c("code_lac1", "code_lac2", "pc4"))
# pc5 <-
#   env[, c(1, 6)] %>% dist()  %>% matrixConvert(colname = c("code_lac1", "code_lac2", "pc5"))
# 
# nom <- paste0("pc", 1:5)

fun_df_to_dist_to_matrix <- function(df, nom) {
  pmap(
    list(
      df %>% dplyr::select(code_lac) %>% rep(ncol(df) - 1),
      map(df[, -1],  ~ .),
      nom
    ),
    ~ dist(bind_cols(..1, ..2) %>%
             column_to_rownames()) %>%
      matrixConvert(colname = c("code_lac1", "code_lac2", ..3))
  ) %>%
    join_all(type = "inner")  %>% return()
}

# df_env <- fun_df_to_dist_to_matrix(env, nom)
# 
# beta_fun <- inner_join(df_env,data_split$jac,type="inner") %>%
#   dplyr::select(-c(code_lac1,code_lac2,indice)) %>%
#   dplyr::filter(taxon %in% c("phy","mac","poi")) %>% spread(taxon,beta)
# beta <- data_split$jac %>%
#   dplyr::select(-c(code_lac1,code_lac2,indice)) %>%
#   dplyr::filter(taxon %in% c("phy","mac","poi")) 
# spread(beta,taxon,beta)

###### BETA TAOXNOMIQUE ----------------------------------------------------------------------------------------------------------
##### sorensen (présence/absence)
list_beta_tax <- list_com
list_beta_fun <- list_beta
nom <- names(list_com) %>% str_sub(-3) 

beta_sor <- map(list_beta_tax,~ mutate_if(.,is.numeric,~ifelse(.>0,1,0)) %>% column_to_rownames() %>% beta.pair() %>% .$beta.sor) %>% 
  map2(nom,~matrixConvert(.x,colname = c("code_lac1","code_lac2",.y)))%>% .[2:4] %>% join_all(type = "inner")

beta_abu <- map(list_beta_tax,~ column_to_rownames(.) %>% beta.pair.abund() %>% .[2:4]%>% .$beta.bray) %>% 
  map2(nom,~matrixConvert(.x,colname = c("code_lac1","code_lac2",.y)))%>% .[2:4] %>% join_all(type = "inner")

beta_fun <- list_beta_fun %>% map(~purrr::keep(.,str_detect(names(.),"jac"))) %>% unlist(recursive = F)%>% 
  map2(nom,~matrixConvert(.x,colname = c("code_lac1","code_lac2",.y))) %>% .[2:4] %>% join_all(type = "inner")

nax_pca = 5

env1 <-
  data_var2$phy[, -c(2:8)] %>%  dplyr::filter(code_lac %in% unique(beta_fun$code_lac1))

nom_pc <- paste0("pc", 1:nax_pca)

pca <-
  env1[, -1] %>% fun_comp(site = env1$code_lac, nax = nax_pca)
env <- pca %>% .[[1]] %>%  fun_df_to_dist_to_matrix(nom=nom_pc)

pca[[2]]
pca[[3]]
pca[[4]]




dist_geo <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/ENV/ENV/env_spatial.csv") %>% 
  column_to_rownames() %>% dist() %>% matrixConvert(colname = c("code_lac1","code_lac2","dist"))

beta_sor <- inner_join(beta_sor,env)
beta_abu <- inner_join(beta_abu,env)
beta_fun <- join_all(list(beta_fun,env),type="inner")






data_comp <- beta_fun %>% select_if(is.numeric) %>%  mutate_all(~fun_boxcox(.+1)) 
data_comp %>% chart.Correlation()

glasso_multi <-  data_comp %>% 
  select_if(is.numeric) %>% 
  na.omit() %>% 
  qgraph::cor_auto(forcePD=TRUE) %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(data_comp),
                 threshold = T, 
                 layout = "spring",
                 gamma = 0.5,
                 # refit = TRUE,
                 cut = 0)
centralityPlot(glasso_multi)



data_sor <- beta_sor %>% select_if(is.numeric) %>%  mutate_all(~fun_boxcox(.+1)) 
data_sor %>% chart.Correlation()

glasso_multi <-  data_sor %>% 
  select_if(is.numeric) %>% 
  na.omit() %>% 
  qgraph::cor_auto(forcePD=TRUE) %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(data_sor),
                 threshold = T, 
                 layout = "spring",
                 gamma = 0,
                 # refit = TRUE,
                 cut = 0)
centralityPlot(glasso_multi)


data_comp <- beta_abu %>% select_if(is.numeric) %>%  mutate_all(~fun_boxcox(.+1)) 
data_comp %>% chart.Correlation()

glasso_multi <-  data_comp %>% 
  select_if(is.numeric) %>% 
  na.omit() %>% 
  qgraph::cor_auto(forcePD=TRUE) %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(data_comp),
                 threshold = T, 
                 layout = "spring",
                 gamma = 0,
                 # refit = TRUE,
                 cut = 0)
centralityPlot(glasso_multi)
