####################################################################################################################
####
####                          MODELE macroinvertebres : HMSC
####
####################################################################################################################
source("/home/theo/Bureau/scriptR/librairies.R")

############################# PREPARATION DES DONNEES ###############################
#### Données brutes
df_macroinvertebres <-
  read_csv("/home/theo/Bureau/Data/Données finales/communautés/df_macroinvertebres_filtres.csv")
df_trait_macroinvertebres <-
  read_csv("/home/theo/Bureau/Data/Données finales/traits/traits_macroinvertebres.csv") %>%
  setnames(colnames(.)[-1], paste0("TR_", colnames(.)[-1]))


df_lac <-
  read_csv("/home/theo/Bureau/Data/Données finales/lacs/df_lac.csv") %>%
  mutate(
    alcalinite = ifelse(alcalinite %in% c(">", "<"), NA, alcalinite) %>% as.numeric(),
    prof_moy_pla = ifelse(
      is.na(prof_moy_pla) &
        !is.na(prof_moy_pla_calculee),
      prof_moy_pla_calculee,
      prof_moy_pla
    )
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
      cd_proj
    )
  ) %>%
  setnames(colnames(.)[-1], paste0("LAC_", colnames(.)[-1]))

df_pc_lac <-
  read_csv("/home/theo/Bureau/Data/Données finales/PC/pc_toutes_annees.csv") %>%
  setnames(colnames(.)[-1], paste0("PC_", colnames(.)[-1])) %>% 
  setnames(c("PC_demande_biochimique_en_oxygene_en_5_jours_dbo5","PC_taux_de_saturation_en_oxygene","PC_potentiel_en_hydrogene_ph"),
           c("PC_dbo5","PC_sat_ox","PC_ph")) %>% 
  dplyr::select_if(is.numeric) %>% 
  as.data.frame() %>% 
  missForest() %>% 
  .$ximp %>% 
  mutate(code_lac= read_csv("/home/theo/Bureau/Data/Données finales/PC/pc_toutes_annees.csv")$code_lac)

coord_lac <-
  df_lac %>% dplyr::select(code_lac, LAC_lat_pla, LAC_long_pla) %>% setnames(colnames(.)[-1], paste0("XY_", colnames(.)[-1]))

df_bv <- read_csv("/home/theo/Bureau/Data/Données finales/bassins versans/table_bv.csv") %>% 
  dplyr::select(-code_lac) %>% 
  mutate_if(is.character,as.factor) %>% 
  as.data.frame() %>% 
  missForest() %>% 
  .$ximp %>% 
  setnames(colnames(.),paste0("BV_",colnames(.))) %>% 
  mutate(code_lac = read_csv("/home/theo/Bureau/Data/Données finales/bassins versans/table_bv.csv")$code_lac)

df_bv_occsol <- read_csv("/home/theo/Bureau/Data/Données finales/bassins versans/table_bv_occ_sol_all_years.csv") %>% 
  dplyr::filter(annee==2018) %>% 
  dplyr::select(-annee)%>% 
  setnames(colnames(.)[-1],paste0("BVO_",colnames(.)[-1]))

#### jointure de toutes les données pour obtenir un data homohène (nombre de lac pzeut vairer)
macroinvertebres_trait <- df_macroinvertebres %>%
  inner_join(df_trait_macroinvertebres, by = "nom_taxo")
macroinvertebres <- macroinvertebres_trait %>%
  mutate(nom_taxo = nom_taxo %>% str_replace_all(" ", "_") %>% tolower()) %>%
  group_by(code_lac, nom_taxo) %>%
  dplyr::summarise(ab = mean(abondance, na.rm = TRUE)) %>%
  spread(nom_taxo, ab, fill = 0) %>%
  setnames(colnames(.)[-1], paste0("COM_", colnames(.)[-1])) %>%
  inner_join(df_pc_lac , by = "code_lac") %>%
  inner_join(coord_lac, by = "code_lac") %>%
  inner_join(df_lac, by = "code_lac") %>%
  inner_join(df_bv, by="code_lac") %>% 
  inner_join(df_bv_occsol,by="code_lac") %>% 
  dplyr::filter(code_lac != "LEM74")

com_macroinvertebres <-
  macroinvertebres %>% dplyr::select_at(vars(matches("COM"))) %>% ungroup()

coord_macroinvertebres <-
  macroinvertebres %>% dplyr::select_at(vars(matches("XY", ignore.case = FALSE)))

pc_macroinvertebres <-
  macroinvertebres %>% dplyr::select_at(vars(matches("PC", ignore.case = FALSE)))

lac_macroinvertebres <-
  macroinvertebres %>% dplyr::select(starts_with("LAC_"), -c(LAC_lat_pla, LAC_long_pla))

bv_macroinvertebres <- macroinvertebres %>% dplyr::select(starts_with("BV_")) %>% 
  dplyr::select(BV_perim_km,BV_alti_moy,BV_pente_moy,
                BV_densite_pop_moy,BV_alea_erosion_annu,BV_battance_pmoy,
                BV_erodibilite_pmoy,BV_p100_calcaire,BV_p100_silice,
                BV_p100_autre,
                BV_roche_predom,BV_r_bv_retenu,BV_tps_res_cumu) 

bv_occsol <- macroinvertebres  %>%
  dplyr::select(starts_with("BVO_")) %>%
  mutate_if(is.numeric,~ log(.+1)) %>%
  ungroup() %>%
  select_if(is.numeric) %>%
  select_if(colSums(.)!=0) %>% 
  mutate(code_lac=macroinvertebres$code_lac)


traits_macroinvertebres <-
  macroinvertebres_trait %>% dplyr::select(nom_taxo, starts_with("TR_")) %>% distinct()

######################################################################################################################
######################################## imputation des données manquantes  ##########################################
######################################################################################################################

pc_macroinvertebres_imputeRF <- pc_macroinvertebres %>%
  ungroup() %>%
  dplyr::select(-code_lac) %>%
  as.data.frame() %>%
  missForest() %>%
  .$ximp

pc_macroinvertebres_imputeRF_log <-
  pc_macroinvertebres_imputeRF %>%  mutate_if(is.numeric, log) %>% mutate(code_lac = com_macroinvertebres$code_lac)

traits_macroinvertebres_imputeRF <-
  traits_macroinvertebres %>% 
  column_to_rownames() %>% 
  as.data.frame() %>%
  missForest(variablewise = TRUE) %>% 
  .$ximp 

lac_macroinvertebres_imputeRF <- lac_macroinvertebres %>%
  ungroup() %>%
  select_if(is.numeric) %>%
  as.data.frame() %>%
  missForest() %>%
  .$ximp %>%
  dplyr::select(-LAC_lake_order, LAC_her2, LAC_kieth)


lac_macroinvertebres_imputeRF_log <-
  lac_macroinvertebres_imputeRF %>% 
  mutate_if(is.numeric, ~ log(.+1)) %>% 
  mutate(code_lac = com_macroinvertebres$code_lac) %>%
  column_to_rownames(ncol(.))

lac_macroinvertebres_imputeRF_log%>% dudi.pca() %>% fun_pca()
comp_env_lac <- lac_macroinvertebres_imputeRF_log %>% 
  dudi.pca() %>% 
  .$l1 %>% 
  mutate(code_lac = lac_macroinvertebres_imputeRF_log$code_lac) %>%
  dplyr::select(c(code_lac,colnames(.)[-ncol(.)]))


############ ACP sur les variables environnementales
tableau_env_comp <- join_all(list(lac_macroinvertebres_imputeRF_log %>% tibble::rownames_to_column(var="code_lac"),
                                  pc_macroinvertebres_imputeRF_log,
                                  bv_macroinvertebres %>% mutate_if(is.numeric,~ log(.+1)),
                                  bv_occsol),by="code_lac") 
tableau_env_comp %>% select_if(is.numeric) %>% chart.Correlation()
pca_env_tot <- tableau_env_comp %>% column_to_rownames() %>%select_if(is.numeric) %>%  dudi.pca()
tab_pca_env <-fun_pca(pca_env_tot,tab = TRUE)
inertia.dudi(pca_env_tot)
fun_pca(pca_env_tot)



# récupérer les axes
df_env_comp <- pca_env_tot$l1 %>% mutate(code_lac = tableau_env_comp$code_lac) %>% dplyr::select(c(code_lac,colnames(.)[-ncol(.)]))


#######################################################################################################################
################################### approche descriptive des jeux de données ##########################################
#######################################################################################################################

########### TABLEAU : ACP AFC ##########
###### COM macroinvertebres
com_macroinvertebres <- com_macroinvertebres %>% column_to_rownames()
com_abondance_rel <- decostand(com_macroinvertebres,method="total")


com_abondance_rel %>%
  CA()

afc_macroinvertebres <-
  com_macroinvertebres %>% 
  dudi.pca(scannf = FALSE, nf =2) %>% fun_pca()


com_nmds <- metaMDS(com_abondance_rel,dist="bray",trymax = 100)
stressplot(com_nmds)
goodness(com_nmds)
com_nmds$stress
plot(com_nmds)
orditorp(com_nmds,display = "sites")
orditorp(com_nmds,display = "species",col="red")

###### PC macroinvertebres
acp_pc_macroinvertebres <-
  pc_macroinvertebres_imputeRF_log[-177, ] %>%
  column_to_rownames(ncol(.)) %>%
  dudi.pca(scannf = FALSE,nf = 2,row.w = afc_macroinvertebres$lw)

fun_pca(acp_pc_macroinvertebres)

###### LAC macroinvertebres
acp_lac_macroinvertebres <- lac_macroinvertebres_imputeRF_log %>%
  dudi.pca(scannf = FALSE,nf = 2) %>% fun_pca(.,biplot = FALSE)

acp_bv_macroinvertebres <-  bv_macroinvertebres %>% 
  column_to_rownames()%>% mutate_all(~.+1 %>%  log()) %>% 
  dplyr::select(-BV_roche_predom) %>% 
  dudi.pca(scannf = FALSE,nf = 2) %>% fun_pca(.,biplot = FALSE)
##### Traits fonctionnels macroinvertebress


pca_trait <-
  traits_macroinvertebres_imputeRF %>% 
  dudi.pca()

tab_pca_trait <-fun_pca(pca_trait,tab=TRUE)

df_trait_comp <- pca_trait$li  %>% 
  mutate(nom_taxo = traits_macroinvertebres_imputeRF %>%
           tibble::rownames_to_column(var="nom_taxo") %>%
           pull(nom_taxo)) %>%
  dplyr::select(c(nom_taxo,colnames(.)[-ncol(.)]))


#### RLQ
afc_macroinvertebres_com <- dudi.coa(com_macroinvertebres %>% column_to_rownames(),scannf = FALSE,nf=2)
acp_macroinvertebres_pc <- dudi.pca(tableau_env_comp%>% column_to_rownames() %>% select_if(is.numeric),row.w = afc_macroinvertebres_com$lw,scannf = FALSE, nf=2)
rownames(traits_macroinvertebres_imputeRF) <- traits_macroinvertebres$nom_taxo
hs_macroinvertebres_traits <- dudi.hillsmith(traits_macroinvertebres_imputeRF   ,row.w = afc_macroinvertebres_com$cw,scannf = FALSE, nf=2)
rlq_macroinvertebres <- rlq(dudiR = acp_macroinvertebres_pc,
                   dudiL = afc_macroinvertebres_com,
                   dudiQ = hs_macroinvertebres_traits)
randtest(rlq_macroinvertebres)
fourthcorner.rlq(rlq_macroinvertebres,type="R.axes")
scatter(rlq_macroinvertebres)
plot(rlq_macroinvertebres)

fourthcorner_macro <- fourthcorner(tableau_env_comp%>% column_to_rownames() %>% select_if(is.numeric),
                                com_macroinvertebres %>% column_to_rownames(),
                                traits_macroinvertebres_imputeRF,
                                modeltype = 6, p.adjust.method.G = "none",p.adjust.method.D = "none", nrepet = 500)
plot(fourthcorner_macro, alpha = 0.05, stat = "D2")



############################################ RIchesse ~ env #################################################
df_c <- com_macroinvertebres
df_t <- df_trait_macroinvertebres %>% column_to_rownames() 
fun_div <- function(df_c,df_t){
  
  df_trait <- df_t %>% mutate(nom_taxo = colnames(df_c)[-1])
  
  esp_0 <- df_c %>% as.data.frame()%>% column_to_rownames() %>%dplyr::select_if( colSums(.)==0) %>% colnames()
  df_com <- df_c%>% dplyr::select(-esp_0)
  df_trait <- df_trait %>% as.data.frame() %>% dplyr::filter(!nom_taxo%in%esp_0) %>% column_to_rownames(ncol(.))
  
  
  diversite_spe <- map_dfc(c("shannon","simpson"),
                           ~df_com %>%
                             dplyr::select(-code_lac) %>%
                             diversity(index=.x)) %>% setnames(colnames(.),c("shannon","simpson"))
  
  diversite_fonct1 <- FD::dbFD(df_trait,df_com %>% column_to_rownames(),corr = "cailliez")
  diversite_fonct <- diversite_fonct1[diversite_fonct1 %>% names() %>% .[-c(length(.),4)]] %>% bind_cols()
  table <- bind_cols(diversite_fonct,diversite_spe) %>%
    mutate(code_lac=df_com$code_lac) %>% 
    setnames(colnames(.)[-ncol(.)],paste0("DIV_",colnames(.)[-ncol(.)])) %>% 
    bind_cols(diversite_fonct1$CWM %>% setnames(colnames(.),paste0("CWM_",colnames(.)))) %>% 
    return()
  
}

mat_div <- fun_div(df_c,df_t)

#### CWM
cwm_macroinv <- mat_div %>%
  dplyr::select_at(vars(code_lac,matches("CWM"))) %>% 
  column_to_rownames() %>%
  setnames(colnames(.),str_extract(colnames(.),"(?<=CWM_TR_).{1,}"))
cwm_macroinv %>% dudi.pca() %>% fun_pca(repel = TRUE)
#### Indices

df_env_div <- df_env_comp %>% inner_join(mat_div %>% dplyr::select(code_lac,DIV_nbsp),by="code_lac") %>% inner_join(coord_lac) 

mod_ric_poi <- lm(DIV_nbsp ~.+poly(XY_LAC_lat_pla,2),data=df_env_div %>% dplyr::select_if(is.numeric))
summary(mod_ric_poi)
plot(mod_ric_poi)
ggplot(df_env_div,aes(x=XY_LAC_long_pla,log(DIV_FRic)))+geom_point()+geom_smooth(method="lm",formula=y~poly(x,2))

a <- df_env_div  %>%select_if(is.numeric) %>%  gather(var,val,-c(DIV_nbsp)) %>% as_tibble()

ggplot(a, aes(x=val, y = log(DIV_nbsp),col=var))+geom_point()+geom_smooth(method="lm")+facet_wrap(.~var, scale= "free")+theme(legend.position="none")
ggplot(a, aes(x=var, y = val,col=var))+geom_violin()+geom_point(alpha=0.5)+facet_wrap(.~var, scale= "free")+theme(legend.position="none")

tab_pca_env





##################################################################################################
################################## HMSC : modèle joint ###########################################
##################################################################################################
### random level : coordonnées géograpohique
studyDesign <- coord_macroinvertebres %>%
  ungroup() %>%
  mutate(code_lac = as.factor(code_lac)) %>%
  dplyr::select(code_lac) %>%
  as.data.frame

random_coord <- coord_macroinvertebres %>%
  ungroup() %>%
  mutate(code_lac = as.factor(code_lac)) %>%
  column_to_rownames() %>%
  as.matrix() %>%
  HmscRandomLevel(sData = .)

### définition des modèles
model_macroinvertebres_comp <-
  Hmsc(
    Y = com_macroinvertebres %>% dplyr::select(-code_lac)  %>% as.matrix(),
    XData = df_env_comp %>% dplyr::select(-code_lac) ,
    XFormula = ~ .,
    TrData = df_trait_comp,
    TrFormula = ~.,
    distr = 'lognormal poisson',
    studyDesign = studyDesign,
    ranLevels = list("code_lac" = random_coord)
  )

model_macroinvertebres_env <-
  Hmsc(
    Y = com_macroinvertebres %>% dplyr::select(-code_lac) %>%  as.matrix(),
    XData = tableau_env_comp %>% dplyr::select(-code_lac) ,
    XFormula = ~ .,
    distr = 'gaussian'
  )

#### échantillonnage des paramètre mcmc
mcmc_model_macroinvertebres <- sampleMcmc(
  model_macroinvertebres_comp,
  transient = 25,
  thin = 1,
  nChains = 1,
  samples = 100,
  verbose = 10
)
posterior <- convertToCodaObject(mcmc_model_macroinvertebres)

effectiveSize(posterior$Beta) # plus cette valeures est importante plus les échantillonnages sont décorélés
gelman.diag(posterior$Beta, multivariate = FALSE) # (facteur de réduction) plus c'est proche de 1 plus les différentes chaines donnent des résultats consistant
# en gros on regarde si la variance des paramètres entre les différentes chaines est égale à un (covariance ?)
plot(posterior$Beta)

prediction <- computePredictedValues(mcmc_model_macroinvertebres)
evaluation_prediction <-
  evaluateModelFit(mcmc_model_macroinvertebres, prediction)
moy_pred <- apply(a, mean,MARGIN=1)
hist(moy_pred)
a <- prediction %>% as_tibble() %>% dplyr::select(1)
res <-scale(com_macroinvertebres$COM_abramis_brama-moy_pred)
hist(res)
plot(moy_pred,res)
abline(a=0,b=0)
m = mcmc_model_macroinvertebres
Gradient <-
  constructGradient(m,
                    focalVariable = "PC_sulfates",
                    non.focalVariables = list(colnames(m$XData)))

