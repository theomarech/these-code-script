####################################################################################################################
####
####                          MODELE phytoplancton: HMSC
####
####################################################################################################################
source("/home/theo/Bureau/scriptR/librairies.R")

############################# PREPARATION DES DONNEES ###############################
#### Données brutes
df_phytoplancton <-
  read_csv("/home/theo/Bureau/Data/Données finales/communautés/df_phytoplanctons.csv") %>% mutate(nom_taxo=nom_ref)

df_trait_phytoplancton <-
  read_csv("/home/theo/Bureau/Data/Données finales/traits/traits_phyto.csv") %>%
  setnames(colnames(.)[-1], paste0("TR_", colnames(.)[-1])) %>% 
  dplyr::select(-c(2:10)) %>%
  mutate(nom_taxo=nom_ref) %>%
  mutate_if(is.character,as.factor) %>% 
  mutate_if(is.binaire,as.logical)

df_lac <-
  read_csv("/home/theo/Bureau/Data/Données finales/lacs/df_lac.csv") %>%
  mutate(
    alcalinite = ifelse(alcalinite %in% c(">", "<"), NA, alcalinite) %>% as.numeric(),
    marnage_pla = ifelse(marnage_pla %in% c(">", "<"), NA, marnage_pla) %>% as.numeric(),
    prof_max_pla = ifelse(prof_max_pla %in% c(">", "<"), NA, prof_max_pla) %>% as.numeric(),
    tps_sejour_moy_pla = ifelse(tps_sejour_moy_pla %in% c(">", "<"), NA, tps_sejour_moy_pla) %>% as.numeric(),
    prof_moy_pla = ifelse(is.na(prof_moy_pla) &!is.na(prof_moy_pla_calculee),prof_moy_pla_calculee,prof_moy_pla),
    tps_sejour_moy_pla = ifelse(is.na(tps_sejour_moy_pla) &!is.na(tps_sejour_moy_calc),tps_sejour_moy_calc,tps_sejour_moy_pla)
    
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
      tps_sejour_moy_calc,
      cd_proj,
      annee_mise_en_eau
    )
  ) %>%
  setnames(colnames(.)[-1], paste0("LAC_", colnames(.)[-1])) %>% 
  mutate_if(is.character,as.factor) %>% 
  as.data.frame() %>% 
  column_to_rownames() %>% 
  missForest() %>% 
  .$ximp %>% 
  tibble::rownames_to_column(var="code_lac")


##### PC #####
df_pc_lac_3_saisons <-
  read_csv("/home/theo/Bureau/Data/Données finales/PC/pc_toutes_annees.csv") %>%
  setnames(colnames(.)[-1], paste0("PC_", colnames(.)[-1])) %>% 
  setnames(c("PC_demande_biochimique_en_oxygene_en_5_jours_dbo5","PC_taux_de_saturation_en_oxygene","PC_potentiel_en_hydrogene_ph"),
           c("PC_dbo5","PC_sat_ox","PC_ph")) %>% 
  dplyr::select_if(is.numeric) %>% 
  as.data.frame() %>% 
  missForest() %>% 
  .$ximp %>% 
  mutate(code_lac= read_csv("/home/theo/Bureau/Data/Données finales/PC/pc_toutes_annees.csv")$code_lac)

df_pc_lac <- read_csv("/home/theo/Bureau/Data/Données finales/PC/pc_travail.csv") %>% 
  dplyr::filter(saison=="printemps",support_prelev=="Eau") %>% 
  dplyr::select(code_lac,nom_par,annee,resultat_ana_homo) %>% 
  group_by(code_lac,nom_par,annee) %>% 
  dplyr::summarise(pc_mean=mean(resultat_ana_homo,na.rm=TRUE),
                   pc_med=median(resultat_ana_homo,na.rm=TRUE)) %>% 
  dplyr::filter(annee>=2005) %>% 
  group_by(code_lac,nom_par) %>% 
  dplyr::summarise(pc_med=mean(pc_med,na.rm=TRUE)) %>% 
  spread(nom_par,pc_med) %>% 
  dplyr::select(-carbone) %>% 
  column_to_rownames() %>% 
  as.data.frame() %>% 
  missForest() %>% 
  .$ximp%>%
  tibble::rownames_to_column(var="code_lac") %>% 
  setnames(colnames(.)[-1], paste0("PC_", colnames(.)[-1])) %>% 
  setnames(c("PC_demande_biochimique_en_oxygene_en_5_jours_dbo5","PC_taux_de_saturation_en_oxygene","PC_potentiel_en_hydrogene_ph"),
           c("PC_dbo5","PC_sat_ox","PC_ph"))
df_pc_lac %>% mutate_if(is.numeric,~log(.+1)) %>% column_to_rownames() %>% PCA()
#############




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
phytoplancton_trait <- df_phytoplancton %>%
  inner_join(df_trait_phytoplancton, by = "nom_taxo")
phytoplancton <- phytoplancton_trait %>%
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
  dplyr::filter(!code_lac %in% c("LEM74","BOU73"))

com_phytoplancton <-
  phytoplancton %>% dplyr::select_at(vars(matches("COM"))) %>% ungroup()

coord_phytoplancton <-
  phytoplancton %>% dplyr::select_at(vars(matches("XY", ignore.case = FALSE)))

pc_phytoplancton <-
  phytoplancton %>% dplyr::select_at(vars(matches("PC", ignore.case = FALSE)))

lac_phytoplancton <-
  phytoplancton %>% dplyr::select(starts_with("LAC_"), -c(LAC_lat_pla, LAC_long_pla))

bv_phytoplancton <- phytoplancton %>% dplyr::select(starts_with("BV_")) %>% 
  dplyr::select(BV_perim_km,BV_alti_moy,BV_pente_moy,
                BV_densite_pop_moy,BV_alea_erosion_annu,BV_battance_pmoy,
                BV_erodibilite_pmoy,BV_p100_calcaire,BV_p100_silice,
                BV_p100_autre,
                BV_roche_predom,BV_r_bv_retenu,BV_tps_res_cumu) 

bv_occsol <- phytoplancton  %>%
  dplyr::select(starts_with("BVO_")) %>%
  mutate_if(is.numeric,~ log(.+1)) %>%
  ungroup() %>%
  select_if(is.numeric) %>%
  select_if(colSums(.)!=0) %>% 
  mutate(code_lac=phytoplancton$code_lac)


traits_phytoplancton <-
  phytoplancton_trait %>% dplyr::select(nom_taxo, starts_with("TR_")) %>% distinct()

######################################################################################################################
######################################## imputation des données manquantes  ##########################################
######################################################################################################################


pc_phytoplancton_imputeRF_log <-
  pc_phytoplancton %>%  mutate_if(is.numeric, ~log(.+1)) 

traits_phytoplancton_imputeRF <-
  traits_phytoplancton %>% 
  column_to_rownames() %>% 
  as.data.frame() %>%
  missForest(variablewise = TRUE) %>% 
  .$ximp 



lac_phytoplancton_imputeRF_log <-
  lac_phytoplancton%>% 
  mutate_if(is.numeric, ~ log(.+1)) 



row

tableau_env_comp_phyto <- join_all(list(lac_phytoplancton_imputeRF_log %>% ungroup(),
                                  pc_phytoplancton_imputeRF_log %>% ungroup(),
                                  bv_phytoplancton %>% mutate_if(is.numeric,~ log(.+1)) %>% ungroup(),
                                  bv_occsol %>% ungroup()),
                                  by="code_lac") 

pca_env_tot <- pc_phytoplancton_imputeRF_log %>% as.data.frame() %>% column_to_rownames() %>%  select_if(is.numeric) %>% dudi.pca()



fun_pca(pca_env_tot,tab = TRUE)
pc_phytoplancton %>%  gather(var,val,-code_lac) %>% ggplot(aes(x=val))+geom_histogram()+facet_wrap(.~var,scale="free")
pc_phytoplancton_imputeRF_log %>%  gather(var,val,-code_lac) %>% ggplot(aes(x=val))+geom_histogram()+facet_wrap(.~var,scale="free")


#######################################################################################################################
################################### approche descriptive des jeux de données ##########################################
#######################################################################################################################

########### TABLEAU : ACP AFC ##########
###### COM phytoplancton
com_phytoplancton[-177, ] %>%
  column_to_rownames() %>% 
  CA()

afc_phytoplancton <-
  com_phytoplancton[-177, ] %>% 
  column_to_rownames() %>% 
  dudi.pca(scannf = FALSE, nf =2)

###### PC phytoplancton
acp_pc_phytoplancton <-
  pc_phytoplancton_imputeRF_log %>%
  column_to_rownames() %>%
  dudi.pca(scannf = FALSE,nf = 2)

fun_pca(acp_pc_phytoplancton)

###### LAC phytoplancton
acp_lac_phytoplancton <- lac_phytoplancton_imputeRF_log %>%
  column_to_rownames() %>% 
  select_if(is.numeric) %>% 
  mutate_all(~ifelse(is.infinite(.)==TRUE,NA,.)) %>% 
  na.omit() %>% 
  dudi.pca(scannf = FALSE,nf = 2) %>% fun_pca(.,biplot = FALSE)

acp_bv_phytoplancton <-  bv_phytoplancton %>% 
  column_to_rownames()%>% mutate_all(~.+1 %>%  log()) %>% 
  dplyr::select(-BV_roche_predom) %>% 
  dudi.pca(scannf = FALSE,nf = 2) %>% fun_pca(.,biplot = FALSE)
##### Traits fonctionnels phytoplanctons


hill_smith_trait <-
  traits_phytoplancton_imputeRF %>% 
  dudi.mix(scannf = FALSE,add.square = FALSE,nf = 2)

scatter(hill_smith_trait)
fish <- hill_smith_trait$l1
trait <- hill_smith_trait$co
plot(fish)
abline(h=0,v=0)
points(trait,col="blue")
arrows(0,0,trait$CS1,trait$CS2)
inertia.dudi(hill_smith_trait)




#### RLQ
afc_phytoplancton_com <- dudi.coa(com_phytoplancton %>% column_to_rownames(),scannf = FALSE,nf=2)
acp_phytoplancton_pc <- dudi.pca(tableau_env_comp%>% column_to_rownames() %>% select_if(is.numeric),row.w = afc_phytoplancton_com$lw,scannf = FALSE, nf=2)
rownames(traits_phytoplancton_imputeRF) <- traits_phytoplancton$nom_taxo
hs_phytoplancton_traits <- dudi.hillsmith(traits_phytoplancton_imputeRF   ,row.w = afc_phytoplancton_com$cw,scannf = FALSE, nf=2)
rlq_phytoplancton <- rlq(dudiR = acp_phytoplancton_pc,
                            dudiL = afc_phytoplancton_com,
                            dudiQ = hs_phytoplancton_traits)
randtest(rlq_phytoplancton)
fourthcorner.rlq(rlq_phytoplancton,type="R.axes")
scatter(rlq_phytoplancton)
plot(rlq_phytoplancton)

fourthcorner_macro <- fourthcorner(tableau_env_comp%>% column_to_rownames() %>% select_if(is.numeric),
                                   com_phytoplancton %>% column_to_rownames(),
                                   traits_phytoplancton_imputeRF,
                                   modeltype = 6, p.adjust.method.G = "none",p.adjust.method.D = "none", nrepet = 500)
plot(fourthcorner_macro, alpha = 0.05, stat = "D2")



############################################ RIchesse ~ env #################################################
df_c_phyto <- com_phytoplancton
df_t_phyto <- traits_phytoplancton_imputeRF %>% mutate_if(is.logical,as.numeric)
fun_div <- function(df_c,df_t,func = TRUE){
  
  df_trait <- df_t %>% mutate(nom_taxo = colnames(df_c)[-1])
  
  esp_0 <- df_c %>% as.data.frame()%>% column_to_rownames() %>%dplyr::select_if( colSums(.)==0) %>% colnames()
  df_com <- df_c%>% dplyr::select(-esp_0)
  df_trait <- df_trait %>% as.data.frame() %>% dplyr::filter(!nom_taxo%in%esp_0) %>% column_to_rownames(ncol(.))
  df_com_rich <- df_com %>% mutate_if(is.numeric,~ifelse(.==0,0,1)) %>% column_to_rownames() %>% rowSums() %>% data.frame(DIV_richesse=.) %>% tibble::rownames_to_column(var="code_lac")
  diversite_spe <- map_dfc(c("shannon","simpson"),
                           ~df_com %>%
                             dplyr::select(-code_lac) %>%
                             diversity(index=.x)) %>% setnames(colnames(.),c("DIV_shannon","DIV_simpson"))
  if(func==TRUE){
    
  diversite_fonct1 <- FD::dbFD(df_trait,df_com %>% column_to_rownames(),corr = "cailliez")
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
df_env_div %>% dplyr::filter(LAC_volume_pla==max(LAC_volume_pla))

mat_div <- fun_div(df_c=df_c_phyto,df_t=df_t_phyto %>% dplyr::select(-TR_Reynolds_Group),func=FALSE)

df_env_div <- tableau_env_comp_phyto %>% inner_join(mat_div %>% dplyr::select(code_lac,DIV_richesse),by="code_lac") %>% inner_join(coord_lac) %>% 
  dplyr::select(!starts_with("BVO")) 
  

mod_ric_poi = lm(DIV_shannon ~ LAC_marnage_pla,data=a %>% ungroup() %>% dplyr::select(-code_lac) )
summary(mod_ric_poi)
a <- df_env_div  %>%select_if(is.numeric) %>%  gather(var,val,-c(DIV_richesse)) %>% as_tibble()

ggplot(a, aes(x=val, y = DIV_shannon,col=var))+geom_point()+geom_smooth()+facet_wrap(.~var, scale= "free")+theme(legend.position="none")+
  scale_x_log10()
ggplot(df_pc_lac %>% gather(var,val,-c(code_lac)), aes(x=var, y = val,col=var))+geom_jitter()+facet_wrap(.~var, scale= "free")+theme(legend.position="none")

a <- lac_phytoplancton %>% column_to_rownames() %>%select_if(is.numeric) %>% tibble::rownames_to_column(var="code_lac") %>%  
  gather(var,val,-c(code_lac)) %>% inner_join(mat_div %>% dplyr::select(code_lac,DIV_shannon),by="code_lac")
a <- lac_phytoplancton %>% 
   inner_join(mat_div %>% dplyr::select(code_lac,DIV_shannon),by="code_lac")



##################################################################################################
################################## HMSC : modèle joint ###########################################
##################################################################################################
### random level : coordonnées géograpohique
studyDesign <- coord_phytoplancton %>%
  ungroup() %>%
  mutate(code_lac = as.factor(code_lac)) %>%
  dplyr::select(code_lac) %>%
  as.data.frame

random_coord <- coord_phytoplancton %>%
  ungroup() %>%
  mutate(code_lac = as.factor(code_lac)) %>%
  column_to_rownames() %>%
  as.matrix() %>%
  HmscRandomLevel(sData = .)

### définition des modèles
model_phytoplancton_comp <-
  Hmsc(
    Y = com_phytoplancton %>% dplyr::select(COM_sander_lucioperca) %>% as.matrix(),
    XData = pc_phytoplancton_imputeRF_log %>% dplyr::select(-code_lac) ,
    XFormula = ~ .,
    distr = 'lognormal phytoplancton',
    studyDesign = studyDesign,
    ranLevels = list("code_lac" = random_coord)
  )

model_phytoplancton_env <-
  Hmsc(
    Y = com_phytoplancton %>% dplyr::select(-code_lac) %>% +1 %>% log() %>%  as.matrix(),
    XData = pc_phytoplancton_imputeRF_log %>% dplyr::select(-code_lac) ,
    XFormula = ~ .,
    distr = 'lognormal phytoplancton'
  )

#### échantillonnage des paramètre mcmc
mcmc_model_phytoplancton <- sampleMcmc(
  model_phytoplancton_env,
  transient = 250,
  thin = 5,
  nChains = 1,
  samples = 1000,
  verbose = 100
)
posterior <- convertToCodaObject(mcmc_model_phytoplancton)

effectiveSize(posterior$Beta) # plus cette valeures est importante plus les échantillonnages sont décorélés
gelman.diag(posterior$Beta, multivariate = FALSE) # (facteur de réduction) plus c'est proche de 1 plus les différentes chaines donnent des résultats consistant
# en gros on regarde si la variance des paramètres entre les différentes chaines est égale à un (covariance ?)
plot(posterior$Beta)

prediction <- computePredictedValues(mcmc_model_phytoplancton)
evaluation_prediction <-
  evaluateModelFit(mcmc_model_phytoplancton, prediction)
moy_pred <- apply(a, mean,MARGIN=1)
hist(moy_pred)
a <- prediction %>% as_tibble() %>% dplyr::select(1)
res <-scale(com_phytoplancton$COM_abramis_brama-moy_pred)
hist(res)
plot(moy_pred,res)
abline(a=0,b=0)
m = mcmc_model_phytoplancton
Gradient <-
  constructGradient(m,
                    focalVariable = "PC_sulfates",
                    non.focalVariables = list(colnames(m$XData)))
predY <- predict(m, Gradient = Gradient, expected = TRUE)
plotGradient(
  m,
  Gradient,
  pred = predY,
  measure = "Y",
  index = 1,
  showData = TRUE
)
posterior$Beta
par(mfrow = c(1, 1))

data <-
  cbind(
    y = log(com_phytoplancton$COM_squalius_cephalus + 1),
    pc_phytoplancton_imputeRF_log %>% dplyr::select(-code_lac)
  )
rf <- randomForest(formula = y ~ ., data = data)
importance(rf)
PerformanceAnalytics::chart.Correlation(data)
corrplot(cor(data))
plot(data$y ~ data$PC_ammonium)

modaic <- stepAIC(mod)
modaic$terms
summary(mod)
fun_table_taxo(df_phytoplancton)

colnames(mcmc_model_phytoplancton$Y)
which.max(evaluation_prediction$RMSE)
com_phytoplancton %>% dplyr::select(-code_lac) %>% dplyr::select(59)
