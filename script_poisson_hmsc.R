####################################################################################################################
####
####                          MODELE POISSON : HMSC
####
####################################################################################################################
source("/home/theo/Bureau/scriptR/librairies.R")

############################# PREPARATION DES DONNEES ###############################
#### Données brutes
typo_pla <- dbGetQuery(con,"SELECT code_lac,typo_dce FROM plan_eau") %>% mutate(typo_dce=str_extract(typo_dce,"[:alpha:]"))

df_poisson <-
  read_csv("/home/theo/Bureau/Data/Données finales/communautés/df_poissons.csv") %>% 
mutate(nom_taxo = case_when(nom_taxo == "Salmo trutta"~"Salmo trutta fario",
                            nom_taxo == "Lizza aurata"~"Chelon auratus",
                            nom_taxo == "Lizza ramada"~"Chelon ramada",
                            nom_taxo == "Gymnocephalus cernuus"~"Gymnocephalus cernua",
                            TRUE ~ nom_taxo),
       nom_taxo = str_replace_all(nom_taxo,' ','_'))
df_trait_poisson <-
  read_ods("/home/theo/Bureau/Data/Données finales/traits/traits_poissons_homo.ods") %>% 
  setnames(colnames(.)[-c(1:2)],paste0("TR_",colnames(.)[-c(1:2)])) %>% 
  mutate(nom_taxo_phylo = str_replace_all(nom_taxo_phylo," ","_"))
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
poisson_trait <- df_poisson %>%
  inner_join(df_trait_poisson, by = c("nom_taxo"="nom_taxo_phylo"))
poisson <- poisson_trait %>%
  mutate(nom_taxo = nom_taxo %>% str_replace_all(" ", "_") %>% tolower()) %>%
  group_by(code_lac, nom_taxo) %>%
  dplyr::summarise(ab = mean(biomasse, na.rm = TRUE)) %>%
  spread(nom_taxo, ab, fill = 0) %>%
  setnames(colnames(.)[-1], paste0("COM_", colnames(.)[-1])) %>%
  inner_join(df_pc_lac , by = "code_lac") %>%
  inner_join(coord_lac, by = "code_lac") %>%
  inner_join(df_lac, by = "code_lac") %>%
  inner_join(df_bv, by="code_lac") %>% 
  inner_join(df_bv_occsol,by="code_lac") %>% 
  dplyr::filter(code_lac != "LEM74")

com_poisson <-
  poisson %>% dplyr::select_at(vars(matches("COM"))) %>% ungroup()

coord_poisson <-
  poisson %>% dplyr::select_at(vars(matches("XY", ignore.case = FALSE)))

pc_poisson <-
  poisson %>% dplyr::select_at(vars(matches("PC", ignore.case = FALSE)))

lac_poisson <-
  poisson %>% dplyr::select(starts_with("LAC_"), -c(LAC_lat_pla, LAC_long_pla))

bv_poisson <- poisson %>% dplyr::select(starts_with("BV_")) %>% 
  dplyr::select(BV_perim_km,BV_alti_moy,BV_pente_moy,
                BV_densite_pop_moy,BV_alea_erosion_annu,BV_battance_pmoy,
                BV_erodibilite_pmoy,BV_p100_calcaire,BV_p100_silice,
                BV_p100_autre,
                BV_roche_predom,BV_r_bv_retenu,BV_tps_res_cumu) 

bv_occsol <- poisson  %>%
  dplyr::select(starts_with("BVO_")) %>%
  mutate_if(is.numeric,~ log(.+1)) %>%
  ungroup() %>%
  select_if(is.numeric) %>%
  select_if(colSums(.)!=0) %>% 
  mutate(code_lac=poisson$code_lac)
map_df(bv_occsol %>% dplyr::select_if(is.numeric),~sum(.)) %>% gather(var,val) %>% .$val %>% hist()
bv_occsol %>% gather(var,val,-code_lac) %>% ggplot(aes(x=val))+geom_histogram()+facet_wrap(~var,scale="free")

traits_poissons <-
  poisson_trait %>% dplyr::select(nom_taxo, starts_with("TR_")) %>%
  mutate_if(is.character, as.factor) %>% 
  distinct() %>% 
  as.data.frame() %>% 
  column_to_rownames() %>% 
  missForest() %>% 
  .$ximp
  

######################################################################################################################
######################################## imputation des données manquantes  ##########################################
######################################################################################################################



pc_poisson_imputeRF_log <-
  pc_poisson_imputeRF %>%  mutate_if(is.numeric, log) %>% mutate(code_lac = com_poisson$code_lac)



lac_poisson_imputeRF <- lac_poisson %>%
  ungroup() %>%
  select_if(is.numeric) %>%
  as.data.frame() %>%
  missForest() %>%
  .$ximp %>%
  dplyr::select(-LAC_lake_order, LAC_her2, LAC_kieth)


lac_poisson_imputeRF_log <-
  lac_poisson_imputeRF %>% 
  mutate_if(is.numeric, ~log(.+1)) %>% 
  mutate(code_lac = com_poisson$code_lac) %>%
  column_to_rownames(ncol(.))



typo_pla <- df_lac %>% dplyr::select(code_lac,LAC_typo_pla)

tableau_env_comp <- join_all(list(lac_poisson_imputeRF_log %>% tibble::rownames_to_column(var="code_lac"),
                                  pc_poisson_imputeRF_log,
                                  bv_poisson %>% mutate_if(is.numeric,~ log(.+1)),
                                  bv_occsol),by="code_lac") 

pca_env_tot <- tableau_env_comp %>% select_if(is.numeric) %>% dudi.pca()

inertia.dudi(pca_env_tot)
fun_pca(pca_env_tot,tab=FALSE)
pca_env_var <- pca_env_tot$l1 %>% dplyr::select(1:26) %>%  mutate(code_lac = tableau_env_comp$code_lac)

pca_pc <- pc_poisson_imputeRF_log %>% column_to_rownames(ncol(.)) %>%dudi.pca() 
pca_pc_comp <-pca_pc %>%  .$l1 %>% dplyr::select(1:2) %>%  
  mutate(code_lac =pc_poisson_imputeRF_log$code_lac) %>% 
  setnames(colnames(.)[-ncol(.)],paste0("PC_",colnames(.)[-ncol(.)]))
fun_pca(pca_pc)


pca_lac <- lac_poisson_imputeRF_log  %>%dudi.pca() 
pca_lac_comp <-pca_lac %>%  .$l1 %>% dplyr::select(1:4) %>% 
  mutate(code_lac =lac_poisson_imputeRF_log %>%  tibble::rownames_to_column(var="code_lac") %>% .$code_lac)%>% 
  setnames(colnames(.)[-ncol(.)],paste0("LAC_",colnames(.)[-ncol(.)]))
fun_pca(pca_lac) 

pca_bv <- bv_poisson %>% mutate_if(is.numeric,~ log(.+1)) %>% select_if(is.numeric) %>%  column_to_rownames() %>%dudi.pca() 
pca_bv_comp <-pca_bv %>%  .$l1 %>% dplyr::select(1:5) %>%
  mutate(code_lac =bv_poisson$code_lac) %>% 
  setnames(colnames(.)[-ncol(.)],paste0("BV_",colnames(.)[-ncol(.)]))
fun_pca(pca_bv,axes = c(1,2)) 

var <- bv_occsol %>% mutate_all(~ifelse(.==0,0,1)) %>% summarise_all(sum) %>% dplyr::select_if(.>20) %>% colnames()
pca_bvocc <- bv_occsol %>%dplyr::select(all_of(var))  %>%  column_to_rownames(loc=ncol(.)) %>%dudi.pca() 
pca_bvocc_comp <-pca_bvocc %>%  .$l1 %>% dplyr::select(1:2) %>%
  mutate(code_lac =bv_occsol$code_lac) %>% 
  setnames(colnames(.)[-ncol(.)],paste0("BVocc_",colnames(.)[-ncol(.)]))
fun_pca(pca_bvocc,axes = c(1,2))

#######################################################################################################################
################################### approche descriptive des jeux de données ##########################################
#######################################################################################################################

########### TABLEAU : ACP AFC ##########
###### COM POISSON
com_poisson[-177, ] %>%
  column_to_rownames() %>% 
  CA()
?decostand
com_poisson_rel <- decostand(com_poisson %>% column_to_rownames(),method = "total")
metaMDS(com_poisson_rel,k=3)
afc_poisson <-
  com_poisson[-177, ] %>% 
  column_to_rownames() %>% 
  dudi.pca(scannf = FALSE, nf =2)

###### PC POISSON
acp_pc_poisson <-
  pc_poisson_imputeRF_log[-177, ] %>%
  column_to_rownames(ncol(.)) %>%
  dudi.pca(scannf = FALSE,nf = 2,row.w = afc_poisson$lw)

fun_pca(acp_pc_poisson)

###### LAC POISSON
acp_lac_poisson <- lac_poisson_imputeRF_log %>%
  dudi.pca(scannf = FALSE,nf = 2) %>% fun_pca(.,biplot = FALSE)

acp_bv_poisson <-  bv_poisson %>% 
  column_to_rownames()%>% mutate_all(~.+1 %>%  log()) %>% 
  dplyr::select(-BV_roche_predom) %>% 
  dudi.pca(scannf = FALSE,nf = 2) %>% fun_pca(.,biplot = FALSE)
##### Traits fonctionnels poissons


hill_smith_trait <-
  traits_poissons_imputeRF %>% 
  mutate(biomasse_med = log(biomasse_med),
         taille_med = log(taille_med)) %>% 
  dudi.mix(scannf = FALSE,add.square = FALSE,nf = 2)

scatter(hill_smith_trait)
fish <- hill_smith_trait$l1
trait <- hill_smith_trait$c1
plot(fish)
abline(h=0,v=0)
points(trait*4,col="blue")
arrows(0,0,trait$CS1*4,trait$CS2*4)
inertia.dudi(hill_smith_trait)




#### RLQ
afc_poisson_com <- dudi.coa(com_poisson %>% column_to_rownames(),scannf = FALSE,nf=2)
acp_poisson_pc <- dudi.pca(tableau_env_comp%>% column_to_rownames() %>% select_if(is.numeric),row.w = afc_poisson_com$lw,scannf = FALSE, nf=2)
rownames(traits_poissons_imputeRF) <- traits_poissons$nom_taxo
hs_poisson_traits <- dudi.hillsmith(traits_poissons   ,row.w = afc_poisson_com$cw,scannf = FALSE, nf=2)
rlq_poisson <- rlq(dudiR = acp_poisson_pc,
                   dudiL = afc_poisson_com,
                   dudiQ = hs_poisson_traits)
randtest(rlq_poisson)
fourthcorner.rlq(rlq_poisson,type="R.axes")
scatter(rlq_poisson)
plot(rlq_poisson)

data("aravo")
aravo$traits
four.comb.aravo <- fourthcorner(tableau_env_comp%>% column_to_rownames() %>% select_if(is.numeric),
                                com_poisson %>% column_to_rownames(),
                                traits_poissons,
                                modeltype = 6, p.adjust.method.G = "none",p.adjust.method.D = "none", nrepet = 1)
plot(four.comb.aravo, alpha = 0.05, stat = "D2")



############################################ RIchesse ~ env #################################################
df_c_poisson <- com_poisson
df_t_poisson <- traits_poissons_imputeRF
fun_div <- function(df_c,df_t){

df_trait <- df_t %>% mutate(nom_taxo = colnames(df_c)[-1])

esp_0 <- df_c %>% as.data.frame()%>% column_to_rownames() %>%dplyr::select_if( colSums(.)==0) %>% colnames()
df_com <- df_c%>% dplyr::select(-esp_0)
df_trait <- df_trait %>% as.data.frame() %>% dplyr::filter(nom_taxo!=esp_0) %>% column_to_rownames(ncol(.))


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

mat_div_poisson <- fun_div(df_c_poisson,df_t_poisson)

df_env_div_poisson <- tableau_env_comp %>%
  inner_join(mat_div_poisson %>% 
               dplyr::select(code_lac,DIV_nbsp),by="code_lac") %>%
  inner_join(coord_lac) %>%
  dplyr::filter(!code_lac%in%c("BOU73"))


 df_env_div_poisson %>% dplyr::filter(LAC_volume_pla==max(LAC_volume_pla))



a <- df_env_div_poisson  %>%select_if(is.numeric) %>%  gather(var,val,-c(DIV_nbsp)) %>% as_tibble()

ggplot(a, aes(x=val, y = DIV_nbsp,col=var))+geom_point()+geom_smooth()+facet_wrap(.~var, scale= "free")+theme(legend.position="none")




ggplot(data=df_env_div_poisson, aes(x=PC_orthophosphates_po4 , y=DIV_nbsp))+geom_point()+geom_smooth()

a <- lm(DIV_nbsp ~poly(XY_LAC_long_pla,2)+LAC_her2+BV_densite_pop_moy+LAC_altitude_pla+BV_erodibilite_pmoy +BV_battance_pmoy +BVO_211, data = df_env_div)







##################################################################################################
################################## HMSC : modèle joint ###########################################
##################################################################################################
phylo_poissons <- read_csv("/home/theo/Bureau/Data/Données finales/phylogénie/phylo_poissons.csv") 
nom_taxo_trait <- traits_poissons %>% tibble::rownames_to_column(var="nom_taxo") %>% dplyr::select(nom_taxo)
nom_taxo_com <- com_poisson %>% 
  as_tibble %>% 
  mutate_all(~ifelse(.==0,0,1)) %>% 
  dplyr::summarise_all(sum) %>%
  dplyr::select_if(.>10) %>% colnames(.) %>% .[-1] %>%
  data.frame(nom_taxo=.) %>% 
  mutate(nom_taxo = as.character(nom_taxo))
# nom_taxo_com <- colnames(com_poisson)[-1]%>% data.frame(nom_taxo=.)
#  %>% str_extract("(?<=COM_).{1,}") %>% str_to_sentence() 

plot(phylo_tree)
tnrs_match_names(c("Rhodeus amarus"))
### phylo sur les données filtrées
phylo_nom <-tnrs_match_names(str_replace_all(nom_taxo_com$nom_taxo,"_"," "))
phylo_tree <- tol_induced_subtree(phylo_nom$ott_id,label_format = 'name')
phylo_branch_length <-  compute.brlen(phylo_tree)
phylo_matcor <- vcv(phylo_branch_length,model="Brownian",corr=TRUE) %>%
  as_tibble() %>%
  setnames(colnames(.),str_extract(colnames(.),".{1,}(?=_\\()|.{1,}(?!=_\\()"))%>%
  mutate(nom_taxo = colnames(.)) 
phylo_tree <- phylo_matcor %>% 
  column_to_rownames(loc=ncol(.)) %>% 
  as.matrix() %>% 
  vcv2phylo(tolerance = 1e-7)
nom_taxo_phylo <- phylo_matcor %>% dplyr::select(nom_taxo)
dplyr::filter(nom_taxo_com,!nom_taxo%in%nom_taxo_phylo$nom_taxo)
nom_taxo_commun <- join_all(list(nom_taxo_com,nom_taxo_trait,nom_taxo_phylo)) %>% 
  as_tibble() %>% 
  mutate(nom_taxo=as.character(nom_taxo)) %>% 
  pull(nom_taxo)
### random level : coordonnées géograpohique

TData <- traits_poissons %>%
  tibble::rownames_to_column(var="nom_taxo") %>% 
  dplyr::filter(nom_taxo %in% nom_taxo_commun) %>% 
  column_to_rownames()

Y <- com_poisson %>%
  dplyr::select(c(code_lac,all_of(nom_taxo_commun))) %>% 
  setnames(colnames(.)[-1], nom_taxo_commun) %>%
  dplyr::select(c(code_lac,all_of(nom_taxo_commun))) %>% 
  column_to_rownames() %>% as.matrix()

# phylo_poissons <- phylo_poissons %>% 
#   dplyr::select(c(nom_taxo,all_of(nom_taxo_commun))) %>%
#   dplyr::filter(nom_taxo%in%nom_taxo_commun) %>% 
#   arrange(nom_taxo) %>% column_to_rownames()

phylo_poissons <- phylo_tree

studyDesign <- coord_poisson %>%
  ungroup() %>%
  mutate(code_lac = as.factor(code_lac)) %>%
  dplyr::select(code_lac) %>%
  as.data.frame

random_coord <- coord_poisson %>%
  ungroup() %>%
  mutate(code_lac = as.factor(code_lac)) %>%
  column_to_rownames() %>%
  as.matrix() %>%
  HmscRandomLevel(sData = .)

XData <- pca_env_var %>% dplyr::filter(code_lac %in%com_poisson$code_lac) %>%  column_to_rownames(loc=length(.))
dim(studyDesign)
dim(random_coord)
dim(Y)
dim(TData)
dim(XData)
Y %>%as.data.frame() %>%  colSums()
### définition des modèles
model_poisson_comp <-
  Hmsc(
    Y = log(Y+1),
    XData = XData,
    TrData = TData,
    XFormula = ~ .,
    TrFormula = ~. ,
    # C= phylo_poissons,
    phyloTree = phylo_poissons,
    distr = 'normal',
    # studyDesign = studyDesign,
    # ranLevels = list("code_lac" = random_coord),
  )


  
#### échantillonnage des paramètre par mcmc 
mcmc_model_poisson <- sampleMcmc(
  hM = model_poisson_comp,
  transient = 25,# période de burnin, période pendant laquelle on considère que les valeurs de la chaines sont instables
  thin = 5,#période d'échantillonnage, on échanntillonne tous les 40 tours après la séquence de burnin, permet d'éviter l'autocorrélation
  nChains = 1,# nombre de chaines, permet d'évaluer si les résultats sont cohérents si on répète l'expérience
  samples = 100, # nombre de données finales que l'on veut du paramètres pour en faire une distribution
  verbose = 1, # pour voir l'avancé de l'échantillonnage
  nParallel = 2# parallèlisation de l'algo sur plusieurs coeur, sur les chaines surement
)

      biPlot(mcmc_model_poisson,etaPost = 0.5,lambdaPost = 0.2)

posterior <- convertToCodaObject(mcmc_model_poisson)

effectiveSize(posterior$Beta) # plus cette valeures est importante plus les échantillonnages sont décorélés
gelman.diag(posterior$Beta, multivariate = FALSE) # (facteur de réduction) plus c'est proche de 1 plus les différentes chaines donnent des résultats consistant

effectiveSize(posterior$) # plus cette valeures est importante plus les échantillonnages sont décorélés
gelman.diag(posterior$Beta, multivariate = FALSE)


# en gros on regarde si la variance des paramètres entre les différentes chaines est égale à un (covariance ?)
plot(posterior$Beta)


preds <- computePredictedValues(mcmc_model_poisson,expected=FALSE)

model_fit_eval <- evaluateModelFit(mcmc_model_poisson,preds)

postBeta <-getPostEstimate(mcmc_model_poisson,parName = "Beta")
plotBeta(mcmc_model_poisson,post=postBeta,param="Sign",plotTree = TRUE,supportLevel = 0.95,split=0.4)

postGamma <-getPostEstimate(mcmc_model_poisson,parName = "Gamma")
plotGamma(mcmc_model_poisson,post=postGamma,param="Sign",supportLevel = 0.7)

summary(posterior$Rho)$quantiles
