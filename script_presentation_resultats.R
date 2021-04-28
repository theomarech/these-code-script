
source("/home/theo/Bureau/scriptR/librairies.R")

setwd("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_CWM/")
dir()
name <- c("inv_iml","inv","mac","phy","poi")
a <- read_csv(dir()[1])
############################## LISTE CONTENANT TOUS LES INDICES POUR TOUS LES MAILLONS ##############################
list_tab_fun<- map2(dir(),
                    name,
                    ~read_csv(.x) %>% 
                      setnames(colnames(.)[-ncol(.)],
                               paste0(.y,"_",str_remove_all(colnames(.)[-ncol(.)],"[:upper:]{1,}_(?=.)"))) %>% 
                      dplyr::select(-c(1,2)) %>% 
                      dplyr::select(code_lac,c(1,2,3,5)))


########################################## Données environnementales #############################################
#### Physico chimie
env[[5]] %>% select_if(is.numeric) %>% PCA()
env[[5]] %>% select_if(is.numeric) %>% cor() %>% corrplot::corrplot()
env[[5]] %>% select_if(is.numeric) %>% chart.Correlation()

acp <- env[[5]] %>% select_if(is.numeric) %>% dudi.pca()
fun_pca(acp,axes=c(1,2),repel = TRUE)
fun_pca(acp,axes=c(3,4),repel = TRUE)



#### Lac
env[[3]] %>% select_if(is.numeric) %>% PCA()
env[[3]] %>% select_if(is.numeric) %>% cor() %>% corrplot::corrplot()
env[[3]] %>% select_if(is.numeric) %>% chart.Correlation()

acp <- env[[3]] %>% select_if(is.numeric) %>% dudi.pca()
fun_pca(acp,axes=c(1,2),repel = TRUE)
fun_pca(acp,axes=c(3,4),repel = TRUE)
fun_pca(acp,axes=c(5,6),repel = TRUE)


#### Bv
env[[2]] %>% select_if(is.numeric) %>% PCA()
env[[2]] %>% select_if(is.numeric) %>% cor() %>% corrplot::corrplot()

acp <- env[[2]] %>% select_if(is.numeric) %>% dudi.pca()
fun_pca(acp,axes=c(1,2),repel = TRUE)
fun_pca(acp,axes=c(3,4),repel = TRUE)
fun_pca(acp,axes=c(5,6),repel = TRUE)
fun_pca(acp,axes=c(5,6),tab = TRUE)



#### Bv occsol
env[[1]] %>% select_if(is.numeric) %>% PCA()
env[[1]] %>% select_if(is.numeric) %>% cor() %>% corrplot::corrplot()
env[[1]] %>% select_if(is.numeric) %>% chart.Correlation()

acp <- env[[1]] %>% select_if(is.numeric) %>% dudi.pca()
fun_pca(acp,axes=c(1,2),repel = TRUE)
fun_pca(acp,axes=c(3,4),repel = TRUE)






############## Toutes les données env

write.csv(table_env_filtre,"/home/theo/Bureau/Data/DATA/ENV/ENV_filtre/df_env_filtre2.csv",row.names = F)
table_env_filtre<-read_csv("/home/theo/Bureau/Data/DATA/ENV/ENV_filtre/df_env_filtre2.csv")
table_env_chr <- table_env_filtre %>% select_if(is.factor)
######## sélection des composantes pour avoir 80% de la variance du jeux de données
env_num <- table_env_filtre %>% select_if(is.numeric) 
corrplot::corrplot(cor(env_num))
chart.Correlation(env_num)
acp<-env_num %>% dudi.pca()
fun_pca(acp,axes=c(1,2),repel = TRUE)
fun_pca(acp,tab=T)
comp_80 <- fun_comp(df=table_env_filtre %>% select_if(is.numeric) ,site=table_env_filtre$code_lac,seuil=70)
comp_80_dummy <- inner_join(comp_80,table_env_chr)
comp_80
data_comp <- inner_join(comp_80,jointure_sauf_sans_inv)
data_comp <- inner_join(comp_80,jointure_tot)

########################################## Données traits #############################################

################## macrophytes ################
trait_macro <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macrophytes.csv") %>% column_to_rownames()

mat_dist <- trait_macro %>% gowdis()
is.euclid(mat_dist)
is.euclid(sqrt(mat_dist))
mat_dist <- trait_macro %>% gowdis() %>% sqrt()
pcoa_macro <- dudi.pco(mat_dist)
pcoa_macro$eig/sum(pcoa_macro$eig)*100
pcoa_macro <- pcoa(mat_dist)
pcoa_macro$trace
biplot.pcoa(pcoa_macro)
biplot(pcoa_macro)
scatter(pcoa_macro)

################## poissons ################
trait_macro <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_poissons.csv")%>% 
  mutate_if(is.binaire,as.numeric) %>% 
  column_to_rownames() 

mat_dist <- trait_macro %>% gowdis()
is.euclid(mat_dist)
is.euclid(sqrt(mat_dist))
mat_dist <- trait_macro %>% gowdis() %>% sqrt()
pcoa_macro <- dudi.pco(mat_dist)
pcoa_macro$eig/sum(pcoa_macro$eig)*100
pcoa_macro <- pcoa(mat_dist)
pcoa_macro$trace
biplot.pcoa(pcoa_macro)
biplot(pcoa_macro)
scatter(pcoa_macro)


################## poissons ################
trait_macro <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_phytoplancton.csv") %>% 
  column_to_rownames() %>% 
  mutate_if(is.binaire,as.numeric)
colnames(trait_macro)
mat_dist <- trait_macro %>% gowdis()
is.euclid(mat_dist)
is.euclid(sqrt(mat_dist))
mat_dist <- trait_macro %>% gowdis() %>% sqrt()
pcoa_macro <- dudi.pco(mat_dist)
pcoa_macro$eig/sum(pcoa_macro$eig)*100
pcoa_macro <- pcoa(mat_dist)
biplot.pcoa(pcoa_macro)
biplot(pcoa_macro)
scatter(pcoa_macro)
a <- hclust(mat_dist)
plot(a)

################## macroinvertébrés ################
trait_macro <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macroinvertebres.csv") %>% 
  mutate_if(is.binaire,as.numeric) %>% 
  column_to_rownames()
  

mat_dist <- trait_macro %>% gowdis()
is.euclid(mat_dist)
is.euclid(sqrt(mat_dist))
mat_dist <- trait_macro %>% gowdis() %>% sqrt()
pcoa_macro <- dudi.pco(mat_dist)
pcoa_macro$eig/sum(pcoa_macro$eig)*100
pcoa_macro <- pcoa(mat_dist)
biplot.pcoa(pcoa_macro)
biplot(pcoa_macro)
scatter(pcoa_macro)
a <- hclust(mat_dist)
plot(a)

########################################" selection de modèle ###################################
env <- read_csv("/home/theo/Bureau/Data/DATA/ENV/ENV_filtre/df_env_filtre2.csv") %>% dplyr::select(-c(mefm)) %>% 
  inner_join(jointure_sauf_sans_inv) %>% 
  mutate_if(is.numeric,~scale(.))
jointure_sauf_sans_inv %>% column_to_rownames() %>% chart.Correlation()
env <- data_comp
iml$r_bv_retenu_cumu %>% hist()
########## modèle IML ################
iml <- list_tab_fun[[2]] %>% inner_join(table_env_filtre) %>% mutate_if(is.numeric,~scale(.))
### richesse
iml_fric <- iml %>% dplyr::select(-c(code_lac,inv_FEve,inv_RaoQ,inv_FDiv)) %>% 
  setnames(colnames(.),str_replace_all(colnames(.),"[,1]",""))
iml_fric %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-inv_FRic) %>% 
  ggplot(aes(x=val,y=log(inv_FRic+1)))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth()
iml_reg_lin <- lm(inv_iml_FRic ~ substrat_shannon, data=iml_fric)
summary(iml_reg_lin)
iml_BIC_lin <- stepAIC(iml_reg_lin,k=log(ncol(iml_fric)))
summary(iml_BIC_lin)
ggplot(data=iml_fric,aes(x=substrat_shannon,y=inv_iml_FRic))+geom_point()+geom_smooth(method='lm')
######### modèle macrophytes #########
### richesse fonctionnelle
mac_fric <- fun_select_tax(env) %>% 
  dplyr::select(-code_lac) %>% 
  na.omit()
mac_fric <- fun_select_tax(env) %>% 
  na.omit() %>%
  inner_join(env %>% dplyr::select(code_lac,mac_DIV_richesse)) %>% 
  mutate(mac_DIV_richesse=scale(log(mac_DIV_richesse+2))) %>% 
  dplyr::select(-code_lac)

reg_res <- lm(mac_FRic ~ mac_DIV_richesse,data=mac_fric) %>% .$res
mac_fric <- mac_fric %>%
  dplyr::select(-c(mac_FRic, mac_DIV_richesse)) %>% 
  bind_cols(data.frame(mac_FRic=reg_res)) %>% na.omit()

samp <- createDataPartition(mac_fric$phy_FRic,p=1)[[1]]
mac_fric_train <- mac_fric[samp,]
mac_fric_test <- mac_fric[-samp,]
#pplot
mac_fric %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-mac_FRic) %>% 
  ggplot(aes(x=val,y=mac_FRic))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth()
plot(mac_fric$mac_FRic~mac_fric$perimetre_pla)
var_chr <- mac_fric %>% dplyr::select_if(is.character) %>% colnames()
mac_fric %>% 
  gather(var,val,-mac_FRic) %>% 
  dplyr::filter(var %in% var_chr) %>% 
  ggplot(aes(x=val,y=mac_FRic))+
  geom_boxplot()+
  facet_wrap(~var,scale="free")

# modèle
ml_macro_ric <- lm(mac_FRic~., data=mac_fric_train)
summary(ml_macro_ric)

#aic
ml_macro_ric_aic <- stepAIC(ml_macro_ric, direction = "both")
summary(ml_macro_ric_aic)
plot(ml_macro_ric_aic)

#bic
n <- nrow(mac_fric_train)
ml_macro_ric_bic <- stepAIC(ml_macro_ric,direction = "both",k=log(n))
summary(ml_macro_ric_bic)
plot(ml_macro_ric_bic)
tab <- tidy(ml_macro_ric_bic) %>% mutate_if(is.numeric,~round(.,3)) %>% arrange(desc(abs(estimate)))
write.csv(tab,"/home/theo/Bureau/Présentation résultats préliminaire/modèle/rf_res_macro_bic.csv")

#erreurs sur échantillons test
test <- env[-samp,]$mac_FRic
pred_tot <-predict(ml_macro_ric,mac_fric_test)
pred_bic <- predict(ml_macro_ric_bic,mac_fric_test)
pred_aic <- predict(ml_macro_ric_aic,mac_fric_test)
plot(pred_bic,na.omit(test))

######### modèle poissons #########

### richesse fonctionnelle
poi_fric <- fun_select_tax(env,tax="poi",ind = "poi_FRic") %>% na.omit() %>% dplyr::select(-c(code_lac,mac_FRic))

poi_fric <- fun_select_tax(env,tax="poi",ind = "poi_FRic") %>% 
  na.omit() %>%
  inner_join(env %>% dplyr::select(code_lac,poi_DIV_richesse)) %>% 
  mutate(poi_DIV_richesse=scale(log(poi_DIV_richesse+2))) %>% 
  dplyr::select(-code_lac)
reg_res <- lm(poi_FRic ~ poi_DIV_richesse,data=poi_fric) %>% .$res
poi_fric <- poi_fric %>%
  dplyr::select(-c(poi_FRic, poi_DIV_richesse)) %>% 
  bind_cols(data.frame(poi_FRic=reg_res))

poi_fric %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-poi_FRic) %>% 
  ggplot(aes(x=val,y=log(poi_FRic)))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth()

var_chr <- poi_fric %>% dplyr::select_if(is.character) %>% colnames()
poi_fric %>% 
  gather(var,val,-poi_FRic) %>% 
  dplyr::filter(var %in% var_chr) %>% 
  ggplot(aes(x=val,y=poi_FRic))+
  geom_boxplot()+
  facet_wrap(~var,scale="free")

ml_poi_ric <- lm(poi_FRic~., data=poi_fric)
summary(ml_poi_ric)

#aic
ml_poi_ric_aic <- stepAIC(ml_poi_ric)
summary(ml_poi_ric_aic)
plot(ml_poi_ric_aic)

#bic
n <- nrow(poi_fric)
ml_poi_ric_bic <- stepAIC(ml_poi_ric,k=log(n),)
summary(ml_poi_ric_bic)
plot(ml_poi_ric_bic)

tab <- tidy(ml_poi_ric_bic) %>% mutate_if(is.numeric,~round(.,3)) %>% arrange(desc(abs(estimate)))
write.csv(tab,"/home/theo/Bureau/Présentation résultats préliminaire/modèle/rf_res_poi_bic.csv")
######### modèle phyto #########

### richesse fonctionnelle
phy_fric <- fun_select_tax(env,tax="phy",ind = "phy_FRic") %>% na.omit() %>% dplyr::select(-c(code_lac,mac_FRic))

phy_fric <- fun_select_tax(env,tax="phy",ind = "phy_FRic") %>% 
  na.omit() %>%
  inner_join(env %>% dplyr::select(code_lac,phy_DIV_richesse)) %>% 
  mutate(phy_DIV_richesse=scale(log(phy_DIV_richesse+2))) %>% 
  dplyr::select(-code_lac)
reg_res <- lm(phy_FRic ~ phy_DIV_richesse,data=phy_fric) %>% .$res
phy_fric <- phy_fric %>%
  dplyr::select(-c(phy_FRic, phy_DIV_richesse)) %>% 
  bind_cols(data.frame(phy_FRic=reg_res))


phy_fric %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-phy_FRic) %>% 
  ggplot(aes(x=val,y=phy_FRic))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth()

var_chr <- phy_fric %>% dplyr::select_if(is.character) %>% colnames()
phy_fric %>% 
  gather(var,val,-phy_FRic) %>% 
  dplyr::filter(var %in% var_chr) %>% 
  ggplot(aes(x=val,y=phy_FRic))+
  geom_boxplot()+
  facet_wrap(~var,scale="free")

ml_phy_ric <- lm(phy_FRic~., data=phy_fric)
summary(ml_phy_ric)

#aic
ml_phy_ric_aic <- stepAIC(ml_phy_ric)
summary(ml_phy_ric_aic)
plot(ml_phy_ric_aic)

#bic
n <- nrow(phy_fric)
ml_phy_ric_bic <- stepAIC(ml_phy_ric,k=log(n),direction = "both")
summary(ml_phy_ric_bic)
plot(ml_phy_ric_bic)
tab <- tidy(ml_phy_ric_bic) %>% mutate_if(is.numeric,~round(.,3)) %>% arrange(desc(abs(estimate)))
write.csv(tab,"/home/theo/Bureau/Présentation résultats préliminaire/modèle/rf_res_phy_bic.csv")






######### modèle macroinv #########
env_macro <- jointure_tot %>% inner_join(env) 
env_macro <- list_tab_fun[[1]] %>% inner_join(env) 

### richesse fonctionnelle
inv_fric <- fun_select_tax(env_macro,tax="inv",ind = "inv_FRic") %>%
  na.omit() %>%
  dplyr::select(-code_lac) 

phy_fric <- fun_select_tax(env,tax="phy",ind = "phy_FRic") %>% 
  na.omit() %>%
  inner_join(env %>% dplyr::select(code_lac,phy_DIV_richesse)) %>% 
  mutate(phy_DIV_richesse=scale(log(phy_DIV_richesse+2))) %>% 
  dplyr::select(-code_lac)
reg_res <- lm(phy_FRic ~ phy_DIV_richesse,data=phy_fric) %>% .$res
phy_fric <- phy_fric %>%
  dplyr::select(-c(phy_FRic, phy_DIV_richesse)) %>% 
  bind_cols(data.frame(phy_FRic=reg_res))


inv_fric %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-inv_FRic) %>% 
  ggplot(aes(x=val,y=inv_FRic))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth()

var_chr <- phy_fric %>% dplyr::select_if(is.character) %>% colnames()
phy_fric %>% 
  gather(var,val,-phy_FRic) %>% 
  dplyr::filter(var %in% var_chr) %>% 
  ggplot(aes(x=val,y=phy_FRic))+
  geom_boxplot()+
  facet_wrap(~var,scale="free")

ml_phy_ric <- lm(phy_FRic~., data=phy_fric)
summary(ml_phy_ric)

#aic
ml_phy_ric_aic <- stepAIC(ml_phy_ric)
summary(ml_phy_ric_aic)
plot(ml_phy_ric_aic)

#bic
n <- nrow(phy_fric)
ml_phy_ric_bic <- stepAIC(ml_phy_ric,k=log(n),direction = "forward")
summary(ml_phy_ric_bic)
plot(ml_phy_ric_bic)
tab <- tidy(ml_phy_ric_bic) %>% mutate_if(is.numeric,~round(.,3)) %>% arrange(desc(abs(estimate)))
write.csv(tab,"/home/theo/Bureau/Présentation résultats préliminaire/modèle/rf_res_phy_bic.csv")

pls <- cv.spls(data.matrix(phy_fric[,-1]), phy_fric$inv_FRic, eta = seq(0.05,0.95,0.05), K = 1:15)
mod_pls <- spls(data.matrix(phy_fric[,-1]), phy_fric$inv_FRic, eta= 0.5, K= 1,)
ci_spls <- ci.spls(mod_pls) %>% correct.spls()
a <- ci_spls %>% as.data.frame() %>% setnames(colnames(.),"pente") %>%  arrange(desc(abs(pente))) %>% dplyr::filter(pente!=0)
colnames(a)




##################################" RAO Q ###########################################


######### modèle macrophytes #########

### richesse fonctionnelle
mac_raoq <- fun_select_tax(env,ind="mac_RaoQ") %>% 
  dplyr::select(-code_lac) %>% 
  na.omit()

mac_raoq <- fun_select_tax(env,ind="mac_RaoQ") %>% 
  na.omit() %>%
  inner_join(env %>% dplyr::select(code_lac,mac_DIV_richesse)) %>% 
  mutate(mac_DIV_richesse=scale(log(mac_DIV_richesse+2))) %>% 
  dplyr::select(-code_lac)
reg_res <- lm(mac_RaoQ ~ mac_DIV_richesse,data=mac_raoq) %>% .$res
mac_raoq <- mac_raoq %>%
  dplyr::select(-c(mac_RaoQ, mac_DIV_richesse)) %>% 
  bind_cols(data.frame(mac_RaoQ=reg_res)) %>% na.omit()

samp <- createDataPartition(mac_raoq$phy_FRic,p=1)[[1]]
mac_raoq_train <- mac_raoq[samp,]
mac_raoq_test <- mac_raoq[-samp,]
#pplot
mac_raoq %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-mac_RaoQ) %>% 
  ggplot(aes(x=val,y=mac_RaoQ))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth()

var_chr <- mac_raoq %>% dplyr::select_if(is.character) %>% colnames()
mac_raoq %>% 
  gather(var,val,-mac_RaoQ) %>% 
  dplyr::filter(var %in% var_chr) %>% 
  ggplot(aes(x=val,y=mac_RaoQ))+
  geom_boxplot()+
  facet_wrap(~var,scale="free")

mod_mac_rao <- fun_modele_div_lin(data=env,tax = "mac",indice = "mac_RaoQ",reg_res = FALSE)
write.csv(mod_mac_rao$bic[[1]],"/home/theo/Bureau/Présentation résultats préliminaire/modèle/raoq_res_macro_bic.csv")

  #erreurs sur échantillons test
test <- env[-samp,]$mac_FRic
pred_tot <-predict(ml_macro_ric,mac_fric_test)
pred_bic <- predict(ml_macro_ric_bic,mac_fric_test)
pred_aic <- predict(ml_macro_ric_aic,mac_fric_test)
plot(pred_bic,na.omit(test))

######### modèle poissons #########

### richesse fonctionnelle
poi_raoq <- fun_select_tax(env,tax="poi",ind = "poi_RaoQ") %>% na.omit() %>% dplyr::select(-code_lac)

poi_raoq <- fun_select_tax(env,tax="poi",ind = "poi_RaoQ") %>% 
  na.omit() %>%
  inner_join(env %>% dplyr::select(code_lac,poi_DIV_richesse)) %>% 
  mutate(poi_DIV_richesse=scale(log(poi_DIV_richesse+4))) %>% 
  dplyr::select(-code_lac)
reg_res <- lm(poi_RaoQ ~ poi_DIV_richesse,data=poi_raoq) %>% .$res
poi_raoq <- poi_raoq %>%
  dplyr::select(-c(poi_RaoQ, poi_DIV_richesse)) %>% 
  bind_cols(data.frame(poi_RaoQ=reg_res))

poi_raoq %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-poi_RaoQ) %>% 
  ggplot(aes(x=val,y=poi_RaoQ))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth()

var_chr <- poi_raoq %>% dplyr::select_if(is.character) %>% colnames()
poi_raoq %>% 
  gather(var,val,-poi_RaoQ) %>% 
  dplyr::filter(var %in% var_chr) %>% 
  ggplot(aes(x=val,y=poi_RaoQ))+
  geom_boxplot()+
  facet_wrap(~var,scale="free")

ml_poi_raoq <- lm(poi_RaoQ~., data=poi_raoq)
summary(ml_poi_raoq)

#aic
ml_poi_raoq_aic <- stepAIC(ml_poi_raoq)
summary(ml_poi_raoq_aic)
plot(ml_poi_ric_aic)

#bic
n <- nrow(poi_raoq)
ml_poi_raoq_bic <- stepAIC(ml_poi_raoq,k=log(n))
summary(ml_poi_raoq_bic)
plot(ml_poi_raoq_bic)

tab <- tidy(ml_poi_raoq_bic) %>% mutate_if(is.numeric,~round(.,3)) %>% arrange(desc(abs(estimate)))
write.csv(tab,"/home/theo/Bureau/Présentation résultats préliminaire/modèle/raoq_poi_bic.csv")
######### modèle phyto #########

### richesse fonctionnelle
phy_raoq <- fun_select_tax(env,tax="phy",ind = "phy_RaoQ") %>% na.omit() %>% dplyr::select(-code_lac)
mod <- fun_modele_div_lin(env,tax="phy",indice="phy_RaoQ")
mod$bic
phy_raoq <- fun_select_tax(env,tax="phy",ind = "phy_RaoQ") %>% 
  na.omit() %>%
  inner_join(env %>% dplyr::select(code_lac,phy_DIV_richesse)) %>% 
  mutate(phy_DIV_richesse=scale(log(phy_DIV_richesse+2))) %>% 
  dplyr::select(-code_lac)
reg_res <- lm(phy_RaoQ ~ phy_DIV_richesse,data=phy_raoq) %>% .$res
phy_raoq <- phy_raoq %>%
  dplyr::select(-c(phy_RaoQ, phy_DIV_richesse)) %>% 
  bind_cols(data.frame(phy_RaoQ=reg_res))


phy_fric %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-phy_FRic) %>% 
  ggplot(aes(x=val,y=phy_FRic))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth()

var_chr <- phy_fric %>% dplyr::select_if(is.character) %>% colnames()
phy_fric %>% 
  gather(var,val,-phy_FRic) %>% 
  dplyr::filter(var %in% var_chr) %>% 
  ggplot(aes(x=val,y=phy_FRic))+
  geom_boxplot()+
  facet_wrap(~var,scale="free")

ml_phy_raoq <- lm(phy_RaoQ~., data=phy_raoq)
summary(ml_phy_ric)

#aic
ml_phy_ric_aic <- stepAIC(ml_phy_ric)
summary(ml_phy_ric_aic)
plot(ml_phy_ric_aic)

#bic
n <- nrow(phy_raoq)
ml_phy_raoq_bic <- stepAIC(ml_phy_raoq,k=log(n),direction = "both")
summary(ml_phy_raoq_bic)
plot(ml_phy_ric_bic)
tab <- tidy(ml_phy_raoq_bic) %>% mutate_if(is.numeric,~round(.,3)) %>% arrange(desc(abs(estimate)))
write.csv(tab,"/home/theo/Bureau/Présentation résultats préliminaire/modèle/raoq_res_phy_bic.csv")






######### modèle macroinv #########
env_macro <- jointure_tot %>% inner_join(table_env_filtre) %>% dplyr::select(-c(mefm)) %>% mutate_if(is.numeric,~scale(.))
env_macro <- read_csv("/home/theo/Bureau/Data/DATA/ENV/ENV_filtre/df_env_filtre2.csv") %>% dplyr::select(-c(mefm)) %>% 
  inner_join(list_tab_fun[[1]]) %>% 
  mutate_if(is.numeric,~scale(.))
list_tab_fun[[1]]$inv_FRic %>%log() %>%  hist() 
### richesse fonctionnelle
phy_fric <- fun_select_tax(env_macro,tax="inv",ind = "inv_FRic") %>%
  na.omit() %>%
  dplyr::select(-code_lac) 

phy_fric <- fun_select_tax(env,tax="phy",ind = "phy_FRic") %>% 
  na.omit() %>%
  inner_join(env %>% dplyr::select(code_lac,phy_DIV_richesse)) %>% 
  mutate(phy_DIV_richesse=scale(log(phy_DIV_richesse+2))) %>% 
  dplyr::select(-code_lac)
reg_res <- lm(phy_FRic ~ phy_DIV_richesse,data=phy_fric) %>% .$res
phy_fric <- phy_fric %>%
  dplyr::select(-c(phy_FRic, phy_DIV_richesse)) %>% 
  bind_cols(data.frame(phy_FRic=reg_res))


phy_fric %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-inv_FRic) %>% 
  ggplot(aes(x=val,y=log(inv_FRic+1)))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth(method="lm",formula = y ~ poly(x,2,raw=TRUE))

var_chr <- phy_fric %>% dplyr::select_if(is.character) %>% colnames()
phy_fric %>% 
  gather(var,val,-phy_FRic) %>% 
  dplyr::filter(var %in% var_chr) %>% 
  ggplot(aes(x=val,y=phy_FRic))+
  geom_boxplot()+
  facet_wrap(~var,scale="free")

ml_phy_ric <- lm(log(inv_FRic+2)~., data=phy_fric)
summary(ml_phy_ric)

#aic
ml_phy_ric_aic <- stepAIC(ml_phy_ric)
summary(ml_phy_ric_aic)
plot(ml_phy_ric_aic)

#bic
n <- nrow(phy_fric)
ml_phy_ric_bic <- stepAIC(ml_phy_ric,k=log(n),direction = "both")
summary(ml_phy_ric_bic)
plot(ml_phy_ric_bic)
tab <- tidy(ml_phy_ric_bic) %>% mutate_if(is.numeric,~round(.,3)) %>% arrange(desc(abs(estimate)))
write.csv(tab,"/home/theo/Bureau/Présentation résultats préliminaire/modèle/rf_res_phy_bic.csv")

pls <- cv.spls(data.matrix(phy_fric[,-1]), phy_fric$inv_FRic, eta = seq(0.05,0.95,0.05), K = 1:15)
mod_pls <- spls(data.matrix(phy_fric[,-1]), phy_fric$inv_FRic, eta= 0.5, K= 1,)
ci_spls <- ci.spls(mod_pls) %>% correct.spls()
a <- ci_spls %>% as.data.frame() %>% setnames(colnames(.),"pente") %>%  arrange(desc(abs(pente))) %>% dplyr::filter(pente!=0)
colnames(a)
