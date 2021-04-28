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


# CWM par groupe taxo

list_tab_CWM<- map2(dir(),
                    name,
                    ~read_csv(.x) %>% dplyr::select(code_lac,starts_with("CWM")))

setwd("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_TAX/")
dir()
list_tab_tax <- map2(dir(),
                     name,
                     ~read_csv(.x) %>% 
                       setnames(colnames(.)[-1],paste0(.y,'_',colnames(.)[-1]))) 
table_divtax_sans_inv <- join_all(list_tab_tax[-1])
  



# /!!\ EN CHANTIER /!!\  ==> coinertie entre les CWM #

lac_com_cwm <-  join_all(list_tab_CWM[c(2,3)],type="inner",by="code_lac") %>% pull(code_lac)
data1 <- list_tab_CWM[[3]] %>% dplyr::filter(code_lac %in% lac_com_cwm) %>% column_to_rownames()
data2 <- list_tab_CWM[[2]] %>% dplyr::filter(code_lac %in% lac_com_cwm)%>% column_to_rownames()
co1 <- dudi.hillsmith(data1 %>% mutate_if(is.character,as.factor))
co2 <- dudi.hillsmith(data2 %>% mutate_if(is.character,as.factor),row.w = co1$lw)
coi <- coinertia(co1,co2)
randtest.coinertia(coi, nrepet = 999)
scatter(co1)
scatter(co2)

RV.rtest(co1$tab,co2$tab,nrepet=999)
a <- dudi.hillsmith(data %>% mutate_if(is.character,as.factor))
scatter(coi)
# /!!\ FIN CHANTIER /!!\ #
plot(co1$l1$RS3~co2$l1$RS2)

plot(log(co1$l1$RS2+5)~log(co2$l1$RS1+5))
fun_pca(co1)
#############################################################################################################################
#############################
############################    ~~~~~~~~     APPROCHE FONCTIONNELLE  ~~~~~~
#############################
##############################################################################################################################


################# analyses descriptive des congruences avec tous les taxons #####################
jointure_tot <-join_all(list_tab_fun,type="inner",by="code_lac")
jointure_tot %>% select_if(is.numeric) %>% chart.Correlation()
jointure_tot %>% select_if(is.numeric) %>% PCA()
a <- jointure_tot %>% select_if(is.numeric) %>% na.omit() %>%  dudi.pca()
fun_pca(a)
################# analyses descriptive des congruences sans macroinv #####################

jointure_sauf_sans_inv <-join_all(list_tab_fun[-1],type="inner",by="code_lac")
jointure_sauf_sans_inv %>% select_if(is.numeric) %>% chart.Correlation(method = "pearson")
jointure_sauf_sans_inv %>% select_if(is.numeric) %>% PCA()
a <- jointure_sauf_sans_inv %>% select_if(is.numeric) %>% na.omit() %>%  dudi.pca()
fun_pca(a)
nrow(jointure_sauf_sans_inv)
################# analyses descriptive des congruences par paires #####################

### phyto - poissons
jointure_phy_poi <-join_all(list_tab_fun[c(3,4)],type="inner",by="code_lac")
jointure_phy_poi %>% select_if(is.numeric) %>% chart.Correlation(method = "pearson")
jointure_phy_poi %>% select_if(is.numeric) %>% PCA()
### phyto - macro
jointure_phy_mac <-join_all(list_tab_fun[c(3,2)],type="inner",by="code_lac")
jointure_phy_mac %>% select_if(is.numeric) %>% chart.Correlation(method = "pearson")
jointure_phy_mac %>% select_if(is.numeric) %>% PCA()
### phyto - inv
jointure_phy_inv <-join_all(list_tab_fun[c(3,1)],type="inner",by="code_lac")%>% mutate(inv_FRic = log(inv_FRic))
jointure_phy_inv %>% select_if(is.numeric) %>% chart.Correlation(method = "pearson")
jointure_phy_inv %>% select_if(is.numeric) %>% PCA()
### poi - inv
jointure_poi_inv <-join_all(list_tab_fun[c(4,1)],type="inner",by="code_lac") %>% mutate(inv_FRic = log(inv_FRic))
jointure_poi_inv %>% select_if(is.numeric) %>% chart.Correlation(method = "pearson")
jointure_poi_inv %>% select_if(is.numeric) %>% PCA()
### poi - mac
jointure_poi_mac <-join_all(list_tab_fun[c(4,2)],type="inner",by="code_lac")
jointure_poi_mac %>% select_if(is.numeric) %>% chart.Correlation(method = "pearson")
jointure_poi_mac %>% select_if(is.numeric) %>% PCA()
### mac - inv
jointure_mac_inv <-join_all(list_tab_fun[c(1,2)],type="inner",by="code_lac")%>% mutate(inv_FRic = log(inv_FRic))
jointure_mac_inv %>% select_if(is.numeric) %>% chart.Correlation(method = "spearman")
jointure_mac_inv %>% select_if(is.numeric) %>% PCA()


jointure_mac_inv %>% dplyr::select(code_lac,inv_FRic) %>% arrange(inv_FRic)


###### relation richesse fonctionnelle inverébrés, rao macrophytes
ggplot(data=jointure_mac_inv%>% dplyr::filter(mac_RaoQ!=0),aes(y=inv_FRic,x=mac_RaoQ))+
  geom_point()+
  geom_smooth(method="lm",formula=y~x)+scale_y_log10()+
  theme_bw()
mod <- lm(log(inv_FRic)~mac_RaoQ,data=jointure_mac_inv %>% dplyr::filter(mac_RaoQ!=0)) 
summary(mod)
plot(mod)

ggplot(data=jointure_mac_inv%>% dplyr::filter(mac_RaoQ!=0),aes(y=inv_FRic,x=mac_FRic))+
  geom_point()+
  geom_smooth(method="lm",formula=y~x)+scale_y_log10()+
  theme_bw()
mod <- lm(log(inv_FRic)~mac_FRic,data=jointure_mac_inv %>% dplyr::filter(mac_RaoQ!=0)) 
summary(mod)
plot(mod)

ggplot(data=jointure_mac_inv,aes(y=inv_RaoQ,x=mac_FEve))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()
mod <- lm(inv_RaoQ~mac_FEve,data=jointure_mac_inv ) 
summary(mod)
plot(mod)


ggplot(data=jointure_mac_inv,aes(y=inv_FDiv,x=mac_FDiv))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_bw()
mod <- lm(inv_FDiv~mac_FDiv,data=jointure_mac_inv ) 
summary(mod)
plot(mod)

mod_comp <- lm(log(inv_FRic)~mac_FEve + mac_FDiv + mac_RaoQ + mac_FRic,data=jointure_mac_inv %>% dplyr::filter(mac_RaoQ!=0)) 
summary(mod_comp)
mod_aic <- stepAIC(mod_comp)
summary(mod_aic)
plot(mod_comp)

###### relation richesse fonctionnelle inverébrés, poissons
jointure_poi_inv %>% arrange(inv_FDiv) ##### ATTENTION !!! outliers
ggplot(data=jointure_poi_inv ,aes(x=inv_FEve,y=poi_FEve))+
  geom_point()+
  geom_smooth(method="lm",formula = y ~ poly(x,2))
mod <- lm(poi_FEve~inv_FEve,data=jointure_poi_inv %>% dplyr::filter(inv_FDiv > 0.65)) 
summary(mod)
plot(mod)

ggplot(data=jointure_poi_inv,aes(x=poi_FEve,y=inv_FEve))+
  geom_point()+
  geom_smooth(method="lm")
mod <- lm(poi_FEve ~ inv_FEve,data=jointure_poi_inv ) 
summary(mod)
plot(mod)
###### relation richesse fonctionnelle macrophytes, poissons
ggplot(data=jointure_poi_mac,aes(x=mac_RaoQ,y=poi_FRic))+
  geom_point()+
  geom_smooth(method="lm")+scale_y_log10()
mod <- lm(log(poi_FRic)~poly(mac_RaoQ,1),data=jointure_poi_mac ) 
summary(mod)
plot(mod)






####################################################" ENVIRONNEMENT ########################################################
setwd("/home/theo/Bureau/Data/DATA/ENV/ENV")
dir()
env <- map(dir(),~read_csv(.))  %>% modify_at(4,~ dplyr::select(.,code_lac,temp_moy_annuelle,precip_tot))
env_pur <- env[-6]
spatial <- env[[6]]
env

env_alber <- read_csv("/home/theo/Bureau/Data/DATA/ENV/ALBER & CHARLI/df_alber.csv") %>% 
  dplyr::select(code_lac = rowname,n_alter_cat,p_alter)
env_pur[[6]]<-env_alber

env_charli <- read_csv("/home/theo/Bureau/Data/DATA/ENV/ALBER & CHARLI/df_charli.csv") %>% 
  select_at(vars(matches("code_lac|shannon"))) %>% na.omit() %>% dplyr::select_at(vars(matches("code_lac|substrat")))
env_pur[[7]]<-env_charli

env[[3]] %>% ggplot(aes(x=sinuosite,y=perimetre_pla))+geom_point()
env[[1]] %>% select_if(is.numeric) %>% PCA()
env[[1]] %>% select_if(is.numeric) %>% chart.Correlation()

env[[2]] %>% select_if(is.numeric) %>% PCA()
env[[3]] %>% select_if(is.numeric) %>% PCA()
env[[4]] %>% select_if(is.numeric) %>% PCA()
env[[5]] %>% select_if(is.numeric) %>% PCA()
env[[5]] %>% select_if(is.numeric) %>% cor() %>% corrplot::corrplot()
table_env <- join_all(env_pur,type="full",by="code_lac") %>% 
  mutate(lake_order = factor(lake_order,ordered=TRUE)) %>% 
  mutate_if(is.character,as.factor)
table_env %>% vis_dat

table_env_filtre <- table_env %>%dplyr::select(-c(calcium, durete_totale, oxygene_dissous,
                   chlorures, module, alcalinite, prof_max_pla,
                   sinuosite,dbo5,turbidite_forma,chlorophylle_a,pheopigments,superficie_pla,magnesium,
                   volume_pla, AGS,p100_autre,precip_tot))%>% 
  mutate(lake_order = factor(lake_order,ordered=TRUE)) %>% 
  mutate_if(is.character,as.factor)
table_env_filtre %>% vis_dat
acp <- table_env_filtre %>% select_if(is.numeric) %>% dudi.pca() 
table_env_filtre %>% select_if(is.numeric) %>% cor() %>% corrplot::corrplot(type="upper") 
fun_pca(acp)

# table_env_filtre %>% column_to_rownames() %>% select_if(is.numeric) %>% PCA(axes = c(3,4))
# table_env_filtre %>% column_to_rownames() %>% select_if(is.numeric) %>% chart.Correlation()
# 
# acp <- table_env_filtre %>% select_if(is.numeric) %>% dudi.pca() %>% fun_pca(tab=TRUE)
# pca <- table_env %>% select_if(is.numeric) %>% dudi.pca() %>% fun_pca(axes=c(2,3),biplot=TRUE)


table_env_chr <- table_env %>% select_if(is.factor)
######## sélection des composantes pour avoir 80% de la variance du jeux de données


comp_80 <- fun_comp(df=table_env_filtre %>% select_if(is.numeric) ,site=table_env$code_lac,seuil=70)
comp_80_dummy <- inner_join(comp_80,table_env_chr)
comp_80

################################################# RELATION FUNCTIONAL - ENV ################################################

list_tab_fun
############## POISSONS ############
fun_poi <- list_tab_fun[[4]] %>% inner_join(comp_80_dummy) %>% inner_join(spatial)%>% dplyr::select(-code_lac) 
fun_poi <- list_tab_fun[[4]] %>% inner_join(table_env_filtre) %>% dplyr::select(-code_lac)

# rich
rich_poi <- fun_poi %>% 
  dplyr::select(-c("poi_FEve", "poi_FDiv", "poi_RaoQ"))
fun_poi_ric <- rich_poi  %>% 
  select_if(is.numeric) %>% 
  gather(comp,val,-poi_FRic) %>% 
  ggplot(aes(x=val,y=poi_FRic))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~comp,scale="free")


mod <- lm(poi_FRic ~.,data=rich_poi)
summary(mod)
select <- stepAIC(mod)
mod_aic <- lm(select)
summary(mod_aic)
plot(mod_aic)


# elsatic net
x <- model.matrix(poi_FRic~., rich_poi)[,-1]
y <- rich_poi$poi_FRic
cv_inv_ric <-glmnet::cv.glmnet(x,y,alpha=1)
cv_inv_ric$lambda.min
a <- glmnet::glmnet(x,y,alpha=1,lambda=cv_inv_ric$lambda.min)
predictions <- a %>% predict(x) %>% as.vector()
coef(a)
plot(predictions,y)
summary(lm(y~predictions))
# eve
eve_poi <- fun_poi %>% 
  dplyr::select(-c("poi_FRic", "poi_FDiv", "poi_RaoQ"))
fun_poi_eve <- eve_poi %>% 
  select_if(is.numeric) %>% 
  gather(comp,val,-poi_FEve) %>% 
  ggplot(aes(x=val,y=log(poi_FEve)))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~comp,scale="free")
mod <- lm(poi_FEve ~.,data=eve_poi)
select <- stepAIC(mod)
mod_aic <- lm(select)
summary(mod_aic)
plot(mod_aic)


# Rao
rao_poi <- fun_poi %>% 
  dplyr::select(-c("poi_FRic", "poi_FDiv", "poi_FEve"))
fun_poi_rao <- rao_poi %>% 
  select_if(is.numeric) %>% 
  gather(comp,val,-poi_RaoQ) %>% 
  ggplot(aes(x=val,y=poi_RaoQ))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~comp,scale="free")
mod <- lm(poi_RaoQ ~.,data=rao_poi)
select <- stepAIC(mod)
mod_aic <- lm(select)
summary(mod_aic)
plot(mod_aic)

library("pls")

############## PHYTOPLANCTON ############
fun_phy <- list_tab_fun[[3]] %>% inner_join(comp_80_dummy) %>% dplyr::select(-code_lac)
fun_phy <- list_tab_fun[[3]] %>% inner_join(table_env_filtre) %>% dplyr::select(-code_lac) %>% dplyr::select(-lake_order)
phy_spatial <-fun_phy%>% inner_join( spatial )
# rich
rich_phy <- fun_phy %>% 
  dplyr::select(-c("phy_FEve", "phy_FDiv", "phy_RaoQ"))
rich_phy_spatial <- phy_spatial %>% 
  dplyr::select(-c("phy_FEve", "phy_FDiv", "phy_RaoQ"))
fun_phy_ric <- rich_phy  %>%
  select_if(is.numeric) %>% 
  dplyr::filter(phy_FRic != 0) %>% 
  mutate(phy_FRic = log(phy_FRic)) %>% 
  dplyr::filter(phy_FRic > -10) %>% 
  gather(comp,val,-phy_FRic) %>% 
  ggplot(aes(x=val,y=phy_FRic))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm")+
  facet_wrap(~comp,scale="free")


mod <- lm(log(phy_FRic) ~ . ,data=rich_phy)
summary(mod)
select <- stepAIC(mod)
mod_aic <- lm(select)
summary(mod_aic)
plot(mod_aic)

mod <- lm(phy_FRic ~.,data=rich_phy)
summary(mod)
mod_final <- lm(stepAIC(mod))
summary(mod_final)
library("vegan")

# elsatic net
training.samples <- rich_phy$phy_FRic %>% createDataPartition(p=0.65,list=FALSE)

train.data <- rich_phy[training.samples,]
test.data <- rich_phy[-training.samples,]
  
  
x <- model.matrix(phy_FRic~., train.data)[,-1]
y <- train.data$phy_FRic
cv_inv_ric <-glmnet::cv.glmnet(x,y,alpha=1)
cv_inv_ric$lambda.min

lasso_ric_phy <- glmnet::glmnet(x,y,alpha=1,lambda=cv_inv_ric$lambda.min)

x.test <- model.matrix(phy_FRic~.,test.data)[,-1]
predictions <- lasso_ric_phy %>% predict(x.test) %>% as.vector()
coef(lasso_ric_phy)
plot(predictions,test.data$phy_FRic)
data.frame(
  RMSE = RMSE(predictions, test.data$phy_FRic),
  Rsquare = caret::R2(predictions, test.data$phy_FRic)
)

mod_pls <- plsr(phy_FRic ~ .,ncomp=55,data=rich_phy,validation="LOO")
summary(mod_pls)
plot(RMSEP(mod_pls), legendpos = "topright")
mod_pls <- plsr(phy_FRic ~ .,ncomp=22,data=rich_phy,validation="LOO")
summary(mod_pls)
plot(mod_pls, ncomp = 2, asp = 1, line = TRUE)
explvar(mod_pls)
plot(mod_pls, "loadings", comps = c(1:2), legendpos = "topleft",  labels = "numbers", xlab = "nm")

model <- caret::train(
  phy_FRic~., data = rich_phy %>% as.data.frame(), method = "pcr",
  scale = FALSE,
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
update.packages("caret")
library(caret)
partition <- varpart(Y=rich_phy[,1],X=rich_phy[,-1],X2=rich_phy_spatial[,3:4]^2)
partition <- varpart(Y=rich_phy[,1],X=rich_phy_spatial[,3:4]^2,X2=rich_phy[,-1])

plot(partition)
ggplot(fun_phy,aes(x=phosphore_total,y=phy_FRic))+geom_point()+geom_smooth(method = "lm",formula=y~I(x^2))+scale_y_log10()
mod <- lm(log(phy_FRic)~I(phosphore_total^1),data=fun_phy)
summary(mod)
# eve
eve_phy <- fun_phy %>% 
  dplyr::select(-c("phy_FRic", "phy_FDiv", "phy_RaoQ"))
fun_phy_ric <- eve_phy  %>% 
  gather(comp,val,-phy_FEve) %>% 
  ggplot(aes(x=val,y=phy_FEve))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~comp,scale="free")

mod <- lm(phy_FEve ~.,data=eve_phy)
summary(mod)
select <- stepAIC(mod)
mod_aic <- lm(select)
summary(mod_aic)
plot(mod_aic)

ggplot(eve_phy,aes(x=carbone_organique,y=phy_FEve))+geom_point()+geom_smooth(method = "lm")


# Rao
rao_phy <- fun_phy %>% 
  dplyr::select(-c("phy_FRic", "phy_FDiv", "phy_FEve"))
fun_phy_rao <- rao_phy %>% 
  gather(comp,val,-phy_RaoQ) %>% 
  ggplot(aes(x=val,y=phy_RaoQ))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~comp,scale="free")
mod <- lm(phy_RaoQ ~.,data=rao_phy)
select <- stepAIC(mod)
mod_aic <- lm(select)
summary(mod_aic)
plot(mod_aic)
 mod <- lm(phy_RaoQ ~poly(comp_PC_4,2)+poly(comp_PC_9,2)*typo_dce,data=rao_phy)
 summary(mod)
# diuv
div_phy <- fun_phy %>% 
  dplyr::select(-c("phy_FRic", "phy_RaoQ", "phy_FEve"))
fun_phy_div <- div_phy %>% 
  gather(comp,val,-phy_FDiv) %>% 
  ggplot(aes(x=val,y=phy_FDiv))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~comp,scale="free")
mod <- lm(phy_FDiv ~.,data=div_phy)
select <- stepAIC(mod)
mod_aic <- lm(select)
summary(mod_aic)
plot(mod_aic)


############## MACROPHYTES ############
fun_mac <- list_tab_fun[[2]] %>% inner_join(comp_80_dummy) %>% dplyr::select(-code_lac)
fun_mac <- list_tab_fun[[2]] %>% inner_join(table_env_filtre) %>% dplyr::select(-code_lac)

# ric
ric_mac <- fun_mac %>% 
  dplyr::select(-c("mac_FDiv", "mac_RaoQ", "mac_FEve")) %>% na.omit()
fun_mac_ric <- ric_mac %>% 
  select_if(is.numeric) %>% 
  gather(comp,val,-mac_FRic) %>% 
  ggplot(aes(x=val,y=mac_FRic))+
  geom_point()+
  geom_smooth(method="gam")+
  facet_wrap(~comp,scale="free")
mod <- lm(mac_FRic ~.,data=ric_mac)
select <- stepAIC(mod)
mod_aic <- lm(select)
summary(mod_aic)
plot(mod_aic)
mod_final <- lm(mac_FRic ~ (comp_PC_1 + comp_PC_4 + comp_PC_5 + comp_PC_6 + poly(comp_PC_7,2) + 
                  comp_PC_9 + comp_PC_10) * typo_dce, data = ric_mac)
summary(mod_final)                


# elsatic net
training.samples <- ric_mac$mac_FRic %>% createDataPartition(p=0.8,list=FALSE)

train.data <- ric_mac[training.samples,]
test.data <- ric_mac[-training.samples,]


x <- model.matrix(mac_FRic~., train.data)[,-1]
y <- train.data$mac_FRic
cv_inv_ric <-glmnet::cv.glmnet(x,y,alpha=1)
cv_inv_ric$lambda.min

lasso_ric_phy <- glmnet::glmnet(x,y,alpha=1,lambda=cv_inv_ric$lambda.min)

x.test <- model.matrix(mac_FRic~.,test.data)[,-1]
predictions <- lasso_ric_phy %>% predict(x.test) %>% as.vector()
coef(lasso_ric_phy)
plot(predictions,test.data$mac_FRic)
data.frame(
  RMSE = RMSE(predictions, test.data$mac_FRic),
  Rsquare = caret::R2(predictions, test.data$mac_FRic)
)
############## MACROINV ############
fun_inv <- list_tab_fun[[1]] %>% inner_join(comp_80_dummy) %>% dplyr::select(-code_lac)
fun_inv <- list_tab_fun[[1]] %>% inner_join(table_env_filtre) %>% dplyr::select(-code_lac)
ric_inv %>% View()s
# ric
ric_inv <- fun_inv %>% 
  dplyr::select(-c("inv_FDiv", "inv_RaoQ", "inv_FEve")) 

fun_inv_ric <- ric_inv %>% 
  dplyr::select_if(is.numeric) %>% 
  gather(comp,val,-inv_FRic) %>% 
  bind_cols(typo_dce=rep(ric_inv$typo_dce,11)) %>% 
  ggplot(aes(x=val,y=log(inv_FRic)))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~comp,scale="free")
mod <- lm(log(inv_FRic) ~.,data=ric_inv %>% dplyr::select_if(is.numeric))
summary(mod)
select <- stepAIC(mod,k=log(ncol(ric_inv)+1))
select <- stepAIC(mod,k=2)
mod_aic <- lm(select)
summary(mod_aic)
  plot(mod_aic)
mod <- lm(log(inv_FRic)~comp_PC_4*precip_saison_min,data=fun_inv)
summary(mod)
fun_inv %>% dplyr::select(inv_FRic) %>% arrange(inv_FRic)
  ggplot(data=fun_inv ,
         aes(x=perimetre_pla,y=log(inv_FRic),col=precip_saison_min))+
    geom_point()+geom_smooth(method="lm",formula= y ~ x)

  # elsatic net
  x <- model.matrix(inv_FRic~., ric_inv)[,-1]
  y <- log(ric_inv$inv_FRic)
  cv_inv_ric <-glmnet::cv.glmnet(x,y,alpha=1)
  cv_inv_ric$lambda.min
  a <- glmnet::glmnet(x,y,alpha=1,lambda=cv_inv_ric$lambda.min)
  predictions <- a %>% predict(x) %>% as.vector()
  coef(a)
summary(lm(log(inv_FRic)~potassium + conductivite + sinuosite, data=ric_inv))
  #############################################################################################################################
  #############################
  ############################    ~~~~~~~~     APPROCHE TAXONOMIQUE  ~~~~~~
  #############################
  ##############################################################################################################################

  
  # métrique taxonomique
  setwd("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/DIVERSITE_INDICE_TAX/")
  
  list_tab_fun <- map2(dir(), name, ~read_csv(.x) %>% 
                         setnames(colnames(.)[-1],
                                  paste0(.y,"_",str_remove_all(colnames(.)[-1],"[:upper:]{1,}_(?=.)"))))
  join_all(list_tab_tax,type="inner",by="code_lac") %>% column_to_rownames() %>% PCA()

  ##  
  
  
  
  ################# analyses descriptive des congruences avec tous les taxons #####################
  jointure_tot <-join_all(list_tab_fun,type="inner",by="code_lac")
  jointure_tot %>% select_if(is.numeric) %>% chart.Correlation()
  jointure_tot %>% select_if(is.numeric) %>% PCA()
  ################# analyses descriptive des congruences sans macroinv #####################
  
  jointure_sauf_sans_inv <-join_all(list_tab_fun[-1],type="inner",by="code_lac")
  jointure_sauf_sans_inv %>% select_if(is.numeric) %>% chart.Correlation(method = "pearson")
  jointure_sauf_sans_inv %>% select_if(is.numeric) %>% PCA()
  
  
  ################# analyses descriptive des congruences par paires #####################
  
  ### phyto - poissons
  jointure_phy_poi <-join_all(list_tab_fun[c(3,4)],type="inner",by="code_lac")
  jointure_phy_poi %>% select_if(is.numeric) %>% chart.Correlation(method = "spearman")
  jointure_phy_poi %>% select_if(is.numeric) %>% PCA()
  ### phyto - macro
  jointure_phy_mac <-join_all(list_tab_fun[c(3,2)],type="inner",by="code_lac")
  jointure_phy_mac %>% select_if(is.numeric) %>% chart.Correlation(method = "spearman")
  jointure_phy_mac %>% select_if(is.numeric) %>% PCA()
  ### phyto - inv
  jointure_phy_inv <-join_all(list_tab_fun[c(3,1)],type="inner",by="code_lac")
  jointure_phy_inv %>% select_if(is.numeric) %>% chart.Correlation(method = "spearman")
  jointure_phy_inv %>% select_if(is.numeric) %>% PCA()
  ### poi - inv
  jointure_poi_inv <-join_all(list_tab_fun[c(4,1)],type="inner",by="code_lac")
  jointure_poi_inv %>% select_if(is.numeric) %>% chart.Correlation(method = "pearson")
  jointure_poi_inv %>% select_if(is.numeric) %>% PCA()
  ### poi - mac
  jointure_poi_mac <-join_all(list_tab_fun[c(4,2)],type="inner",by="code_lac")
  jointure_poi_mac %>% select_if(is.numeric) %>% chart.Correlation(method = "pearson")
  jointure_poi_mac %>% select_if(is.numeric) %>% PCA()
  ### mac - inv
  jointure_mac_inv <-join_all(list_tab_fun[c(1,2)],type="inner",by="code_lac")
  jointure_mac_inv %>% select_if(is.numeric) %>% chart.Correlation(method = "spearman")
  jointure_mac_inv %>% select_if(is.numeric) %>% PCA()
  
  
  jointure_mac_inv %>% dplyr::select(code_lac,inv_FRic) %>% arrange(inv_FRic)
  
  
  ###### relation richesse fonctionnelle inverébrés, rao macrophytes
  ggplot(data=jointure_mac_inv%>% dplyr::filter(mac_RaoQ!=0),aes(y=inv_FRic,x=mac_RaoQ))+
    geom_point()+
    geom_smooth(method="lm",formula=y~x)+scale_y_log10()+
    theme_bw()
  mod <- lm(log(inv_FRic)~mac_RaoQ,data=jointure_mac_inv %>% dplyr::filter(mac_RaoQ!=0)) 
  summary(mod)
  plot(mod)
  
  ggplot(data=jointure_mac_inv%>% dplyr::filter(mac_RaoQ!=0),aes(y=inv_FRic,x=mac_FRic))+
    geom_point()+
    geom_smooth(method="lm",formula=y~x)+scale_y_log10()+
    theme_bw()
  mod <- lm(log(inv_FRic)~mac_FRic,data=jointure_mac_inv %>% dplyr::filter(mac_RaoQ!=0)) 
  summary(mod)
  plot(mod)
  
  ggplot(data=jointure_mac_inv,aes(y=inv_RaoQ,x=mac_FEve))+
    geom_point()+
    geom_smooth(method="lm")+
    theme_bw()
  mod <- lm(inv_RaoQ~mac_FEve,data=jointure_mac_inv ) 
  summary(mod)
  plot(mod)
  
  
  ggplot(data=jointure_mac_inv,aes(y=inv_FDiv,x=mac_FDiv))+
    geom_point()+
    geom_smooth(method="lm")+
    theme_bw()
  mod <- lm(inv_FDiv~mac_FDiv,data=jointure_mac_inv ) 
  summary(mod)
  plot(mod)
  
  mod_comp <- lm(log(inv_FRic)~mac_FEve + mac_FDiv + mac_RaoQ + mac_FRic,data=jointure_mac_inv %>% dplyr::filter(mac_RaoQ!=0)) 
  summary(mod_comp)
  mod_aic <- stepAIC(mod_comp)
  summary(mod_aic)
  plot(mod_comp)
  
  ###### relation richesse fonctionnelle inverébrés, poissons
  jointure_poi_inv %>% arrange(inv_FDiv) ##### ATTENTION !!! outliers
  ggplot(data=jointure_poi_inv %>% dplyr::filter(inv_FDiv > 0.65),aes(x=poi_FRic,y=inv_FDiv))+
    geom_point()+
    geom_smooth(method="lm")+scale_y_log10()+scale_x_log10()
  mod <- lm(log(inv_FDiv)~log(poi_FRic),data=jointure_poi_inv %>% dplyr::filter(inv_FDiv > 0.65)) 
  summary(mod)
  plot(mod)
  
  ggplot(data=jointure_poi_inv,aes(x=poi_FEve,y=inv_FEve))+
    geom_point()+
    geom_smooth(method="lm")
  mod <- lm(poi_FEve ~ inv_FEve,data=jointure_poi_inv ) 
  summary(mod)
  plot(mod)
  ###### relation richesse fonctionnelle macrophytes, poissons
  ggplot(data=jointure_poi_mac,aes(x=mac_RaoQ,y=poi_FRic))+
    geom_point()+
    geom_smooth(method="lm")+scale_y_log10()
  mod <- lm(log(poi_FRic)~poly(mac_RaoQ,1),data=jointure_poi_mac ) 
  summary(mod)
  plot(mod)
  

  ################################################# RELATION TAXO - ENV ################################################
  
  list_tab_fun
  ############## POISSONS ############
  fun_poi <- list_tab_fun[[4]] %>% inner_join(comp_80) %>% inner_join(spatial)%>% dplyr::select(-code_lac) 
  fun_poi <- list_tab_fun[[4]] %>% inner_join(table_env_filtre) %>% dplyr::select(-code_lac)
  
  # rich
  rich_poi <- fun_poi %>% 
    dplyr::select(-c("poi_simpson", "poi_shannon"))
  fun_poi_ric <- rich_poi  %>% 
    select_if(is.numeric) %>% 
    gather(comp,val,-poi_richesse) %>% 
    ggplot(aes(x=val,y=log(poi_richesse)))+
    geom_point()+
    geom_smooth(method="lm")+
    facet_wrap(~comp,scale="free")
  
  
  mod <- lm(poi_richesse ~.,data=rich_poi)
  summary(mod)
  select <- stepAIC(mod)
  mod_aic <- lm(select)
  summary(mod_aic)
  plot(mod_aic)
  
  
  # shan
  shan_poi <- fun_poi %>% 
    dplyr::select(-c("poi_simpson", "poi_richesse"))
  fun_poi_ric <- shan_poi  %>% 
    select_if(is.numeric) %>% 
    gather(comp,val,-poi_shannon) %>% 
    ggplot(aes(x=val,y=log(poi_shannon)))+
    geom_point()+
    geom_smooth(method="lm")+
    facet_wrap(~comp,scale="free")
  
  
  mod <- lm(log(poi_shannon) ~.,data=shan_poi)
  summary(mod)
  select <- stepAIC(mod)
  mod_aic <- lm(select)
  summary(mod_aic)
  plot(mod_aic)
  
  
  # simpson
  sim_poi <- fun_poi %>% 
    dplyr::select(-c("poi_shannon", "poi_richesse"))
  fun_poi_ric <- sim_poi  %>% 
    select_if(is.numeric) %>% 
    gather(comp,val,-poi_simpson) %>% 
    ggplot(aes(x=val,y=poi_simpson))+
    geom_point()+
    geom_smooth(method="lm")+
    facet_wrap(~comp,scale="free")
  
  
  mod <- lm(log(poi_simpson) ~.,data=sim_poi)
  summary(mod)
  select <- stepAIC(mod)
  mod_aic <- lm(select)
  summary(mod_aic)
  plot(mod_aic)