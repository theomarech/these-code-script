##################################################" RELATION TRAITS ENVIRONNEMENT POUR CHAQUE GROUPES #######################################################

################################################
############### ENVIRONNEMENT###################
################################################
setwd("/home/theo/Bureau/Data/DATA/ENV/ENV")
nom_env <- c("clc_","bv_","lac_","cli_","pc_","spa_")
env <- map2(dir(),nom_env,~read_csv(.x) %>% setnames(colnames(.)[-1],paste0(.y,colnames(.)[-1])))
env_pur <- env[-6]
spatial <- env[[6]]

table_env <- join_all(env,type="inner",by="code_lac") %>% 
  mutate(lac_lake_order = factor(lac_lake_order,ordered=TRUE)) %>% 
  mutate_if(is.character,as.factor)

table_env_filtre <- table_env %>%
  dplyr::select(-c(pc_calcium, pc_durete_totale,pc_oxygene_dissous,pc_chlorures,lac_module,lac_alcalinite,lac_prof_max_pla))%>% 
  mutate_if(is.character,as.factor)
comp_90 <- fun_comp(df=table_env_filtre %>% select_if(is.numeric) ,site=table_env$code_lac,seuil=90)
comp_80 <- fun_comp(df=table_env_filtre %>% select_if(is.numeric) ,site=table_env$code_lac,seuil=80)


################################################
################### POISSONS ###################
################################################

com_poissons <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/COM/com_poissons.csv")
tra_poissons <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/TRAIT/tra_poissons.csv")
env_poisson <- comp_90 %>% dplyr::filter(code_lac %in% com_poissons$code_lac)
com_poissons <- com_poisson %>% dplyr::filter(code_lac %in% env_poisson$code_lac)


nom_poi <- com_poissons %>% mutate_if(is.numeric,~ifelse(.==0,0,1)) %>%
  gather(val,var,-code_lac) %>%
  group_by(val) %>% 
  dplyr::summarise(n=sum(var)/224*100) %>% 
  arrange(desc(n)) %>%
  dplyr::filter(n>5) %>% 
  pull(val)
com_poissons_filtre <- com_poissons %>% 
  # dplyr::filter(!code_lac %in% c("BOU43","ISS07","RMC73","GAU65")) %>% 
  dplyr::select(code_lac,all_of(nom_poi)) %>% 
  dplyr::filter(code_lac%in%env_poisson$code_lac)
env_poisson <- env_poisson %>% dplyr::filter(code_lac %in% com_poissons_filtre$code_lac)
fun_var_part <- function(df){
  pc <- df %>% dplyr::select(starts_with("pc_"))
  lac <- df %>% dplyr::select(starts_with("lac_"))
  cli <- df %>% dplyr::select(starts_with("cli_"))
  bv <- df %>% dplyr::select(starts_with("clc_"),starts_with("bv_"))
  spa <- df %>% dplyr::select(starts_with("spa_"))
  return(list(pc,lac,cli,bv,clc,spa))
  
}

### histo distrib
com_poissons_filtre %>%  gather(var,val,-code_lac) %>% ggplot(aes(sqrt(val+1)))+geom_density(stat = "density")+facet_wrap(~var,scale="free")
##### (1) communautés : NMDS et CA ########

com_poissons_filtre %>% column_to_rownames()%>%CA()

mds_poissons <- com_poissons_filtre %>% column_to_rownames() %>% decostand(method='hellinger',MARGIN = 1) %>%  metaMDS(k=2,trymax = 100)
mds_poissons
plot(mds_poissons)


##### (2) RDA poissons vs env ##########
hellinger
rda_poissons_h0 <- rda(log(com_poissons_filtre[,-1]+1) ~ 1,
                    data=env_poisson %>% dplyr::select_if(is.numeric),
                    gamma = 1,  lambda = 1)
rda_poissons <- rda(log(com_poissons_filtre[,-1]+1) ~ .,
                    data=env_poisson %>% dplyr::select_if(is.numeric),
                    gamma = 1,  lambda = 1)
summary(rda_poissons)
adjr2 <- RsquareAdj(rda_poissons)

rda_select_poissons <- ordiR2step(rda_poissons_h0,
                                  scope = formula(rda_poissons),
                                  R2scope = adjr2$adj.r.squared,
                                  direction = "both",
                                  permutations = 999)

rda_select_poissons$anova$`Pr(>F)` <- p.adjust(rda_select_poissons$anova$`Pr(>F)`,method = "holm",n=ncol(env_poisson %>% dplyr::select_if(is.numeric)))
RsquareAdj(rda_select_poissons)

a <- fun_var_part(env_poisson)

a <- varpart(log(com_poissons_filtre[,-1]+1),X = pc, X2=lac, X3 = spa, X4 = cli)
plot(a)




################################################
################### PHYTOPLANCTON ###################
################################################

com_poissons <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_phytoplancton.csv")
tra_poissons <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/TRAIT/tra_poissons.csv")
env_poisson <- table_env_filtre %>% dplyr::filter(code_lac %in% com_poissons$code_lac)
com_poissons <- com_poissons %>% dplyr::filter(code_lac %in% env_poisson$code_lac)


nom_poi <- com_poissons %>% mutate_if(is.numeric,~ifelse(.==0,0,1)) %>%
  gather(val,var,-code_lac) %>%
  group_by(val) %>% 
  dplyr::summarise(n=sum(var)/224*100) %>% 
  arrange(desc(n)) %>%
  dplyr::filter(n>5) %>% 
  pull(val)
com_poissons_filtre <- com_poissons %>% 
  # dplyr::filter(!code_lac %in% c("BOU43","ISS07","RMC73","GAU65")) %>% 
  dplyr::select(code_lac,all_of(nom_poi)) %>% 
  dplyr::filter(code_lac%in%env_poisson$code_lac)
df <- env_poisson
fun_var_part <- function(df){
  pc <- df %>% dplyr::select(starts_with("pc_"))
  lac <- df %>% dplyr::select(starts_with("lac_"))
  cli <- df %>% dplyr::select(starts_with("cli_"))
  bv <- df %>% dplyr::select(starts_with("clc_"),starts_with("bv_"))
  spa <- df %>% dplyr::select(starts_with("spa_"))
  return(list(pc,lac,cli,bv,clc,spa))
  
}

### histo distrib
com_poissons_filtre %>%  gather(var,val,-code_lac) %>% ggplot(aes(sqrt(val+1)))+geom_density(stat = "density")+facet_wrap(~var,scale="free")
##### (1) communautés : NMDS et CA ########

com_poissons_filtre %>% column_to_rownames()%>%CA()

mds_poissons <- com_poissons_filtre %>% column_to_rownames() %>% decostand(method='hellinger',MARGIN = 1) %>%  metaMDS(k=2,trymax = 100)
mds_poissons
plot(mds_poissons)


##### (2) RDA poissons vs env ##########
hellinger
rda_poissons_h0 <- rda(log(com_poissons_filtre[,-1]+1) ~ 1,
                       data=env_poisson %>% dplyr::select_if(is.numeric),
                       gamma = 1,  lambda = 1)
rda_poissons <- rda(log(com_poissons_filtre[,-1]+1) ~ .,
                    data=env_poisson %>% dplyr::select_if(is.numeric),
                    gamma = 1,  lambda = 1)
summary(rda_poissons)
adjr2 <- RsquareAdj(rda_poissons)

rda_select_poissons <- ordiR2step(rda_poissons_h0,
                                  scope = formula(rda_poissons),
                                  R2scope = adjr2$adj.r.squared,
                                  direction = "both",
                                  permutations = 999)

rda_select_poissons$anova$`Pr(>F)` <- p.adjust(rda_select_poissons$anova$`Pr(>F)`,method = "holm",n=ncol(env_poisson %>% dplyr::select_if(is.numeric)))
RsquareAdj(rda_select_poissons)

a <- fun_var_part(env_poisson)
plot(rda_select_poissons)
a <- varpart(log(com_poissons_filtre[,-1]+1),X = pc, X2=lac, X3 = bv, X4 = cli)
plot(a)
a


################################################
################### MACROINV ###################
################################################

com_poissons <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres.csv")
tra_poissons <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/TRAIT/tra_poissons.csv")
env_poisson <- table_env_filtre %>% dplyr::filter(code_lac %in% com_poissons$code_lac)
com_poissons <- com_poissons %>% dplyr::filter(code_lac %in% env_poisson$code_lac)


nom_poi <- com_poissons %>% mutate_if(is.numeric,~ifelse(.==0,0,1)) %>%
  gather(val,var,-code_lac) %>%
  group_by(val) %>% 
  dplyr::summarise(n=sum(var)/224*100) %>% 
  arrange(desc(n)) %>%
  dplyr::filter(n>5) %>% 
  pull(val)
com_poissons_filtre <- com_poissons %>% 
  # dplyr::filter(!code_lac %in% c("BOU43","ISS07","RMC73","GAU65")) %>% 
  dplyr::select(code_lac,all_of(nom_poi)) %>% 
  dplyr::filter(code_lac%in%env_poisson$code_lac)
df <- env_poisson
fun_var_part <- function(df){
  pc <- df %>% dplyr::select(starts_with("pc_"))
  lac <- df %>% dplyr::select(starts_with("lac_"))
  cli <- df %>% dplyr::select(starts_with("cli_"))
  bv <- df %>% dplyr::select(starts_with("clc_"),starts_with("bv_"))
  spa <- df %>% dplyr::select(starts_with("spa_"))
  return(list(pc,lac,cli,bv,clc,spa))
  
}

### histo distrib
com_poissons_filtre %>%  gather(var,val,-code_lac) %>% ggplot(aes(sqrt(val+1)))+geom_density(stat = "density")+facet_wrap(~var,scale="free")
##### (1) communautés : NMDS et CA ########

com_poissons_filtre %>% column_to_rownames()%>%CA()

mds_poissons <- com_poissons_filtre %>% column_to_rownames() %>% decostand(method='hellinger',MARGIN = 1) %>%  metaMDS(k=2,trymax = 100)
mds_poissons
plot(mds_poissons)


##### (2) RDA poissons vs env ##########
hellinger
rda_poissons_h0 <- rda(log(com_poissons_filtre[,-1]+1) ~ 1,
                       data=env_poisson %>% dplyr::select_if(is.numeric),
                       gamma = 1,  lambda = 1)
rda_poissons <- rda(log(com_poissons_filtre[,-1]+1) ~ .,
                    data=env_poisson %>% dplyr::select_if(is.numeric),
                    gamma = 1,  lambda = 1)
plot(rda_poissons)
summary(rda_poissons)
adjr2 <- RsquareAdj(rda_poissons)

rda_select_poissons <- ordiR2step(rda_poissons_h0,
                                  scope = formula(rda_poissons),
                                  # R2scope = adjr2$adj.r.squared,
                                  direction = "both",
                                  permutations = 999)

rda_select_poissons$anova$`Pr(>F)` <- p.adjust(rda_select_poissons$anova$`Pr(>F)`,method = "holm",n=ncol(env_poisson %>% dplyr::select_if(is.numeric)))
RsquareAdj(rda_select_poissons)

a <- fun_var_part(env_poisson)
plot(rda_select_poissons)
a <- varpart(com_poissons_filtre[,-1],X = pc, X2=lac, X3 = bv)
plot(a)
a
densityplot(log(com_poissons_filtre[,-1]+1)[,7])






################################################
################### MACROINV ###################
################################################

com_poissons <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macrophytes.csv")
tra_poissons <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macrophytes.csv")
env_poisson <- table_env_filtre %>% dplyr::filter(code_lac %in% com_poissons$code_lac)
com_poissons <- com_poissons %>% dplyr::filter(code_lac %in% env_poisson$code_lac)


nom_poi <- com_poissons %>% mutate_if(is.numeric,~ifelse(.==0,0,1)) %>%
  gather(val,var,-code_lac) %>%
  group_by(val) %>% 
  dplyr::summarise(n=sum(var)/224*100) %>% 
  arrange(desc(n)) %>%
  dplyr::filter(n>5) %>% 
  pull(val)
com_poissons_filtre <- com_poissons %>% 
  # dplyr::filter(!code_lac %in% c("BOU43","ISS07","RMC73","GAU65")) %>% 
  dplyr::select(code_lac,all_of(nom_poi)) %>% 
  dplyr::filter(code_lac%in%env_poisson$code_lac)
df <- env_poisson
fun_var_part <- function(df){
  pc <- df %>% dplyr::select(starts_with("pc_"))
  lac <- df %>% dplyr::select(starts_with("lac_"))
  cli <- df %>% dplyr::select(starts_with("cli_"))
  bv <- df %>% dplyr::select(starts_with("clc_"),starts_with("bv_"))
  spa <- df %>% dplyr::select(starts_with("spa_"))
  return(list(pc,lac,cli,bv,clc,spa))
  
}

### histo distrib
com_poissons_filtre %>%  gather(var,val,-code_lac) %>% ggplot(aes(sqrt(val+1)))+geom_density(stat = "density")+facet_wrap(~var,scale="free")
##### (1) communautés : NMDS et CA ########

com_poissons_filtre %>% column_to_rownames()%>%CA()

mds_poissons <- com_poissons_filtre %>% column_to_rownames() %>% decostand(method='hellinger',MARGIN = 1) %>%  metaMDS(k=2,trymax = 100)
mds_poissons
plot(mds_poissons)


##### (2) RDA poissons vs env ##########
vega
rda_poissons_h0 <- rda(log(com_poissons[,-1]+1) ~ 1,
                       data=env_poisson %>% dplyr::select_if(is.numeric))
rda_poissons <- rda(log(com_poissons[,-1]+1) ~ .,
                    data=env_poisson %>% dplyr::select_if(is.numeric),
                    gamma = 1,  lambda = 1)
anova(rda_poissons)

plot(rda_poissons)
summary(rda_poissons)
adjr2 <- RsquareAdj(rda_poissons)

rda_select_poissons <- ordiR2step(rda_poissons_h0,
                                  scope = formula(rda_poissons),
                                  R2scope = adjr2$adj.r.squared,
                                  direction = "both",
                                  permutations = 999)
anova(rda_select_poissons)
rda_select_poissons$anova$`Pr(>F)` <- p.adjust(rda_select_poissons$anova$`Pr(>F)`,method = "holm")
                                               
RsquareAdj(rda_select_poissons)

a <- fun_var_part(env_poisson)
plot(rda_select_poissons)
a <- varpart(com_poissons_filtre[,-1],X = pc, X2=lac, X3 = spa, X4 = cli)
plot(a)
a
densityplot(log(com_poissons_filtre[,-1]+1)[,2])


###### (3) RLQ et fourthcorner macrophytes ##########

c