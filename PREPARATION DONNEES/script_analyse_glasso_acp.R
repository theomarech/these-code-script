######################################################################################
###
###               SCRIPT ANALYSE VIA GLASSO MULTITAXO et composantes
###
######################################################################################

# L'objectif est de regarder quelles sont les types de variables participant le plus à la diversité fonctionnelle
# sous ces différentes composantes : équitabilité, divergence, richesse
# les variables seront des composantes principales
# les autres variables seront les autres indices biotiques
# l'hypothèse est que ces bien l'environnement qui structure la diversité fonctionnel principalement
# les convergences (corrélation des réponses) soont principalement due à l'environnement
# ensuite pour chaque taxons suivant l'endroit (benthique/ pelagique/littoral)
# taille et niveau trophique des individus

# un glasso pour chaque indicateur avec : tous les taxons ; les composantes ; et les variables
# sans les inv
# avec chaque inv 
rm(list=ls(all=TRUE))
gc()
######################################
source("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/scriptR/librairies.R",
       encoding = "utf8")

setwd("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/PRET_ANALYSE/")
dir()
var_quali <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/ENV/ENV/env_lac.csv") %>% 
                        dplyr::select(code_lac,typo = typo_lac_clust)
data_mixte <- read_csv("table_mixte.csv") %>%
  split(.$libelle) %>% 
  map(~dplyr::select(.,-libelle) %>% inner_join(var_quali))

data_var <- read_csv("table_var.csv") %>%
  split(.$libelle) %>% 
  purrr::map(~dplyr::select(.,-libelle)%>% inner_join(var_quali) )

data_var2 <- read_csv("table_var_2.csv") %>%
  split(.$libelle) %>% 
  map(~dplyr::select(.,-libelle))

data_PC <- read_csv("table_PC.csv") %>%
  split(.$libelle) %>% 
  map(~dplyr::select(.,-c(libelle)))


"code_lac|fev|fri|fdi|pie|sha|ric"
###################################### ANALYSE MULTI RIC ################################
data <- data_poi_multi

data_env_new <- table_env_impute %>% tibble::rownames_to_column(var="code_lac")
data <- inner_join(data %>% dplyr::select(matches("mac|poi|phy"),code_lac:rao),data_env_new)

data<- data %>% dplyr::rename(sub = substrat_shannon)
multi_fev  <- data %>% dplyr::select(!matches("fri|ric|fdi|pie|sha|fev|rao")) 
acp <- fun_comp(select_if(multi_fev,is.numeric),site=multi_fev$code_lac,nax=5)
data_acp <- acp[[1]]
data_comp <- data  %>%  dplyr::select(matches("code_lac|fri"))  %>%mutate_if(is.numeric, ~fun_boxcox(.,lim = c(-10,10),test=FALSE)) %>% 
                    inner_join(data_acp)


data_comp <- data  %>% 
  dplyr::select(matches("code_lac|fri"))  %>%
  mutate_if(is.numeric, ~fun_boxcox(.,lim = c(-10,10))) %>% 
  inner_join(multi_fev)
data_poly <- data_comp %>% select_if(is.numeric) %>%  map2(colnames(.),~fun_poly(.x,name=.y,degree = 2)) %>% bind_cols %>% 
  mutate_all(scale)

# plot(data_comp$mac_fri~data_comp$phy_fri)
# a <- list(ggplot(data=data_comp,aes(phy_fri,mac_fri))+geom_point()+geom_smooth(method="lm",formula = y ~poly(x,2)),
# ggplot(data=data_comp,aes(PC_4,mac_fri))+geom_point()+geom_smooth(method="lm",formula = y ~poly(x,2)),
# ggplot(data=data_comp,aes(PC_4,phy_fri))+geom_point()+geom_smooth(method="lm",formula = y ~poly(x,2)))
# ggarrange(plotlist = a)
# 
# a <- list(ggplot(data=data_comp,aes(phy_fri,mac_fri))+geom_point()+geom_smooth(method="lm",formula = y ~poly(x,2)),
#           ggplot(data=data_comp,aes(PC_3,mac_fri))+geom_point()+geom_smooth(method="lm",formula = y ~poly(x,2)),
#           ggplot(data=data_comp,aes(PC_3,phy_fri))+geom_point()+geom_smooth(method="lm",formula = y ~poly(x,2)))
# ggarrange(plotlist = a)

data_comp %>%select_if(is.numeric) %>%  chart.Correlation(method="spearman")
data_comp %>%select_if(is.numeric) %>% cor(method = "spearman")

lm(data_comp$mac_fri~poly(data_comp$phy_fri,2,raw=TRUE)) %>% summary()
#boot_data <- data %>% sample_n(size=nrow(data))
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
plot(acp[[2]])
acp[[3]]
acp[[4]]

sparse_gam <- spacejam::SJ(data_comp %>% select_if(is.numeric) %>% na.omit() %>% mutate_all(scale),lambda =seq(0.01,1,0.02),bfun = function(x){cbind(x,x^2)})
sparse_gam <- spacejam::SJ(data_comp %>% select_if(is.numeric) %>% na.omit(),length = 10,bfun = function(x){cbind(x,x^2)})

sparse_gam

glasso_multi <-  data_comp %>% 
  select_if(is.numeric) %>% 
  na.omit() %>% 
  cor(method="spearman") %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(data_comp),
                 threshold = T, 
                 layout = "spring",
                 gamma = 0,
                 # refit = TRUE,
                 cut = 0)
glasso_multi <-   data_comp %>%
  dplyr::select_if(is.numeric) %>%
  na.omit() %>% 
  bootnet::bootnet(nBoots = 1000,
                   default = "EBICglasso",
                   corMethod = "cor_auto",
                   sampleSize = nrow(data_comp))

plot(glasso_multi$sample)

glasso_multi$bootTable
d
centrality_plot <- centralityPlot(glasso_multi)
centrality_plot$data %>% dplyr::filter(value==max(value))
data_poly %>% colnames() %>% sort()

################### MGM test ---------------------------------------------------------------------------
data <- data_comp %>% inner_join(var_quali) %>% mutate(typo = case_when(typo == "LP" ~ 1,
                                                                                 typo =="P" ~ 2,
                                                                                 TRUE ~ 3))
type = c(rep("g",ncol(data)-2),"c")

type <- c("g","p","g","g","g","g","g","p","g","g","g","g","g","p","g","g","g","g","c")

level = c(rep(1,ncol(data)-2),3)

data <- data[,-1] %>% na.omit()

mgm_mod <- mgm(data,
    type = type,
    level = level,
    lambdaSel = "EBIC",
    k = 4)


qgraph(mgm_mod$pairwise$wadj,
       edge.color = mgm_mod$pairwise$edgecolor,
       layout = "spring",
       labels =  colnames(data))
autism_data$type
autism_data$data
data_comp %>% select_if(is.numeric) %>%  chart.Correlation()
data_comp$mac_sha %>% hist()
data_comp <- data_comp %>% dplyr::filter(mac_sha>0.5)
