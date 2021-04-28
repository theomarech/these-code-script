
source("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/scriptR/librairies.R",
       encoding = "utf8")


setwd("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/PRET_ANALYSE/")
table <- map(dir(),~read_csv(.)) 
names(table) <- dir()
dir()
pos_tax <- c(1:4)
pos_fun <- c(1,5:8)
pos_env <- c(9:(ncol(table$iml_all.csv)-5))
pos_env_comp <- c((ncol(table$poi_all.csv)-4):(ncol(table$phy_all.csv)-1))

name_indice <- str_extract(colnames(table$poi_all.csv)[2:9],"(?<=_)[:alpha:]{1,}(?!_|[:alpha:])") %>%
  str_sub(start = 1,end = 3) %>% 
  str_to_lower()


###################################### Poissons ######################################
table_poisson <- table$poi_all.csv  %>% 
  setnames(colnames(.)[2:9],name_indice)

########## GLASSO ###########
cor_poi_lasso <- table_poisson %>%
  select_if(is.numeric) %>%
  dplyr::select(!matches("comp")) %>%
  na.omit() %>%
  mutate_all( ~ scale(.)) %>%
  as.data.frame()

glasso_poi <-   cor_poi_lasso %>%
  qgraph::cor_auto() %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(cor_poi_lasso),
                 threshold = TRUE, layout = "spring",
                 cut = 0)

########## Richesse fonctionnelle ##########


###################################### IML ######################################
table_iml <- table$iml_all.csv  %>% 
  setnames(colnames(.)[2:9],name_indice)

########## GLASSO ###########
cor_iml_lasso <- table_iml %>%
  select_if(is.numeric) %>%
  dplyr::select(!matches("comp")) %>%
  na.omit() %>%
  mutate_all( ~ scale(.)) %>%
  as.data.frame()

glasso_iml <-   cor_iml_lasso %>%
  qgraph::cor_auto() %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(cor_iml_lasso),
                 threshold = TRUE, layout = "spring",
                 cut = 0)

########## Richesse fonctionnelle ##########

###################################### Phyto ######################################
table_phy <- table$phy_all.csv  %>% 
  setnames(colnames(.)[2:9],name_indice)

########## GLASSO ###########
cor_phy_lasso <- table_phy %>%
  select_if(is.numeric) %>%
  dplyr::select(!matches("comp")) %>%
  na.omit() %>%
  mutate_all( ~ scale(.)) %>%
  as.data.frame()

glasso_phy <-   cor_phy_lasso %>%
  qgraph::cor_auto() %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(cor_phy_lasso),
                 threshold = TRUE, layout = "spring",
                 cut = 0)

########## Richesse fonctionnelle ##########