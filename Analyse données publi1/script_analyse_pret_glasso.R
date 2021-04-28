
source("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/scriptR/librairies.R",
       encoding = "utf8")


setwd("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/PRET_ANALYSE/")
table <- map(dir(),~read_csv(.) %>%
               dplyr::select(!matches("simpson")) %>% 
               dplyr::select(-c(perim_km,battance_pmoy))%>% 
               mutate_if(is.character,as.factor)) 
names(table) <- dir()
dir()

name_indice <- str_extract(colnames(table$poi_all.csv)[2:8],"(?<=_)[:alpha:]{1,}(?!_|[:alpha:])") %>%
  str_sub(start = 1,end = 3) %>% 
  str_to_lower()

table_poi <- table$poi_all.csv  %>% 
  setnames(colnames(.)[2:8],name_indice)
table_mac <- table$mac_all.csv  %>% 
  setnames(colnames(.)[2:8],name_indice)
table_iml <- table$iml_all.csv  %>% 
  setnames(colnames(.)[2:8],name_indice)
table_inv <- table$inv_all.csv  %>% 
  setnames(colnames(.)[2:8],name_indice)
table_phy <- table$phy_all.csv  %>% 
  setnames(colnames(.)[2:8],name_indice)


###################################### Poissons ######################################


########## GLASSO ###########
cor_poi_lasso <- table_poi %>%
  select_if(is.numeric) %>%
  dplyr::select(!matches("comp")) %>%
  na.omit() %>%
  mutate_all( ~ scale(.)) %>%
  as.data.frame()

glasso_poi <-   multi_sans_inv %>%
  dplyr::select_if(is.numeric) %>% 
  qgraph::cor_auto() %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(multi_sans_inv),
                 threshold = TRUE, layout = "spring",
                 cut = 0)


###################################### IML ######################################
)

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
                 threshold = FALSE, layout = "spring",
                 lambda.min.ratio = 0.1,
                 gamma = 0,
                 cut = 0)

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

###################################### macrophytes ######################################


########## GLASSO ###########
cor_mac_lasso <- table_mac %>%
  select_if(is.numeric) %>%
  dplyr::select(!matches("comp")) %>%
  na.omit() %>%
  mutate_all( ~ scale(.)) %>%
  as.data.frame()

glasso_mac <-   cor_mac_lasso %>%
  qgraph::cor_auto() %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(cor_mac_lasso),
                 threshold = TRUE, layout = "spring",
                 cut = 0)

###################################### inv ######################################


########## GLASSO ###########
cor_inv_lasso <- table_inv %>%
  select_if(is.numeric) %>%
  dplyr::select(!matches("comp")) %>%
  na.omit() %>%
  mutate_all( ~ scale(.)) %>%
  as.data.frame()

glasso_inv <-   cor_inv_lasso %>%
  qgraph::cor_auto() %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(cor_inv_lasso),
                 threshold = TRUE, layout = "spring",
                 cut = 0)
###################################### poissons ######################################


########## GLASSO ###########
cor_poi_lasso <- table_poi %>%
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
