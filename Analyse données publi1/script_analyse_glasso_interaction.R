#####################################################################################################
#####
#####                     SCRIPT interactions
#####
#####################################################################################################
fun_select_tax <- function(data,seuil = 0.05){
  nom <- data  %>%
    gather(nom_taxo,val,-code_lac) %>% 
    mutate(val = ifelse(val>0,1,0)) %>% 
    group_by(nom_taxo) %>% 
    dplyr::summarise(val=sum(val)/nrow(data)) %>% 
    dplyr::filter(val>seuil) %>% 
    pull(nom_taxo) %>% return()
}





com_inv <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres.csv")
com_inv_iml <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres_iml.csv")
com_mac <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macrophytes.csv")
com_phy <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_phytoplancton_abondance.csv")
com_poi <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_poissons.csv")





########## IML
nom <- fun_select_tax(com_inv_iml,seuil=0.1)

phy <- com_inv_iml %>% dplyr::select(nom)%>% 
  # mutate_all(~log(.+1)) %>%
  qgraph::cor_auto(forcePD = TRUE) %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(com_inv_iml),
                 threshold = TRUE, layout = "spring",
                 # lambda.min.ratio = 0.3,
                 gamma=0.05)

                 phy %>% 
  # vegan::decostand(method="total") %>%
  mutate_all(~log(.+1)) %>%
  cor() %>%  
  corrplot::corrplot()

########## INV
nom <- fun_select_tax(com_inv,seuil=0.09)


phy <- com_inv %>% dplyr::select(nom)%>% 
  qgraph::cor_auto() %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(com_inv),
                 threshold = TRUE, layout = "spring",
                 # lambda.min.ratio = 0.3,
                 gamma=0.5,
                 cut = 0)


########## MAC
nom <- fun_select_tax(com_mac,seuil=0.1)


phy <- com_mac %>% dplyr::select(-code_lac)%>% 
  # mutate_all(~log(.+1)) %>%
  qgraph::cor_auto(forcePD= TRUE) %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(com_mac),
                 threshold = FALSE, layout = "spring",
                 # lambda.min.ratio = 0.3,
                 gamma= 0.05,
                 cut = 0)

phy
########## PHY
nom <- fun_select_tax(com_phy,seuil=0.1)


phy <- com_phy %>% dplyr::select(nom)%>% 
  mutate_all(~log(.+1)) %>%
  qgraph::cor_auto(forcePD=TRUE) %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(com_phy),
                 threshold =TRUE, layout = "spring",
                 # lambda.min.ratio = 0.3,
                 # gamma=0,
                 cut = 0)
log(phy$Ankistrodesmus_arcuatus+1) %>% hist()
phy$Achnanthidium %>% hist()
plot(phy$`Aphanizomenon_flos-aquae`,phy$Ceratium_hirundinella)



########## POI
nom <- fun_select_tax(com_poi,seuil=0.05)


phy <- com_poi %>% dplyr::select(-code_lac)%>% 
  mutate_all(~log(.+1)) %>%
  qgraph::cor_auto(forcePD= TRUE) %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(com_poi),
                 threshold =FALSE, layout = "spring",
                 # lambda.min.ratio = 0.3,
                 gamma=0,
                 cut = 0)
log(phy$Ankistrodesmus_arcuatus+1) %>% hist()
phy$Achnanthidium %>% hist()
plot(phy$`Aphanizomenon_flos-aquae`,phy$Ceratium_hirundinella)




######○ multi sans inv
# poi <- com_poi %>% dplyr::select(code_lac,fun_select_tax(com_poi,seuil=0.1))
# phy <- com_phy %>% dplyr::select(code_lac,fun_select_tax(com_phy,seuil=0.4))
# mac <- com_mac %>% dplyr::select(code_lac,fun_select_tax(com_mac,seuil=0.3))
# inv <- com_inv %>% dplyr::select(code_lac,fun_select_tax(com_inv,seuil=0.4))
poi <- com_poi
phy <- com_phy
mac <- com_mac
inv <- com_inv

mutli_sans_inv <- join_all(list(poi,mac,phy),type="inner") %>%
  dplyr::select(-code_lac) %>% 
  select_if(~sum(.)!= 0)

phy <- mutli_sans_inv %>%
  # mutate_all(~log(.+1)) %>%
  qgraph::cor_auto(forcePD=TRUE) %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(mutli_sans_inv),
                 threshold =FALSE, layout = "spring",
                 # lambda.min.ratio = 0.3,
                 gamma=0,
                 cut = 0)
phy %>% ncol()
