
nom_col_ind_tax <- c("shannon","simpson","richesse", "pielou")




###### Poisson -----------------------------------------------------------------------------------------------------------------------
com_poi <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_poissons.csv")

list_tra_poi <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_poissons.csv") %>%
  dplyr::filter(typealimenttion %in% c("omni","inv","pisc")) %>% 
  split(.$typealimenttion)

list_com_poi <- map(list_tra_poi,~dplyr::select(com_poi,code_lac,all_of(.$nom_taxo)))


nom_col_poi <- paste0("poi","_",names(list_tra_poi)) 


list_indtax_poi <- pmap(list(list_com_poi,list_tra_poi,nom_col_poi), ~fun_div(..1,..2,func=FALSE) %>%
                          setnames(colnames(.)[-1], paste0(..3,"_",colnames(.) %>% .[-1] %>% str_extract("(?<=\\_)[:alpha:]{1,3}"))))

list_indfun_poi <- pmap(list(list_com_poi,list_tra_poi,nom_col_poi), ~fun_div(..1,..2,func=TRUE) %>%
                          setnames(colnames(.)[-1], paste0(..3,"_",colnames(.) %>% .[-1] %>% str_extract("(?<=\\_)[:alpha:]{1,3}"))))

test <- join_all(list_indtax_poi,type="full",by="code_lac")
test %>% select_if(is.numeric) %>% chart.Correlation()


###### Maccrophytes  -----------------------------------------------------------------------------------------------------------------------
com_mac <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macrophytes.csv")
tra_mac <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macrophytes.csv")
aqua <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/Données traits/Synthèse traits/macrophytes/aquaticite_complet.csv") %>% 
  mutate(nom_taxo = str_replace_all(nom_taxon,' ',"_")) %>% dplyr::select(-nom_taxon) %>% distinct() %>% inner_join(tra_mac,by="nom_taxo") %>% 
  mutate(aqua = case_when(aquaticite%in%c(1,2)~"hydr",
                                aquaticite==4 ~"helo",
                                aquaticite %in% c(5,8) ~"hygr")) %>% dplyr::select(-aquaticite) %>% distinct() 

nom_aqua <- aqua$nom_taxo %>% count() %>% arrange(freq) %>% dplyr::filter(freq ==2) %>% .$x

list_tra_mac <- aqua %>% 
  mutate(aqua = ifelse(nom_taxo %in% nom_aqua,"hygro",aqua)) %>% distinct() %>% split(.$aqua)




list_com_mac <- map(list_tra_mac,~dplyr::select(com_mac,code_lac,all_of(.$nom_taxo)))


nom_col_mac <- paste0("mac","_",names(list_tra_mac)) 


list_indtax_mac <- pmap(list(list_com_mac,list_tra_mac,nom_col_mac), ~fun_div(..1,..2,func=FALSE) %>%
                          setnames(colnames(.)[-1], paste0(..3,"_",colnames(.) %>% .[-1] %>% str_extract("(?<=\\_)[:alpha:]{1,3}"))))

list_indtax_poi <- pmap(list(list_com_poi,list_tra_poi,nom_col_poi), ~fun_div(..1,..2,func=TRUE) %>%
                          setnames(colnames(.)[-1], paste0(..3,"_",colnames(.) %>% .[-1] %>% str_extract("(?<=\\_)[:alpha:]{1,3}"))))

mac_all<- join_all(
  list_indtax_mac,type="full",by="code_lac")
test %>% select_if(is.numeric) %>% chart.Correlation()
inner_join(mac_all,test) %>% dplyr::select(matches("ric")) %>% mutate_all(~fun_boxcox(.+1)) %>% chart.Correlation


###### Maccrophytes  -----------------------------------------------------------------------------------------------------------------------
com_inv_iml <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres_iml.csv")
tra_inv_iml <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macroinvertebres_iml.csv") %>% 
  dplyr::select(nom_taxo,AFF:XYL)



tra_inv <- apply(select_if(tra_inv_iml,is.numeric),1,function(x)which.max(x) %>% names()) 
list_tra_inv_iml <- tra_inv_iml %>% mutate(alim_1 = tra_inv,
                       alim = case_when(alim_1 %in% c("AFF","GAT","SHR","XYL","PFF")~"detritivore",
                                        alim_1 %in% c("GRA")~"scrappers",
                                        alim_1 %in% c("PRED") ~"predatore",
                                        TRUE ~ NA_character_)) %>% 
  dplyr::select(nom_taxo,alim) %>% 
  na.omit() %>% 
  split(.$alim)


list_com_iml <- map(list_tra_inv_iml,~dplyr::select(com_inv_iml,code_lac,all_of(.$nom_taxo)))


nom_col_iml <- paste0("iml","_",names(list_tra_inv_iml)) 


list_indtax_iml <- pmap(list(list_com_iml,list_tra_inv_iml,nom_col_iml), ~fun_div(..1,..2,func=FALSE) %>%
                          setnames(colnames(.)[-1], paste0(..3,"_",colnames(.) %>% .[-1] %>% str_extract("(?<=\\_)[:alpha:]{1,3}"))))

list_indfun_iml <- pmap(list(list_com_poi,list_tra_poi,nom_col_poi), ~fun_div(..1,..2,func=TRUE) %>%
                          setnames(colnames(.)[-1], paste0(..3,"_",colnames(.) %>% .[-1] %>% str_extract("(?<=\\_)[:alpha:]{1,3}"))))

mac_all<- join_all(
  list_indtax_mac,type="full",by="code_lac")
test %>% select_if(is.numeric) %>% chart.Correlation()
inner_join(mac_all,test) %>% dplyr::select(matches("ric")) %>% mutate_all(~fun_boxcox(.+1)) %>% chart.Correlation




data <- join_all(list(list_indtax_iml %>% join_all(type="inner"),
list_indtax_mac %>% join_all(type="inner"),
list_indtax_poi %>% join_all(type="inner")),type="inner") 
data_comp_2 <- data %>% dplyr::select(matches("code_lac|pie")) %>% mutate_if(is.numeric,~fun_boxcox(.+1))

data <- inner_join(data_comp %>% dplyr::select(code_lac,PC_1:PC_5),data_comp_2)
