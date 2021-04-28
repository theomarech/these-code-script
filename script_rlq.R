env <- read_csv("/home/theo/Bureau/Data/DATA/ENV/ENV_filtre/df_env_filtre2.csv")
setwd("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/") 
com <- map(dir(),~read_csv(.)) 
setwd("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/") 
trait <- map(dir(),~read_csv(.)) 

############### macrophytes #################
# tax > 5% des lacs retenues$
 
com_macrophytes <- com[[2]] %>% 
  mutate_if(is.numeric,~ifelse(.>0,1,0)) %>%
  dplyr::select_if(is.numeric) %>%
  gather(var,val) %>% 
  group_by(var) %>% 
  dplyr::summarise(n= sum(val)/nrow(com[[2]])*100) %>% 
  dplyr::filter(n>5) %>% 
  pull(var) %>% 
  dplyr::select(.data=com[[2]],code_lac,.) %>% 
  dplyr::filter()

trait_macrophyte <- trait[[2]] %>% dplyr::filter(nom_taxo%in%colnames(com_macrophytes)[-1])

lac <- inner_join(env,com_macrophytes) %>% pull(code_lac)

env_macrophytes <- env %>% dplyr::filter(code_lac%in%lac) %>% dplyr::filter(!code_lac %in% c("ALL04","NEG06","CAR66"))
com_macrophytes <- com_macrophytes %>%
  dplyr::filter(code_lac %in% lac) %>%
  dplyr::filter(!code_lac %in% c("ALL04","NEG06","CAR66"))

com_macrophytes %>% column_to_rownames() %>% CA()

macro_com <- com_macrophytes %>% column_to_rownames()
macro_env <- env_macrophytes  %>%  mutate_if(is.character,as.factor)%>%  column_to_rownames(loc=ncol(.))
macro_tra <- trait_macrophyte  %>% mutate_if(is.character,as.factor)%>% column_to_rownames()
afc_L <- dudi.coa(macro_com,scannf=FALSE)
acp_R <- dudi.hillsmith(macro_env,scannf=FALSE,row.w = afc_L$lw)
acp_trait <- dudi.hillsmith(macro_tra,scannf=FALSE,row.w = afc_L$cw)
     
rlq_macro <- rlq(acp_R,afc_L,acp_trait)                   
plot(rlq_macro)
summary(rlq_macro)

four_macro <- ade4::fourthcorner(macro_env %>% select_if(is.numeric),
                                macro_com,
                                macro_tra,
                                nrepet = 999)
plot(four.comb.aravo,stat = "G")


############### macrophytes #################
# tax > 5% des lacs retenues$

com_inv <- com[[1]] %>% 
  mutate_if(is.numeric,~ifelse(.>0,1,0)) %>%
  dplyr::select_if(is.numeric) %>%
  gather(var,val) %>% 
  group_by(var) %>% 
  dplyr::summarise(n= sum(val)/nrow(com[[1]])*100) %>% 
  dplyr::filter(n>5) %>% 
  pull(var) %>% 
  dplyr::select(.data=com[[1]],code_lac,.) %>% 
  dplyr::filter()

trait_inv <- trait[[1]] %>% dplyr::filter(nom_taxo%in%colnames(com_inv)[-1])

lac <- inner_join(env,com_inv) %>% pull(code_lac)

env_inv <- env %>% dplyr::filter(code_lac%in%lac)
com_inv <- com_inv %>%
  dplyr::filter(code_lac %in% lac)

com_inv %>% column_to_rownames() %>% CA()

inv_com <- com_inv %>% column_to_rownames()
inv_env <- env_inv  %>%  mutate_if(is.character,as.factor)%>%  column_to_rownames(loc=ncol(.))
inv_tra <- trait_inv  %>% mutate_if(is.character,as.factor)%>% column_to_rownames()
afc_L <- dudi.coa(inv_com,scannf=FALSE)
acp_R <- dudi.hillsmith(inv_env,scannf=FALSE,row.w = afc_L$lw)
acp_trait <- dudi.hillsmith(inv_tra,scannf=FALSE,row.w = afc_L$cw)

rlq_inv <- rlq(acp_R,afc_L,acp_trait)                   
plot(rlq_inv)
summary(rlq_macro)

four_macro <- ade4::fourthcorner(inv_env %>% select_if(is.numeric),
                                 inv_com,
                                 inv_tra,
                                 nrepet = 999)
plot(four_macro,stat = "D2")

############### phytoplancton #################
# tax > 5% des lacs retenues$

com_inv <- com[[3]] %>% 
  mutate_if(is.numeric,~ifelse(.>0,1,0)) %>%
  dplyr::select_if(is.numeric) %>%
  gather(var,val) %>% 
  group_by(var) %>% 
  dplyr::summarise(n= sum(val)/nrow(com[[3]])*100) %>% 
  dplyr::filter(n>20) %>% 
  pull(var) %>% 
  dplyr::select(.data=com[[3]],code_lac,.) %>% 
  dplyr::filter()

trait_inv <- trait[[3]] %>% dplyr::filter(nom_taxo%in%colnames(com_inv)[-1])

lac <- inner_join(env,com_inv) %>% pull(code_lac)

env_inv <- env %>% dplyr::filter(code_lac%in%lac)
com_inv <- com_inv %>%
  dplyr::filter(code_lac %in% lac)

com_inv %>% column_to_rownames() %>% CA()

inv_com <- com_inv %>% column_to_rownames()
inv_env <- env_inv  %>%  mutate_if(is.character,as.factor)%>%  column_to_rownames(loc=ncol(.))
inv_tra <- trait_inv  %>% mutate_if(is.character,as.factor)%>% column_to_rownames()
afc_L <- dudi.coa(inv_com,scannf=FALSE)
acp_R <- dudi.hillsmith(inv_env,scannf=FALSE,row.w = afc_L$lw)
acp_trait <- dudi.hillsmith(inv_tra,scannf=FALSE,row.w = afc_L$cw)

rlq_inv <- rlq(acp_R,afc_L,acp_trait)                   
plot(rlq_inv)
summary(rlq_inv)

four_macro <- ade4::fourthcorner(inv_env %>% select_if(is.numeric),
                                 inv_com,
                                 inv_tra,
                                 nrepet = 10)
plot(four_macro,stat = "D2")