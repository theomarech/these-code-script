############################################################################################################################
#######
######                                      ARBRE phylogénétique 
#######
############################################################################################################################
source("/home/theo/Bureau/scriptR/librairies.R")
error_ott <- function(data,context_name = NULL,label_format = "name"){
  ott_ids <- tnrs_match_names(data,context_name = context_name) 
  safe_tol_induced_subtree <- safely(tol_induced_subtree)
  arbre <- safe_tol_induced_subtree(ott_ids = ott_ids$ott_id, label_format = label_format)
  erreur <- arbre$error %>%
    as.character() %>%
    str_extract_all("(?<=ott)[:digit:]{1,}(?=[:blank:])") %>%
    purrr::flatten_chr()
  
  ott_ids_final <- tnrs_match_names(data, context_name = context_name) %>%
    dplyr::filter(!ott_id %in% erreur)
  
  arbre_final <- tol_induced_subtree(ott_ids_final$ott_id,label_format = 'name') %>% 
    compute.brlen()
  nom <-  ott_ids %>%  dplyr::filter(ott_id %in% erreur)
  
  mat_cor_phylo <- arbre_final %>% 
    vcv(model = "Brownian", corr = TRUE) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "nom_taxo") %>% 
    mutate(nom_taxo = str_replace_all(nom_taxo,"_"," ")) %>% 
    setnames(colnames(.)[-1],str_replace_all(colnames(.)[-1],"_"," ")) %>% 
    inner_join(ott_ids_final, by = c("nom_taxo" = "unique_name")) %>% 
    dplyr::select(nom_taxo,search_string,all_of(.$nom_taxo))
  
  
  a <- list(mat_cor_phylo=mat_cor_phylo,arbre =arbre_final,erreur =nom)
  return(a)
}
########################################### MACROINVERTEBRES ###############################################################
taxa_macroinv <-
  read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres.csv") %>% 
  gather(var,val,-code_lac) %>% 
  pull(var) %>% 
  unique()
  
resolved_names_macroinv <- tnrs_match_names(taxa_macroinv,context_name = "Animals") %>%
  dplyr::filter(!ott_id %in% c('335844','588286',"5665485","5805045","588286","7117041","7176241"))# recherche des taxons dans la base de données de tree of life

# /!\ TRAVAUX /!\ #
arbre_macroinv <- safe_tol_induced_subtree(ott_ids = resolved_names_macroinv$ott_id,label_format = "name")
arbre_macroinv$error %>% as.character() %>% str_extract_all("(?<=ott)[:digit:]{1,}(?=[:blank:])")
arbre_macroinv$error %>% class()
arbre_macroinv$error %>% as.character() %>% class()



data <- error_ott(data = taxa_macroinv,context_name = "Animals")

## vérifier que le nombre de vairable est le meme dans nom_taxo et les noms de colonnes, sinon == synonymie ou un truc du genre avec une duplication
mat_cor_propre <- data$mat_cor_phylo  %>% 
  mutate(search_string = ifelse(search_string == "physella","physa",search_string)) %>% 
  mutate(search_string = str_to_sentence(.$search_string)) %>% 
  distinct() %>% 
  dplyr::select(-nom_taxo) %>%
  setnames(colnames(.)[-1],.$search_string)

## filtrer les données fonctionnelle et taxo avec les données phylo
taxa_macroinv <-
  read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres.csv") %>% 
  gather(var,val,-code_lac) %>% 
  dplyr::filter(var %in% mat_cor_propre$search_string) %>% 
  spread(var,val)
taxa_macroinv %>% dplyr::filter(code_lac !="NAN01") %>% column_to_rownames()  %>% CA()

fun_macroinv <-
  read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macroinvertebres.csv") %>% 
  dplyr::filter(nom_taxo %in% mat_cor_propre$search_string) 
fun_macroinv %>%  column_to_rownames() %>% CA()
# /!\ TRAVAUX /!\ #
write.csv(taxa_macroinv,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/COM/com_macroinvertebres.csv",row.names = FALSE)
write.csv(fun_macroinv,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/TRAIT/tra_macroinvertebres.csv",row.names = FALSE)
write.csv(mat_cor_propre,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/PHYLO/phylo_macroinvertebres.csv",row.names = FALSE)



########################################### POISSONS ###############################################################
taxa_poissons <-
  read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_poissons.csv") %>% 
  gather(var,val,-code_lac) %>% 
  dplyr::select(var) %>% 
  distinct()
error_ott <- function(data,context_name = NULL,label_format = "name"){
  safe_tol_induced_subtree <- safely(tol_induced_subtree)
  ott_ids <- tnrs_match_names(data,context_name = context_name) %>% na.omit()
  
  arbre <- safe_tol_induced_subtree(ott_ids = ott_ids$ott_id, label_format = label_format)
  erreur <- arbre$error %>%
    as.character() %>%
    str_extract_all("(?<=ott)[:digit:]{1,}(?=[:blank:])") %>%
    purrr::flatten_chr()
  
  ott_ids_final <- tnrs_match_names(data, context_name = context_name) %>%
    dplyr::filter(!ott_id %in% erreur) %>% na.omit()
  
  arbre_final <- tol_induced_subtree(ott_ids_final$ott_id,label_format = 'name') %>% 
    compute.brlen()
  nom <-  ott_ids %>%  dplyr::filter(ott_id %in% erreur)
  
  mat_cor_phylo <- arbre_final %>% 
    vcv(model = "Brownian", corr = TRUE) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "nom_taxo") %>% 
    mutate(nom_taxo = str_replace_all(nom_taxo,"_"," ")) %>% 
    setnames(colnames(.)[-1],str_replace_all(colnames(.)[-1],"_"," ")) %>% 
    inner_join(ott_ids_final, by = c("nom_taxo" = "unique_name")) %>% 
    dplyr::select(nom_taxo,search_string,all_of(.$nom_taxo))
  
  
  a <- list(mat_cor_phylo=mat_cor_phylo,arbre =arbre_final,erreur =nom)
  return(a)
}
poisson <- error_ott(data=taxa_poissons %>% pull(var),context_name = "Animals")

mat_cor_poisson <- poisson$mat_cor_phylo %>% as_tibble() %>%  dplyr::filter(!duplicated(nom_taxo))%>% 
  dplyr::select(-nom_taxo) %>%
  setnames(colnames(.)[-1],str_to_sentence(.$search_string)) %>% 
  mutate(search_string = str_to_sentence(search_string))
com_poisson_phylo <- 
  read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_poissons.csv") %>% 
  gather(var,val,-code_lac) %>% 
  mutate(var = ifelse(var == "Salmo_trutta_lacustris","Salmo_trutta_fario",var)) %>% 
  group_by(code_lac,var) %>% 
    dplyr::summarise(val = sum(val,na.rm=TRUE)) %>% 
  spread(var,val)
fun_poisson <- 
  read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_poissons.csv") %>% 
  mutate(nom_taxo = ifelse(nom_taxo == "Salmo_trutta_lacustris","Salmo_trutta_fario",nom_taxo)) %>% 
  dplyr::filter(nom_taxo %in% mat_cor_poisson$search_string) 
fun_macroinv %>%  column_to_rownames() %>% CA()
# /!\ TRAVAUX /!\ #
write.csv(com_poisson_phylo,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/COM/com_poissons.csv",row.names = FALSE)
write.csv(fun_poisson,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/TRAIT/tra_poissons.csv",row.names = FALSE)
write.csv(mat_cor_poisson,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/PHYLO/phylo_poissons.csv",row.names = FALSE)


########################################### phytoplancton ###############################################################
taxa_phytoplancton <-
  read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_phytoplancton.csv") %>% 
  gather(var,val,-code_lac) %>% 
  dplyr::select(var) %>% 
  distinct() %>% 
  mutate(var = ifelse(str_detect(var,".{1,}(?=(_var._)|(_f._)|(_subsp._))"),
                      str_extract(var,".{1,}(?=(_var._)|(_f._)|(_subsp._))"),
                      var)
         ) %>% 
  distinct() %>% 
  mutate(var = case_when(var == "Ankyra_inerme" ~ "Ankyra_inermis",
                          var == "Scenedesmus_pulloideus" ~ NA_character_,
                         TRUE ~ var)) %>% 
  na.omit() %>% 
  distinct()

  



error_ott <- function(data,context_name = NULL,label_format = "name"){
  safe_tol_induced_subtree <- safely(tol_induced_subtree)
  ott_ids <- tnrs_match_names(data,context_name = context_name) %>% na.omit()
  
  arbre <- safe_tol_induced_subtree(ott_ids = ott_ids$ott_id, label_format = label_format)
  erreur <- arbre$error %>%
    as.character() %>%
    str_extract_all("(?<=ott)[:digit:]{1,}(?=[:blank:])") %>%
    purrr::flatten_chr()
  
  ott_ids_final <- tnrs_match_names(data, context_name = context_name) %>%
    dplyr::filter(!ott_id %in% erreur) %>% na.omit()
  
  arbre_final <- tol_induced_subtree(ott_ids_final$ott_id,label_format = 'name') %>% 
    compute.brlen()
  nom <-  ott_ids %>%  dplyr::filter(ott_id %in% erreur)
  
  mat_cor_phylo <- arbre_final %>% 
    vcv(model = "Brownian", corr = TRUE) %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "nom_taxo") %>% 
    mutate(nom_taxo = str_replace_all(nom_taxo,"_"," ")) %>% 
    setnames(colnames(.)[-1],str_replace_all(colnames(.)[-1],"_"," ")) %>% 
    inner_join(ott_ids_final, by = c("nom_taxo" = "unique_name")) %>% 
    dplyr::select(nom_taxo,search_string,all_of(.$nom_taxo))
  
  
  a <- list(mat_cor_phylo=mat_cor_phylo,arbre =arbre_final,erreur =nom)
  return(a)
}


data <- taxa_phytoplancton %>% pull(var)
phylo_phyto <- error_ott(data)
phyto <- phylo_phyto$mat_cor_phylo
mat_cor_phyto <- phyto %>% as_tibble() %>%  dplyr::filter(!duplicated(nom_taxo))%>% 
  dplyr::select(-nom_taxo) %>%
  setnames(colnames(.)[-1],str_to_sentence(.$search_string)) %>% 
  mutate(search_string = str_to_sentence(search_string))

taxa_phyto <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_phytoplancton.csv") %>% 
  gather(var,val,-code_lac) %>% 
  mutate(var = ifelse(str_detect(var,".{1,}(?=(_var._)|(_f._)|(_subsp._))"),
                      str_extract(var,".{1,}(?=(_var._)|(_f._)|(_subsp._))"),
                      var),
         var = ifelse(var == "Ankyra_inerme","Ankyra_inermis",var)
  ) %>% 
  group_by(code_lac,var) %>% 
  dplyr::summarise(val = sum(val,na.rm=TRUE)) %>% 
  dplyr::filter(var %in% mat_cor_phyto$search_string) %>% 
  spread(var,val)

fun_phyto <-
  read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_phytoplancton.csv") %>%  
  mutate(nom_taxo = ifelse(str_detect(nom_taxo,".{1,}(?=(_var._)|(_f._)|(_subsp._))"),
                      str_extract(nom_taxo,".{1,}(?=(_var._)|(_f._)|(_subsp._))"),
                      nom_taxo),
         nom_taxo = ifelse(nom_taxo == "Ankyra_inerme","Ankyra_inermis",nom_taxo)
  )%>% dplyr::filter(!duplicated(nom_taxo)) %>% 
  dplyr::filter(nom_taxo %in% mat_cor_phyto$search_string)

fun_phyto %>% column_to_rownames() %>% mutate_if(is.character,as.factor) %>% dudi.hillsmith() %>% scatter()

anti_join(mat_cor_phyto,fun_phyto,by=c("search_string"="nom_taxo"))
write.csv(taxa_phyto,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/COM/com_phyto.csv",row.names = FALSE)
write.csv(fun_phyto,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/TRAIT/tra_phyto.csv",row.names = FALSE)
write.csv(mat_cor_phyto,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/PHYLO/phylo_phytoplancton.csv",row.names = FALSE)


########################################### macrophytes ###############################################################
########################################### POISSONS ###############################################################
taxa_macrophytes <-
  read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macrophytes.csv") %>% 
  gather(var,val,-code_lac) %>% 
  dplyr::select(var) %>% 
  distinct()

  
macrophytes <- error_ott(data=taxa_macrophytes %>% pull(var),context_name = "Vascular plants")
mat_cor_macrophytes <- macrophytes$mat_cor_phylo %>% as_tibble() %>%  dplyr::filter(!duplicated(nom_taxo))%>% 
  dplyr::select(-nom_taxo) %>%
  setnames(colnames(.)[-1],str_to_sentence(.$search_string)) %>% 
  mutate(search_string = str_to_sentence(search_string))

com_macro_phylo <- 
  read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macrophytes.csv") %>% 
  gather(var,val,-code_lac) %>% 
  dplyr::filter(var %in% mat_cor_macrophytes$search_string) %>% 
  spread(var,val)

fun_macro <- 
  read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macrophytes.csv") %>% 
  dplyr::filter(nom_taxo %in% mat_cor_macrophytes$search_string) 
acm <- fun_macro %>% column_to_rownames() %>% mutate_if(is.character,as.factor) %>% MCA()
fviz_mca_biplot(acm)
acm.disjonctif(fun_macro %>% column_to_rownames() %>% mutate_if(is.character,as.factor)) %>% dudi.acm()
write.csv(com_macro_phylo,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/COM/com_macro.csv",row.names = FALSE)
write.csv(fun_macro,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/TRAIT/tra_macro.csv",row.names = FALSE)
write.csv(mat_cor_macrophytes,"/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE&PHYLO/PHYLO/phylo_macrophytes.csv",row.names = FALSE)























taxa_macrophytes <-
  read_csv("/home/theo/Bureau/Data/Données finales/communautés/df_macrophytes.csv")  %>% dplyr::select(nom_taxo) %>% 
  mutate(nom_taxo = case_when(str_detect(nom_taxo,".{1,}(?=[:blank:](subsp|ssp.|var.[:blank:]|fo.[:blank:]|f.[:blank:]|morphotype))")~
                              str_extract(nom_taxo,".{1,}(?=[:blank:](subsp|ssp.|var.[:blank:]|fo.[:blank:]|f.[:blank:]|morphotype))"),
                              nom_taxo == "Erica tetralix"~NA_character_,
                              TRUE ~ nom_taxo))%>%
  distinct() %>% 
  na.omit()

### recherche des nom de taxons dans la base de données API de TOL
resolved_names_macrophytes <- tnrs_match_names(taxa_macrophytes$nom_taxo) %>% 
  dplyr::filter(!ott_id %in% c("486200","225532","486200","5144695","5144697","5325989","561402","738907","7564213"))

anti_resolved_names_macrophytes <- tnrs_match_names(taxa_macrophytes$nom_taxo) %>% 
  dplyr::filter(ott_id %in% c("486200","225532","486200","5144695","5144697","5325989","561402","738907","7564213"))

### Construction de l'arbre
arbre_macrophytes <- tol_induced_subtree(ott_ids = na.omit(resolved_names_macrophytes$ott_id),label_format = "name")
plot(arbre_macrophytes,no.margin=TRUE)

dev.off()
arbre_macrophytes_temps <- compute.brlen(arbre_macrophytes)

### COnstruction de la matirce de corrélation => rentrera dans les modèles joints
phylo_matcor_macrophytes <- vcv(arbre_macrophytes_temps,model="Brownian",corr=TRUE) %>%
  as_tibble() 

nom_taxon_macrophytes <- str_extract(colnames(phylo_matcor_macrophytes),".{1,}(?=_\\()|.{1,}(?!=_\\()")

phylo_matcor_macrophytes <- phylo_matcor_macrophytes %>%
  setnames(colnames(.),nom_taxon_macrophytes) %>%
  mutate(nom_taxo = colnames(.)) %>%
  dplyr::filter(!nom_taxo %in% c('mrcaott1976ott20943','mrcaott594ott6864')) %>%
  dplyr::select_at(vars(!matches("mrcaott")))
  # column_to_rownames(loc=ncol(.)) %>% 
  
phylo_matcor_macrophytes_j1 <- phylo_matcor_macrophytes %>%   
  inner_join(resolved_names_macrophytes %>% mutate(search_string = str_to_sentence(search_string) %>% str_replace_all("[:blank:]{1,}","_")),
             by=c("nom_taxo"="search_string"))

phylo_matcor_macrophytes_aj1 <- phylo_matcor_macrophytes %>% anti_join(resolved_names_macrophytes %>% mutate(search_string = str_to_sentence(search_string) %>% str_replace_all("[:blank:]{1,}","_")),
                                                                         by=c("nom_taxo"="search_string"))

phylo_matcor_macrophytes_j1var <- phylo_matcor_macrophytes_aj1 %>% dplyr::select(-phylo_matcor_macrophytes_aj1$nom_taxo)

phylo_matcor_macrophytes_j2 <- phylo_matcor_macrophytes_aj1 %>% inner_join(resolved_names_macrophytes %>% mutate(unique_name =str_replace_all(unique_name,"[:blank:]{1,}","_") %>% str_extract(".{1,}(?=_\\()|.{1,}(?!=_\\()")),
                                                                         by=c("nom_taxo"="unique_name")) 
#####################################################################################################################
#########
#########                           DATA COM + PHYLO + TRAITS
#########
####################################################################################################################


###################################################### Macrophytes #################################################
df_trait_macrophyte <- read_csv("/home/theo/Bureau/Data/Données finales/traits/traits_macrophytes_final.csv") %>%
  mutate(nom_taxo = ifelse(str_detect(nom_taxo,"Isoetes"),"Isoetes",nom_taxo))

df_trait_phylo_macrophyte <- tnrs_match_names(df_trait_macrophyte$nom_taxo)

arbre_macrophytes <- tol_induced_subtree(ott_ids = df_trait_phylo_macrophyte$ott_id,label_format = "name")
plot(arbre_macrophytes,no.margin=TRUE)

arbre_macrophytes_temps <- compute.brlen(arbre_macrophytes)

phylo_matcor_macrophytes <- vcv(arbre_macrophytes_temps,model="Brownian",corr=TRUE) %>%
  as_tibble() %>% 
  mutate(nom_taxo = colnames(.),
         nom_taxo=str_replace_all(nom_taxo,"[:blank:]{1,}","_") %>% str_extract(".{1,}(?=_\\()|.{1,}(?!=_\\()"))

nom <- colnames(phylo_matcor_macrophytes) %>% str_extract(".{1,}(?=_\\()|.{1,}(?!=_\\()")
nom_filtre <- df_trait_macrophyte %>% mutate(nom_taxo = str_replace_all(nom_taxo," ","_")) %>% .$nom_taxo
macrophytes_traits_phylo <- phylo_matcor_macrophytes %>%
  setnames(colnames(.),nom) %>% 
  dplyr::filter(nom_taxo %in% nom_filtre)

vis_dat(df_trait_macrophyte)


###################################################### Poissons #################################################
df_trait_poisson <- read_ods("/home/theo/Bureau/Data/Données finales/traits/traits_poissons_homo.ods") %>%
  as.data.frame() %>% 
  mutate_if(is.character,as.factor) %>% 
  column_to_rownames() %>% 
  missForest() %>% .$ximp %>% 
  tibble::rownames_to_column(var="nom_taxo")

df_phylo_poisson <- tnrs_match_names(df_trait_poisson$nom_taxo)

arbre_phylo_poisson <- tol_induced_subtree(ott_ids = df_phylo_poisson$ott_id,label_format = "name")
plot(arbre_phylo_poisson)

arbre_poissons_temps <- compute.brlen(arbre_phylo_poisson)

phylo_matcor_poissons <- vcv(arbre_poissons_temps,model="Brownian",corr=TRUE) %>%
  as_tibble() %>% 
  mutate(nom_taxo = colnames(.),
         nom_taxo=str_replace_all(nom_taxo,"[:blank:]{1,}","_") %>% str_extract(".{1,}(?=_\\()|.{1,}(?!=_\\()"))

nom <- colnames(phylo_matcor_poissons) %>% str_extract(".{1,}(?=_\\()|.{1,}(?!=_\\()")
nom_filtre <- df_trait_poisson %>% mutate(nom_taxo = str_replace_all(nom_taxo," ","_")) %>% .$nom_taxo
poissons_traits_phylo <- phylo_matcor_poissons %>%
  setnames(colnames(.),nom) %>% 
  dplyr::filter(nom_taxo %in% nom_filtre)

corrplot(poissons_traits_phylo %>% dplyr::select(-nom_taxo) %>% as.matrix)
write.csv(poissons_traits_phylo,"/home/theo/Bureau/Data/Données finales/phylogénie/phylo_poissons.csv",row.names = FALSE)


df_trait_poisson %>% vis_dat
hill_smith <- df_trait_poisson %>% dplyr::filter(!nom_taxo %in% c("Liza ramada","Anguilla anguilla","Acipenser ruthenus","Gambusia affinis")) %>% column_to_rownames() %>% dudi.hillsmith()
vis_dat(traits_poissons)

fun_pca(hill_smith,tab=TRUE)
scatter(hill_smith,axes=c(3,4))
s.label(hill_smith$l1,boxes=FALSE,xax=1,yax=3)
