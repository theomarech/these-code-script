################################################################################################################
##############                                                                                     #############
##############                               Traits - macroinvertébrés                             #############
##############                                                                                     #############
################################################################################################################

source("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/scriptR/librairies.R")
bdd_plando(TRUE)
####### traits finaux:
trait_macroinv <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/Données traits/Données/traits_macroinvertebres.csv")
############# Ouverture et description des données taxons
df_macroinv <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/Données finales/communautés/df_macroinvertebres.csv")
df_macroinv %>% group_by(nom_taxo) %>% dplyr::summarise(n=n()) %>% arrange(desc(n))
df_macroinv %>% 
  dplyr::select(nom_taxo,niveau_taxo) %>% 
  distinct() %>%
  group_by(niveau_taxo) %>% 
  dplyr::summarise(effectif = n()) %>% 
  ggplot(aes(x=reorder(niveau_taxo,desc(effectif)), y = effectif))+
  geom_bar(stat="identity")+
  geom_text(aes(x= niveau_taxo,y = effectif +15,label=effectif),col="gray40")+
  geom_text( aes(x = "Tribu", y =150, label = paste("n =", sum(effectif),"taxons")), col="orange", family = "Times New Roman",size = rel(5))+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, h = 1, v = 1.3),
        axis.title.x = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(family = "Times New Roman"))+
  geom_segment(aes(x = niveau_taxo, y = effectif+2, xend = niveau_taxo, yend = effectif +8),col = "gray40",alpha = 0.5)

vec_taxon_unique_5 <-  fun_table_taxo(df_macroinv)[[1]]$nom_taxo

df_macroinv <- df_macroinv %>% dplyr::select(nom_taxo) %>% distinct()
############## TRAITS TACHETS ###############
nom_var <- 
  c(rep("feedingtype",10),
  rep("food",9),
  rep("feedhab",8),
  rep("locosub",8),
  rep("resp",5),
  rep("maxsize",7),
  rep("resform",5),
  rep("disp",4),
  rep("aqustage",4),
  rep("lifecycledur",2),
  rep("numcycleyear",3),
  rep("repro",8)
  )
nom_var %>% unique()
####### Esp et genre présent
esp_tachet <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/Données traits/Synthèse traits/macroinvertebres/traits_tachets_freshwater.csv") %>% 
  slice(which(rowSums(.)!=0))
esp_tachet <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/Données traits/Synthèse traits/macroinvertebres/traits_tachets_freshwater.csv")%>% 
  dplyr::slice(which(rowSums(.!="nom_taxo",na.rm = TRUE)!=1)) %>% 
  mutate_if(is.numeric,~ifelse(is.na(.),0,.)) %>% 
  mutate(nom_taxo = str_replace_all(nom_taxo,"[:blank:]sp.|Lv.|Ad.|Gen.","") %>% str_trim()) %>% 
  setnames(colnames(.),c("nom_taxo",paste0(nom_var,sep="_",colnames(.)[-1])) %>% str_replace_all(" ","_")) 

esp_tachet$nom_taxo %>% sort() %>% unique()
  
traits_macroinvertebres <- inner_join(df_macroinv %>% dplyr::select(nom_taxo) %>% distinct() ,esp_tachet, by = "nom_taxo") 
reste_traits <- anti_join(df_macroinv %>% dplyr::select(nom_taxo) %>% distinct() ,esp_tachet, by = "nom_taxo")
write.csv(traits_macroinvertebres,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/Données traits/Données/traits_macroinvertebres.csv",row.names=FALSE)

esp_tachet %>% View()
df <- esp_tachet
####### Famille et genre extrapolé
fun_taxo <- function(df) {
    df <- df %>% mutate(famille = NA_character_)
    for (i in 1:nrow(df)) {
      if (str_detect(df$nom_taxo[i], "[:upper:]{2,}")) {
        df[i, "famille"] <- str_extract(df$nom_taxo[i], "[:upper:]{1,}")
      } else{
        df[i, "famille"] <- df[i - 1, "famille"]
      }
    }
    df <- df %>% 
      mutate(genre = NA_character_) %>% 
      dplyr::filter(str_detect(nom_taxo,"[:upper:]{3,}",negate = TRUE)) %>% 
      mutate(genre = str_extract(nom_taxo,"[:alpha:]{1,}"))

  return(df)
}
fun_NA <- function(df){
  df %>% 
    mutate(id = 1:nrow(.)) %>% 
    gather("traits", "valeurs", -c(nom_taxo, famille,genre,id))%>%
    mutate(mod = str_extract(traits,"(?<=_).{1,}"),
           traits = str_extract(traits,"[:alpha:]{1,}(?=_)")) %>%
    group_by(nom_taxo,famille,genre,traits,id) %>% 
    nest() %>% 
    mutate(isna = map_chr(data,~ifelse(sum(.$valeurs)==0,"oui","non")))%>% 
    unnest(data) %>% 
    mutate(valeurs=ifelse(isna == "oui",NA,valeurs)) %>% 
    ungroup() %>% 
    mutate(traits = str_c(traits,mod,sep="_")) %>% 
    dplyr::select(-c(mod,isna))%>%
    pivot_wider(id_cols = c(1:4),names_from=traits,values_from = valeurs) %>% 
    dplyr::select(-id) %>% 
    return()
}

fun_dup <- function(df) {
  if (nrow(df) > 1) {
    df <- df %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column() %>%
      as_tibble() %>%
      group_by(rowname) %>% dplyr::summarise(val = mean(c(V1,V2),na.rm=TRUE))%>%
      ungroup() %>% 
      dplyr::select(traits=rowname,val) %>% 
      spread(traits, val)
  }else{
    df
  }
  return(df)
}

vec_nom_taxo <- df_macroinv$nom_taxo %>% unique()

esp_tachet <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/Données traits/Synthèse traits/macroinvertebres/traits_tachets_freshwater.csv")[-1,]%>% 
  mutate_if(is.numeric,~ifelse(is.na(.),0,.)) %>% 
  mutate(nom_taxo = str_replace_all(nom_taxo,"[:blank:]sp.|Lv.|Ad.|Gen.","") %>% str_trim()) %>% 
  setnames(colnames(.),c("nom_taxo",paste0(nom_var,sep="_",colnames(.)[-1])) %>% str_replace_all(" ","_")) %>% 
  fun_taxo(.) %>% 
  .[which(select_if(.,is.numeric) %>% rowSums()!=0),] %>% 
  fun_NA(.) %>% 
  group_by(famille,genre,nom_taxo) %>% 
  nest() %>%
  mutate(n = map(data, ~fun_dup(.))) %>% 
  dplyr::select(-data) %>% 
  unnest(n) %>% 
  ungroup() %>% 
  mutate(famille = str_to_title(famille)) %>% dplyr::select(matches("nom_taxo|famille|genre|feed|food|size|locosub"))


traits_macroinvertebres_tax <- inner_join(df_macroinv %>% dplyr::select(nom_taxo) %>% distinct() ,esp_tachet, by = "nom_taxo") 
reste_traits_tax <- anti_join(df_macroinv %>% dplyr::select(nom_taxo) %>% distinct() ,esp_tachet, by = "nom_taxo")

gen_tachet <- esp_tachet %>% group_by(genre) %>% summarise_if(is.numeric,mean,na.rm=TRUE)
traits_macroinvertebres_genre <- inner_join(reste_traits_tax,gen_tachet,by = c("nom_taxo"="genre"))
traits_tax_gen <- bind_rows(traits_macroinvertebres_genre,traits_macroinvertebres_tax)
reste_tax_gen <- anti_join(df_macroinv %>% dplyr::select(nom_taxo) %>% distinct(),traits_tax_gen,by="nom_taxo")


fam_tachet <- esp_tachet %>% group_by(famille) %>% summarise_if(is.numeric,mean,na.rm=TRUE)
traits_macroinvertebres_famille <- inner_join(reste_tax_gen,fam_tachet,by = c("nom_taxo"="famille"))
traits_tax_gen_fam <- bind_rows(traits_macroinvertebres_genre,traits_macroinvertebres_tax,traits_macroinvertebres_famille)
reste_tax_gen_fam <- anti_join(df_macroinv %>% dplyr::select(nom_taxo) %>% distinct(),traits_tax_gen_fam,by="nom_taxo")


table_traits_finale <- traits_tax_gen_fam %>% map_df(~ifelse(is.na(.),0,.)) %>% dplyr::select(-c(famille,genre))
write.csv(table_traits_finale,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/Données traits/Données/traits_macroinvertebres_food.csv",row.names=FALSE)





############ données traits issues de la publication de murria et al 2020 (à retrouver dans zotero dans traits fonctionnelles > macroinv..)
## ce sont les tratis de tachets qui sont compilé pour les données européennes dans l'objectif d'avoir des traits sur pas mal de taxons
df_trait_macroinv_fresh <- esp_tachet %>%
  mutate(nom_taxo2 = ifelse(str_detect(nom_taxo,"/"),str_extract(nom_taxo,"(?<=/[:alpha:]{1,}"),NA),
         nom_taxo2 = i)
df_trait_macroinv_murria <- read.csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/Données traits/Synthèse traits/macroinvertebres/traits_tachets_murria2020.csv",dec=",") 
df_trait_macroinv_fresh %>% arrange(nom_taxo) %>% View()
### niveau taxo = famille
# pour les taxons séparé par un "/" on créée une seconde colonne nom_taxo2, qui contiendra le contenue après le slash (je suppose de la synonymie)
# pour les taxons chr(chr) concerne uniquement le genre rhyacophila => je récupère les données contenues entre parenthèses
#
df_trait_macroinv_fresh <- esp_tachet %>%
  mutate(nom_taxo2 = case_when(str_detect(nom_taxo,"/") ~str_extract(nom_taxo,"(?<=/)[:alpha:]{1,}"),
                               str_detect(nom_taxo,"\\(") ~str_extract(nom_taxo,"(?<=\\()[:alpha:]{1,}"),
                               TRUE ~ NA_character_))
df_trait_macroinv_fresh1.1 <- df_trait_macroinv_fresh %>%
  dplyr::select(-nom_taxo) %>%
  na.omit() %>%
  setnames("nom_taxo2","nom_taxo") %>%
  bind_rows(df_trait_macroinv_fresh) %>% 
  dplyr::select(-nom_taxo2)
df_trait_macroinv_fresh1.1$nom_taxo

###### approche sur le total ######
jointure_tot <- inner_join(df_macroinv %>% dplyr::select(nom_taxo) %>% distinct(),df_trait_macrinv_fresh1.1 %>% distinct(),by="nom_taxo")
reste_jointure_tot_tax <- df_macroinv %>% dplyr::select(nom_taxo) %>% distinct() %>% dplyr::filter(!nom_taxo%in%jointure_tot$nom_taxo)

df_trait_macroinv_murria %>% dplyr::filter(Genus%in%reste_tax_gen_fam$nom_taxo) %>% as_tibble()













##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################
##############################################################################################################################################


df_macroinv %>% pull(nom_taxo) %>% unique()

########### Ouverture des donnée brutes : ###########
df_macroinv$zone_inv
#### Données sur les taxons
df_macroinv <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/Données finales/communautés/df_macroinvertebres.csv") %>% 
  mutate(nom_taxo = case_when(nom_taxo=="Tubificinae avec soies capillaires"~"Tubificidae",
                              nom_taxo=="Tubificinae sans soies capillaires"~"Tubificidae",
                              nom_taxo=="Achaeta"~"Enchytraeidae",
                              nom_taxo=="Acricotopus"~"Chironomus",
                              nom_taxo=="Apsectrotanypus"~"Tanypodinae",
                              nom_taxo=="Asellus (Asellus) aquaticus"~"Asellus aquaticus",
                              nom_taxo %in% c("Aulodrilus","Aulodrilus japonicus",
                                              "Aulodrilus limnobius","Aulodrilus pigueti",
                                              "Aulodrilus pluriseta")~"Tubificidae",
                              nom_taxo=="Batracobdella paludosa"~"Glossiphonia paludosa",# esp -> esp
                              nom_taxo=="Bichaeta sanguinea"~"Lumbriculidae",# esp -> F
                              nom_taxo=="Boreobdella"~"Glossiphonia", # G -> G 
                              nom_taxo=="Brillia"~"Dicrotendipes",# G -> G ; (?)
                              nom_taxo=="Bothrioneurum"~"Naididae",#G -> F
                              nom_taxo=="Bryophaenocladius"~"Orthocladiinae",#G -> F
                              nom_taxo=="Caenis horaria"~"Caenis",#esp -> G
                              nom_taxo=="Chaetocladius"~"Orthocladiinae",#G -> F
                              nom_taxo=="Chironominae"~"Chironomidae",#SF -> F
                              nom_taxo=="Chaetogaster diaphanus"~"Chaetogaster",# esp -> G; (?) Nais sp. colle aussi 
                              nom_taxo=="Chaoborus flavicans"~"Chaoborus",#esp -> G
                              nom_taxo=="Chironomus plumosus"~"Chironomus",#esp -> G
                              nom_taxo=="Cladopelma"~"Chironomini",#G -> F
                              nom_taxo=="Cladotanytarsus"~"Tanytarsini",#G -> F
                              nom_taxo=="Cladotanytarsus mancus"~"Tanytarsini",# G -> F
                              nom_taxo=="Clinotanypus"~"Tanypodinae",
                              nom_taxo=="Cognettia"~"Enchytraeidae",
                              nom_taxo=="Cognettia glandulosa"~"Enchytraeidae",
                              nom_taxo=="Conchapelopia"~"Tanypodinae",
                              nom_taxo=="Constempellina"~"Tanytarsus",
                              nom_taxo=="Corynoneura"~"Orthocladiinae",
                              nom_taxo=="Cryptotendipes"~"Chironomini",
                              nom_taxo=="Cyphomella"~"Chironomini",
                              nom_taxo=="Demicryptochironomus"~"Chironomini",
                              nom_taxo %in% c("Cyrnus flavidus","Cyrnus trimaculatus")~"Cyrnus",
                              nom_taxo %in% c("Dero digitata","Dero Furcata","Dero obtusa")~"Dero",
                              nom_taxo=="Diamesa"~"Chironomus",
                              nom_taxo=="Endochironomus"~"Chironomus",
                              nom_taxo=="Epoicocladius"~"Orthocladiinae",
                              nom_taxo=="Einfeldia"~"Chironomus",
                              nom_taxo=="Eukiefferiella"~"Orthocladiinae",
                              nom_taxo=="Fridericia"~"Enchytraeidae",
                              nom_taxo=="Gammaroidea"~"Gammaridea",
                              nom_taxo=="Glyptotendipes"~"Chironomus",
                              nom_taxo=="Guttipelopia"~"Tanypodinae",
                              nom_taxo=="Haber"~"Naididae",
                              nom_taxo=="Harnischia"~"Chironomini",
                              nom_taxo=="Heterotanytarsus"~"Orthocladiinae",
                              nom_taxo=="Heterotrissocladius"~"Orthocladiinae",
                              nom_taxo=="Hydrozoa"~"Chironomus",
                              nom_taxo=="Ilyodrilus templetoni"~"Tubificidae",
                              nom_taxo=="Krenopelopia"~"Chironomus",
                              nom_taxo=="Labrundinia"~"Tanypodinae",# à partir de là vérifier chironomidae vs sous famile
                              nom_taxo=="Larsia"~"Tanypodinae",
                              nom_taxo=="Lauterborniella"~"Chironomini",
                              nom_taxo=="Limnephilinae"~"Limnephilidae",
                              nom_taxo=="Limnephilus"~"Limnephilini",
                              nom_taxo%in%c("Limnodrilus","Limnodrilus","Limnodrilus claparedianus",  
                                            "Limnodrilus hoffmeisteri","Limnodrilus profundicola",
                                            "Limnodrilus udekemianus")~"Tubificidae",
                              nom_taxo=="Limnophyes"~"Orthocladiinae",
                              nom_taxo=="Lumbriculidae immatures"~"Lumbriculidae",
                              nom_taxo=="Lumbriculus variegatus"~"Lumbriculidae",
                              nom_taxo=="Macropelopia"~"Tanypodinae",
                              nom_taxo%in%c("Marionina","Marionina riparia")~"Enchytraeidae",
                              nom_taxo%in%c("Menetus","Menetus dilatatus")~"Planorbis",
                              nom_taxo%in%c("Microchironomus tener","Microchironomus")~"Chironomini",
                              nom_taxo=="Micropsectra"~"Tanytarsini",
                              nom_taxo=="Microtendipes"~"Chironomus",
                              nom_taxo=="Molannodes tinctus"~"Molannodes",
                              nom_taxo %in% c("Nais alpina","Nais barbata","Nais bretscheri","Nais christinae",             
                                              "Nais communis","Nais elinguis","Nais pardalis","Nais pseudobtusa",          
                                              "Nais simplex","Nais variabilis")~"Nais",
                              nom_taxo=="Nanocladius"~"Orthocladiinae",
                              nom_taxo=="Natarsia"~"Tanypodinae",
                              nom_taxo=="Nilothauma"~"Chironomini",
                              nom_taxo=="Niphargidae"~"Gammaridae",
                              nom_taxo=="Orthocladius"~"Orthocladiinae",
                              nom_taxo=="Ostracoda"~"Crustacea",
                              nom_taxo=="Pagastiella"~"Chironomini",
                              nom_taxo=="Palpomyia"~"Ceratopogonidae",
                              nom_taxo=="Parachironomus"~"Chironomini",
                              nom_taxo=="Paracladius"~"Chironomidae",
                              nom_taxo=="Paracladopelma"~"Chironomini",
                              nom_taxo=="Paracricotopus"~"Chironomidae",
                              nom_taxo=="Parakiefferiella"~"Orthocladiinae",
                              nom_taxo=="Paralauterborniella"~"Chironomini",
                              nom_taxo=="Paramerina"~"Tanypodinae",
                              nom_taxo=="Parametriocnemus"~"Orthocladiinae",
                              nom_taxo=="Paranais frici"~"Paranais",
                              nom_taxo=="Paratendipes"~"Chironomini",
                              nom_taxo=="Paratrissocladius"~"Orthocladiinae",
                              nom_taxo=="Parorthocladius"~"Orthocladiinae",
                              nom_taxo=="Pentaneura"~"Tanypodinae",
                              nom_taxo=="Phaenopsectra"~"Chironomini",
                              nom_taxo=="Physa lato-sensu"~"Physa",
                              nom_taxo=="Polycentropus flavomaculatus"~"Polycentropus",
                              nom_taxo=="Polypedilum nubeculosum"~"Polypedilum",
                              nom_taxo%in%c("Potamothrix bavaricus","Potamothrix bedoti",       
                                            "Potamothrix hammoniensis","Potamothrix heuscheri",
                                            "Potamothrix vejdovskyi")~"Potamothrix",
                              nom_taxo=="Potthastia"~"Diamesinae",
                              nom_taxo%in%c("Pristina aequiseta","Pristina aequiseta fo foreli",
                                            "Pristina jenkinae","Pristina longiseta",         
                                            "Pristina osborni")~"Pristina",
                              nom_taxo=="Prodiamesa"~"Prodiamesinae",
                              nom_taxo=="Prostoma graecense"~"Prostoma",
                              nom_taxo=="Psammoryctes barbatus"~"Tubificidae",
                              nom_taxo=="Psammoryctides"~"Tubificidae",
                              nom_taxo=="Psectrocladius sordidellus"~"Psectrocladius",
                              nom_taxo=="Pseudochironomus"~"Chironomidae",
                              nom_taxo=="Pseudodiamesa"~"Diamesinae",
                              nom_taxo=="Pseudosmittia"~"Orthocladiinae",
                              nom_taxo=="Quistadrilus multisetosus"~"Tubificidae",
                              nom_taxo=="Rheocricotopus"~"Orthocladiinae",
                              nom_taxo=="Rheotanytarsus"~"Tanytarsini",
                              nom_taxo=="Rhyacodrilus"~"Naididae",
                              nom_taxo=="Sergentia"~"Chironomini ",
                              nom_taxo=="Smittia"~"Orthocladiinae",
                              nom_taxo=="Spirosperma"~"Tubificidae",
                              nom_taxo=="Stempellina"~"Tanytarsini",
                              nom_taxo=="Stempellinella"~"Tanytarsini",
                              nom_taxo=="Stenochironomus"~"Chironomini",
                              nom_taxo=="Stictochironomus"~"Chironomini",
                              nom_taxo=="Stylodrilus lemani"~"Stylodrilus",
                              nom_taxo=="Synorthocladius"~"Orthocladiinae",
                              nom_taxo=="Tanypus"~"Chironomidae",
                              nom_taxo=="Thienemannimyia"~"Tanypodinae",
                              nom_taxo=="Tribelos"~"Chironomini",
                              nom_taxo%in%c("Tubifex","Tubifex ignotus","Tubifex tubifex")~"Tubificidae",
                              nom_taxo%in%c("Vejdovskyella comata","Vejdovskyella intermedia")~"Naididae",
                              nom_taxo=="Zavrelia"~"Tanytarsini",
                              nom_taxo=="Zavrelimyia"~"Tanypodinae",
                              TRUE ~ nom_taxo)) %>%
  dplyr::filter(!nom_taxo %in% c("Bryozoa","Cladocera","Copepoda","Crambidae","Curculionidae","Daphnia","Glossoscolecidae","Nemathelmintha","Nematoda"))
  # # dplyr::filter(!nom_taxo %in%c("Lumbricidae","Limoniidae","Enchytraeidae","Crustacea")) %>% 
  # group_by(date,nom_taxo,code_lac) %>% 
  # dplyr::summarise(abondance=sum(abondance)) %>% 
  # ungroup() %>% 
  # group_by(nom_taxo,code_lac) %>% 
  # dplyr::summarise(abondance=mean(abondance,na.rm=TRUE)) %>% 
  # ungroup()  %>%  dplyr::select(nom_taxo) %>% unique() 
df_macroinv_zp <- df_macroinv %>% dplyr::filter(zone_inv == "ZP")
df_macroinv_zl <- df_macroinv %>% dplyr::filter(zone_inv=="ZL")
write.csv(df_macroinv_zl, file="C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES TRAVAIL/communautés/df_macroinvertebres_zl.csv", row.names = FALSE)
write.csv(df_macroinv_zp, file="C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES TRAVAIL/communautés/df_macroinvertebres_zp.csv", row.names = FALSE)


df_macroinv <- df_macroinv_zl
  #### Traits issues de freshwater.info : 
# les traits gardés sont ceux similaires à la base de données récupérées via murria et al 2020 : les codes ont été homogénéiser pour les modalités
# afin de correspondre à ceux de murria
# les taxons sont identifiées à partir de la sous espèces
table_trait_macroinv_fw <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/Données traits/Synthèse traits/macroinvertebres/traits_tachets_freshwater_code_murria.csv")

#### Traits issues de murria et al 2020: 
# les taxons sont identifiés : au genre/famille/ordre
table_trait_macroinv_mu <- read.csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/Données traits/Synthèse traits/macroinvertebres/traits_tachets_murria2020.csv",dec=",")

table_trait_macroinv_fw$nom_taxo %>% unique()


######## TRAITEMENT ##########
###### nom des variables ordonnées pour fw

nom_var <- 
  c(rep("feedinghab",10),
    rep("locosub",8),
    rep("resp",4),
    rep("maxsize",7),
    rep("resform",5),
    rep("disp",4),
    rep("aqustage",4),
    rep("lifecycledur",2),
    rep("numcycleyear",3),
    rep("repro",8)
  )


df_trait_macroinv_fw1.2<- table_trait_macroinv_fw %>%
  mutate(nom_taxo=1:nrow(.)) %>% 
  setnames(colnames(.)[-1],paste0(nom_var,'_',colnames(.)[-1])) %>%
  gather(modalites,val,-nom_taxo) %>%
  mutate(trait=str_extract(modalites,"[:alpha:]{1,}(?=_)")) %>% 
  group_by(trait,nom_taxo) %>% 
  mutate(val=val/sum(val,na.rm=TRUE),
         val=ifelse(!is.na(sum(val,na.rm=TRUE))&is.na(val),0,val)) %>%
  nest() %>% 
  ungroup() %>% 
  mutate(isna = map(data,~ifelse(sum(.$val)==0,TRUE,FALSE))) %>% 
  unnest() %>% 
  mutate(val = ifelse(isna,NA,val)) %>% 
  dplyr::select(-c(isna,trait)) %>% 
  spread(modalites,val) %>% 
  inner_join(table_trait_macroinv_fw %>% 
               mutate(.id=1:nrow(.)) %>% 
               dplyr::select(.id,nom_taxo),by=c("nom_taxo"=".id")) %>% 
  mutate(nom_taxo = nom_taxo.y) %>% 
  dplyr::select(-c(nom_taxo.y)) %>%
  setnames(colnames(.)[-1],str_extract(colnames(.)[-1],'(?<=_).{1,}'))

df_trait_macroinv_fw1.3  <- df_trait_macroinv_fw1.2 %>% mutate(famille = NA_character_) %>% dplyr::slice(-1)
for (i in 1:nrow(df_trait_macroinv_fw1.3)) {
  if (str_detect(df_trait_macroinv_fw1.3$nom_taxo[i], "[:upper:]{2,}")) {
    df_trait_macroinv_fw1.3[i, "famille"] <- str_extract(df_trait_macroinv_fw1.3$nom_taxo[i], "[:upper:]{1,}")
  } else{
    df_trait_macroinv_fw1.3[i, "famille"] <- df_trait_macroinv_fw1.3[i - 1, "famille"]
  }
}
df_trait_macroinv_fw1.4 <- df_trait_macroinv_fw1.3 %>% 
  .[which(select_if(.,is.numeric) %>% rowSums()!=0),] %>% 
  mutate(nom_taxo = case_when(str_detect(nom_taxo,"sp.")~str_extract(nom_taxo,"[:alpha:]{1,}"),
                              TRUE~str_extract(nom_taxo,"[:alpha:]{1,}[:blank:][:alpha:]{1,}"))) %>% distinct()





df_macroinv <- df_macroinv %>% dplyr::select(nom_taxo) %>% distinct()
############### JOINTURE #################
jointure_tot_fw <- inner_join(df_macroinv,df_trait_macroinv_fw1.4,"nom_taxo")
reste_tot_fw <- anti_join(df_macroinv,df_trait_macroinv_fw1.4,"nom_taxo")# correspond aux taxons qui n'ont pas matché avec els traits

##### jointure avec les genre dans muria
jointure_genre_mw <- inner_join(reste_tot_fw,table_trait_macroinv_mu,by=c("nom_taxo"="Genus"))
reste_genre_mw <- anti_join(reste_tot_fw,table_trait_macroinv_mu,by=c("nom_taxo"="Genus"))

#### jointure avec les genre extrapolées
df_trait_macroinv_fw1.4_genre <- df_trait_macroinv_fw1.4 %>% 
  mutate(nom_taxo=str_extract(nom_taxo,"[:alpha:]{1,}")) %>% 
  group_by(nom_taxo) %>%
  dplyr::summarise_if(is.numeric,mean)
jointure_genre_fw <- inner_join(reste_genre_mw,df_trait_macroinv_fw1.4_genre,"nom_taxo")
reste_genre_fw <- anti_join(reste_genre_mw,df_trait_macroinv_fw1.4_genre,"nom_taxo")

#### jointure avec les familles dans muria
df_trait_macroinv_mu_fam <- table_trait_macroinv_mu %>% group_by(Family) %>% dplyr::summarise_if(is.numeric,mean) 
jointure_fam_mu <- inner_join(reste_genre_fw,df_trait_macroinv_mu_fam,by=c("nom_taxo"="Family"))
reste_fam_mu <- anti_join(reste_genre_fw,df_trait_macroinv_mu_fam,by=c("nom_taxo"="Family"))


#### jointure avec les genre extrapolées sur les données taxonomique


reste_genre <- reste_fam_mu %>% mutate(nom_taxo=str_extract(nom_taxo,"[:alpha:]{1,}")) %>% distinct()


# avec murria
reste_mu_gen <- anti_join(table_trait_macroinv_mu,reste_tot_fw,by=c("Genus"="nom_taxo"))
jointure_gen_tax_mu <-inner_join(reste_genre, table_trait_macroinv_mu,by=c("nom_taxo"="Genus"))
reste_gen_tax_mu <- anti_join(reste_genre,reste_mu_gen,by=c("nom_taxo"="Genus"))
reste_gen_tax_mu$nom_taxo %>% sort()
# avec fw
jointure_gen_tax_fw <- inner_join(reste_gen_tax_mu,df_trait_macroinv_fw1.4_genre,"nom_taxo")



df_macroinv$nom_taxo %>% unique()



jointure_finale <- bind_rows(jointure_tot_fw,
                             jointure_genre_mw,
                             jointure_genre_fw,
                             jointure_fam_mu,
                             jointure_gen_tax_mu,
                             jointure_gen_tax_fw) %>% 
  group_by(nom_taxo) %>% 
  dplyr::summarise_if(is.numeric,mean,na.rm=TRUE) %>% 
  mutate(nom_taxo = str_replace_all(nom_taxo," ","_"))

jointure_finale$nom_taxo %>% unique()
jointure_finale %>% dplyr::select_if(is.numeric) %>% PCA
reste_totale_sans_genre <- anti_join(df_macroinv,jointure_finale) 
reste_totale_avec_genre <- anti_join(reste_totale_sans_genre %>% mutate(nom_taxo = str_extract(nom_taxo,"[:alpha:]{1,}")),jointure_finale)





write.csv(jointure_finale,
          "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macroinvertebres_inv_zp.csv",
          row.names = FALSE)




df_finale_tax <- df_macroinv_zl %>% 
  dplyr::select(nom_taxo,code_lac,abondance) %>%
  group_by(nom_taxo,code_lac) %>% 
  dplyr::summarise(abondance=mean(abondance,na.omit=T)) %>% 
  mutate(nom_taxo = str_replace_all(nom_taxo," ","_")) %>% 
  dplyr::filter(nom_taxo %in% all_of(jointure_finale$nom_taxo)) %>% 
  spread(nom_taxo,abondance,fill=0) %>% na.omit()

write.csv(table_contingence_iml_tax,
          "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES DIVERSITES/macroinvertebres_iml.csv",
          row.names = FALSE)

write.csv(df_finale_tax,
          "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres_inv_zl.csv",
          row.names = FALSE)


##################################################" MACROINV IML ######################################################

data_tax_iml <- read_ods("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DONNEES TRAITS INV/Data_Base_Bio75_04-02-20.ods") %>% 
  mutate(nom_taxo = GENRE_TAXON,
         nom_taxo = case_when(
           nom_taxo == "Limnephilinae" ~ "Limnephilidae",
           nom_taxo == "Limnephilus" ~ "Limnephilini",
           nom_taxo == "Corixinae" ~ "Corixidae",
           nom_taxo == "Curculionidae" ~ "Coleoptera",
           nom_taxo == "Agabinae" ~ "Dytiscidae",
           nom_taxo == "Hydroporinae" ~ "Dytiscidae",
           nom_taxo == "Laccophilinae" ~ "Dytiscidae",
           nom_taxo == "Ochtebius" ~ "Ochthebius",
           nom_taxo == "Acidocerinae" ~ "Hydrophilidae",
           nom_taxo == "Enochrinae" ~ "Hydrophilidae",
           nom_taxo == "Hydrachara" ~ "Hydrochara",
           nom_taxo == "Hydrophilinae" ~ "Hydrophilidae",
           nom_taxo == "Sphaeridiinae" ~ "Hydrophilidae",
           nom_taxo == "Anthomyidae" ~ "Diptera",
           nom_taxo == "Acricotopus" ~ "Orthocladiinae",
           nom_taxo == "Arctopelopia" ~ "Tanypodinae",
           nom_taxo == "Brillia" ~ "Orthocladiinae",
           nom_taxo == "Bryophaenocladius" ~ "Orthocladiinae",
           nom_taxo == "Camptocladius" ~ "Orthocladiinae",
           nom_taxo == "Chaetocladius" ~ "Orthocladiinae",
           nom_taxo == "Cladopelma" ~ "Chironomini",
           nom_taxo == "Colymbetinae" ~ "Dytiscidae",
           nom_taxo == "Dystiscinae" ~ "Dytiscidae",
           nom_taxo == "Cladotanytarsus" ~ "Tanytarsini",
           nom_taxo == "Clinotanypus" ~ "Tanypodinae",
           nom_taxo == "Constempellina" ~ "Tanytarsini",
           nom_taxo == "Corynoneura" ~ "Orthocladiinae",
           nom_taxo == "Cricotopus/Orthocladius" ~ "Cricotopus",
           nom_taxo == "Cryptotendipes" ~ "Chironomini",
           nom_taxo == "Demicryptochironomus" ~ "Chironomini",
           nom_taxo == "Diamesa" ~ "Diamesinae",
           nom_taxo == "Einfeldia" ~ "Chironomini",
           nom_taxo == "Endochironomus" ~ "Chironomini",
           nom_taxo == "Epoicocladius" ~ "Orthocladiinae",
           nom_taxo == "Eukiefferiella" ~ "Orthocladiinae",
           nom_taxo == "Euorthocladius" ~ "Orthocladiinae",
           nom_taxo == "Glyptotendipes" ~ "Chironomini",
           nom_taxo == "Guttipelopia" ~ "Tanypodinae",
           nom_taxo == "Harnischia" ~ "Chironomini",
           nom_taxo == "Heterotanytarsus" ~ "Orthocladiinae",
           nom_taxo == "Heterotrissocladius" ~ "Orthocladiinae",
           nom_taxo == "Hydrobaenus" ~ "Orthocladiinae",
           nom_taxo == "Krenopelopia" ~ "Tanypodinae",
           nom_taxo == "Labrundinia" ~ "Tanypodinae",
           nom_taxo == "Larsia" ~ "Tanypodinae",
           nom_taxo == "Lauterborniella" ~ "Chironomini",
           nom_taxo == "Limnophyes" ~ "Orthocladiinae",
           nom_taxo == "Macropelopia" ~ "Tanypodinae",
           nom_taxo == "Mesocricotopus" ~ "Orthocladiinae",
           nom_taxo == "Metriocnemus" ~ "Orthocladiinae",
           nom_taxo == "Microchironomus" ~ "Chironomini",
           nom_taxo == "Micropsectra" ~ "Tanytarsini",
           nom_taxo == "Microtendipes" ~ "Chironomini",
           nom_taxo == "Monodiamesa" ~ "Prodiamesinae",
           nom_taxo == "Monopelopia" ~ "Tanypodinae",
           nom_taxo == "Nanocladius" ~ "Orthocladiinae",
           nom_taxo == "Natarsia" ~ "Tanypodinae",
           nom_taxo == "Neozavrelia" ~ "Orthocladiinae",
           nom_taxo == "Nilotanypus" ~ "Tanypodinae",
           nom_taxo == "Pagastiella" ~ "Chironomini",
           nom_taxo == "Parachaetocladius" ~ "Orthocladiinae",
           nom_taxo == "Parachironomus" ~ "Chironomini",
           nom_taxo == "Paracladius" ~ "Orthocladiinae",
           nom_taxo == "Paracladopelma" ~ "Chironomini",
           nom_taxo == "Paracricotopus" ~ "Orthocladiinae",
           nom_taxo == "Parakiefferiella" ~ "Orthocladiinae",
           nom_taxo == "Paralauterborniella" ~ "Chironomini",
           nom_taxo == "Paramerina" ~ "Tanypodinae",
           nom_taxo == "Parametriocnemus" ~ "Orthocladiinae",
           nom_taxo == "Paratendipes" ~ "Chironomini",
           nom_taxo == "Paratrissocladius" ~ "Orthocladiinae",
           nom_taxo == "Pentaneurella" ~ "Tanypodinae",
           nom_taxo == "Phaenopsectra" ~ "Chironomini",
           nom_taxo == "Potthastia" ~ "Diamesinae",
           nom_taxo == "Prodiamesa" ~ "Chironomidae",
           nom_taxo == "Psectrotanypus" ~ "Tanypodinae",
           nom_taxo == "Pseudochironomus" ~ "Chironomidae",
           nom_taxo == "Pseudodiamesa" ~ "Diamesinae",
           nom_taxo == "Pseudorthocladius" ~ "Orthocladiinae",
           nom_taxo == "Rheocricotopus" ~ "Orthocladiinae",
           nom_taxo == "Sergentia" ~ "Chironomini",
           nom_taxo == "Smittia" ~ "Orthocladiinae",
           nom_taxo == "Stempellina" ~ "Tanytarsini",
           nom_taxo == "Stempellinella" ~ "Tanytarsini",
           nom_taxo == "Stenochironomus" ~ "Chironomini",
           nom_taxo == "Stictochironomus" ~ "Chironomini",
           nom_taxo == "Synorthocladius" ~ "Orthocladiinae",
           nom_taxo == "Tanypus" ~ "Tanypodinae",
           nom_taxo == "Telopelopia" ~ "Tanypodinae",
           nom_taxo == "Thienemannia" ~ "Orthocladiinae",
           nom_taxo == "Thienemanniella" ~ "Orthocladiinae",
           nom_taxo == "ThienemannimyiaGroupe" ~ "Tanypodinae",
           nom_taxo == "Thienemanniola" ~ "Tanytarsini",
           nom_taxo == "Virgatanytarsus" ~ "Tanytarsini",
           nom_taxo == "Xenochironomus" ~ "Chironomini",
           nom_taxo == "Xenopelopia" ~ "Tanypodinae",
           nom_taxo == "Zalutschia" ~ "Orthocladiinae",
           nom_taxo == "Zavrelia" ~ "Tanytarsini",
           nom_taxo == "Zavreliella" ~ "Tanytarsini",
           nom_taxo == "Zavrelimyia" ~ "Tanypodinae",
           nom_taxo == "Sciaridae" ~ "Diptera",
           nom_taxo == "Aeshnidae" ~ "Odonata",
           nom_taxo == "Crambidae" ~ "Pyralidae",
           nom_taxo == "Limnomysis" ~ "Crustacea",
           nom_taxo == "Mysidae" ~ "Crustacea",
           nom_taxo == "Mysis" ~ "Crustacea",
           nom_taxo == "Niphargidae" ~ "Gammaridae",
           nom_taxo == "Orchestia" ~ "Gammaridae",
           nom_taxo == "PLATHELMINTHES" ~ "Animalia",
           TRUE ~ nom_taxo))


##################### JOINTURE IML PAR nom_taxo = plus petit niveau possible ############################
esp_tachet
nom <- data_tax_iml %>% dplyr::select(nom_taxo) %>% distinct()

jointure_iml_brute <- df_trait_macroinv_fw1.4 %>% dplyr::filter(nom_taxo %in% nom$nom_taxo)
reste_iml_brute <- nom %>% dplyr::filter(!nom_taxo %in% jointure_iml_brute$nom_taxo)

jointure_iml_brute_mu <- inner_join(table_trait_macroinv_mu,reste_iml_brute, by=c("Genus"="nom_taxo"))
reste_iml_brute_mu <- anti_join(reste_iml_brute,jointure_iml_brute_mu,by=c("nom_taxo"="Genus"))


table_mu_family <- table_trait_macroinv_mu %>% dplyr::select(-c(1,2,4)) %>% group_by(Family) %>% 
  summarise_all(mean)

jointure_iml_famille <- inner_join(table_mu_family,reste_iml_brute_mu, by=c("Family"="nom_taxo"))
reste_iml_famille_mu <- anti_join(reste_iml_brute_mu,jointure_iml_famille,by=c("nom_taxo"="Family"))


jointure_iml_finale_tax <- bind_rows(jointure_iml_brute,
                                     jointure_iml_brute_mu %>% dplyr::select(-c(Group,Code,Family)) %>% setnames('Genus','nom_taxo'),
                                     jointure_iml_famille %>% setnames('Family','nom_taxo')) %>% 
  dplyr::select(-famille) %>% 
  group_by(nom_taxo) %>% 
  summarise_all(mean,na.rm=TRUE)

write.csv(jointure_iml_finale_tax,
          "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macroinvertebres_iml.csv",
          row.names = FALSE)



table_contingence_iml_tax <- data_tax_iml %>% 
  dplyr::select(-c(1:7)) %>% 
  dplyr::select(nom_taxo,1:ncol(.)-1) %>% 
  gather(code_lac,val,-nom_taxo) %>% 
  mutate(val = ifelse(is.na(val),0,val)) %>% 
  group_by(nom_taxo,code_lac) %>% 
  dplyr::summarise(val=mean(val,na.rm=TRUE)) %>% 
  ungroup() %>% 
  spread(nom_taxo,val,fill=0) 

write.csv(table_contingence_iml_tax,
          "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES DIVERSITES/macroinvertebres_iml.csv",
          row.names = FALSE)

df_tax_filtre <- table_contingence_iml_tax %>%  dplyr::select(code_lac,all_of(jointure_iml_finale_tax$nom_taxo))
write.csv(df_tax_filtre,
          "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres_iml.csv",
          row.names = FALSE)
##################### JOINTURE IML PAR FAMILLE ############################
nom <- data_tax_iml %>% dplyr::select(FAMILLE) %>% distinct()

jointure_iml_brute <- df_trait_macroinv_fw1.4 %>% dplyr::filter(nom_taxo %in% nom$FAMILLE)
reste_iml_brute <- nom %>% dplyr::filter(!FAMILLE %in% jointure_iml_brute$nom_taxo)


table_mu_family <- table_trait_macroinv_mu %>% dplyr::select(-c(1,2,4)) %>% group_by(Family) %>% 
  summarise_all(mean)

jointure_iml_famille <- inner_join(table_mu_family,reste_iml_brute, by=c("Family"="FAMILLE"))
reste_iml_famille_mu <- anti_join(reste_iml_brute,jointure_iml_famille,by=c("FAMILLE"="Family"))

jointure_iml_finale_fam <- bind_rows(jointure_iml_brute,
                                     jointure_iml_famille %>% setnames('Family','nom_taxo')) %>% 
  dplyr::select(-famille) %>% 
  group_by(nom_taxo) %>% 
  summarise_all(mean,na.rm=TRUE)

table_contingence_iml_fam <- data_tax_iml %>% 
  dplyr::select(-c(1:4,6:7,ncol(.))) %>% 
  gather(code_lac,val,-FAMILLE) %>% 
  mutate(val = ifelse(is.na(val),0,val)) %>% 
  group_by(FAMILLE,code_lac) %>% 
  dplyr::summarise(val=mean(val,na.rm=TRUE)) %>% 
  ungroup() %>% 
  spread(FAMILLE,val,fill=0) 
table_contingence_iml_fam %>% column_to_rownames() %>% CA()
