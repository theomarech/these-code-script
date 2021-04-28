setwd("/home/theo/Bureau/Bibliographie/Synthèse traits/poissons/maxime logez")

######## base d'origine
poisson_plando <- read_csv("/home/theo/Bureau/Data/Données finales/communautés/df_poissons.csv") %>% dplyr::select(nom_taxo) %>% distinct() 

traits_poissons_camille <- read_csv("traits_poissons_camille.csv") %>% 
  inner_join(poisson_plando, by=c("Espèce"="nom_taxo")) %>% 
  setnames(colnames(.),c("nom_taxo",paste0("CAM_",colnames(.)[-1])))

traits_poissons_eco <- read_csv("traits_poissons_eco.csv") %>% 
  inner_join(poisson_plando, by=c("Espèce"="nom_taxo")) %>% 
  setnames(colnames(.),c("nom_taxo",paste0("ECO_",colnames(.)[-1])))

traits_poissons_rhenato <- read_csv("traits_poissons_rhenato.csv") %>% 
  inner_join(poisson_plando, by=c("Espèce"="nom_taxo")) %>% 
  setnames(colnames(.),c("nom_taxo",paste0("RHE_",colnames(.)[-1])))




### traits issues de freshwater
nom_var <- c(
  rep("migration",4),
  rep("habitat",3),
  rep("rheophilie",3),
  rep("habitatalimentation",2),
  rep("habitatrepro",5),
  rep("salinite",5),
  rep("typealimenttion",6),
  rep("dureevie",3),
  rep("taillecorps",3),
  rep("formeindice",4),
  rep("nageindice",3),
  rep("maturitefemelle",5),
  rep("tempseclosion",2),
  rep("periodeincubation",3),
  rep("fecondite",3),
  rep("feconditerelative",3),
  rep("diametreoeufs",3),
  rep("taillelarves",3),
  rep("soinparentaux",3),
  rep("dureestadelarvaire",3)
)


traits_poissons_fresh <- read_csv("/home/theo/Bureau/Data/Données traits/Synthèse traits/poissons/freshwaterinfo/taxadbexport.csv") %>% 
  setnames(colnames(.),c("nom_taxo",paste0(nom_var,"_",colnames(.)[-1]))) %>% 
  gather("var","val",-nom_taxo)%>% 
  na.omit() %>%
  mutate(traits = str_extract(var, "[:alpha:]{1,}(?=_)"),
         modalites = str_extract(var, "(?<=_).{1,}")) %>% 
  dplyr::select(nom_taxo,traits,modalites) %>% 
  spread(traits,modalites) %>% 
  mutate(nom_taxo = ifelse(nom_taxo == "Gymnocephalus cernua","Gymnocephalus cernuus",nom_taxo))
traits_jointure_fresh <-   inner_join(traits_poissons_fresh,poisson_plando, by="nom_taxo") %>% 
  setnames(colnames(.),c("nom_taxo",paste0("FRE_",colnames(.)[-1])))
rest_tax_fresh <- dplyr::filter(poisson_plando,!(nom_taxo %in% traits_jointure_fresh$nom_taxo))

dplyr::filter(traits_poissons_fresh, str_detect(traits_poissons_fresh$nom_taxo,"Ponticola")) %>% pull(nom_taxo)
# Leuciscus burdigalensis / Scardinius erythrophthalmus/ Ponticola kessleri

df_traits_poissons <- join_all(list(traits_poissons_camille,traits_poissons_eco,traits_poissons_rhenato,traits_jointure_fresh),type="full", by="nom_taxo")


    write.csv(df_traits_poissons, file = "/home/theo/Bureau/Data/Données traits/Synthèse traits/poissons/full_jointure/traits_poissons_jointure.csv",row.names = FALSE)

####### travail
  
df_traits_poissons <- read_csv("traits_poissons_jointure.csv") %>% 
  as_tibble()%>%
  mutate_if(is.character,as.factor) %>%
  mutate(Marine = as.factor(Marine)) %>% 
  dplyr::select(-c(Freshwater,Brackish))


a <- dudi.hillsmith(na.omit(df_traits_poissons[,-1]),scannf=FALSE,nf=2)
a$eig[1]/(a$eig %>% sum())
a$eig[2]/(a$eig %>% sum())

scatter(a)

poisson <- read_csv(file.choose())
vis_dat(poisson)
poisson
a <- load("/home/theo/Bureau/Data/Données traits/Synthèse traits/poissons/maxime logez/.RData")
taxo_trait <- poisson$Espèce
dplyr::filter(poisson_plando,!(nom_taxo %in% taxo_trait))
