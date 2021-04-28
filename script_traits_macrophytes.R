#################################################################################################################################
################################################### TRAITS MACROPHYTES ##########################################################
#################################################################################################################################

df_macrophytes <- read_csv(file.choose())

################################## BASE FINIE
df_traits_macrophytes <- read_csv("/home/theo/Bureau/Data/Données traits/Données/traits_macrophytes.csv")

################################## PREPARATION DONNEES




nom_macro_esp <- fun_table_taxo(df_macrophytes %>% dplyr::filter(niveau_taxo == "Espèce"),seuil_prop =  0)[[1]] %>% dplyr::select(nom_taxo)
nom_macro_gen <- fun_table_taxo(df_macrophytes %>% dplyr::filter(niveau_taxo == "Genre"),seuil_prop =  0)[[1]] %>% dplyr::select(nom_taxo)
niveau_taxo <- df_macrophytes %>% group_by(nom_taxo,niveau_taxo) %>% dplyr::summarise(n=n()) %>% ungroup() %>% count("niveau_taxo")
ggplot(data=niveau_taxo)+geom_bar(aes(x=reorder(niveau_taxo,desc(freq)),y=freq),stat = "identity")+geom_text(aes(x=niveau_taxo,y=freq+10,label=freq))

# ggplot(nom_macro[c(1:40),], aes(x=reorder(nom_taxo,prop),y=prop))+geom_bar(stat="identity")+theme(axis.text.x = element_text(angle=45,h=1))

vec_nom_macro <- nom_macro_esp$nom_taxo
dplyr::filter




############################################ TRAITS PAN ET AL 2020 MACROPHYTES  ####################################################
#### trois traits d'intérêts pouyr les macrophtyes 
# root porosity = 37 espèces de notre bdd
# root/shoot ratio = 5 espèces
# photosynthèse sous l'eau : 24 esp
table_traits_pan <-map2(1:3,
                       c("Root porosity (%)",
                         "Root/Shoot ratio (g/g)",
                         "Underwater Photosynthesis (mol m-2 s-1)"),
                       ~read_ods("/home/theo/Bureau/Data/Données traits/Synthèse traits/macrophytes/traits_macrophytes_final/pan et al 2020/pan_macrophytes.ods",
                             sheet=.x) %>% as_tibble() %>% dplyr::select(matches("Species"),"Life form",.y) %>% 
                         setnames(colnames(.),tolower(colnames(.) %>% str_replace_all(" |-","_"))))


### portosité des racines
table_traits_pan[[1]] %>% inner_join(nom_macro_esp,by=c("species_name"="nom_taxo")) %>%
  group_by(species_name,life_form) %>% 
  dplyr::summarize(root_porosity = mean(`root_porosity_(%)`,na.rm = TRUE))
### ratio racine tige (masse)
table_traits_pan[[2]] %>% inner_join(nom_macro_esp,by=c("species_name"="nom_taxo")) %>%
  group_by(species_name,life_form) %>% 
  dplyr::summarize(root_shoot_ratio = mean(`root/shoot_ratio_(g/g)`,na.rm = TRUE))
### photosynthèse sous l'eau
table_traits_pan[[3]] %>% inner_join(nom_macro_esp,by=c("species_name"="nom_taxo")) %>% 
  group_by(species_name,life_form) %>% 
  dplyr::summarize(root_shoot_ratio = mean(`underwater_photosynthesis_(mol_m_2_s_1)`,na.rm = TRUE))








############################################ PHILIPPE JULVE  ####################################################
table_traits_macrophytes_PJ <-  read_csv("/home/theo/Bureau/Data/Données traits/Synthèse traits/macrophytes/traits_macrophytes_final/PJ/traits_macrophytes_PJ_plantes_algues.csv")%>% group_by(rang_taxinomique) %>% nest()


#### les espèce
colnames(table_traits_macrophytes_PJ$data[[1]])
table_macro_pj_esp <- table_traits_macrophytes_PJ$data[[1]] %>%
  mutate(nom_scientifique = str_extract(.$nom_scientifique,"[:alpha:]{1,}[:blank:]{1,}[:alpha:]{1,}"),
         nom_phytobase = str_extract(.$nom_phytobase,"[:alpha:]{1,}[:blank:]{1,}[:alpha:]{1,}"))


lien_pj_esp1 <- inner_join(nom_macro_esp,table_macro_pj_esp,by=c("nom_taxo"="nom_scientifique")) %>% 
  dplyr::select(nom_taxo,matches("pref"))
reste_pj <- anti_join(nom_macro_esp,table_macro_pj_esp,by=c("nom_taxo"="nom_scientifique"))
lien_pj_esp2 <- inner_join(reste_pj,table_macro_pj_esp,by=c("nom_taxo"="nom_phytobase"))%>% 
  dplyr::select(nom_taxo,matches("pref"))

#### les genre : soit on peut faire nous même nos classe en moyennant les résultats par espèce soit on regarde directement les genre
#en moyennant ==> en fait non le plus grand rang taxo est l'espèe ici
colnames(table_traits_macrophytes_PJ$data[[1]])

table_macro_pj_gen_nom_scientifique <- table_traits_macrophytes_PJ$data[[1]] %>%
  mutate(nom_scientifique = str_extract(.$nom_scientifique,"[:alpha:]{1,}"),
         nom_phytobase = str_extract(.$nom_phytobase,"[:alpha:]{1,}")) %>% 
  group_by(nom_scientifique) %>% dplyr::select_at(vars(matches("pref"))) %>%  dplyr::mutate_if(is.numeric,mean,na.rm=TRUE) %>% distinct()

table_macro_pj_gen_nom_phytobase <- table_traits_macrophytes_PJ$data[[1]] %>%
  mutate(nom_scientifique = str_extract(.$nom_scientifique,"[:alpha:]{1,}"),
         nom_phytobase = str_extract(.$nom_phytobase,"[:alpha:]{1,}")) %>% 
  group_by(nom_phytobase) %>% dplyr::select_at(vars(matches("pref"))) %>%  dplyr::mutate_if(is.numeric,mean,na.rm=TRUE) %>% distinct()

lien_pj_gen1 <- inner_join(nom_macro_gen,table_macro_pj_gen_nom_scientifique,by=c("nom_taxo"="nom_scientifique"))
reste_pj <- anti_join(nom_macro_gen,table_macro_pj_gen_nom_scientifique,by=c("nom_taxo"="nom_scientifique"))
lien_pj_gen2 <- inner_join(reste_pj,table_macro_pj_gen_nom_phytobase,by=c("nom_taxo"="nom_phytobase"))




##### BDD final PJ
df_traits_pj <- bind_rows(lien_pj_esp1,lien_pj_esp2,lien_pj_gen1,lien_pj_gen2) %>% 
  setnames(colnames(.),
           paste0("PJ_",colnames(.)))











############################################ david et christian #############################################
load("/home/theo/Bureau/Data/Données traits/Synthèse traits/macrophytes/traits_transcode_macrophytes.RData")

traits_macrophytes
taxo_macrophytes_cc
traits_david <- right_join(taxo_macrophytes_cc,traits_macrophytes,by = c("cd_taxon"="cd_taxon")) %>% 
  mutate(nom_taxon = str_replace(.$nom_taxon," sp.","") %>% as.character()
)
a <- traits_david %>% dplyr::filter(Aquaticit. %in% c(1,2,4))
traits_david %>% View()

lien_david_esp <- inner_join(nom_macro_esp,traits_david,by=c("nom_taxo"="nom_taxon"))
lien_david_genre <- inner_join(nom_macro_gen,traits_david ,by=c("nom_taxo"="nom_taxon"))
write.csv(traits_david, file = "/home/theo/Bureau/Data/Données traits/Synthèse traits/macrophytes/traits_macrophytes_final/macro_traits_david.csv")


df_traits_david <- bind_rows(lien_david_esp,lien_david_genre) %>% 
  dplyr::select(-c(cd_taxon,cd_taxon.VAL,cd_taxon.CONTRIB,Contrib_Syn,Contrib_Met,Contrib_App,ordre,famille)) %>% 
  mutate_at(vars(-one_of("nom_taxo","Aquaticit.")),~str_replace(.,"[:alpha:]{1,}_","")) %>% 
  mutate_all(~na_if(.,"")) %>% 
  setnames(colnames(.),paste0("CD_",tolower(colnames(.))))






########################################################################################
#####
#####                   Traits macrophytes : BDD LEDA                              #####
#####
########################################################################################
setwd("/home/theo/Bureau/Data/Données traits/Synthèse traits/macrophytes/traits_macrophytes_final/LEDA/LEDA_travail")
vec_nom_macro_esp <- nom_macro_esp$nom_taxo
vec_nom_macro_gen <- nom_macro_gen$nom_taxo

dir()
LEDA_BDD_esp <- map(dir(),~read_csv(.) %>%
                      dplyr::filter(`SBS name` %in% vec_nom_macro_esp) %>% 
                      setnames(colnames(.),str_replace_all(colnames(.)," ","_"))
) 

names(LEDA_BDD_esp) <- str_replace(dir(),".csv","")

map(LEDA_BDD_esp,~pull(.,SBS_name) %>% unique())



################################ traits plantes entière ################################

#### (1) forme de croissance de la plante : PGF ####
# forme de croissance de la plante + forme de croissance générale de la plante
vis_dat(LEDA_BDD_esp$plant_growth_form)
liste_PGF <- LEDA_BDD_esp
liste_PGF$plant_growth_form <- LEDA_BDD_esp$plant_growth_form %>% 
  dplyr::select(SBS_name,plant_growth_form = gen._plant_growth_form)
colnames(LEDA_BDD_esp$morphology_dispersal_unit)

PGF <- fun_codage_flou(liste_PGF,"plant_growth_form","plant_growth_form","PGF_")[[2]]

#### (2) hauteur de la canopée : CAH ####
# associée à la capcacité compétitive, la fécondité et le temps de
# régénération après une perturbation
# median, moy, max et min de la hauteur de la canopée
# valeurs median correspond àa la moyenne entre min et max

liste_CAH <- LEDA_BDD_esp
liste_CAH$canopy_height <- LEDA_BDD_esp$canopy_height %>%
  setnames(
    colnames(.)[-1],
    c(
      "hauteur_canop_valeur",
      "hauteur_canop_moy",
      "hauteur_canop_max",
      "hauteur_canop_min"
    )
  ) %>%
  mutate(hauteur_canop = hauteur_canop_valeur)


CAH <- liste_CAH$canopy_height %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(canopy_height = mean(hauteur_canop_valeur,na.rm=TRUE)) %>%
  na.omit()

#### (3) durée de vie de la plante : PLS ####
vis_dat(LEDA_BDD_esp$plant_life_span)
liste_PLS <- LEDA_BDD_esp
liste_PLS$plant_life_span <- LEDA_BDD_esp$plant_life_span %>% 
  dplyr::select(SBS_name,plant_life_span=gen._plant_life_span)
colnames(LEDA_BDD_esp$morphology_dispersal_unit)

PLS <- fun_codage_flou(liste_PLS,"plant_life_span","plant_life_span","PGF_")[[2]]

#### (4) age de première floraison AFF ####
AFF <- fun_codage_flou(LEDA_BDD_esp, "age_of_first_flowering","age_of_first_flowering",var_name = "AFF_")[[2]]



################################ traits feuilles        ################################
#### (1) taille des feuilles (surface) : LES ####
# surface des feuilles en mm2
vis_dat(LEDA_BDD_esp$leaf_size)
liste_LES <- LEDA_BDD_esp
liste_LES$leaf_size <- LEDA_BDD_esp$leaf_size %>%
  dplyr::filter(leaf_state == "without petiole and rachis")

LES<- liste_LES$leaf_size %>%
  group_by(SBS_name) %>%
  dplyr::summarise(leaf_size=mean(`single_value_[mm^2]`))

#### (2) masse des feuilles : LEM ####
# masse des feuilles en mg
vis_dat(liste_LEM$leaf_mass)
liste_LEM <- LEDA_BDD_esp
liste_LEM$leaf_mass <- LEDA_BDD_esp$leaf_mass %>%
  dplyr::filter(leaf_state == "without petiole and rachis")

LEM<- liste_LEM$leaf_mass %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(leaf_mass=mean(`single_value_leaf_mass_[mg]`))

#### (3) matière sèche des feuilles :LDM ####
# LMDC c'est la concentration en matière sèche des feuilles 
# exprimé en mg/g de matière fraiche
liste_LDM <- LEDA_BDD_esp

liste_LDM$leaf_dry_matter_content <- LEDA_BDD_esp$leaf_dry_matter_content %>% 
  dplyr::filter(leaf_state=="without petiole and rachis",
         plant_stage == "adult",
         leaf_specific_method == "leaf rehydration") %>% 
  dplyr::select(SBS_name,lmdc=`mean_LMDC_[mg/g]`)

LDM <- liste_LDM$leaf_dry_matter_content %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(leaf_dry_matter=mean(lmdc))

#### (4) surface foliaire spécifique : SLA ####
# mm²/mg
# variables indispensable
# a voir pour la méthode
LEDA_BDD_esp$sla
vis_dat(LEDA_BDD_esp$sla)
liste_SLA <- LEDA_BDD_esp
liste_SLA$sla <- LEDA_BDD_esp$sla %>% 
  dplyr::filter(plant_stage == "adult")

SLA <- liste_SLA$sla %>%
  group_by(SBS_name) %>%
  dplyr::summarise(sla = mean(`single_value_[mm^2/mg]`,na.rm=TRUE))



################################ traits tiges           ################################

#### (1) LIGNOSITÉ ####
LEDA_BDD_esp$woodiness
vis_dat(LEDA_BDD_esp$woodiness)
liste_WOO <- LEDA_BDD_esp
liste_WOO$woodiness <- LEDA_BDD_esp$woodiness

WOO <- fun_codage_flou(liste_WOO,"woodiness","woodiness","WOO_")[[2]]

#### (2) forme de croissance de la tige : SGF ####
# variables indispensable
LEDA_BDD_esp$shoot_growth_form
vis_dat(LEDA_BDD_esp$shoot_growth_form)
liste_SGF <- LEDA_BDD_esp
liste_SGF$shoot_growth_form <- LEDA_BDD_esp$shoot_growth_form

SGF <- fun_codage_flou(liste_SGF,"shoot_growth_form","shoot_growth_form","SGF_")[[2]]

#### (3)ramification BRA : oui/non ####
BRA <- fun_codage_flou(LEDA_BDD_esp, "branching","branching","BRA_")[[2]]

#### (4) Distribution des feuilles :LED_ ####
# une seul variables décrivant la distribution des feuilles le long de la tige
liste_LED <- LEDA_BDD_esp 
liste_LED$leaf_distribution <- LEDA_BDD_esp$leaf_distribution %>%
  dplyr::select(-X3) %>% 
  mutate(leaf_distribution = str_sub(leaf_distribution,start=1,end=3))

LED <- fun_codage_flou(liste_LED, "leaf_distribution","leaf_distribution",var_name = "LED_")[[2]]



################################ traits clonaux         ################################
#### (1) distribution verticale des bourgeons (nombre+position) : BVD_ ####
# "buds_in_layer_>10_cm"  BL[>10]
# "buds_in_layer_1-10cm" BL[1-10]
# "buds_in_layer_-10-0cm"BL[-10:0]
# "buds_in_layer_<_-10cm" BL[<-10]
# "buds_above_ground"    BAG
# "buds_at_soil_surface" BSS
# "buds_below_ground" BBG

LEDA_BDD_esp$buds_vertical_distrib
colnames(LEDA_BDD_esp$buds_vertical_distrib)
vec_var <- colnames(LEDA_BDD_esp$buds_vertical_distrib)[-1]
nom_var <- paste0(rep("BVD_",length(vec_var)),
                  c("BL[>10]","BL[1-10]","BL[-10:0]","BL[<-10]","BAG","BSS","BBG"))
BVD_flou<- map2(vec_var,nom_var,~fun_codage_flou(LEDA_BDD_esp,
                                                 "buds_vertical_distrib",
                                                 .x,.y)[[1]])
BVD_unique<- map2(vec_var,nom_var,~fun_codage_flou(LEDA_BDD_esp,
                                                   "buds_vertical_distrib",
                                                   .x,.y)[[2]] %>% ungroup()) %>%
  join_all(by="SBS_name",type="full") %>% 
  as_tibble()

#### (2) saisonnalité des bourgeons : 4 variables ####
#  "BBS_above_ground"  BAG
# "BBS_below_ground"    BBG
# "budb_seas._at_soil_surface" BSS
# "budb_seas_at_layer_10-0cm" BL10
vec_var <- colnames(LEDA_BDD_esp$buds_seasonality)[-1]
nom_var <- paste0(rep("BSE_",length(vec_var)),c("BAG","BBG","BBS","BL10"))
BSE_flou <- map2(vec_var,nom_var,~fun_codage_flou(LEDA_BDD_esp,
                                                  "buds_seasonality",
                                                  .x,.y)[[1]])
BSE_unique <- map2(vec_var,nom_var,~fun_codage_flou(LEDA_BDD_esp,
                                                    "buds_seasonality",
                                                    .x,.y)[[2]] %>% ungroup()) %>% 
  join_all(by="SBS_name",type="full") %>% 
  as_tibble()

#### (3) organe de croissance clonaux : CGO ####
# deux variables retenue sur les 8, "clonal_growth_organ_1" "role_of_CGO_1"  car très peu de données pour les autres niveaux
# "clonal_growth_organ_2" 
# "role_of_CGO_2"        
# "clonal_growth_organ_3" 
# "role_of_CGO_3"   
# "clonal_growth_organ_4" 
# "role_of_CGO_4"       
# en tout 278 espèces de la BDD macro sont concernées
colnames(LEDA_BDD_esp$clonal_growth_organs)
liste_CGO <- LEDA_BDD_esp
liste_CGO$clonal_growth_organs <- LEDA_BDD_esp$clonal_growth_organs %>%
  dplyr::select("SBS_name","clonal_growth_organ_1","role_of_CGO_1")

colnames(liste_CGO$clonal_growth_organs)
nom_var <- c("CGO_1","CGO_1_role")
vec_var <- colnames(liste_CGO$clonal_growth_organs)[-1]

CGO_flou<- map2(vec_var,nom_var,~fun_codage_flou(liste_CGO,
                                                 "clonal_growth_organs",
                                                 .x,.y)[[1]])
CGO_unique<- map2(vec_var,nom_var,~fun_codage_flou(liste_CGO,
                                                   "clonal_growth_organs",
                                                   .x,.y)[[2]] %>% ungroup()) %>% 
  join_all(by="SBS_name", type = "full") %>% 
  as_tibble()
################################ traits graines         ################################
#### (1) nombre de graine par tige SNS ####
# deux variables d'intérêt : single value (quanti) ou var catégorielle issue de cette variable quanti
# nécessité de fixé l'unité de reproduction choisit (comme d'hab sur les données les plus présentes)
# ici ca sera les mesure effectuées sur per "ramet/tussock or individual plant"

LEDA_BDD_esp$seed_number_per_shoot
vis_dat(LEDA_BDD_esp$seed_number_per_shoot)
liste_SNS <- LEDA_BDD_esp
liste_SNS$seed_number_per_shoot <- LEDA_BDD_esp$seed_number_per_shoot%>% 
  dplyr::filter(reproduction_unit_measured == "per ramet/tussock or individual plant") %>% 
  dplyr::select(SBS_name,single_value,seed_number_category)

SNS <- liste_SNS$seed_number_per_shoot %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(seed_number_per_shoot = mean(single_value, na.rm = TRUE)) %>% 
  mutate(seed_number_per_shoot_cat = cut(seed_number_per_shoot,c(0,10,100,1000,10000,100000,Inf))  %>% 
           ordered() %>% 
           factor(levels(.),labels=c("seednumber <= 10",
                                     "10 < seednumber <= 100",
                                     "100 < seednumber <= 1000",
                                     "1000 < seednumber <= 10000",
                                     "10000 < seednumber <= 100000",
                                     "seednumber > 100000"))) %>%
  na.omit()
#### (2) nombre de graines SNU ####
# deux variables d'intérêt : single value (quanti) ou var catégorielle issue de cette variable quanti
# nécessité de fixé l'unité de reproduction choisit (comme d'hab sur les données les plus présentes)
# ici ca sera les mesure effectuées sur per "ramet/tussock or individual plant"

LEDA_BDD_esp$seed_number
vis_dat(LEDA_BDD_esp$seed_number)
liste_SNU <- LEDA_BDD_esp
liste_SNU$seed_number <- LEDA_BDD_esp$seed_number%>% 
  dplyr::filter(reproduction_unit_measured == "per ramet/tussock or individual plant") %>% 
  dplyr::select(SBS_name,single_value,seed_number_category)

SNU <- liste_SNU$seed_number %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(seed_number = mean(single_value, na.rm = TRUE)) %>% 
  mutate(seed_number_cat = cut(seed_number,c(0,10,100,1000,10000,100000,Inf))  %>% 
           ordered() %>% 
           factor(levels(.),labels=c("seednumber <= 10",
                                     "10 < seednumber <= 100",
                                     "100 < seednumber <= 1000",
                                     "1000 < seednumber <= 10000",
                                     "10000 < seednumber <= 100000",
                                     "seednumber > 100000"))) %>% na.omit()


#### (3) masse des graines : SEM [mg] ####
# peut concerner différents organes, on prends ici les plus répendu dans le jeux de données
# afin d'avoir des résultats homogène 
# type 2a :one-seed generative
#moyenne des single value (moyenne si présente sinon mediane sinon moyenne min max sinon ce qui reste (min ou max))
LEDA_BDD_esp$seed_mass
vis_dat(LEDA_BDD_esp$seed_mass)
liste_SEM <- LEDA_BDD_esp
liste_SEM$seed_mass <- LEDA_BDD_esp$seed_mass%>% 
  dplyr::filter(diaspore_type_code == "2a") %>% 
  dplyr::select(SBS_name,`single_value_[mg]`)


SEM <- liste_SEM$seed_mass %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(seed_mass = mean(`single_value_[mg]`, na.rm = TRUE))

#### (4) forme des graines : SES ####
# largeur, longueur, hauteur en mm
# filtre sur les diaspore lees plus communs
LEDA_BDD_esp$seed_shape
vis_dat(LEDA_BDD_esp$seed_shape)
liste_SES <- LEDA_BDD_esp
liste_SES$seed_shape <- LEDA_BDD_esp$seed_shape %>% 
  dplyr::filter(diaspore_type == "generative dispersule")

    SES <- liste_SES$seed_shape %>% 
  group_by(SBS_name) %>% 
  na.omit() %>% 
  dplyr::summarise(seed_length = var(`length_(single_value)_[mm]`/`length_(single_value)_[mm]`, na.rm = TRUE),
                   seed_width = var(`width_(single_value)_[mm]`/`length_(single_value)_[mm]`,na.rm=TRUE),
                   seed_height = var(`height_(single_value)_[mm]`/`length_(single_value)_[mm]`,na.rm=TRUE)) 
var
#### (5) banque de graines + longévité des graines ===> compliqué : SEB ####
# beaucoup de variables avec beaucoup de variabilité de dépendance entre variable et d'erreur possible
# je vais garder unqiuement seed_bank_type trop difficile à traiter sinon
# je vais quand même récupérer le type de banque de graine(transitoire, court termes transitoire, long term)
# j'ai mixé avec la variable seed_longévity qui contient les même info pour la banque
# création d'une variable indice présente dans la base seed_longevity qui traduit en codage numeric la variable catégorielle pour pouvoir moyenné

# longévité des graines ==> revient à SEB ici
vis_dat(LEDA_BDD_esp$seed_longevity)
liste_SBL <- LEDA_BDD_esp
liste_SBL$seed_longevity <- LEDA_BDD_esp$seed_longevity %>% 
  dplyr::filter(seed_bank_type != "present") %>% 
  dplyr::select(SBS_name,seed_bank_type,SSB_seed_longevity_index)


SBL_cat <- fun_codage_flou(liste_SBL,"seed_longevity","seed_bank_type","SBL_")[[2]]
SBL_num <- liste_SBL$seed_longevity %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(seed_bank_longevity = mean(SSB_seed_longevity_index, na.rm = TRUE))

#### (6) morphologie de l'organe de dispersion MOD ####
# plusieur variables d'intérêt ici qui sont reliée au type de diaspore ou dispersule d'un coté et de la graine de l'autre
# 5 variables en tout mais seed_structur_ehooked = trop de NA par rapport au nombre total d'observation
#"diaspore_type"     
# "gen._diaspore_type" 
# "seed_structure"     
# "gen._seed_structure"  
# "seed_structure_hooked"
# on va garder aussi uniquement les variables generale "gen." 
# car ce sont les mois variables en intra espèces ca évite de rajouter des biais
vis_dat(LEDA_BDD_esp$morphology_dispersal_unit)
liste_MOD <- LEDA_BDD_esp
liste_MOD$morphology_dispersal_unit <- LEDA_BDD_esp$morphology_dispersal_unit %>% 
  dplyr::select(SBS_name,gen._diaspore_type,gen._seed_structure)



colnames(LEDA_BDD_esp$morphology_dispersal_unit)

nom_var <- c(rep("MOD_",2))
vec_var <- colnames(liste_MOD$morphology_dispersal_unit)[-1]

MOD_flou<- map2(vec_var,nom_var,~fun_codage_flou(liste_MOD,
                                                 "morphology_dispersal_unit",
                                                 .x,.y)[[1]])
MOD_unique<- map2(vec_var,nom_var,~fun_codage_flou(liste_MOD,
                                                   "morphology_dispersal_unit",
                                                   .x,.y)[[2]] %>% ungroup()) %>% 
  join_all(by="SBS_name", type = "full") %>% 
  as_tibble()



################################ traits dispersabilité  ################################

#### (1) hauteur de la libération des graines SRH ####
# on prend single value et moyenne
vis_dat(LEDA_BDD_esp$releasing_height)
liste_SRH <- LEDA_BDD_esp
liste_SRH$releasing_height <- LEDA_BDD_esp$releasing_height %>% 
  dplyr::filter(record_valid == 1) %>% 
  dplyr::select(SBS_name,seed_releasing_height=`single_value_[m]`)
colnames(LEDA_BDD_esp$morphology_dispersal_unit)

SRH <- liste_SRH$releasing_height %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(seed_releasing_height = mean(seed_releasing_height,na.rm=TRUE))


#### (2) vélocité terminale :TVE ####
# vitesse de chute des graines = liée à la capacité de dispersion par le vent
LEDA_BDD_esp$terminal_velocity
vis_dat(LEDA_BDD_esp$terminal_velocity)
liste_TVE <- LEDA_BDD_esp
liste_TVE$terminal_velocity <- LEDA_BDD_esp$terminal_velocity %>% 
  dplyr::filter(diaspore_type == "one-seeded generative dispersule",
         measured_with_structures == "yes")

TVE <- liste_TVE$terminal_velocity %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(terminal_velocity = mean(`single_value_[m/s]`,na.rm=TRUE))


#### (3) Buoyancy p124: BUO_ ####
# mesure de la durée de flotaison des graines ou des organes de dispersion
# variables concernées : fixed_time_step
# 5 autres variables en compléments mais qui sont surement présente dans d'autre tableau qui leur est consacré : à vérifier
# "diaspore_type"   
# "dispersal_type"     
# "dispersal_vector"    
# "gen._diaspore_type"   
# "gen._dispersal_type"  
# "gen._dispersal_vector"  

liste_buo <- LEDA_BDD_esp 
liste_buo$buoyancy <- LEDA_BDD_esp$buoyancy %>% 
  mutate(dispersal_vector=str_replace_all(dispersal_vector,"_",""),
         buoyancy = ifelse(str_detect(fixed_time_step,"[:alpha:][:digit:]")==TRUE,
                           str_extract(fixed_time_step, "[:alpha:][:digit:]"),
                           NA_character_)) %>% 
  dplyr::filter(`single_value_[%]`=="50")

colnames(LEDA_BDD_esp$buoyancy) %>% sort()

BUO <- fun_codage_flou(liste_buo, "buoyancy","buoyancy",var_name = "BUO_")[[2]]

#### (4) type de dispersion : DIT
# "dispersal_type"   : type de dispersion       
# "dispersal_vector"  
# "gen._dispersal_vector"        
# "gen._dispersal_type"       
# "BYC_avg._float._cap._(sv)_[%]"
# on récupère que la manière générale : moins d'erreurs car ce sont des observations + problème de plasticité on va éviter de faire des conneries
# création de longue distance : variable binaire à partir des tableaux présent dans les docs
# se base sur la variable disp_gen_type mais peut aussi se baser sur les animaux concernés donc disp_vec_type
# disp_vec_type compliqué beaucoup de modalité différentes je devrait pas la garder dans un premier temps

colnames(LEDA_BDD_esp$dispersal_type)
LEDA_BDD_esp$dispersal_type %>% vis_dat()
liste_DIT <- LEDA_BDD_esp
liste_DIT$dispersal_type <- LEDA_BDD_esp$dispersal_type %>%
  dplyr::select("SBS_name",disp_gen_type="gen._dispersal_type",disp_gen_vect="gen._dispersal_vector") %>% 
  mutate(disp_long_dist = ifelse(disp_gen_type %in% c("autochor","ombrochor"),"no","yes"))

colnames(liste_DIT$dispersal_type)
nom_var <- c(rep("DIT_",3))
vec_var <- colnames(liste_DIT$dispersal_type)[-1]

DIT_flou<- map2(vec_var,nom_var,~fun_codage_flou(liste_DIT,
                                                 "dispersal_type",
                                                 .x,.y)[[1]])
DIT_unique<- map2(vec_var,nom_var,~fun_codage_flou(liste_DIT,
                                                   "dispersal_type",
                                                   .x,.y)[[2]] %>% ungroup()) %>% 
  join_all(by="SBS_name", type = "full") %>% 
  as_tibble()









############################## BASE LEDA FINAL



df_traits_leda_esp <- list(
  PGF %>% ungroup(),
  CAH,
  PLS %>% ungroup(),
  AFF %>% ungroup(),
  LES,
  LEM,
  LDM,
  SLA,
  WOO %>% ungroup(),
  SGF %>% ungroup(),
  BRA %>% ungroup(),
  LED %>% ungroup(),
  BVD_unique,
  BSE_unique,
  CGO_unique,
  SNS,
  SNU,
  SEM,
  SES,
  SBL_cat %>% ungroup(),
  SBL_num,
  MOD_unique,
  SRH,
  TVE,
  BUO %>% ungroup(),
  DIT_unique
) %>% 
  join_all(by="SBS_name", type="full") %>% 
  as_tibble()




########################################################################################
#####
#####                   Traits macrophytes : BDD LEDA                              #####
#####
########################################################################################
setwd("/home/theo/Bureau/Data/Données traits/Synthèse traits/macrophytes/traits_macrophytes_final/LEDA/LEDA_travail")
vec_nom_macro_esp <- nom_macro_esp$nom_taxo
vec_nom_macro_gen <- nom_macro_gen$nom_taxo

dir()
LEDA_BDD_gen <- map(dir(),~read_csv(.) %>% 
                      setnames(colnames(.),str_replace_all(colnames(.)," ","_"))%>%
                      mutate(SBS_name = str_extract(SBS_name,"[:alpha:]{1,}")) %>% 
                      dplyr::filter(SBS_name %in% vec_nom_macro_gen)
) 

names(LEDA_BDD_gen) <- str_replace(dir(),".csv","")

map(LEDA_BDD_gen,~pull(.,SBS_name) %>% unique())



################################ traits plantes entière ################################

#### (1) forme de croissance de la plante : PGF ####
# forme de croissance de la plante + forme de croissance générale de la plante
vis_dat(LEDA_BDD_gen$plant_growth_form)
liste_PGF <- LEDA_BDD_gen
liste_PGF$plant_growth_form <- LEDA_BDD_gen$plant_growth_form %>% 
  dplyr::select(SBS_name,plant_growth_form = gen._plant_growth_form)
colnames(LEDA_BDD_gen$morphology_dispersal_unit)

PGF <- fun_codage_flou(liste_PGF,"plant_growth_form","plant_growth_form","PGF_")[[2]]

#### (2) hauteur de la canopée : CAH ####
# associée à la capcacité compétitive, la fécondité et le temps de
# régénération après une perturbation
# median, moy, max et min de la hauteur de la canopée
# valeurs median correspond àa la moyenne entre min et max

liste_CAH <- LEDA_BDD_gen
liste_CAH$canopy_height <- LEDA_BDD_gen$canopy_height %>%
  setnames(
    colnames(.)[-1],
    c(
      "hauteur_canop_valeur",
      "hauteur_canop_moy",
      "hauteur_canop_max",
      "hauteur_canop_min"
    )
  ) %>%
  mutate(hauteur_canop = hauteur_canop_valeur)


CAH <- liste_CAH$canopy_height %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(canopy_height = mean(hauteur_canop_valeur,na.rm=TRUE)) %>%
  na.omit()

#### (3) durée de vie de la plante : PLS ####
vis_dat(LEDA_BDD_gen$plant_life_span)
liste_PLS <- LEDA_BDD_gen
liste_PLS$plant_life_span <- LEDA_BDD_gen$plant_life_span %>% 
  dplyr::select(SBS_name,plant_life_span=gen._plant_life_span)
colnames(LEDA_BDD_gen$morphology_dispersal_unit)

PLS <- fun_codage_flou(liste_PLS,"plant_life_span","plant_life_span","PGF_")[[2]]

#### (4) age de première floraison AFF ####
AFF <- fun_codage_flou(LEDA_BDD_gen, "age_of_first_flowering","age_of_first_flowering",var_name = "AFF_")[[2]]



################################ traits feuilles        ################################
#### (1) taille des feuilles (surface) : LES ####
# surface des feuilles en mm2
vis_dat(LEDA_BDD_gen$leaf_size)
liste_LES <- LEDA_BDD_gen
liste_LES$leaf_size <- LEDA_BDD_gen$leaf_size %>%
  dplyr::filter(leaf_state == "without petiole and rachis")

LES<- liste_LES$leaf_size %>%
  group_by(SBS_name) %>%
  dplyr::summarise(leaf_size=mean(`single_value_[mm^2]`))

#### (2) masse des feuilles : LEM ####
# masse des feuilles en mg
vis_dat(liste_LEM$leaf_mass)
liste_LEM <- LEDA_BDD_gen
liste_LEM$leaf_mass <- LEDA_BDD_gen$leaf_mass %>%
  dplyr::filter(leaf_state == "without petiole and rachis")

LEM<- liste_LEM$leaf_mass %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(leaf_mass=mean(`single_value_leaf_mass_[mg]`))

#### (3) matière sèche des feuilles :LDM ####
# LMDC c'est la concentration en matière sèche des feuilles 
# exprimé en mg/g de matière fraiche
liste_LDM <- LEDA_BDD_gen

liste_LDM$leaf_dry_matter_content <- LEDA_BDD_gen$leaf_dry_matter_content %>% 
  dplyr::filter(leaf_state=="without petiole and rachis",
         plant_stage == "adult",
         leaf_specific_method == "leaf rehydration") %>% 
  dplyr::select(SBS_name,lmdc=`mean_LMDC_[mg/g]`)

LDM <- liste_LDM$leaf_dry_matter_content %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(leaf_dry_matter=mean(lmdc))

#### (4) surface foliaire spécifique : SLA ####
# mm²/mg
# variables indispensable
# a voir pour la méthode
LEDA_BDD_gen$sla
vis_dat(LEDA_BDD_gen$sla)
liste_SLA <- LEDA_BDD_gen
liste_SLA$sla <- LEDA_BDD_gen$sla %>% 
  dplyr::filter(plant_stage == "adult")

SLA <- liste_SLA$sla %>%
  group_by(SBS_name) %>%
  dplyr::summarise(sla = mean(`single_value_[mm^2/mg]`,na.rm=TRUE))



################################ traits tiges           ################################

#### (1) LIGNOSITÉ ####
LEDA_BDD_gen$woodiness
vis_dat(LEDA_BDD_gen$woodiness)
liste_WOO <- LEDA_BDD_gen
liste_WOO$woodiness <- LEDA_BDD_gen$woodiness

WOO <- fun_codage_flou(liste_WOO,"woodiness","woodiness","WOO_")[[2]]

#### (2) forme de croissance de la tige : SGF ####
# variables indispensable
LEDA_BDD_gen$shoot_growth_form
vis_dat(LEDA_BDD_gen$shoot_growth_form)
liste_SGF <- LEDA_BDD_gen
liste_SGF$shoot_growth_form <- LEDA_BDD_gen$shoot_growth_form

SGF <- fun_codage_flou(liste_SGF,"shoot_growth_form","shoot_growth_form","SGF_")[[2]]

#### (3)ramification BRA : oui/non ####
BRA <- fun_codage_flou(LEDA_BDD_gen, "branching","branching","BRA_")[[2]]

#### (4) Distribution des feuilles :LED_ ####
# une seul variables décrivant la distribution des feuilles le long de la tige
liste_LED <- LEDA_BDD_gen 
liste_LED$leaf_distribution <- LEDA_BDD_gen$leaf_distribution %>%
  dplyr::select(-X3) %>% 
  mutate(leaf_distribution = str_sub(leaf_distribution,start=1,end=3))

LED <- fun_codage_flou(liste_LED, "leaf_distribution","leaf_distribution",var_name = "LED_")[[2]]



################################ traits clonaux         ################################
#### (1) distribution verticale des bourgeons (nombre+position) : BVD_ ####
# "buds_in_layer_>10_cm"  BL[>10]
# "buds_in_layer_1-10cm" BL[1-10]
# "buds_in_layer_-10-0cm"BL[-10:0]
# "buds_in_layer_<_-10cm" BL[<-10]
# "buds_above_ground"    BAG
# "buds_at_soil_surface" BSS
# "buds_below_ground" BBG

LEDA_BDD_gen$buds_vertical_distrib
colnames(LEDA_BDD_gen$buds_vertical_distrib)
vec_var <- colnames(LEDA_BDD_gen$buds_vertical_distrib)[-1]
nom_var <- paste0(rep("BVD_",length(vec_var)),
                  c("BL[>10]","BL[1-10]","BL[-10:0]","BL[<-10]","BAG","BSS","BBG"))
BVD_flou<- map2(vec_var,nom_var,~fun_codage_flou(LEDA_BDD_gen,
                                                 "buds_vertical_distrib",
                                                 .x,.y)[[1]])
BVD_unique<- map2(vec_var,nom_var,~fun_codage_flou(LEDA_BDD_gen,
                                                   "buds_vertical_distrib",
                                                   .x,.y)[[2]] %>% ungroup()) %>%
  join_all(by="SBS_name",type="full") %>% 
  as_tibble()

#### (2) saisonnalité des bourgeons : 4 variables ####
#  "BBS_above_ground"  BAG
# "BBS_below_ground"    BBG
# "budb_seas._at_soil_surface" BSS
# "budb_seas_at_layer_10-0cm" BL10
vec_var <- colnames(LEDA_BDD_gen$buds_seasonality)[-1]
nom_var <- paste0(rep("BSE_",length(vec_var)),c("BAG","BBG","BBS","BL10"))
BSE_flou <- map2(vec_var,nom_var,~fun_codage_flou(LEDA_BDD_gen,
                                                  "buds_seasonality",
                                                  .x,.y)[[1]])
BSE_unique <- map2(vec_var,nom_var,~fun_codage_flou(LEDA_BDD_gen,
                                                    "buds_seasonality",
                                                    .x,.y)[[2]] %>% ungroup()) %>% 
  join_all(by="SBS_name",type="full") %>% 
  as_tibble()

#### (3) organe de croissance clonaux : CGO ####
# deux variables retenue sur les 8, "clonal_growth_organ_1" "role_of_CGO_1"  car très peu de données pour les autres niveaux
# "clonal_growth_organ_2" 
# "role_of_CGO_2"        
# "clonal_growth_organ_3" 
# "role_of_CGO_3"   
# "clonal_growth_organ_4" 
# "role_of_CGO_4"       
# en tout 278 espèces de la BDD macro sont concernées
colnames(LEDA_BDD_gen$clonal_growth_organs)
liste_CGO <- LEDA_BDD_gen
liste_CGO$clonal_growth_organs <- LEDA_BDD_gen$clonal_growth_organs %>%
  dplyr::select("SBS_name","clonal_growth_organ_1","role_of_CGO_1")

colnames(liste_CGO$clonal_growth_organs)
nom_var <- c("CGO_1","CGO_1_role")
vec_var <- colnames(liste_CGO$clonal_growth_organs)[-1]

CGO_flou<- map2(vec_var,nom_var,~fun_codage_flou(liste_CGO,
                                                 "clonal_growth_organs",
                                                 .x,.y)[[1]])
CGO_unique<- map2(vec_var,nom_var,~fun_codage_flou(liste_CGO,
                                                   "clonal_growth_organs",
                                                   .x,.y)[[2]] %>% ungroup()) %>% 
  join_all(by="SBS_name", type = "full") %>% 
  as_tibble()
################################ traits graines         ################################
#### (1) nombre de graine par tige SNS ####
# deux variables d'intérêt : single value (quanti) ou var catégorielle issue de cette variable quanti
# nécessité de fixé l'unité de reproduction choisit (comme d'hab sur les données les plus présentes)
# ici ca sera les mesure effectuées sur per "ramet/tussock or individual plant"

LEDA_BDD_gen$seed_number_per_shoot
vis_dat(LEDA_BDD_gen$seed_number_per_shoot)
liste_SNS <- LEDA_BDD_gen
liste_SNS$seed_number_per_shoot <- LEDA_BDD_gen$seed_number_per_shoot%>% 
  dplyr::filter(reproduction_unit_measured == "per ramet/tussock or individual plant") %>% 
  dplyr::select(SBS_name,single_value,seed_number_category)

SNS <- liste_SNS$seed_number_per_shoot %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(seed_number_per_shoot = mean(single_value, na.rm = TRUE)) %>% 
  mutate(seed_number_per_shoot_cat = cut(seed_number_per_shoot,c(0,10,100,1000,10000,100000,Inf))  %>% 
           ordered() %>% 
           factor(levels(.),labels=c("seednumber <= 10",
                                     "10 < seednumber <= 100",
                                     "100 < seednumber <= 1000",
                                     "1000 < seednumber <= 10000",
                                     "10000 < seednumber <= 100000",
                                     "seednumber > 100000"))) %>%
  na.omit()
#### (2) nombre de graines SNU ####
# deux variables d'intérêt : single value (quanti) ou var catégorielle issue de cette variable quanti
# nécessité de fixé l'unité de reproduction choisit (comme d'hab sur les données les plus présentes)
# ici ca sera les mesure effectuées sur per "ramet/tussock or individual plant"

LEDA_BDD_gen$seed_number
vis_dat(LEDA_BDD_gen$seed_number)
liste_SNU <- LEDA_BDD_gen
liste_SNU$seed_number <- LEDA_BDD_gen$seed_number%>% 
  dplyr::filter(reproduction_unit_measured == "per ramet/tussock or individual plant") %>% 
  dplyr::select(SBS_name,single_value,seed_number_category)

SNU <- liste_SNU$seed_number %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(seed_number = mean(single_value, na.rm = TRUE)) %>% 
  mutate(seed_number_cat = cut(seed_number,c(0,10,100,1000,10000,100000,Inf))  %>% 
           ordered() %>% 
           factor(levels(.),labels=c("seednumber <= 10",
                                     "10 < seednumber <= 100",
                                     "100 < seednumber <= 1000",
                                     "1000 < seednumber <= 10000",
                                     "10000 < seednumber <= 100000",
                                     "seednumber > 100000"))) %>% na.omit()


#### (3) masse des graines : SEM [mg] ####
# peut concerner différents organes, on prends ici les plus répendu dans le jeux de données
# afin d'avoir des résultats homogène 
# type 2a :one-seed generative
#moyenne des single value (moyenne si présente sinon mediane sinon moyenne min max sinon ce qui reste (min ou max))
LEDA_BDD_gen$seed_mass
vis_dat(LEDA_BDD_gen$seed_mass)
liste_SEM <- LEDA_BDD_gen
liste_SEM$seed_mass <- LEDA_BDD_gen$seed_mass%>% 
  dplyr::filter(diaspore_type_code == "2a") %>% 
  dplyr::select(SBS_name,`single_value_[mg]`)


SEM <- liste_SEM$seed_mass %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(seed_mass = mean(`single_value_[mg]`, na.rm = TRUE))

#### (4) forme des graines : SES ####
# largeur, longueur, hauteur en mm
# filtre sur les diaspore lees plus communs
LEDA_BDD_gen$seed_shape
vis_dat(LEDA_BDD_gen$seed_shape)
liste_SES <- LEDA_BDD_gen
liste_SES$seed_shape <- LEDA_BDD_gen$seed_shape %>% 
  dplyr::filter(diaspore_type == "generative dispersule")

SES <- liste_SES$seed_shape %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(seed_length = mean(`length_(single_value)_[mm]`, na.rm = TRUE),
                   seed_width = mean(`width_(single_value)_[mm]`,na.rm=TRUE),
                   seed_height = mean(`height_(single_value)_[mm]`,na.rm=TRUE)) 

#### (5) banque de graines + longévité des graines ===> compliqué : SEB ####
# beaucoup de variables avec beaucoup de variabilité de dépendance entre variable et d'erreur possible
# je vais garder unqiuement seed_bank_type trop difficile à traiter sinon
# je vais quand même récupérer le type de banque de graine(transitoire, court termes transitoire, long term)
# j'ai mixé avec la variable seed_longévity qui contient les même info pour la banque
# création d'une variable indice présente dans la base seed_longevity qui traduit en codage numeric la variable catégorielle pour pouvoir moyenné

# longévité des graines ==> revient à SEB ici
vis_dat(LEDA_BDD_gen$seed_longevity)
liste_SBL <- LEDA_BDD_gen
liste_SBL$seed_longevity <- LEDA_BDD_gen$seed_longevity %>% 
  dplyr::filter(seed_bank_type != "present") %>% 
  dplyr::select(SBS_name,seed_bank_type,SSB_seed_longevity_index)


SBL_cat <- fun_codage_flou(liste_SBL,"seed_longevity","seed_bank_type","SBL_")[[2]]
SBL_num <- liste_SBL$seed_longevity %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(seed_bank_longevity = mean(SSB_seed_longevity_index, na.rm = TRUE))

#### (6) morphologie de l'organe de dispersion MOD ####
# plusieur variables d'intérêt ici qui sont reliée au type de diaspore ou dispersule d'un coté et de la graine de l'autre
# 5 variables en tout mais seed_structur_ehooked = trop de NA par rapport au nombre total d'observation
#"diaspore_type"     
# "gen._diaspore_type" 
# "seed_structure"     
# "gen._seed_structure"  
# "seed_structure_hooked"
# on va garder aussi uniquement les variables generale "gen." 
# car ce sont les mois variables en intra espèces ca évite de rajouter des biais
vis_dat(LEDA_BDD_gen$morphology_dispersal_unit)
liste_MOD <- LEDA_BDD_gen
liste_MOD$morphology_dispersal_unit <- LEDA_BDD_gen$morphology_dispersal_unit %>% 
  dplyr::select(SBS_name,gen._diaspore_type,gen._seed_structure)



colnames(LEDA_BDD_gen$morphology_dispersal_unit)

nom_var <- c(rep("MOD_",2))
vec_var <- colnames(liste_MOD$morphology_dispersal_unit)[-1]

MOD_flou<- map2(vec_var,nom_var,~fun_codage_flou(liste_MOD,
                                                 "morphology_dispersal_unit",
                                                 .x,.y)[[1]])
MOD_unique<- map2(vec_var,nom_var,~fun_codage_flou(liste_MOD,
                                                   "morphology_dispersal_unit",
                                                   .x,.y)[[2]] %>% ungroup()) %>% 
  join_all(by="SBS_name", type = "full") %>% 
  as_tibble()



################################ traits dispersabilité  ################################

#### (1) hauteur de la libération des graines SRH ####
# on prend single value et moyenne
vis_dat(LEDA_BDD_gen$releasing_height)
liste_SRH <- LEDA_BDD_gen
liste_SRH$releasing_height <- LEDA_BDD_gen$releasing_height %>% 
  dplyr::filter(record_valid == 1) %>% 
  dplyr::select(SBS_name,seed_releasing_height=`single_value_[m]`)
colnames(LEDA_BDD_gen$morphology_dispersal_unit)

SRH <- liste_SRH$releasing_height %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(seed_releasing_height = mean(seed_releasing_height,na.rm=TRUE))


#### (2) vélocité terminale :TVE ####
# vitesse de chute des graines = liée à la capacité de dispersion par le vent
LEDA_BDD_gen$terminal_velocity
vis_dat(LEDA_BDD_gen$terminal_velocity)
liste_TVE <- LEDA_BDD_gen
liste_TVE$terminal_velocity <- LEDA_BDD_gen$terminal_velocity %>% 
  dplyr::filter(diaspore_type == "one-seeded generative dispersule",
         measured_with_structures == "yes")

TVE <- liste_TVE$terminal_velocity %>% 
  group_by(SBS_name) %>% 
  dplyr::summarise(terminal_velocity = mean(`single_value_[m/s]`,na.rm=TRUE))


#### (3) Buoyancy p124: BUO_ ####
# mesure de la durée de flotaison des graines ou des organes de dispersion
# variables concernées : fixed_time_step
# 5 autres variables en compléments mais qui sont surement présente dans d'autre tableau qui leur est consacré : à vérifier
# "diaspore_type"   
# "dispersal_type"     
# "dispersal_vector"    
# "gen._diaspore_type"   
# "gen._dispersal_type"  
# "gen._dispersal_vector"  

liste_buo <- LEDA_BDD_gen 
liste_buo$buoyancy <- LEDA_BDD_gen$buoyancy %>% 
  mutate(dispersal_vector=str_replace_all(dispersal_vector,"_",""),
         buoyancy = ifelse(str_detect(fixed_time_step,"[:alpha:][:digit:]")==TRUE,
                           str_extract(fixed_time_step, "[:alpha:][:digit:]"),
                           NA_character_)) %>% 
  dplyr::filter(`single_value_[%]`=="50")

colnames(LEDA_BDD_gen$buoyancy) %>% sort()

BUO <- fun_codage_flou(liste_buo, "buoyancy","buoyancy",var_name = "BUO_")[[2]]

#### (4) type de dispersion : DIT
# "dispersal_type"   : type de dispersion       
# "dispersal_vector"  
# "gen._dispersal_vector"        
# "gen._dispersal_type"       
# "BYC_avg._float._cap._(sv)_[%]"
# on récupère que la manière générale : moins d'erreurs car ce sont des observations + problème de plasticité on va éviter de faire des conneries
# création de longue distance : variable binaire à partir des tableaux présent dans les docs
# se base sur la variable disp_gen_type mais peut aussi se baser sur les animaux concernés donc disp_vec_type
# disp_vec_type compliqué beaucoup de modalité différentes je devrait pas la garder dans un premier temps

colnames(LEDA_BDD_gen$dispersal_type)
LEDA_BDD_gen$dispersal_type %>% vis_dat()
liste_DIT <- LEDA_BDD_gen
liste_DIT$dispersal_type <- LEDA_BDD_gen$dispersal_type %>%
  dplyr::select("SBS_name",disp_gen_type="gen._dispersal_type",disp_gen_vect="gen._dispersal_vector") %>% 
  mutate(disp_long_dist = ifelse(disp_gen_type %in% c("autochor","ombrochor"),"no","yes"))

colnames(liste_DIT$dispersal_type)
nom_var <- c(rep("DIT_",3))
vec_var <- colnames(liste_DIT$dispersal_type)[-1]

DIT_flou<- map2(vec_var,nom_var,~fun_codage_flou(liste_DIT,
                                                 "dispersal_type",
                                                 .x,.y)[[1]])
DIT_unique<- map2(vec_var,nom_var,~fun_codage_flou(liste_DIT,
                                                   "dispersal_type",
                                                   .x,.y)[[2]] %>% ungroup()) %>% 
  join_all(by="SBS_name", type = "full") %>% 
  as_tibble()


df_traits_leda_gen <- list(
  PGF %>% ungroup(),
  CAH,
  PLS %>% ungroup(),
  AFF %>% ungroup(),
  LES,
  LEM,
  LDM,
  SLA,
  WOO %>% ungroup(),
  SGF %>% ungroup(),
  BRA %>% ungroup(),
  LED %>% ungroup(),
  BVD_unique,
  BSE_unique,
  CGO_unique,
  SNS,
  SNU,
  SEM,
  SES,
  SBL_cat %>% ungroup(),
  SBL_num,
  MOD_unique,
  SRH,
  TVE,
  BUO %>% ungroup(),
  DIT_unique
) %>% 
  join_all(by="SBS_name", type="full") %>% 
  as_tibble()



df_traits_leda <- bind_rows(df_traits_leda_gen,df_traits_leda_esp) %>% 
  setnames(colnames(.),paste0("LEDA_",tolower(colnames(.)))) 
colnames(df_traits_leda)[-1]
df_traits_leda %>% 
  mutate_all(~ifelse(is.na(.),0,1)) %>% 
  gather("var","valeur") %>%
  group_by(var) %>%
  dplyr::summarise(somme=sum(valeur)) %>% 
  arrange(desc(somme)) %>% 
  ggplot(data=.,aes(x=reorder(var,desc(somme)),y=somme))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=45, hjust=1))

#################################### FUSION des BDD #########################################
df_macro_nom <- df_macrophytes %>% dplyr::select(nom_taxo) %>% distinct()
df_traits_macrophytes <- join_all(list(
  df_traits_leda %>% plyr::rename(c("LEDA_sbs_name"="nom_taxo")),
  df_traits_pj %>% plyr::rename(c("PJ_nom_taxo"="nom_taxo")),
  df_traits_david %>% plyr::rename(c("CD_nom_taxo"="nom_taxo"))),
  type="full",
  by = "nom_taxo"
) %>% as_tibble()
vis_dat(df_traits_macrophytes)
df_traits_macrophytes %>% write.csv(file="/home/theo/Bureau/Data/Données traits/Données/traits_macrophytes.csv",row.names = FALSE)
