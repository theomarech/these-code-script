##########################################################################################################
######
######          TABLEAU PHYSICO_CHIMIE
######
##########################################################################################################
df_lac_taxons <- read_csv("/home/theo/Bureau/Data/df_lac_taxons.csv")


table_physicochimie <- dbGetQuery(con,"SELECT
  ppc.id_point_prelev,
  date_debut_prel date,
  cd_par code_par,
  nom_par,
  resultat_ana,
  cd_unite,
  lb_urf,
  sym_urf,
  cd_zone,
  cd_support,
  code_lac,
  nom_met,
  comm_met
FROM prelevement_physico_chimique ppc
INNER JOIN point_prel_eaux_surf ppes
  ON ppc.id_point_prelev = ppes.id_point_prelev
INNER JOIN res_pc rp
  ON rp.id_prelev = ppc.id_prelev
INNER JOIN tsa_parametre_par tpp
  ON tpp.cd_par = rp.cd_parametre
INNER JOIN tsa_unite_mesure_urf tumu
  ON tumu.cd_urf = rp.cd_unite
LEFT JOIN tsa_methode_met tmm
  ON tmm.cd_met = rp.cd_methode")

##############################################################################################################

write.table(table_physicochimie, file = "/home/theo/Bureau/Data/table_physico_chimie.csv")

data_pc_plot <- table_physicochimie %>%
  group_by(nom_par) %>%
  dplyr::summarise(n = n()) %>%
  arrange(desc(n))

df_pc_lac <- table_physicochimie 
# %>%
#   dplyr::filter(code_lac %in% df_lac_taxons$code_lac)

data_pc_plot_lac <- df_pc_lac %>%
  group_by(nom_par) %>%
  dplyr::summarise(n = n()) %>%
  arrange(desc(n))

ggplot(data=data_pc_plot_lac %>% slice(.,1:30))+
  geom_bar( aes(x=reorder(nom_par,n),y=n), stat = "identity")+
  theme(axis.text.x = element_text(angle =45, hjust = 0.5,vjust=0.5),
        axis.line = element_line(NULL),
        panel.grid.major.x = element_blank()) + 
  geom_text(aes(x=reorder(nom_par,n),y=n,label = n), color = "white", vjust = 2)+
  xlab(NULL)  


###################### selection, description et homogénéisation des  paramètre et de leurs unité ##########################################
vec_parametre <- data_pc_plot_lac$nom_par[1:27]

df_pc_lac <- df_pc_lac %>% dplyr::filter(nom_par %in% vec_parametre)


df_pc_lac %>% 
  group_by(nom_par) %>%
  dplyr::summarise(n=n_distinct(lb_urf)) %>%
  arrange(desc(n)) %>% 
  as.data.frame() %>% 
  head(27)


# unite selection est un tableau qui relit le nom des paramètre (température, oxygène dissous..) aux unités utilisées dans l'estimation et au nombre d'échantillonnage réalisé
unite_selection <- map2(rep(list(df_pc_lac),length(vec_parametre)),vec_parametre,
     ~.x %>% dplyr::filter(nom_par == .y) %>%
       group_by(nom_par,lb_urf,sym_urf) %>%
       dplyr::summarise(n=n()) %>% arrange(desc(n))) %>%
  bind_rows()
# vec unite correspond à toutes les unités utilisées
vec_unite <- unite_selection$sym_urf

#vec sub unite correspond uniquement aux unités relative à une mesure en masse 
vec_sub_unite <- c(str_subset(vec_unite,"/kg"),
                                str_subset(vec_unite,"poids sec"),
                                str_subset(vec_unite,"MS"))

# df pc filtre => permet de supprimer les mesures non cohérentes ou non homogénéisable avec l'unité la plus représentées ou
# lorsque l'unité ne représenté qu'un point d'échantillonnage avec une untié différentes mais homogénéisable
# je l'ai enlevé dans ce cas là pour éviter que ca soit une erreur + très très très peu de perte d'info (quand la mesure a été réalisé 2 fois sur 100000 quoi)
df_pc_filtre <- df_pc_lac %>% dplyr::filter(!sym_urf %in%(vec_sub_unite),
                                     !(nom_par=="Phosphore total" & sym_urf %in% c("%","µS/cm","µg/L")),
                                     !(nom_par=="Orthophosphates (PO4)" & sym_urf %in% c("mg(P)/L","mg/g","µg/L")),
                                     !(nom_par=="Ammonium" & sym_urf %in% c("mg(NH3)/L","mg(N)/L","µg/L")),
                                     !(nom_par=="Nitrates" & sym_urf %in% c("mg(N)/L","µg/L")),
                                     !(nom_par=="Carbone Organique" & sym_urf %in% c("%","mg/m3")),
                                     !(nom_par=="Nitrites" & sym_urf %in% c("mg(N)/L")),
                                     !(nom_par=="Ammonium" & sym_urf %in% c("mg(NH3)/L","mg(N)/L","µg/L")),
                                     !(nom_par=="Dureté totale" & sym_urf %in% c("°F","meq/L")),
                                     !(nom_par=="Titre alcalimétrique (T.A.)" & sym_urf %in% c("mmol/L")),
                                     !(nom_par=="Matières en suspension" & sym_urf %in% c("mg(O2)/L")),
                                     !(nom_par=="Azote Kjeldahl" & sym_urf %in% c("%")),
                                     !(nom_par=="Magnésium" & sym_urf %in% c("µg/L")),
                                     !(nom_par=="Potassium" & sym_urf %in% c("µg/L")),
                                     !(nom_par=="Titre alcalimétrique (T.A.)" & sym_urf == "meq/L"),
                                     !(nom_par=="Calcium" & sym_urf %in% c("meq/L")),
                                     !(nom_par=="Carbone" & sym_urf %in% c("µg/L")),)

# unite selection = les unités d'intérêts ; affiche un tableau reliant les paramètres mesurées avec les unités retenues afin de vérifier qu'on à ce qui faut apèrs avoir filtré les paramètres
unite_selection <- map2(rep(list(df_pc_filtre),length(vec_parametre)),vec_parametre,
                        ~.x %>% dplyr::filter(nom_par == .y) %>%
                          group_by(nom_par,lb_urf,sym_urf) %>%
                          dplyr::summarise(n=n()) %>% arrange(desc(n))) %>% bind_rows()

# df pc final : 
# (1) on change les unité dans celle qui va nous intéressé (le nom)
# (2) on homogénéise les valeurs ug --> mg par exemple
df_pc_final <- df_pc_filtre %>% mutate(
  unite_homo = case_when(.$nom_par == "Oxygène dissous" & .$sym_urf %in% c("mg(O2)/L") ~ "mg/L",
                         .$nom_par =="Phosphore total" & .$sym_urf %in% c("mg(P)/L","µg/L") ~ "mg/L",
                         .$nom_par =="Orthophosphates (PO4)" & .$sym_urf %in% c("mg(PO4)/L","µg/L") ~ "mg/L",
                         .$nom_par =="Ammonium" & .$sym_urf %in% c("mg(NH4)/L") ~  "mg/L",
                         .$nom_par =="Nitrates" & .$sym_urf %in% c("mg(NO3)/L","µg/L") ~  "mg/L",
                         .$nom_par =="Chlorophylle a" & .$sym_urf %in% c("mg/m3") ~  "µg/L",
                         .$nom_par =="Carbone Organique" & .$sym_urf %in% c("mg(C)/L","µg/L") ~  "mg/L",
                         .$nom_par =="Silice" & .$sym_urf %in% c("mg(SiO2)/L") ~  "mg/L",
                         .$nom_par =="Nitrites" & .$sym_urf %in% c("mg(NO2)/L") ~  "mg/L",
                         .$nom_par =="Calcium" & .$sym_urf %in% c("mg(Ca)/L","meq/L") ~  "mg/L",
                         .$nom_par =="Magnésium" & .$sym_urf %in% c("mg(Mg)/L","µg/L","meq/L") ~  "mg/L",
                         .$nom_par =="Potassium" & .$sym_urf %in% c("mg(K)/L","µg/L") ~  "mg/L",
                         .$nom_par =="Chlorures" & .$sym_urf %in% c("mg(Cl)/L") ~  "mg/L",
                         .$nom_par =="Sulfates" & .$sym_urf %in% c("mg(SO4)/L") ~  "mg/L",
                         .$nom_par =="Sodium" & .$sym_urf %in% c("mg(Na)/L") ~  "mg/L",
                         .$nom_par =="Azote Kjeldahl" & .$sym_urf %in% c("mg(N)/L") ~  "mg/L",
                         .$nom_par =="Phéopigments" & .$sym_urf %in% c("mg/m3") ~  "µg/L",
                         .$nom_par =="Carbone" & .$sym_urf %in% c("mg(C)/L","µg/L") ~  "mg/L",
                         .$nom_par =="Demande Biochimique en oxygène en 5 jours (D.B.O.5)" & .$sym_urf %in% c("mg(O2)/L") ~  "mg/L",
                         TRUE~.$sym_urf),
  resultat_ana_homo = case_when(.$nom_par %in% c("Phosphore total",
                                                 "Orthophosphates (PO4)",
                                                 "Nitrates",
                                                 "Carbone Organique",
                                                 "Magnésium",
                                                 "Potassium",
                                                 "Carbone",
                                                 "Demande Biochimique en oxygène en 5 jours (D.B.O.5)") & .$sym_urf == "µg/L" ~ .$resultat_ana/1000,
                                TRUE ~ .$resultat_ana),
) %>% dplyr::select(-c(nom_met,comm_met)) %>% as_tibble()



df_pc_final2 <- df_pc_final %>%
  mutate(resultat_ana_homo = ifelse(.$resultat_ana_homo<0,NA_real_,.$resultat_ana_homo)) %>% 
  na.omit() %>% 
  mutate(resultat_ana_homo = dplyr::case_when(.$nom_par == "Oxygène dissous" & .$resultat_ana_homo > 60 ~ NA_real_,
                                       .$nom_par == "Potentiel en Hydrogène (pH)" & .$resultat_ana_homo > 14 ~ NA_real_,
                                       .$nom_par == "Taux de saturation en oxygène" & .$resultat_ana_homo > 500 ~ NA_real_,
                                       .$nom_par == "Phosphore total" & .$resultat_ana_homo > 750 ~ NA_real_,
                                       .$nom_par == "Ammonium" & .$resultat_ana_homo > 200 ~ NA_real_,
                                       .$nom_par == "Azote Kjeldahl" & .$resultat_ana_homo > 75 ~ NA_real_,
                                       .$nom_par == "Phéopigments" & .$resultat_ana_homo > 500 ~ NA_real_,
                                       .$nom_par == "Turbidité Formazine Néphélométrique" & .$resultat_ana_homo > 3000 ~ NA_real_,
                                       TRUE ~ .$resultat_ana_homo))


df_pc_final2 %>%  ggplot(aes(x=nom_par,y=scale(resultat_ana_homo))) + geom_jitter() + geom_violin()+facet_wrap(~nom_par,scale = "free")

df_pc_final2 %>%  ggplot(aes(x=resultat_ana_homo,col=nom_par)) + geom_density()+facet_wrap(~nom_par,scale = "free")+theme(legend.position = "none")



# données physico-chimique homogénéisée + support et zone propspecté => c le tableau sur lequel il faut travailler
df_pc_final3 <- df_pc_final2 %>%
  inner_join(cd_support,by=c("cd_support"="cd_sup")) %>%
  inner_join(cd_zone_verticale,by=c("cd_zone"="cd")) %>% mutate(
  annee = lubridate::year(date),
  mois = lubridate::month(date),
  saison = case_when(mois %in% c(1,2,3)~"hiver",
                     mois %in% c(4,5,6)~"printemps",
                     mois %in% c(7,8,9)~"ete",
                     TRUE~ "automne")) %>%
  dplyr::select(-c(resultat_ana,cd_support,cd_zone,cd_unite,sym_urf,code_par,lb_urf)) %>% 
  mutate(nom_par = nom_par %>% 
           str_replace_all("[éêè]","e") %>% 
           str_replace_all("[àâ]","a") %>% 
           str_replace_all("[:punct:]","") %>% 
           str_replace_all(" ","_") %>% 
           str_replace_all("_a_25°C","") %>% 
           str_replace_all("demande_biochimique_en_oxygene_en_5_jours_","") %>% 
           str_replace_all("potentiel_en_hydrogene_","") %>% 
           str_replace_all("_po4","") %>% 
           tolower())

df_pc_final3$nom_par %>% unique()
s <- "demande_biochimique_en_oxygene_en_5_jours_dbo5"
str_replace_all(s,"demande_biochimique_en_oxygene_en_5_jours_","")
# moyenne et médiane des valeurs par saison au sein des lacs pour chaque année et chaque paramètre
# si on enlève les données d'automne on passe de 1200 à 800 combinaison annee lacs avec trois saisons
# dans tous les cas on se rend compte qu'on aura pas les données pour chaque année pour chaque taxons
# par exemple macrophytes environ 300 lacs
# on peut par contre supprimer les années trop éloignées dans la moyenne
# moyenne par année sur trois saisons (sauf automne qui limite beaucoup sinon)
write
df_pc_final3.1 <- df_pc_final3%>% 
  dplyr::filter(saison != "automne", annee >= 2005) %>%
  group_by(code_lac,annee) %>% 
  nest() %>% 
  mutate(n_sais = map_dbl(data, ~dplyr::select(.,saison) %>% n_distinct())) %>% 
  dplyr::filter(n_sais == 3) %>% 
  unnest() %>% 
  ungroup() %>% 
  group_by(code_lac,annee,nom_par,saison) %>% 
  dplyr::summarise(pc_mean = mean(resultat_ana_homo,na.rm=TRUE),
                   pc_med = median(resultat_ana_homo,na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(code_lac,annee,nom_par)  %>% 
  dplyr::summarise(pc_mean = mean(pc_mean,na.rm=TRUE),
                   pc_med = mean(pc_med,na.rm=TRUE)) %>%
  ungroup() 

df_pc_final3.1.1 <- df_pc_final3.1 %>% 
  group_by(code_lac,nom_par) %>% 
  dplyr::summarise(pc_mean = mean(pc_mean),
                   pc_med = mean(pc_med)) %>% 
  ungroup() %>% 
  dplyr::select(-pc_med) %>% 
  spread(nom_par,pc_mean,fill = NA) %>% 
  dplyr::select(-c(carbone,titre_alcalimetrique_ta))%>% 
  setnames(colnames(.)[-1],paste0("PC_",colnames(.)[-1])
           
write.csv(df_pc_final3,"/home/theo/Bureau/Data/Données finales/PC/pc_travail.csv",row.names = FALSE)

write.csv(df_pc_final3.1.1,"/home/theo/Bureau/Data/Données finales/PC/pc_toutes_annees.csv",row.names = FALSE)

################################################################## TABLE MESURES ENVIRONNEMENTALES #########################################################
table_mesure_env <- dbGetQuery(con,"SELECT
  me.id_point_prelev,
  date_mesure_cond,
  resultat_mesure_cond,
  cd_unite,
  DISTINCT nom_par,
  code_lac,
  sym_urf,
  lb_urf
  FROM mesure_environnementale me
  INNER JOIN tsa_unite_mesure_urf tumu
    ON tumu.cd_urf = me.cd_unite
  INNER JOIN prelevement_physico_chimique ppc
    ON ppc.id_point_prelev = ppc.id_point_prelev
  INNER JOIN point_prel_eaux_surf ppes
    ON ppes.id_point_prelev = ppc.id_point_prelev
  INNER JOIN tsa_parametre_par tpp
    ON tpp.cd_par = me.cd_parametre
  FETCH FIRST 100 ROWS ONLY
  ");table_mesure_env
str_subset(dbListTables(con),"mesure_env")

table_mesure_env <- dbGetQuery(con,"SELECT
  nom_par,
  COUNT (nom_par)
  FROM mesure_environnementale me
  INNER JOIN tsa_unite_mesure_urf tumu
    ON tumu.cd_urf = me.cd_unite
  INNER JOIN prelevement_physico_chimique ppc
    ON ppc.id_point_prelev = ppc.id_point_prelev
  INNER JOIN point_prel_eaux_surf ppes
    ON ppes.id_point_prelev = ppc.id_point_prelev
  INNER JOIN tsa_parametre_par tpp
    ON tpp.cd_par = me.cd_parametre
  GROUP BY nom_par
  ORDER BY 2 DESC
  ");table_mesure_env


  



df1 <- df_lac_taxons %>% mutate(annee = lubridate::year(date))
df2 <- df_pc_final3 %>%  mutate(annee = lubridate::year(date)) %>%
  dplyr::select(-c(code_par,id_point_prelev))
fun_lag_annee <- function(df1,df2,lag=6){
  df <- df1 %>% 
    group_by(annee,code_lac) %>% 
    nest() %>% 
    inner_join(df2 %>% group_by(code_lac) %>% nest(), by = 'code_lac') %>% 
    mutate(lag_annee = map(annee, ~seq(.-(lag-1),., by = 1)),
           df_pc = pmap(list(lag_annee,data.y), ~..2 %>%
                          filter(annee %in% ..1) %>% 
                          mutate(nom_par = tolower(nom_par) %>%
                                   str_replace_all(c("°|[:punct:]|(|)"),"") %>% 
                                   str_replace_all(" ","_") %>%
                                   stri_trans_general(id = "Latin-ASCII")) %>% 
                          group_by(nom_par) %>%
                          dplyr::summarise(resultat_ana_mean = mean(resultat_ana_homo,na.rm=TRUE)) %>%
                          spread(nom_par,resultat_ana_mean)))  
  df_final <- inner_join(df %>% dplyr::select(code_lac,annee,data.x) %>% unnest(data.x),
                         df %>% dplyr::select(code_lac,annee,df_pc) %>% unnest(df_pc),
                         by=c("code_lac","annee")) %>% return()
}

df_pc_taxons <- fun_lag_annee(df1,df2,lag=6)
write.csv(df_pc_taxons,"/home/theo/Bureau/Data/Données finales/PC/physico_chimie.csv",row.names=FALSE)

df_pc_taxons %>% ungroup()%>% dplyr::filter(nom_taxo %in% vec_nom) %>% 
  
  dplyr::select(code_lac,nom_taxo,abondance,temperature_de_leau) %>% 
  group_by(code_lac,nom_taxo) %>% 
  dplyr::summarise_all(mean,na.rm=TRUE) %>% 
  ggplot(aes(x=temperature_de_leau,y=abondance,col=nom_taxo))+
  geom_point()+
  facet_wrap(~nom_taxo)+theme(legend.position = "none")+
  geom_smooth(method = "lm")


colnames(df_pc_taxons)
vec_nom <- fun_table_taxo(df_pc_taxons%>% ungroup()%>% dplyr::filter(libelle=="phytoplancton"))[[1]] %>% dplyr::filter(prop>60) %>% .$nom_taxo

sum(dummy$trait$num1*dummy$abun[1,],na.rm=TRUE)/sum(dummy$abun[1,],na.rm=TRUE)

dummy$trait
dummy$abun
functcomp(dummy$trait,dummy$abun)






################### MAJ 11/01/2021 : objectif récupérer un tableau de données non centrée réduit ni logé----------------------------

table_pc <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES TRAVAIL/PC/pc_travail.csv") 
table_pc %>% dplyr::select(nom_par,unite_homo) %>% table() # ok pas de problème d'unité
table_pc %>% dplyr::select(nom_par,unite_homo) %>% distinct() %>% View()
table_pc$saison %>% count() %>% ggplot(aes(x = x,y = freq))+geom_bar(stat = "identity")

table_pc %>%  ggplot(aes(x = resultat_ana_homo))+geom_density()+facet_wrap(~nom_par, scale="free")

data <- table_pc %>%
  group_by(code_lac, nom_par, annee,libelle, saison) %>% 
  dplyr::summarise(moy= mean(resultat_ana_homo,na.rm=TRUE),
                   med= median(resultat_ana_homo,na.rm=TRUE)) %>%
  ungroup() %>% 
  dplyr::filter(annee >= 2005, saison != "automne") %>% 
  group_by(code_lac, annee, nom_par,libelle) %>% nest() %>% 
  mutate(n_sais = map_dbl(data,~nrow(.)),
         nom_sais = map_chr(data,~.$saison %>% sort() %>% paste0(collapse = "-") ),
         ete = map_dbl(nom_sais, ~str_detect(.,"ete")),
         pri = map_dbl(nom_sais, ~str_detect(.,"printemps")),
         hiv = map_dbl(nom_sais, ~str_detect(.,"hiver")))


a <- data %>% group_by(code_lac,nom_par) %>% dplyr::summarise(nete = sum(ete),
                                                               npri = sum(pri),
                                                               nhiv = sum(hiv)) %>% mutate_if(is.numeric,~ifelse(.>0,1,0))


data_pc_pri <- pca_data <- data%>%
  unnest(data)  %>%
  ungroup() %>% 
  dplyr::filter(saison == "printemps") %>% 
  group_by(code_lac,nom_par,libelle) %>% 
  dplyr::summarise(med = mean(moy,na.rm = TRUE)) %>% 
  spread(nom_par,med) %>% 
  dplyr::select(-c(code_lac,carbone))  %>% dplyr::select(ammo=ammonium,azote=azote_kjeldahl,carb=carbone_organique,chla=chlorophylle_a,
                              dbo5=demande_biochimique_en_oxygene_en_5_jours_dbo5,mes=matieres_en_suspension,no2=nitrites,
                              no3=nitrates,ortho = orthophosphates_po4,pheo=pheopigments,phos=phosphore_total)

data_pc_pri %>% ungroup %>%  select_if(is.numeric) %>%mutate_all(~fun_boxcox(.)) %>%  PCA()
pca_data %>% column_to_rownames() %>%  PCA()



data_pc_peh <-data %>% dplyr::filter(nom_sais == "ete-hiver-printemps") %>%
  unnest(data) %>% 
  ungroup() %>% 
  group_by(code_lac,nom_par,annee) %>% 
  dplyr::summarise(med = mean(moy,na.rm=TRUE)) %>%
  group_by(code_lac,nom_par) %>%
  dplyr::summarise(val=median(med,na.rm=TRUE)) %>% 
  ungroup() %>% 
  spread(nom_par,-code_lac) %>% dplyr::select(-carbone)
data_pc_peh$chlorophylle_a %>% log() %>%  hist(breaks = 30)
plot(density(na.omit(data_pc_peh$chlorophylle_a %>% log() )))
data_pc_peh %>% vis_dat

write.csv(data_pc_pri,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/ENV/ENV/env_physicochimie_pri.csv",row.names = FALSE)

df <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/ENV/ENV/env_physicochimie.csv")

df_impute <- df %>% column_to_rownames() %>% missForest()

impute <- df_impute %>% .$ximp %>% mutate_all(~fun_boxcox(.+1,test=FALSE)) 

impute %>%  gather(var,val) %>% ggplot(aes(x=val))+geom_density()+facet_wrap(~var,scale="free")
 impute %>% select_if(is.numeric) %>%  PCA()
impute %>% cor() %>% corrplot::corrplot() 
plot(df$conductivite~df$durete_totale)

df %>% gather(var,val,-code_lac) %>% group_by(var) %>% dplyr::summarise(mean = mean(val,na.rm=TRUE),
                                                                        min = min(val,na.rm=TRUE),
                                                                        max = max(val, na.rm=TRUE),
                                                                        sd = SD(val,na.rm =TRUE)) %>% View()



# calcul variabilité sur les lacs communs à poi mac phy -------------------
load("C:/Users/marec/OneDrive/Bureau/R/these/publication_1/data/donnees_all_compile.RData")

lac_commun <- join_all(liste_final[c("poi","mac","phy")], type="inner") %>% pull(code_lac)


data <- table_pc %>% dplyr::filter(code_lac %in% lac_commun) %>% 
  group_by(code_lac, nom_par, annee, saison) %>% 
  nest()
  
data$data[[2]] 
  dplyr::summarise(moy= mean(resultat_ana_homo,na.rm=TRUE),
                   med= median(resultat_ana_homo,na.rm=TRUE)) %>%
  ungroup() %>% 
  dplyr::filter(annee >= 2005, saison != "automne") %>% 
  group_by(code_lac, annee, nom_par) %>% nest() %>% 
  mutate(n_sais = map_dbl(data,~nrow(.)),
         nom_sais = map_chr(data,~.$saison %>% sort() %>% paste0(collapse = "-") ),
         ete = map_dbl(nom_sais, ~str_detect(.,"ete")),
         pri = map_dbl(nom_sais, ~str_detect(.,"printemps")),
         hiv = map_dbl(nom_sais, ~str_detect(.,"hiver")))


a <- data %>% group_by(code_lac,nom_par) %>% dplyr::summarise(nete = sum(ete),
                                                              npri = sum(pri),
                                                              nhiv = sum(hiv)) %>% mutate_if(is.numeric,~ifelse(.>0,1,0))

data_pc_printemps %>% vis_dat
data_pc_pri <- pca_data <- data%>%
  unnest(data)  %>%
  ungroup() %>% 
  dplyr::filter(saison == "printemps") %>% 
  group_by(code_lac,nom_par) %>% 
  dplyr::summarise(med = mean(moy,na.rm = TRUE)) %>% 
  spread(nom_par,med) %>% 
  dplyr::select(-c(code_lac,carbone)) 


data_pc_printemps %>% vis_dat()

pca_data %>% column_to_rownames() %>%  PCA()



data_pc_peh <-data %>% dplyr::filter(nom_sais == "ete-hiver-printemps") %>%
  unnest(data) %>% 
  ungroup() %>% 
  group_by(code_lac,nom_par,annee) %>% 
  dplyr::summarise(med = mean(moy,na.rm=TRUE)) %>%
  group_by(code_lac,nom_par) %>%
  dplyr::summarise(val=median(med,na.rm=TRUE)) %>% 
  ungroup() %>% 
  spread(nom_par,-code_lac) %>% dplyr::select(-carbone)
data_pc_peh$chlorophylle_a %>% log() %>%  hist(breaks = 30)
plot(density(na.omit(data_pc_peh$chlorophylle_a %>% log() )))

