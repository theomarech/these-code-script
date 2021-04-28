
dir <- setwd("/home/theo/Bureau/Data/Données finales/communautés/") %>% dir()

list_multitaxa <- map(dir, ~read_csv(.[[1]]) %>%
                        mutate(nom_taxo = nom_taxo %>% str_replace_all(" ", "_") %>% tolower()) %>%
                        group_by(code_lac, nom_taxo) %>%
                        dplyr::summarise(ab = mean(biomasse, na.rm = TRUE)) %>%
                        spread(nom_taxo, ab, fill = 0))
df_poisson <-
  read_csv("/home/theo/Bureau/Data/Données finales/communautés/df_poissons.csv")  %>%
  mutate(nom_taxo = nom_taxo %>% str_replace_all(" ", "_") %>% tolower()) %>%
  group_by(code_lac, nom_taxo) %>%
  dplyr::summarise(ab = mean(biomasse, na.rm = TRUE)) %>%
  spread(nom_taxo, ab, fill = 0) %>% fun_div()%>% 
  dplyr::select(code_lac,
                poisson_shannon = shannon,
                poisson_richesse = richesse,
                poisson_simpson = simpson)
df_macrophyte <-
  read_csv("/home/theo/Bureau/Data/Données finales/communautés/df_macrophytes.csv") %>%
  mutate(nom_taxo = nom_taxo %>% str_replace_all(" ", "_") %>% tolower()) %>%
  group_by(code_lac, nom_taxo) %>%
  dplyr::summarise(ab = mean(abondance, na.rm = TRUE)) %>%
  spread(nom_taxo, ab, fill = 0) %>% fun_div()%>% 
  dplyr::select(code_lac,
                macro_shannon = shannon,
                macro_richesse = richesse,
                macro_simpson = simpson)
df_phytoplancton <-
  read_csv("/home/theo/Bureau/Data/Données finales/communautés/df_phytoplanctons.csv") %>% 
  mutate(nom_taxo=nom_ref) %>%
  mutate(nom_taxo = nom_taxo %>% str_replace_all(" ", "_") %>% tolower()) %>%
  group_by(code_lac, nom_taxo) %>%
  dplyr::summarise(ab = mean(abondance, na.rm = TRUE)) %>%
  spread(nom_taxo, ab, fill = 0) %>% fun_div()%>% 
  dplyr::select(code_lac,
                phyto_shannon = shannon,
                phyto_richesse = richesse,
                phyto_simpson = simpson)
df_macroinvertebres <-
  read_csv("/home/theo/Bureau/Data/Données finales/communautés/df_macroinvertebres.csv") %>%
  mutate(nom_taxo = nom_taxo %>% str_replace_all(" ", "_") %>% tolower()) %>%
  group_by(code_lac, nom_taxo) %>%
  dplyr::summarise(ab = mean(abondance, na.rm = TRUE)) %>%
  spread(nom_taxo, ab, fill = 0) %>% ungroup() %>%na.omit() %>%  fun_div() %>% 
  dplyr::select(code_lac,
                macroinv_shannon = shannon,
                macroinv_richesse = richesse,
                macroinv_simpson = simpson)

multitaxon <- join_all(list(df_phytoplancton,df_macrophyte,df_macroinvertebres,df_poisson),type="inner",by="code_lac") %>% distinct()
multitaxon <- join_all(list(df_phytoplancton,df_poisson,df_macrophyte),type="inner",by="code_lac")
multitaxon <- bind_rows(df_phytoplancton,df_poisson,df_macrophyte) %>% dplyr::select(date,libelle,abondance,code_lac,nom_taxo)

############################################################################################################


multitaxon %>% select_if(is.numeric) %>%  PCA()
multitaxon %>% select_if(is.numeric) %>% chart.Correlation()




richesse <- multitaxon %>% dplyr::select(matches("sim")) %>% mutate_all(log)

richesse %>% PCA()
richesse %>% chart.Correlation(method="spearman")





########" macroinv - phyto
multitaxon <- join_all(list(df_phytoplancton,df_macrophyte),type="inner",by="code_lac") 
richesse <- multitaxon %>% dplyr::select(matches("richesse"))

ggplot(data=richesse, aes(x=phyto_richesse,y=macro_richesse))+geom_point()+geom_smooth(method = "lm",formula=y~I(x^2))
ggplot(data=richesse, aes(x=phyto_richesse,y=macro_richesse))+geom_point()+geom_smooth(method="lm",formula=y~poly(x,3))

ml_ric_inv_m <- lm(macro_richesse~poly(phyto_richesse,2),data=richesse)
summary(ml_ric_inv_m)
plot(ml_ric_inv_m)
hist(ml_ric_inv_m$res)

