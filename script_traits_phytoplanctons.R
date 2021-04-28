###################################################################################################################################
########
########                        TRAITS PHYTOPLANCTONS
########
####################################################################################################################################
source("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/scriptR/librairies.R",encoding = "utf8")


################################################ BASE DE DONNEES CHRISTOPHE ##############################################
table_phyto_tra <- read_ods("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DONNEES TRAITS PHYTO/BDD_TRAVAIL/BDD_originale_travail.ods") %>% 
  mutate(Taxa_Name = str_replace_all(Taxa_Name," ", "_"))

table_phyto_tax <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES TRAVAIL/communautés/df_phytoplanctons")

##################### Table de contingence pour biovolume et abondance ########################
table_phyto_biovolume <- table_phyto_tax %>% 
  dplyr::select(nom_taxo = nom_ref, code_lac,biovolume) %>% 
  mutate(nom_taxo = str_replace_all(nom_taxo," ","_")) %>% 
  group_by(code_lac,nom_taxo) %>% 
  dplyr::summarise(m = mean(biovolume,na.rm=TRUE)) %>% 
  dplyr::filter(m!=0) %>% 
  spread(nom_taxo,m,fill=0)

write.csv(table_phyto_biovolume,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES DIVERSITES/phytoplancton_biovolume.csv",row.names = FALSE)

table_phyto_abondance <- table_phyto_tax %>% 
  dplyr::select(nom_taxo = nom_ref, code_lac,abondance) %>% 
  mutate(nom_taxo = str_replace_all(nom_taxo," ","_")) %>% 
  group_by(code_lac,nom_taxo) %>% 
  dplyr::summarise(m = mean(abondance,na.rm=TRUE)) %>% 
  dplyr::filter(m!=0) %>% 
  spread(nom_taxo,m,fill=0) 

write.csv(table_phyto_biovolume,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES DIVERSITES/phytoplancton_abondance.csv",row.names = FALSE)
  
##################### Jointure pour les traits ########################
table_tra_abondance <- table_phyto_tra %>% dplyr::filter(Taxa_Name %in% colnames(table_phyto_abondance))
write.csv(table_tra_abondance ,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_phytoplancton_abondance.csv",row.names = FALSE)

table_tra_biovolume <- table_phyto_tra %>% dplyr::filter(Taxa_Name %in% colnames(table_phyto_biovolume))
write.csv(table_tra_biovolume ,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_phytoplancton_biovolume.csv",row.names = FALSE)


table_tax_abondance <- table_phyto_abondance %>% dplyr::select(all_of(table_trait_abondance$Taxa_Name))
write.csv(table_tax_abondance ,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_phytoplancton_abondance.csv",row.names = FALSE)

table_tax_biovolume <- table_phyto_biovolume %>% dplyr::select(all_of(table_trait_biovolume$Taxa_Name))
write.csv(table_tax_biovolume,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_phytoplancton_biovolume.csv",row.names = FALSE)



################################################ BASE DE DONNEES Rimet ##############################################

table_phyto_rim <- read_ods("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DONNEES TRAITS PHYTO/BDD_TRAVAIL/BDD_originale_Rimet.ods") %>% 
  mutate(nom_taxo = str_replace_all(nom_taxo_avant," ", "_")) %>% 
  mutate_at(.vars = vars(matches("Colony_|Cumulated_")),~ifelse(Nb_cells_colony==1,0,.))

table_phyto_rim %>% vis_dat


##### Exploration #####

table_phyto_rim_num <- table_phyto_rim %>% 
  as_tibble %>% 
  dplyr::select_if(is.numeric) %>% 
  mutate(cell_surf_volume = Cell_surface/Cell_biovolume,
         colo_surf_volume = ifelse(Nb_cells_colony >1,Colony_surface_avec_mucilage/Colony_biovolume_sans_mucilage,0)) %>% 
  mutate_all(~log(.+1)) %>%  
  as.data.frame() %>% 
  missForest() %>% 
  .$ximp

x11();chart.Correlation(table_phyto_rim_num)
x11();mean %>% PCA()


classif <- hclust(dist(table_phyto_rim_num),method = "ward.D2")
plot(classif)
km_trait <- kmeans(scale(table_phyto_rim_num),centers=7,nstart = 20)

group_kmeans <- as.factor(km_trait$cluster)
acp <- PCA(table_phyto_rim_num[,-ncol(table_phyto_rim_num)] %>% dplyr::select(-Organic_carbon_ratio))
s.class(acp$ind$coord[,1:2],group_kmeans)

# fviz_nbclust(scale(table_phyto_rim_num), kmeans, method = "wss") +
#   geom_vline(xintercept = 4, linetype = 2)+
#   labs(subtitle = "Elbow method")
# fviz_nbclust(scale(table_phyto_rim_num), kmeans, method = "silhouette") +
#   geom_vline(xintercept = 4, linetype = 2)+
#   labs(subtitle = "Elbow method")
# fviz_nbclust(scale(table_phyto_rim_num), kmeans, method = "gap_stat") +
#   geom_vline(xintercept = 4, linetype = 2)+
#   labs(subtitle = "Elbow method")


################################################## JOINTURE DES DEUX TABLES #######################################################
table_phyto_rim_num <- table_phyto_rim %>% 
  as_tibble %>% 
  dplyr::select_if(is.numeric) 

table_phyto_rim1 <- table_phyto_rim_num %>%
  mutate(Taxa_Name = table_phyto_rim$nom_taxo_avant) %>% 
  mutate(Taxa_Name = str_replace_all(Taxa_Name,"_sp.","") %>% str_replace_all(" ","_"))

table_phyto_rim2 <- table_phyto_rim_num %>% 
  mutate(Taxa_Name = table_phyto_rim$synonyme1) %>% 
  mutate(Taxa_Name = str_replace_all(Taxa_Name,"_sp.","") %>% str_replace_all(" ","_"))

table_phyto_rim3 <- table_phyto_rim_num %>% 
   mutate(Taxa_Name = table_phyto_rim$synonyme2) %>% 
  mutate(Taxa_Name = str_replace_all(Taxa_Name,"_sp.","") %>% str_replace_all(" ","_"))

table_phyto_rim4 <- table_phyto_rim_num %>% 
  mutate(Taxa_Name = table_phyto_rim$synonyme3) %>% 
  mutate(Taxa_Name = str_replace_all(Taxa_Name,"_sp.","") %>% str_replace_all(" ","_"))

table_phyto_rim_num_genre <- table_phyto_rim1 %>%
  mutate(Taxa_Name = str_extract(Taxa_Name,"[:alpha:]{1,}(?=\\_)")) %>% 
  group_by(Taxa_Name) %>% 
  summarise_all(~mean(.,na.rm=TRUE))

jointure_1 <- table_phyto_tra %>% inner_join(table_phyto_rim1)
nom_diff <- anti_join(table_phyto_tra,table_phyto_rim1) %>% dplyr::select(Taxa_Name)

jointure_2<- table_phyto_tra %>% 
  dplyr::filter(Taxa_Name %in% nom_diff$Taxa_Name) %>% 
  inner_join(table_phyto_rim_num_genre)


nom_diff2 <- nom_diff %>% dplyr::filter(!Taxa_Name %in% jointure_2$Taxa_Name)
table_phyto_tra2  <- table_phyto_tra %>% dplyr::filter(Taxa_Name %in% nom_diff2$Taxa_Name)
jointure_3 <-table_phyto_tra2 %>% inner_join(table_phyto_rim2)


nom_diff3 <- nom_diff2%>% dplyr::filter(!Taxa_Name %in% jointure_3$Taxa_Name)
table_phyto_tra3  <- table_phyto_tra %>% dplyr::filter(Taxa_Name %in% nom_diff3$Taxa_Name)
jointure_4 <-table_phyto_tra3 %>% inner_join(table_phyto_rim3)

nom_diff4 <- nom_diff3%>% dplyr::filter(!Taxa_Name %in% jointure_4$Taxa_Name)
table_phyto_tra4  <- table_phyto_tra %>% dplyr::filter(Taxa_Name %in% nom_diff4$Taxa_Name)
jointure_5 <-table_phyto_tra4 %>% inner_join(table_phyto_rim4)


jointure_finale <- bind_rows(jointure_1,
                             jointure_2,
                             jointure_3,
                             jointure_4,
                             jointure_5
                            )

nom_var_num_tra  <- c("max_Lenght",
                      "Cell_Biovolume",
                      # "Ind_Biovolume",
                      "Carbon", 
                      "Min_Width", 
                      "Max_Width", 
                      "Min_Length")

table_impute <- left_join(table_phyto_tra,jointure_finale) %>% 
  dplyr::select(-c(Species,Genus,Order,Family,Phytobs_Taxa_Code,Author)) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.binaire,as.logical) %>%
  mutate(Nb_cells_colony = as.integer(Nb_cells_colony),
         Nb_Flagellum = factor(Nb_Flagellum,order=TRUE)) %>%
  mutate_at(c(16,17,18,21,29,39,40,73),factor,order=TRUE) %>% 
  mutate(Ind_Biovolume = ifelse(Ind_Biovolume=="#NA",NA,Ind_Biovolume)) %>% 
  .[-391,] 


write.csv(table_impute, "C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/tableau_phyto_impute.csv",row.names = FALSE)


data <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DONNEES TRAITS PHYTO/BDD_TRAVAIL/tableau_phyto_impute.csv") %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.binaire,as.logical) %>%
  mutate(Nb_cells_colony = as.integer(Nb_cells_colony),
         Nb_Flagellum = factor(Nb_Flagellum,order=TRUE),
         Maximal_length_algal_object = as.numeric(Maximal_length_algal_object)) %>%
  mutate_at(c(16,17,18,21,29,39,40,74),factor,order=TRUE) %>% 
  column_to_rownames() %>% 
  as.data.frame() %>% 
  missForest(maxiter = 40, 
             ntree = 100,
             variablewise = TRUE)
vis_dat(data$ximp)
colnames(data)


table_finale <- data$ximp  %>% 
  tibble::rownames_to_column(var='nom_taxo') %>% 
  mutate(cell_surf_volume = Cell_surface/Cell_biovolume,
         colo_surf_volume = ifelse(Nb_cells_colony >1,Colony_surface_avec_mucilage/Colony_biovolume_sans_mucilage,0)) %>% 
  mutate_if(is.numeric,~log(.+1)) %>%  
  as.data.frame() %>% 
  mutate(Res_env = ifelse(Akinete == TRUE | Cyst == TRUE, TRUE,FALSE)) %>% 
  dplyr::select(nom_taxo,
                Toxin,
                Life_Form,
                # Life_Form2,
                Motility,
                Organic_carbon_ratio,
                Nutrition,
                Reproduction,
                Res_env,
                Min_Width,
                Min_Length,
                Nb_cells_colony,
                Cell_Biovolume,
                Ind_Biovolume,
                cell_surf_volume,
                Maximal_length_algal_object,
                # Colony_surface_avec_mucilage,
                
                
                )
colnames(table_finale)
table_finale %>% dplyr::select_if(is.numeric) %>% chart.Correlation()
write.csv(table_finale,"C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/tableau_phyto_impute.csv",row.names = FALSE)
colnames(table_finale)
table_finale %>% dplyr::mutate(res_env =)






