##########################################################################################################
######                                                                                          ##########
######                                 TABLEAU BASSIN VERSAN                                    ########## 
######                                                                                          ##########    
##########################################################################################################


source("/home/theo/Bureau/scriptR/librairies.R")

########################### CONNEXION BDD PLAN D'EAU  ############################

drv <- dbDriver("PostgreSQL") 

con <- dbConnect(
  drv,
  dbname = "bd_plando",
  host = "195.221.114.149",
  port = 5434,
  user = "theo.marechal",
  password = theo
)


##########################################"" Bassin versans ####################################################

table_bv <- dbGetQuery(con,"SELECT * FROM bv") 

table_bv$valid %>% unique()

df_lac_bv <- table_bv %>% 
  as_tibble() %>% 
  dplyr::filter(code_lac %in% df_lac$code_lac,
         valid %in% c("Correct","Incertain")) %>% 
  mutate(perim_km = ifelse(is.na(perim_km) & is.na(perim_km_old)!=TRUE,perim_km_old,perim_km),
         surf_km2 = ifelse(is.na(surf_km2) & is.na(surf_km2_old)!=TRUE,surf_km2_old,surf_km2)) %>% 
  dplyr::select(-c(valid,coord_x,coord_y,surf_hors_fr,cd_proj,frontiere,perim_km_old,surf_km2_old,rq_valid))

vis_dat(df_lac_bv)
dataPCA <- df_lac_bv %>% dplyr::select(-c("roche_predom","code_lac")) %>% mutate_all(as.numeric) %>% as.data.frame() %>% missForest::missForest()

log(dataPCA$ximp+1) %>% PCA(axes = c(3,4))
PerformanceAnalytics::chart.Correlation(log(dataPCA$ximp+1))
fun_pca(dudi.pca(log(dataPCA$ximp+1),scannf = FALSE,nf=),biplot = FALSE)

df_lac_bv %>% write.csv("/home/theo/Bureau/Data/Données finales/bassins versans/table_bv.csv",row.names=FALSE)

##########################################"" Bassin versans occ sol####################################################

table_bv_occ_sol <- map(c("2006","2012","2018"), 
                        ~dbGetQuery(con, paste0("SELECT * FROM bv_clc",.)) %>%
                          as_tibble() %>%
                          mutate(annee = .x)) %>%
  bind_rows() %>%
  write.csv("/home/theo/Bureau/Data/Données finales/bassins versans/table_bv_occ_sol_all_years.csv",row.names=FALSE)

table_bv_occ_sol %>% gather(var,val,-c(code_lac,annee)) %>% ggplot(aes(x=annee,y=val,col=annee))+geom_jitter()+facet_wrap(.~var,scale="free_y")
table_bv_occ_sol %>%  dplyr::select(-code_lac) %>% mutate_if(is.character,as.factor) %>% PCA(quali.sup = ncol(.))
dbListTables(con) %>% str_subset("bv_clc")


########################################## Corridor rivulaire ####################################################
lac_corridor <- dbGetQuery(con, "SELECT * FROM corridor")
##########################################"" BATHYMETRIE ####################################################

lac_morpho <- dbGetQuery(con, "SELECT * FROM bathy_morpho") %>% dplyr::filter(code_lac %in% pc_poisson$code_lac)
