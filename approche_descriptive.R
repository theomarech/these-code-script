############################################# APPROCHE DESCRIPTIVE SUR LA TABLE [plan_eau] ##################################################

rm(list = ls())
source("/home/theo/Bureau/scriptR/connectionBDDplando.R")

##La question est : est-ce que la variabilité des conditions des lacs ne va pas cacher le signal fonctionnelle
##est-ce qu'il faut étudier des groupes de lacs séparéments ?
## qu'elles sont les variables qui classifient le mieux les lacs



############## [plan_eau] ################## => table_lac
str(table_lac)
head(table_lac)
colnames(table_lac)
gg_miss_var(table_lac)
vis_dat(table_lac, )

##### CLUSTERING DES LACS ~~~~~~~~~

### Sélection des variables
lac <- table_lac %>% dplyr::select(
  code_lac,
  long_pla,
  lat_pla,
  superficie_pla,
  altitude_pla,
  perimetre_pla,
  volume_pla,
  marnage_pla,
  prof_max_pla,
  prof_moy_pla,
  prof_moy_pla_calculee,
  longueur_max_pla,
  tps_sejour_moy_pla,
  tps_sejour_moy_calc,
  alcalinite
)
nrow(lac)
lac2 <- lac[,-1] %>% map_df(~as.numeric(.)) %>% bind_cols(lac[,"code_lac"])

lac2 <- as.data.frame(lac2)
rownames(lac2) <- lac2$code_lac
lac3 <- lac2[,-length(lac2)]

### imputation de données avec RF et PCA (puis PCA pour regarder quelles variables retenir)

df_impute_PCA <- imputePCA(lac3)[[1]]
df_impute_RF <- missForest(lac3)[[1]]

## ACP => on enlève LEM74 et PSA973 qui tire sur les axes ici (très grandes surface/volume)

PCA(df_impute_PCA[-which(rownames(df_impute_PCA)=="LEM74"|rownames(df_impute_PCA)=="PSA973"),-c(1:2)])
pca_pca <- dudi.pca(scale(df_impute_PCA[-which(rownames(df_impute_PCA)=="LEM74"|rownames(df_impute_PCA)=="PSA973"),-c(1:2)]))
# Dim1 = 38.26% Dim2 = 16.85%
#perimetre_pla /longueur_max_pla/prof_moy_pla/tps_sejour_moy_pla/altitude_pla (à vérifier avec les cos)

PCA(df_impute_RF[-which(rownames(df_impute_RF)=="LEM74"|rownames(df_impute_RF)=="PSA973"),-c(1:2)])
pca_rf<- dudi.pca(scale(df_impute_RF[-which(rownames(df_impute_RF)=="LEM74"|rownames(df_impute_RF)=="PSA973"),-c(1:2)]))
# Dim1 = 36.68% Dim2 = 16.73%
#alcalinite, longueur_max_pla, profm_moy_pla, altitude_pla (à vérifier avec les cos)

s.label(pca_rf$li,clabel=0.2)


##### classification 



## CAH
tbl_df(df_impute_PCA)
d <- dist(pca_pca$li)

dendro1 = hclust(d, method = "single")
dendro2 = hclust(d, method = "complete")
dendro3 = hclust(d, method = "average")
dendro4 = hclust(d, method = "centroid")
dendro5 = hclust(d, method = "ward.D")
dendro6 = hclust(d, method = "ward.D2")

d1 = cophenetic(dendro1)
d2 = cophenetic(dendro2)
d3 = cophenetic(dendro3)
d4 = cophenetic(dendro4)
d5 = cophenetic(dendro5)
d6 = cophenetic(dendro6)

cor(d1,d)
cor(d2,d)
cor(d3,d)# m?thode average est la meilleure
cor(d4,d)
cor(d5,d)
cor(d6,d)


sum((d1-d)^2)
sum((d2-d)^2)
sum((d3-d)^2)# crit?re choisi avec cette m?thode aussi 
sum((d4-d)^2)
sum((d5-d)^2)
sum((d6-d)^2)

plot(dendro3)

b <- cutree(dendro3, k = 4)

france <- getData(name = "GADM",
                  country = "FRA",
                  level = 2)
plot(france)
points(lac3$long_pla, lac3$lat_pla, col = b, lwd = 1,pch=17)




## k_mean
cm <- kmeans(scale(df_impute_PCA), centers = 6, nstart = 25)

cm$cluster

plot(france)
points(lac3$long_pla, lac3$lat_pla, col = cm$cluster, lwd = 1,pch=19)

cm = kmeans(scale(jv73$morpho),3, nstart = 25) # r?sultat final est celui le plus fr?quemment obtenue
s.label(jv73$xy, contour = jv73$contour, incl =F)
points(jv73$xy, col=cm$cluster, pch=18, cex=3)



############## PHYTOPLANCTON ################
# Table de contingence
# AFC

#### AFC sur les genre
CA(phyto_contingence[, 2:length(phyto_contingence)])
