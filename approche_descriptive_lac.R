############################################# APPROCHE DESCRIPTIVE SUR LA TABLE [plan_eau] ##################################################

rm(list = ls())

source("/home/theo/Bureau/scriptR/connectionBDDplando.R")

##La question est : est-ce que la variabilité des conditions des lacs ne va pas cacher le signal fonctionnelle
##est-ce qu'il faut étudier des groupes de lacs séparéments ?
## qu'elles sont les variables qui classifient le mieux les lacs à notre dispositions ?



############## [plan_eau] ################## => table_lac
str(table_lac)
head(table_lac)
colnames(table_lac)
gg_miss_var(table_lac)
vis_dat(table_lac)

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

lac2 <-
  lac[, -1] %>% 
  map_df( ~ as.numeric(.)) %>% 
  bind_cols(lac[, "code_lac"]) %>% 
  filter(code_lac != "LEM74" & code_lac != "PSA973" & code_lac != "MAN972")

lac2 <- as.data.frame(lac2)

rownames(lac2) <- lac2$code_lac

lac3 <- lac2[, -length(lac2)]

vis_dat(lac3)



### Corrélation entre les variables utilisée pour le clustering
chart.Correlation(lac3[, -c(1, 2)], method = "spearman") # pas du tout gaussien + nécessité de transformation en log je pense pour pas mal de variables

lac3_log <- log(lac3 + 1)

chart.Correlation(lac3_log[, -c(1, 2)], method = "spearman") # ca me semble mieux on peut tester du pearson
chart.Correlation(lac3_log[, -c(1, 2)], method = "pearson")



### imputation de données avec RF et PCA (puis PCA pour regarder quelles variables retenir)

df_impute_PCA <- imputePCA(lac3_log)[[1]]
chart.Correlation(df_impute_PCA[, -c(1:2)], method = "pearson")

df_impute_RF <- missForest(lac3_log, ntree = 200)[[1]]
chart.Correlation(df_impute_PCA[, -c(1:2)], method = "pearson")




### ACP => on enlève LEM74 et PSA973 qui tire sur les axes ici (très grandes surface/volume)

PCA(df_impute_PCA[, -c(1:2)])

pca_pca <-
  dudi.pca(scale(df_impute_PCA[, -c(1:2)]))

# Dim1 = 38.26% Dim2 = 16.85%
#perimetre_pla /longueur_max_pla/prof_moy_pla/tps_sejour_moy_pla/altitude_pla (à vérifier avec les cos)

PCA(df_impute_RF[, -c(1:2)])
pca_rf <-
  dudi.pca(scale(df_impute_RF[, -c(1:2)]))
# Dim1 = 36.68% Dim2 = 16.73%
#alcalinite, longueur_max_pla, profm_moy_pla, altitude_pla (à vérifier avec les cos)

s.label(pca_pca$li, clabel = 0.2)





##### classification



## CAH



a <-
  fun_cah(scale(df_impute_PCA[, -c(1:2)]))
plot(a[[1]][[6]])

par(mfrow = c(1, 2))
fviz_nbclust(scale(df_impute_PCA[, -c(1:2)]),
             FUNcluster = hcut ,
             method = "wss") # var intra groupe, on coupe en deux on re regarde la var des deux groupes, on re coupe en trois .... jusq'u? ce que ca ne diminue plus beaucoup la var intra groupe
# ici au niveau 4 environs il y a une rupture  dans la pente de la courbe
# permet d'?valuer pour la classification hierarchique aussi on peut l'utiliser direct pour la class hiera


fviz_nbclust(scale(df_impute_PCA[, -c(1:2)]), hcut, method = "silhouette") # methode des ilhouettes

b <- cutree(a[[1]][[3]], k = 2)

france <- getData(name = "GADM",
                  country = "FRA",
                  level = 2)
plot(france)
points(
  lac3$long_pla,
  lac3$lat_pla,
  col = b,
  lwd = 1,
  pch = 19
)




## k_mean
cm <-
  kmeans(scale(df_impute_PCA[, -c(1:2)]), centers = 2, nstart = 100)


plot(france)
points(
  lac3$long_pla,
  lac3$lat_pla,
  col = cm$cluster,
  lwd = 1,
  pch = 19
)

par(mfrow = c(1, 2))
fviz_nbclust(scale(df_impute_PCA[, -c(1:2)]), kmeans, method = "wss") # var intra groupe, on coupe en deux on re regarde la var des deux groupes, on re coupe en trois .... jusq'u? ce que ca ne diminue plus beaucoup la var intra groupe
# ici au niveau 4 environs il y a une rupture  dans la pente de la courbe
# permet d'?valuer pour la classification hierarchique aussi on peut l'utiliser direct pour la class hiera


fviz_nbclust(scale(df_impute_PCA[, -c(1:2)]), kmeans, method = "silhouette") # methode des ilhouettes



fviz_cluster(cm, data = scale(df_impute_PCA[, -c(1:2)]))







##### classification sur coordonnées acp

pca_pca <-
  dudi.pca(scale(df_impute_PCA[, -c(1:2)]))

## CAH
df_classif_pca <- pca_pca$li



a <-
  fun_cah(scale(df_classif_pca))
plot(a[[1]][[6]])

par(mfrow = c(1, 2))
fviz_nbclust(scale(df_classif_pca),
             FUNcluster = hcut ,
             method = "wss") # var intra groupe, on coupe en deux on re regarde la var des deux groupes, on re coupe en trois .... jusq'u? ce que ca ne diminue plus beaucoup la var intra groupe
# ici au niveau 4 environs il y a une rupture  dans la pente de la courbe
# permet d'?valuer pour la classification hierarchique aussi on peut l'utiliser direct pour la class hiera


fviz_nbclust(scale(df_classif_pca), hcut, method = "silhouette") # methode des ilhouettes

b <- cutree(a[[1]][[3]], k = 5)

france <- getData(name = "GADM",
                  country = "FRA",
                  level = 2)
plot(france)
points(
  lac3$long_pla,
  lac3$lat_pla,
  col = b,
  lwd = 1,
  pch = 19
)

df_classif_pca_hclust <-
  bind_cols(as.data.frame(b), df_classif_pca)
ggplot(data = df_classif_pca_hclust, aes(
  x = Axis1,
  y = Axis2,
  col = as.factor(b)
)) + geom_point() + 
  theme(panel.background = element_rect(colour = "gray3"))+
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_vline(xintercept = 0, linetype = "longdash")

b$
## k_mean
cm <-
  kmeans(scale(df_classif_pca),
         centers = 5,
         nstart = 100)

cm$cluster

plot(france)
points(
  lac3$long_pla,
  lac3$lat_pla,
  col = cm$cluster,
  lwd = 0.5,
  pch = 19
)

par(mfrow = c(1, 1))
fviz_nbclust(scale(df_classif_pca), kmeans, method = "wss") # var intra groupe, on coupe en deux on re regarde la var des deux groupes, on re coupe en trois .... jusq'u? ce que ca ne diminue plus beaucoup la var intra groupe
# ici au niveau 4 environs il y a une rupture  dans la pente de la courbe
# permet d'?valuer pour la classification hierarchique aussi on peut l'utiliser direct pour la class hiera


fviz_nbclust(scale(df_classif_pca), kmeans, method = "silhouette") # methode des ilhouettes



fviz_cluster(cm, data = scale(df_classif_pca))




df_classif_pca_kmeans <-
  bind_cols(as.data.frame(cm$cluster), df_classif_pca)
ggplot(data = df_classif_pca_clust, aes(
  x = Axis1,
  y = Axis2,
  col = as.factor(cm$cluster)
)) + geom_point() + 
  theme(panel.background = element_rect(colour = "gray3"))+
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_vline(xintercept = 0, linetype = "longdash")

test <- bind_cols(as.data.frame(df_impute_PCA),cluster=cm$cluster) %>% mutate(cluster=as.factor(.$cluster))

pca_clust_fmr <- PCA(test,ncp= 4, graph = TRUE,quali.sup = 15)
plot(pca_clust_fmr,habillage = "cluster")
length(cm$cluster)== nrow(df_impute_PCA)
head(df_classif_pca)
