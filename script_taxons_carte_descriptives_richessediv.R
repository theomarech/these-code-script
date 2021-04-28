##########################################################################################################
######
######          ANALYSE DESCRIPTIVE DES DONNES TAXONS 
######
##########################################################################################################

library("maps")
################# Description niveauu taxonomique par taxons ######################
carte_france <- map_data("france")
df_carte <- df_lac_taxons %>% ungroup() %>% dplyr::select(lat_pla,long_pla,abondance,code_lac,libelle,Espèce,Genre,Famille,Ordre)

df_carte_richesse <- df_carte %>%
  group_by(libelle,lat_pla,long_pla,code_lac,add=TRUE) %>%
  dplyr::summarize(indice = n_distinct(Espèce)) %>% 
  ungroup() %>%
  group_by(libelle) %>%
  nest()

df_carte_diversite <- df_carte %>%
  group_by(libelle,lat_pla,long_pla,code_lac,Espèce) %>%
  dplyr::summarize(abondance = mean(abondance)) %>% 
  ungroup() %>% na.omit() %>% 
  group_by(libelle) %>%
  nest() %>% 
  mutate(diversite_shannon = purrr::map2(data,libelle, ~group_by(.x,lat_pla,long_pla,code_lac) %>%
                                          dplyr::summarise(indice = diversity(abondance)) %>%
                                          ungroup() %>% mutate(taxon = .y)),
         diversite_simpson = purrr::map2(data,libelle, ~group_by(.x,lat_pla,long_pla,code_lac) %>%
                                           dplyr::summarise(indice = diversity(abondance, index = "simpson") %>% round(digits = 0)) %>%
                                           ungroup() %>% mutate(taxon = .y)),
         richesse = purrr::map2(data,libelle, ~group_by(.x,lat_pla,long_pla,code_lac) %>%
                                  dplyr::summarise(indice = n_distinct(Espèce)) %>% mutate(taxon = .y))
         ) 
df_carte_diversite$diversite_simpson

################## RICHESSE par groupe taxonomique
fun_carte<- function(df,index="simpson"){
  ggplot() +
  geom_polygon(data=carte_france, aes(x = long, y = lat, group = group),fill="transparent", colour = "gray60")+
  geom_point(data=df, aes(x = long_pla,y=lat_pla,col=indice),size = 2)+scale_color_distiller(palette="Spectral")+
    theme_void()+theme(legend.direction = "vertical",
                       legend.position = c(0.2,0.4),
                       legend.key.size = unit(0.4,"cm"))+labs(caption=df$taxon[[1]],col=index)+coord_quickmap()
}
list_plot_simpson <- purrr::map(df_carte_diversite$diversite_simpson,~fun_carte(.)) %>%
  ggarrange(plotlist = .) %>%
  annotate_figure(fig.lab = "Figure 1 : diversité de simpson dans les lacs")
                                                                                                  
list_plot_shannon <- purrr::map(df_carte_diversite$diversite_shannon,~fun_carte(.,index="shannon")) %>%
  ggarrange(plotlist = .) %>%
  annotate_figure(fig.lab = "Figure 2 : diversité de shannon dans les lacs")

list_plot_richesse <-  purrr::map(df_carte_diversite$richesse,~fun_carte(.,index="richesse")) 

list_plot_richesse[[1]]






data_

df <- env_spatial %>% dplyr::filter(code_lac %in% data_poi_multi$code_lac)
ggplot() +
  geom_polygon(data=carte_france, aes(x = long, y = lat, group = group),fill="transparent", colour = "gray60")+
  geom_point(data=df, aes(x = long_pla,y=lat_pla),size = 2)+coord_quickmap()
