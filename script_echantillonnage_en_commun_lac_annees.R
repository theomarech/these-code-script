############################### Les données par groupes ##########################
source("/home/theo/Bureau/scriptR/connectionBDDplando.R")

#### prélèvement biologique ####
#[prelevement_biologique] : pour avoir les point d'échantillonnage et les dates de prélèvement
#[point_prel_eaux_surf] : pour avoir les lacs
#[type_prel_bio] pour avoir les types de taxons échantillonnées


df_taxon_prelevbio_date <- dbGetQuery(
  con,
  "
SELECT
  pb.date_debut_prel_bio,
  libelle,
  code_lac
FROM prelevement_biologique pb
INNER JOIN point_prel_eaux_surf pp
  ON pb.id_point_prelev = pp.id_point_prelev
INNER JOIN type_prel_bio tp
  ON pb.cd_type_prel_bio = tp.cd_type_prel_bio
WHERE tp.cd_type_prel_bio = '4' 
  OR tp.cd_type_prel_bio = '3' 
  OR tp.cd_type_prel_bio = '6'
  OR tp.cd_type_prel_bio = '7'
                              "
) %>%
  tbl_df() %>%
  mutate(
    taxon = as.factor(libelle),
    date = as.Date(date_debut_prel_bio),
    annee = lubridate::floor_date(date, unit = "year")
  ) %>%
  dplyr::select(-libelle, -date_debut_prel_bio) %>%
  split(.$taxon)

distinct_at(df_taxon_macroinv,c("code_lac","annee"),keep_all = TRUE)
  df_taxon_macroinv <- df_taxon_prelevbio_date[[1]]
df_taxon_mollusques <- df_taxon_prelevbio_date[[2]]
df_taxon_oligochetes <- df_taxon_prelevbio_date[[3]]
df_taxon_phytozoo <- df_taxon_prelevbio_date[[4]]




#### poissons #### 77 codes unique
df_taxon_poissons <- dbGetQuery(
  con,
  "
SELECT
  pp.date_releve,
  code_lac
FROM prelevement_piscicole pp
INNER JOIN point_prel_eaux_surf ppes
  ON pp.id_point_prelev = ppes.id_point_prelev
                              "
) %>% tbl_df() %>%  mutate(
  date = date_releve,
  annee = lubridate::floor_date(date, unit = "year"),
  taxon = 'poisson'
) %>% dplyr::select(-date_releve) %>% glimpse()

nrow(unique(df_taxon_poissons))


#### macrophytes #### 679 codes unique
df_taxon_macrophytes <- dbGetQuery(
  con,
  "
SELECT
  code_lac,
  date_prel
FROM prelevement_macro pm
INNER JOIN point_prel_eaux_surf ppes
  ON pm.id_point_prelev = ppes.id_point_prelev
                              "
) %>% tbl_df()  %>% mutate(
  taxon = "macrophytes",
  date = ymd(date_prel),
  annee = lubridate::floor_date(date, unit = "year")
) %>% dplyr::select(-date_prel) %>% glimpse()

nrow(unique(df_taxon_macrophytes))

df_taxons <- bind_rows(
  df_taxon_macrophytes,
  df_taxon_poissons,
  df_taxon_phytozoo,
  df_taxon_macroinv,
  df_taxon_mollusques,
  df_taxon_oligochetes
)



#### nombre de lac échantillonnées par taxon ######

df_total_lac_taxon <-
  split(df_taxons, df_taxons$taxon) %>%
  map_df(~ unique(.$code_lac) %>%
           length()) %>%
  gather("taxon", "effectif")

ggplot(df_total_lac_taxon, aes(x = taxon, y = effectif)) +
  geom_bar(stat = "identity", color = "gray35", fill = "gray85") +
  theme(
    legend.position = c(0.8, 0.2),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey"),
    panel.background = element_blank(),
    axis.ticks = element_line(color = "grey"),
    legend.background = element_rect(color = "black"),
    text = element_text(
      family = "Times New Roman",
      size = rel(4),
      color = "gray30"
    ),
    axis.text.x = element_text(angle = 45, vjust = 0.7),
    axis.ticks.x = element_blank()
  ) +
  ylab("nombre de lac") +
  xlab(NULL) +
  geom_text(
    label =  paste0("n = ", df_total_lac_taxon$effectif),
    vjust = 1.2,
    size = rel(5),
    color = "gray30",
    fontface = "italic",
    family = "Times New Roman"
  )

#### multi-taxon #### total lac par taxon en fonction des années

df_multitaxons_date <-
  df_taxons %>%
  dplyr::group_by(annee, taxon,  code_lac) %>%
  dplyr::summarise(nombre_occ = 1)  %>% dplyr::group_by(annee, taxon) %>% dplyr::summarise(nombre_lac = n())

ggplot(df_multitaxons_date, aes(x = annee, y = nombre_lac, col = taxon)) +
  geom_line() + geom_point() + theme(legend.position = c(0.18, 0.85)) + ylab("nombre de lacs échantillonnées")

ggplot(df_multitaxons_date, aes(x = annee, y = nombre_lac, fill = taxon)) +
  geom_bar(stat = "identity") + theme(legend.position = c(0.18, 0.85)) + ylab("nombre de lacs échantillonnées")


#### multi-taxon #### Lac commun uniquement en fonction des années


df_multitaxons_date_laccommun <-
  plyr::join_all(
    map(
      list(
        df_taxon_macrophytes,
        df_taxon_poissons,
        df_taxon_phytozoo,
        df_taxon_mollusques,
        df_taxon_oligochetes
      ),
      ~ as.data.frame(.) %>% 
        dplyr::group_by(code_lac, annee, taxon) %>% 
        dplyr::summarize(n =1) %>%
        as.data.frame()
    ),
    type = "inner",
    by = c("annee", "code_lac")
  )[, 1:2] %>% 
  group_by(annee) %>%
  dplyr::summarise(nombre_lac = n())

ggplot(df_multitaxons_date_laccommun, aes(x = annee, y = nombre_lac)) +
  geom_bar(stat = "identity") + geom_text(aes(label = nombre_lac), color = "gray85", vjust = 1.6)

#### multi-taxon #### Lac commun total (nombre de lac ayant eu des échantillonnage pour tous les taxons)



