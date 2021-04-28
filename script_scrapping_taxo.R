


#####################################   Ouverture des code_taxons #####################################




################################ Les données par groupes ##########################
# On récupère les codes taxos de chaque table d'intérêt

#### prélèvement biologique ####
df_cd_taxon_prelevement_bio <- dbGetQuery(
  con,
  "
SELECT
  cd_apt
FROM prelevement_biologique pb
LEFT JOIN type_prel_bio tp
  ON pb.cd_type_prel_bio = tp.cd_type_prel_bio
LEFT JOIN taxon_denombre td
  ON pb.id_prelev_bio = td.id_prelev_bio
INNER JOIN tsa_appellation_taxon_apt ta
  ON td.cd_taxon = ta.cd_apt
WHERE td.cd_taxon IS NOT NULL
                              "
) %>% tbl_df() %>% glimpse()

nrow(unique(df_cd_taxon_prelevement_bio))


#### poissons #### 77 codes unique
df_cd_taxon_poissons <- dbGetQuery(
  con,
  "
SELECT
  cd_apt
FROM lot_prelev_poisson lp
INNER JOIN tsa_appellation_taxon_apt ta
  ON lp.cd_taxon = ta.cd_apt
                              "
) %>% tbl_df() %>% glimpse()

nrow(unique(df_cd_taxon_poissons))


#### macrophytes #### 679 codes unique
df_cd_taxon_macrophytes <- dbGetQuery(
  con,
  "
SELECT
  cd_apt
FROM taxon_denombre_macro tdm
INNER JOIN tsa_appellation_taxon_apt ta
  ON tdm.code_taxon = ta.cd_apt
                              "
) %>% tbl_df() %>% glimpse()

nrow(unique(df_cd_taxon_macrophytes))


df_code_taxon <-
  bind_rows(
    unique(df_cd_taxon_macrophytes),
    unique(df_cd_taxon_poissons),
    unique(df_cd_taxon_prelevement_bio)
  )

#### animalia : 5217 ; Bacteria : 6276 ; plantae : 4745



################## FONCTION taxonomie #######################
df_code_d <-
  dbGetQuery(
    con,
    "SELECT * FROM tsa_appellation_taxon_apt"
  ) 
   df_niveau_taxonomique <- read_csv("/home/theo/Bureau/Data/df_code_niveau_taxonomique_SANDRE.csv") %>% mutate(cd_niv_apt = as.character(cd_niv_apt))


df_code <-
   dbGetQuery(con,
              "SELECT * FROM tsa_appellation_taxon_apt") %>% left_join(df_niveau_taxonomique, by = "cd_niv_apt") %>% dplyr::rename(., niveau_taxo = LbElement)

a <- fun_remonter_codetaxon(df_code_taxon,df_code)


############### OUVERTURE DU TABLEAU ISSUE DE LA REMONTER ET DU NETTOYAGE CONTENANT LES CODES ASSOCIES A LA PHYLOGENIE ##############
df_code_taxon_final <- read.csv("/home/theo/Bureau/Data/code_taxons.csv")