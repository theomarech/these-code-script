


# source("/home/theo/Bureau/scriptR/librairies.R")

####################################################################################################
####################
####################    Principaux packages triés par thématique
####################
####################################################################################################

#################### Bayésien ######################
library("R2jags")
################### data wrangling ####################
library("rlist") # pour sauver les listes complexe (marche mieux que save)
library("tidytext") #N pour réarranger les valeur dans un wrap (ordonnée)
library("rlang")
library(spacejam)
library(dplyr)
library(plyr)
library(purrr)
library(tidyr)
library(lubridate)
library(scales)
library(stringr)
library("stringi")
library(forcats)
library(DBI)
library(RSQLite)
library(RPostgreSQL)
library(readr)
library(readxl)
library("readODS")
library(XML)
library(gdata)
library(data.table)
library("textshape")
library("tidytree")
    ### scraping
library(rvest)
library(jsonlite)
library("xml2")
library("RSelenium")
library("broom")
#################### programmation ####################
library(tictoc)
library(microbenchmark)
library('rbenchmark')
library("furrr")
library("doParallel")
library(here)
#################### data vizualisation ####################
library(ggplot2)
library(ggrepel)
library(lattice)
library("ggpubr")
library(RColorBrewer)
library("gridExtra")
# library(JLutils)
library("grid")
library("formattable") 
library("kableExtra")
library("plotly")
### outils diagnostiques
library(corrplot)
library(PerformanceAnalytics)
library(naniar) # pour regarder les NA et compagnie dans un jeu de données
library(visdat) # comme naniar
library("rstatix")#résumé statistiques divers "get_summary_stats(var1,varx...,mean_sd, mean_se); identification d'outliers avec identify_outliers()
library("GGally")# corplot avec ggpairs()
## SIG
library(sp)
library(sf)
library(rgdal)
library(raster)
library(cartography)
library("adespatial")
library("adegraphics")
library("spdep")
#################### data analysis ####################

## comparaison de moyenne non paramétrique
library("PMCMR")
# library("PMCMRplus")

library(car)
# library(RVAideMemoire)
### gestion des données manquantes par imputation
#library(missMDA)
library(missForest)
library('mice')
### réduction de dimensions
library("Rtsne")
### modèle linéaire (GLM) à effet aléatoire et mixte
library(MASS)

### multivariées : méthode d'ordination, clustering, SDM,SEM ###
library("psych")
library(FactoMineR)
library(factoextra)
library("Hmsc")
library("bootSVD")
library(lavaan)
library("piecewiseSEM")
# library(plspm)
# library('NbClust')
### machine learning ###
library(caret)
library(dismo)
library(xgboost)
library(randomForest)
library(elasticnet)
library(mlr)
library("ParamHelpers")

### spécialisé écologie  ###
library(gawdis)
library(FD)
library(rotl)
library(vegan)
library(ade4)
library(betapart)
### série temporelles ###
library(WaveletComp)
library(rEDM)
library(lmtest)
library(segclust2d)
library(astsa)
library(forecast)
library(vars)
library(dLagM)
library(fGarch)
library("tseries")
library("rugarch")
library("imputeTS")
library("sarima")
### gestion de données en grandes dimensions (pls, regularisation)
library(pls)
library("spls")
library("HDCI")
### SIG et géostatistiques
### analyse de réseaux, graphes copules, chaines de markov
library(ecoCopula) # Popovic 
library("glasso") # GArcia & Giron, 2020, Friedman 2007
library("qgraph") # ebic glasso
library("bootnet") # sensibilité glasso
library("mgm") # glasso pour mixed data
library("psych")
#install.packages("devtools")
library('MuMIn')
library("StepReg")
# install.packages("RPostgreSQL", dependencies=TRUE, INSTALL_opts = c('--no-lock'))
# install.packages("DBI") #marche pas
theo <- "mdpbddplando"


################################ FONCTIONS #############################

#####Fonction sélection de variables corrélées pour les ACP#####
# tableau doit être entièrement en numérique
#l'objectif est de pouvoir sortir sur une paire de variable > à un seuil de corrélation l'une des variables pour éviter collinéarité
#attention à penser au préalable à la gestion des NAs => sur deux variables trops corrélée on peut avoir besoin de regarder laquelle est la plus complete
# cette fonction ne fait pas la différence
fun_selection_var_cor <-
  function(df, seuil = 0.8, method = "pearson") {
    class <- map_lgl(df, ~ is.numeric(.))
    if (any(class) != TRUE) {
      stop("le tableau contient au moins une variable non numeric")
    }
    cor_mat <-
      cor(df, method = method, use = "pairwise.complete.obs")
    col_sel <-
      colnames(cor_mat)[!apply((lower.tri(cor_mat) &
                                  abs(cor_mat) >= seuil), 2, any)]
    var_select <- df[, col_sel]
    return(var_select)
  }

#####Fonction CAH pour comparer les arbres avec les différentes méthodes#########
fun_caf <- function(df) {
  tibble(
    data = list(df),
    data_dist = map(data,  ~ dist(.)),
    methode_aggreg = c("single","complete","average","centroid","ward.D","ward.D2"),
    cah = map2(data_dist, methode_aggreg,  ~ hclust(d = .x, method =.y)),
    coph_cor = map(cah,  ~ cophenetic(.)) %>%
      map2_dbl(., data_dist,  ~cor(.x, .y)),
    coph_ss = map(cah,  ~ cophenetic(.)) %>%
      map2_dbl(., data_dist,  ~ sum((.x - .y) ^ 2))
  ) %>%
    dplyr::select(methode_aggreg, cah, coph_cor, coph_ss) %>%
    arrange(coph_cor) %>%
    return()
}

fun_cah2 <- function(df) {
  d <- dist(df)
  list_dendro <- list()
  list_coph <- list()
  method <-c("single","complete", "average","centroid","ward.D","ward.D2")
  df_cor <- data.frame(method = method, cor = 0, ss = 0)
  for (i in 1:length(method)) {
    list_dendro[[i]] <- hclust(d, method = method[i])
    list_coph[[i]] <- cophenetic(list_dendro[[i]])
    df_cor[i, 2] <- cor(d, list_coph[[i]])
    df_cor[i, 3] <- sum((list_coph[[i]] - d) ^ 2)
  }
  list_final <- list(list_dendro, df_cor)
  return(list_final)
}


#####Fonction ondelettes #########
# prend en entrée une lsite dont chaque élément est un df
# chaque df est composée d'au moins trois variables
# ces variables doivent être ordonnée comme suis :
# première variable doit être de format date
# seconde variables doit correspondre au site d''échantillonnage (format character)
# les variables suivante doivent être au format numérique
# la fonction renvoie la décomposition en ondelette et le specctre de puissance pour chacune des variables de chaque condition

fun_ondelettes_un <-
  function(liste, chemin = "/home/theo/Bureau/Vincent/ondelettes") {
    dir.create(paste0(chemin))
    for (i in 1:length(liste)) {
      nom_condition <- names(liste[i])
      dir.create(paste0(chemin, "/",
                        nom_condition,
                        "/"))
      
      for (j in 3:length(liste[[i]])) {
        nom_variable <- colnames(liste[[i]][j])
        variable <- as.data.frame(na.omit(liste[[i]][c(1, j)]))
        ondelette <- analyze.wavelet(
          variable,
          nom_variable,
          n.sim = 1,
          dt = 1 / 24,
          upperPeriod = 360
        )
        
        
        spectre_puissance <-
          data.frame(
            Puissance = ondelette$Power.avg,
            Periode = ondelette$Period,
            pval = ondelette$Power.avg.pval
          )
        spectre_puissance <-
          spectre_puissance %>% filter(pval <= 0.05)
        spectre_puissance <-
          ggplot(spectre_puissance, aes(Periode, Puissance)) +
          geom_line(col = "red3", size = rel(1)) +
          geom_point(col = "darkblue") +
          xlab("P?riode (j)") +
          geom_vline(
            xintercept = seq(0, 360, by = 20),
            linetype = "longdash",
            col = "grey40",
            alpha = 0.5
          ) +
          theme(
            axis.line = element_line(
              linetype = 1,
              size = rel(2),
              color = "grey30"
            ),
            panel.background = element_rect(fill = NA),
            axis.text = element_text(size = rel(1), color = "grey30"),
            axis.text.x = element_text(vjust = 1, hjust = 1),
            axis.ticks = element_line(size = rel(1), color = "grey30"),
            axis.title = element_text(
              size = rel(1),
              hjust = 0.5,
              vjust = 4,
              colour = "grey30"
            )
          )
        
        filename <-
          paste0(chemin,
                 "/",
                 nom_condition,
                 "/Ondelette_",
                 nom_variable,
                 ".pdf")
        pdf(filename)
        plot_ondelette <-
          wt.image(
            ondelette,
            color.key = "quantile",
            n.levels = 250,
            show.date = TRUE,
            date.format = "%Y %M",
            legend.params = list(lab = "wavelet power levels", mar = 4.7)
          )
        print(spectre_puissance)
        dev.off()
      }
    }
  }

fun_ondelette <-
  function(liste, var, chemin = "/home/theo/Bureau/Vincent/ondelettes_sat_ox/") {
    dir.create(chemin)
    liste_plot <- list()
    compteur <- 1
    for (i in 1:length(liste)) {
      nom_condition <- names(liste[i])
      
      nom_variable <- colnames(liste[[i]][var])
      variable <- as.data.frame(na.omit(liste[[i]][c(1, j)]))
      ondelette <- analyze.wavelet(
        variable,
        nom_variable,
        n.sim = 1,
        dt = 1 / 24,
        upperPeriod = 360
      )
      
      spectre_puissance <-
        data.frame(
          Puissance = ondelette$Power.avg,
          Periode = ondelette$Period,
          pval = ondelette$Power.avg.pval
        )
      
      matrice_ondelette <- ondelette$Power %>%
        as.data.frame()  %>%
        setnames(old = colnames(.),
                 new = ondelette$series$date %>%
                   as.character()) %>%
        mutate(periode = seq(1, length(ondelette$Period))) %>%
        gather("date", "puissance", -periode) %>%
        mutate(puissance = round(puissance) + 0.1) %>%
        as_tibble() %>% mutate(date = lubridate::ymd_hms(date))
      
      
      
      liste_plot[[compteur + 1]] <-
        ggplot(matrice_ondelette, aes(x = date, y = periode)) +
        geom_raster(aes(fill = puissance)) +
        theme(
          legend.position = c(1 - 0.905, 0.825),
          legend.title = element_text(size = rel(0.5)),
          legend.text = element_text(size = rel(0.5)),
          legend.key.width  = unit(rel(0.5), "cm"),
          legend.key.height = unit(rel(0.3), "cm"),
          panel.background = element_rect(fill = NA),
          axis.line.x = element_line(
            linetype = 1,
            size = 1,
            color = "grey30"
          ),
          axis.text.y = element_text(color = "white"),
          axis.text.x = element_text(colour = "white"),
          axis.ticks.y = element_blank()
        ) +
        scale_x_datetime(date_breaks = "1 months", labels = date_format("%b")) +
        ylab(NULL) + xlab(NULL) + scale_fill_distiller(palette = "Spectral")
      
      
      
      
      liste_plot[[compteur]] <-
        ggplot(spectre_puissance, aes(Periode, Puissance)) +
        geom_line(col = "red3", size = rel(1)) +
        xlab(NULL) +
        scale_x_log10() +
        scale_y_continuous(breaks = seq(0, 5, 1)) +
        theme(
          axis.line = element_line(
            linetype = 1,
            size = 1,
            color = "grey30"
          ),
          panel.background = element_rect(fill = NA)
        ) + coord_flip() + geom_vline(xintercept = 1, linetype = "longdash") +
        scale_y_reverse()
      
      
      
      liste_plot[[compteur + 2]] <- ggplot() +
        geom_rect(
          data = data.frame(
            x1 = lubridate::round_date(variable$date, unit = 'month') %>% unique(),
            x2 = lead(
              lubridate::round_date(variable$date, unit = 'month') %>% unique()
            ),
            ymin = 0,
            ymax = Inf
          ) %>% na.omit() %>% slice(-seq(2, nrow(.), by = 2)) %>% tbl_df(),
          mapping = aes(
            xmin = x1,
            xmax = x2,
            ymin = ymin,
            ymax = ymax
          ),
          alpha = 0.2
        ) + geom_line(data = variable, aes(x = date, y = sat_od)) + geom_line(
          data = ondelette$series,
          aes(x = date, y = sat_od.trend),
          col = "red",
          size = 1
        ) +
        theme(
          axis.line = element_line(
            linetype = 1,
            size = 1,
            color = "grey30"
          ),
          panel.background = element_rect(fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1)
          
        ) + scale_x_datetime(breaks = "1 months", labels = date_format("%b")) +
        ylab(NULL)
      
      
      compteur <- compteur + 3
      
    }
    
    plot_all <-
      ggarrange(
        liste_plot[[1]] + ylab(NULL) + xlab("Période (j)"),
        liste_plot[[2]] + xlab(NULL),
        liste_plot[[4]] + ylab(NULL),
        liste_plot[[5]] + xlab(NULL),
        NA,
        liste_plot[[3]] + xlab(NULL),
        NA,
        liste_plot[[6]] + xlab(NULL),
        liste_plot[[7]] + ylab(NULL) + xlab("Période (j)"),
        liste_plot[[8]],
        liste_plot[[10]] + ylab(NULL),
        liste_plot[[11]],
        NA,
        liste_plot[[9]] + xlab(NULL),
        NA,
        liste_plot[[12]] + xlab(NULL),
        widths = c(1, 3.5),
        heights = c(2.5, 1, 2.5, 1)  ,
        nrow = 4,
        ncol = 4
      )
    
    ggsave(
      "ondelette.plot.png",
      plot_all,
      path = chemin,
      width = 10,
      height = 8,
      units = "cm",
      scale = 3
    )
  }
#####Fonction scrapping sur le SANDRE #########

fun_scrap_taxo <- function(df) {
  liste_final <-
    list() # liste dont chaque élément sera un code taxo associés avec ses différents parents
  
  for (j in 1:nrow(df)) {
    # boucle qui va tourner sur un df avec une seule colonne de classe chr contenant les code unique
    
    code <- df[j,] %>% as.numeric() # code unique en format numeric
    
    liste_while <- list() # liste dans laquelle on va
    
    html_link <-
      # construction du lien pour allez chercher le code sur le SANDRE
      read_html(
        paste0(
          "http://www.sandre.eaufrance.fr/urn.php?urn=urn:sandre:donnees:APT:FRA:code:",
          code,
          ":::referentiel:3.1:html#"
        )
      )
    
    test <-
      html_link %>% # test contient les variables du tableau du SANDRE, l'objet nous servira à tester si ce tableau contien bien les variables d'intérêts
      html_nodes(xpath = '//*[@id="table-informations"]') %>%
      html_table(fill = TRUE) %>%
      as.data.frame() %>%
      na.omit() %>%
      transpose() %>%
      setnames(colnames(.),
               slice(., 1) %>% as.character()) %>%
      slice(-1)
    
    if (any(colnames(test) == "Libellé de l'Appellation Référente du Taxon")) {
      # On vérifie qu'il y a pas un taxon qui fait référence (i.e. synonymie)
      
      code <-
        c(
          code,
          # si c'est le cas l'objet code contiendra le code de référence en seconde position
          str_extract_all(
            test$`Libellé de l'Appellation Référente du Taxon`,
            "[:digit:]{1,}",
            simplify = TRUE
          ) %>% as.numeric()
        )
      
      html_link <-
        read_html(
          # on ira à ce moment là se connecter sur une nouvelle page: celle du code de référence
          paste0(
            "http://www.sandre.eaufrance.fr/urn.php?urn=urn:sandre:donnees:APT:FRA:code:",
            code[2],
            ":::referentiel:3.1:html#"
          )
        )
    }
    
    i <- 1 # compteur de la boucle while
    
    while (any(colnames(test) == "Nom latin de l'Appellation du Taxon") &
           any(colnames(test) == "Niveau Taxonomique") &
           any(colnames(test) == "Appellation du Taxon Parent")) {
      # cette boucle fait du scraping à partir d'un code taxo elle remonte jusqu'au parent
      
      
      liste_while[[i]] <- html_link %>%
        html_nodes(xpath = '//*[@id="table-informations"]') %>%
        html_table(fill = TRUE) %>%
        as.data.frame() %>%
        na.omit() %>%
        transpose() %>%
        setnames(colnames(.),
                 slice(., 1) %>% as.character()) %>%
        slice(-1) %>%
        dplyr::select(
          "Nom latin de l'Appellation du Taxon",
          "Niveau Taxonomique",
          "Appellation du Taxon Parent"
        ) %>%
        setnames(colnames(.),
                 c("nom_latin",
                   "niv_taxo",
                   "nom_latin_par")) %>%
        mutate(
          nom_latin_parent = str_extract_all(nom_latin_par, "[:alpha:]{1,}", simplify = TRUE) %>% paste0(collapse = ' '),
          cd_taxon = html_link %>%
            html_node(xpath = '//*[@id="contentheader"]') %>%
            html_text() %>%
            str_extract_all("[:digit:]{1,}", simplify = TRUE) %>%
            as.numeric(),
          cd_taxon_par = str_extract_all(nom_latin_par, "[:digit:]{1,}", simplify = TRUE) %>% as.numeric()
        )
      
      html_link <-
        read_html(
          paste0(
            "http://www.sandre.eaufrance.fr/urn.php?urn=urn:sandre:donnees:APT:FRA:code:",
            liste_while[[i]]["cd_taxon_par"],
            ":::referentiel:3.1:html#"
          )
        )
      
      code <- liste_while[[i]]["cd_taxon_par"] %>% as.numeric()
      
      test <- html_link %>%
        html_nodes(xpath = '//*[@id="table-informations"]') %>%
        html_table(fill = TRUE) %>%
        as.data.frame() %>%
        na.omit() %>%
        transpose() %>%
        setnames(colnames(.),
                 slice(., 1) %>% as.character()) %>%
        slice(-1)
      
      if (any(colnames(test) == "Libellé de l'Appellation Référente du Taxon")) {
        # On vérifie qu'il y a pas un taxon qui fait référence (i.e. synonymie)
        
        code <-
          c(
            code,
            # si c'est le cas l'objet code contiendra le code de référence en seconde position
            str_extract_all(
              test$`Libellé de l'Appellation Référente du Taxon`,
              "[:digit:]{1,}",
              simplify = TRUE
            ) %>% as.numeric()
          )
        
        html_link <-
          read_html(
            # on ira à ce moment là se connecter sur une nouvelle page: celle du code de référence
            paste0(
              "http://www.sandre.eaufrance.fr/urn.php?urn=urn:sandre:donnees:APT:FRA:code:",
              code[2],
              ":::referentiel:3.1:html#"
            )
          )
      }
      
      i = i + 1
      
    }
    
    liste_final[[j]] <-
      bind_rows(liste_while) %>%
      dplyr::select(niv_taxo, nom_latin) %>%
      transpose() %>%
      tbl_df() %>%
      setnames(old = colnames(.),
               new = slice(., 1) %>% as.character()) %>%
      slice(-1) %>%
      dplyr::mutate(code = df[j,] %>% as.numeric())
    
  }
  
  return(bind_rows(liste_final))
}

################## fonction rechecher une table   ###############

fun_recherche_table <-  function(pattern) {
  liste_table[str_detect(dbListTables(con), pattern)]
} #, fonction de recheche de tables

################# fonction pour relier les taxons avec leurs parents  ################
fun_remonter_codetaxon2 <-
  function(df, df_code, lien = "/home/theo/Bureau/code_taxons.csv") {
    # Df = tableau d'une colonne contenant uniquement les noms de taxons à rechercher; df_code l'ensemble du tableau tsa_appellation_taxons de la BDD plando
    df <-
      filter(df, cd_apt != 0) # on enlève ceux qui n'ont pas de code
    liste_finale <-
      list() # la liste qui contiendra notre tableau finale (chaque fin de boucle while sera un tableau dans cette liste, i.e. une ligne dans le tableau finale)
    times2 <- c() # juste pour un décompte du temps
    for (i in 1:nrow(df)) {
      # boucle qui tourne sur les codes à rmeonter
      tic()# on commence à décompter
      code_origine <- df[[1]][i] #
      code_base <- df[[1]][i]
      liste_taxo <-list() # liste intermédiaire rempli à chauqe boucle while qui sera donc une ligne dans liste_finale
      j = 1 # indice pour la boucle while
      
      
      
      while (code_origine %>% is.na() != TRUE) {
        # tant qu'on peut remonter dans l'arbre taxo
        # boucle qui teste si le dernier parent a été atteint i.e. cd_apt_parent == NA
        test_code <- filter(df_code, cd_apt == code_origine)
        
        if ((test_code[["typ_apt"]] != "R" |
             test_code[["st_apt"]] != "Validé") &
            str_extract(test_code[["comment_apt"]], "\\[([:digit:]{1,})\\]") %>% is.na() == FALSE) {
          code_origine <-str_extract(test_code[["comment_apt"]], "\\[([:digit:]{1,})\\]") %>% str_extract("[:digit:]{1,}")
        }
        
        if ((test_code[["typ_apt"]] != "R" |
             test_code[["st_apt"]] != "Validé") &
            str_extract(test_code[["comment_apt"]], "\\[([:digit:]{1,})\\]") %>% is.na()) {
          test <-
            read_html(
              paste0(
                "http://www.sandre.eaufrance.fr/urn.php?urn=urn:sandre:donnees:APT:FRA:code:",
                test_code$cd_apt,
                ":::referentiel:3.1:html"
              )
            ) %>% # test contient les variables du tableau du SANDRE, l'objet nous servira à tester si ce tableau contien bien les variables d'intérêts
            html_nodes(xpath = '//*[@id="table-informations"]') %>%
            html_table(fill = TRUE) %>%
            as.data.frame() %>%
            na.omit() %>%
            transpose() %>%
            setnames(colnames(.),
                     slice(., 1) %>% as.character()) %>%
            slice(-1)
          
          if (any(colnames(test) == "Libellé de l'Appellation Référente du Taxon")) {
            code_origine <-
              str_extract(test$`Libellé de l'Appellation Référente du Taxon`,
                          "[:digit:]{1,}")
          }
        }

        
        liste_taxo[[j]] <-
          filter(df_code, cd_apt == code_origine) #retrouve le taxon en question dans la BDD
        
        code_origine <- liste_taxo[[j]][["cd_apt_parent"]] #test
        
        j = j + 1
        
      }
      
      
      
      
      code <- liste_taxo[[1]][1] %>% as.numeric()
      liste_taxo <- bind_rows(liste_taxo)
      
      for (k in 1:nrow(liste_taxo)) {
        if (any(liste_taxo[k, "niveau_taxo"] == liste_taxo[-k, "niveau_taxo"])) {
          liste_taxo[which(liste_taxo[k, "niveau_taxo"] == liste_taxo[-k, "niveau_taxo"]), "niveau_taxo"] <-
            paste0(liste_taxo[which(liste_taxo[k, "niveau_taxo"] == liste_taxo[-k, "niveau_taxo"]), "niveau_taxo"], 1)
        }
      }
      
      liste_finale[[i]] <- liste_taxo %>%
        dplyr::select(niveau_taxo, nom_latin_apt) %>%
        transpose() %>%
        as.data.frame() %>%
        setnames(colnames(.), slice(., 1) %>% as.character()) %>% slice(-1) %>%
        dplyr::mutate(code_base = code_base, code_retenu = code_origine)
      
      
      ### Barre de chargement et timeur pour la fonction
      times1 <- toc(quiet = TRUE)
      times2[i] <-
        times1$toc[[1]] - times1$tic[[1]] # temps passer pour faire la boucle i ti
      temps <-
        (sum(times2) / i) * (nrow(df) - i) # temps restant estimé
      
      if (temps > 60 & temps <= 3600) {
        temps <- round(temps / 60)
        unites <- "min"
      } else{
        temps <- temps
        unites <- "sec"
      }
      
      Sys.sleep(.0001)
      avance <-
        str_c(rep("=", round(i / nrow(df) * 100)), collapse = "")
      reste <-
        str_c(rep(".", 100 - round(i / nrow(df) * 100)), collapse = "")
      if (i != nrow(df)) {
        cat('\014')
        cat(
          paste0(
            "[",
            avance,
            reste,
            "]",
            round(i / nrow(df) * 100),
            "%, restant: ",
            round(temps),
            unites,
            ", itération n°",
            i + 1
          )
        )
      } else{
        cat('\014')
        cat(paste0("[", avance, reste, "]", round(i / nrow(df) * 100), "%: Done"))
      }
      ## fin barre de chargement et timeur
    }## fin boucle for
    
    liste_finale <- bind_rows(liste_finale)
    
    #HOmogénéisation des taxons entre variétés et espèces (morphe, variété, sous-espèce)
    liste_finale$Espèce <-   ifelse(
      is.na(liste_finale$Espèce) == TRUE &
        is.na(liste_finale$"Sous-Espèce") == FALSE,
      str_extract(
        liste_finale$"Sous-Espèce",
        "[:alpha:]{1,}[:blank:]{1,}[:alpha:]{1,}"
      ),
      liste_finale$Espèce
    )
    
    liste_finale$Espèce <-   ifelse(
      is.na(liste_finale$Espèce) == TRUE &
        is.na(liste_finale$Forme) == FALSE,
      str_extract(
        liste_finale$"Forme",
        "[:alpha:]{1,}[:blank:]{1,}[:alpha:]{1,}"
      ),
      liste_finale$Espèce
    )
    
    liste_finale$Espèce <-   ifelse(
      is.na(liste_finale$Espèce) == TRUE &
        is.na(liste_finale$Variété) == FALSE,
      str_extract(
        liste_finale$"Variété",
        "[:alpha:]{1,}[:blank:]{1,}[:alpha:]{1,}"
      ),
      liste_finale$Espèce
    )
    
    liste_finale$Espèce <-
      ifelse(
        str_detect(liste_finale$Espèce, " x ") == FALSE,
        str_extract(
          liste_finale$Espèce,
          "[:alpha:]{1,}[:blank:]{1,}[:alpha:]{1,}"
        ),
        liste_finale$Espèce
      )
    
    
    liste_finale$Genre <-   ifelse(
      is.na(liste_finale$Espèce) == FALSE &
        is.na(liste_finale$Genre) == TRUE,
      str_extract(liste_finale$"Espèce", "[:alpha:]{1,}"),
      liste_finale$Genre
    )
    
    ###### travaux /!\ ######
    liste_finale$Ordre <- ifelse(
      liste_finale$Ordre %>%
        str_detect("Groupe[:blank:]{1}[:alpha:]{1,}[:blank:]{1}indéterminées") != FALSE,
        liste_finale$Ordre %>% str_extract("(?<=[:blank:])[:alpha:]{1,}(?=[:blank:])"),
      liste_finale$Ordre)
    #####fin travaux /!\ ######
    
    if (lien %>% is.na() == FALSE) {
      write.csv(liste_finale, file = lien,row.names = FALSE)
    }
    
    return(liste_finale)
  }

fun_remonter_codetaxon <-
  function(df, df_code, lien = "/home/theo/Bureau/code_taxons.csv",adresse_sandre = 2) {
    # Df = tableau d'une colonne contenant uniquement les noms de taxons à rechercher; df_code l'ensemble du tableau tsa_appellation_taxons de la BDD plando
    df <-
      filter(df, cd_apt != 0) # on enlève ceux qui n'ont pas de code
    liste_finale <-
      list() # la liste qui contiendra notre tableau finale (chaque fin de boucle while sera un tableau dans cette liste, i.e. une ligne dans le tableau finale)
    times2 <- c() # juste pour un décompte du temps
    for (i in 1:nrow(df)) {
      # boucle qui tourne sur les codes à rmeonter
      tic()# on commence à décompter
      code_origine <- df[[1]][i] #
      code_base <- df[[1]][i]
      liste_taxo <-list() # liste intermédiaire rempli à chauqe boucle while qui sera donc une ligne dans liste_finale
      
      if(adresse_sandre == 1){
        fun_test <- function(cd) {
          read_html(paste0("http://www.sandre.eaufrance.fr/urn.php?urn=urn:sandre:donnees:APT:FRA:code:",cd,":::referentiel:3.1:html")) %>% # test contient les variables du tableau du SANDRE, l'objet nous servira à tester si ce tableau contien bien les variables d'intérêts
            html_nodes(xpath = '//*[@id="table-informations"]') %>%
            html_table(fill = TRUE) %>%
            as.data.frame() %>%
            na.omit() %>%
            transpose() %>%
            setnames(colnames(.),
                     slice(., 1) %>% as.character()) %>%
            slice(-1) %>% return()
        } 
      }else{
        fun_test <- function(cd) {
          read_html(paste0("http://id.eaufrance.fr/apt/",cd)) %>% # test contient les variables du tableau du SANDRE, l'objet nous servira à tester si ce tableau contien bien les variables d'intérêts
            html_nodes(xpath = '//*[@id="table-informations"]') %>%
            html_table(fill = TRUE) %>%
            as.data.frame() %>%
            na.omit() %>%
            transpose() %>%
            setnames(colnames(.),
                     slice(., 1) %>% as.character()) %>%
            slice(-1) %>% return()
        }
      }
      j = 1 # indice pour la boucle while
      
      
      
      while (code_origine %>% is.na() != TRUE) {
        # tant qu'on peut remonter dans l'arbre taxo
        # boucle qui teste si le dernier parent a été atteint i.e. cd_apt_parent == NA
        test_code <- filter(df_code, cd_apt == code_origine)
        
        if ((test_code[["typ_apt"]] != "R" |
             test_code[["st_apt"]] != "Validé") &
            str_extract(test_code[["comment_apt"]], "\\[([:digit:]{1,})\\]") %>% is.na() == FALSE) {
          code_origine <-str_extract(test_code[["comment_apt"]], "\\[([:digit:]{1,})\\]") %>% str_extract("[:digit:]{1,}")
        }
        
        if ((test_code[["typ_apt"]] != "R" |
             test_code[["st_apt"]] != "Validé") &
            str_extract(test_code[["comment_apt"]], "\\[([:digit:]{1,})\\]") %>% is.na()) {
          
          test_erreur <-
            possibly(fun_test, "erreur")(code_origine)
          
          if(test_erreur[[1]]=="erreur"){
            while(test_erreur[[1]]=="erreur"){
              test_erreur <-
                possibly(fun_test, "erreur")(code_origine)
              Sys.sleep(10)
            }
          }
          
          
          if (test_erreur[[1]] !="erreur" & any(colnames(test_erreur) == "Libellé de l'Appellation Référente du Taxon")) {
            code_origine <-
              str_extract(test_erreur$`Libellé de l'Appellation Référente du Taxon`,
                          "[:digit:]{1,}")
          }
        }
        
        
        liste_taxo[[j]] <-
          filter(df_code, cd_apt == code_origine) #retrouve le taxon en question dans la BDD
        
        code_origine <- liste_taxo[[j]][["cd_apt_parent"]] #test
        
        j = j + 1
        
      }
      
      
      
      
      code <- liste_taxo[[1]][1] %>% as.numeric()
      liste_taxo <- bind_rows(liste_taxo)
      
      for (k in 1:nrow(liste_taxo)) {
        if (any(liste_taxo[k, "niveau_taxo"] == liste_taxo[-k, "niveau_taxo"])) {
          liste_taxo[which(liste_taxo[k, "niveau_taxo"] == liste_taxo[-k, "niveau_taxo"]), "niveau_taxo"] <-
            paste0(liste_taxo[which(liste_taxo[k, "niveau_taxo"] == liste_taxo[-k, "niveau_taxo"]), "niveau_taxo"], 1)
        }
      }
      
      liste_finale[[i]] <- liste_taxo %>%
        dplyr::select(niveau_taxo, nom_latin_apt) %>%
        transpose() %>%
        as.data.frame() %>%
        setnames(colnames(.), slice(., 1) %>% as.character()) %>% slice(-1) %>%
        dplyr::mutate(code_base = df[[1]][i], code_retenu = code)
      
      
      ### Barre de chargement et timeur pour la fonction
      times1 <- toc(quiet = TRUE)
      times2[i] <-
        times1$toc[[1]] - times1$tic[[1]] # temps passer pour faire la boucle i ti
      temps <-
        (sum(times2) / i) * (nrow(df) - i) # temps restant estimé
      
      if (temps > 60 & temps <= 3600) {
        temps <- round(temps / 60)
        unites <- "min"
      } else{
        temps <- temps
        unites <- "sec"
      }
      
      Sys.sleep(.0001)
      avance <-
        str_c(rep("=", round(i / nrow(df) * 100)), collapse = "")
      reste <-
        str_c(rep(".", 100 - round(i / nrow(df) * 100)), collapse = "")
      if (i != nrow(df)) {
        cat('\014')
        cat(
          paste0(
            "[",
            avance,
            reste,
            "]",
            round(i / nrow(df) * 100),
            "%, restant: ",
            round(temps),
            unites,
            ", itération n°",
            i + 1
          )
        )
      } else{
        cat('\014')
        cat(paste0("[", avance, reste, "]", round(i / nrow(df) * 100), "%: Done"))
      }
      ## fin barre de chargement et timeur
    }## fin boucle for
    
    liste_finale <- bind_rows(liste_finale)
    
    #HOmogénéisation des taxons entre variétés et espèces (morphe, variété, sous-espèce)
    liste_finale$Espèce <-   ifelse(
      is.na(liste_finale$Espèce) == TRUE &
        is.na(liste_finale$"Sous-Espèce") == FALSE,
      str_extract(
        liste_finale$"Sous-Espèce",
        "[:alpha:]{1,}[:blank:]{1,}[:alpha:]{1,}"
      ),
      liste_finale$Espèce
    )
    
    liste_finale$Espèce <-   ifelse(
      is.na(liste_finale$Espèce) == TRUE &
        is.na(liste_finale$Forme) == FALSE,
      str_extract(
        liste_finale$"Forme",
        "[:alpha:]{1,}[:blank:]{1,}[:alpha:]{1,}"
      ),
      liste_finale$Espèce
    )
    
    liste_finale$Espèce <-   ifelse(
      is.na(liste_finale$Espèce) == TRUE &
        is.na(liste_finale$Variété) == FALSE,
      str_extract(
        liste_finale$"Variété",
        "[:alpha:]{1,}[:blank:]{1,}[:alpha:]{1,}"
      ),
      liste_finale$Espèce
    )
    
    liste_finale$Espèce <-
      ifelse(
        str_detect(liste_finale$Espèce, " x ") == FALSE,
        str_extract(
          liste_finale$Espèce,
          "[:alpha:]{1,}[:blank:]{1,}[:alpha:]{1,}"
        ),
        liste_finale$Espèce
      )
    
    
    liste_finale$Genre <-   ifelse(
      is.na(liste_finale$Espèce) == FALSE &
        is.na(liste_finale$Genre) == TRUE,
      str_extract(liste_finale$"Espèce", "[:alpha:]{1,}"),
      liste_finale$Genre
    )
    
    ###### travaux /!\ ######
    liste_finale$Ordre <- ifelse(
      liste_finale$Ordre %>%
        str_detect("Groupe[:blank:]{1}[:alpha:]{1,}[:blank:]{1}indéterminées") != FALSE,
      liste_finale$Ordre %>% str_extract("(?<=[:blank:])[:alpha:]{1,}(?=[:blank:])"),
      liste_finale$Ordre)
    #####fin travaux /!\ ######
    
    if (lien %>% is.na() == FALSE) {
      write.csv(liste_finale%>%
                  dplyr::select(
                    code_base,
                    code_retenu,
                    Forme,
                    Variété,
                    Sous.Espèce,
                    Espèce,
                    Genre,
                    Tribu,
                    Tribu1,
                    Sous.Famille,
                    Sous.Famille1,
                    Famille,
                    Super.Famille,
                    Micro.Ordre,
                    Infra.Ordre,
                    Sous.Ordre,
                    Ordre,
                    Ordre1,
                    Super.Ordre,
                    Infra.Classe,
                    Sous.Classe,
                    Sous.Classe1,
                    Classe,
                    Classe1,
                    Super.Classe,
                    Infra.Embranchement,
                    Sous.Embranchement,
                    Sous.Embranchement1,
                    Embranchement,
                    Embranchement1,
                    Infra.Règne,
                    Infra.Règne1,
                    Sous.Règne,
                    Règne
                  ), file = lien,row.names = FALSE)
    }
    
    return(liste_finale%>%
             dplyr::select(
               code_base,
               code_retenu,
               Forme,
               Variété,
               Sous.Espèce,
               Espèce,
               Genre,
               Tribu,
               Tribu1,
               Sous.Famille,
               Sous.Famille1,
               Famille,
               Super.Famille,
               Micro.Ordre,
               Infra.Ordre,
               Sous.Ordre,
               Ordre,
               Ordre1,
               Super.Ordre,
               Infra.Classe,
               Sous.Classe,
               Sous.Classe1,
               Classe,
               Classe1,
               Super.Classe,
               Infra.Embranchement,
               Sous.Embranchement,
               Sous.Embranchement1,
               Embranchement,
               Embranchement1,
               Infra.Règne,
               Infra.Règne1,
               Sous.Règne,
               Règne
             ))
  }









############# fonction pour faire travauller sur des tableau espèces qui renvoie un tableau de contingence avec les moyennes d'abondance
##  renvoie un tablmeau de contingence + un tableau résumant par espèce, le nombre de lacs où elle a été échantillonnée
fun_table_taxo <- function(df, seuil_prop = 5) {
  df_descriptif <- df %>%
    group_by(code_lac, nom_taxo) %>%
    dplyr::summarise(p = 1) %>%
    spread(code_lac, p, fill = 0) %>%
    mutate(n = dplyr::select_if(., is.numeric) %>% rowSums,
           prop = (n / (ncol(.) - 2)) * 100) %>%
    dplyr::select(nom_taxo, n, prop) %>%
    arrange(desc(prop)) %>% 
    dplyr::filter(prop > seuil_prop)
  vec_taxon_filtre <- df_descriptif$nom_taxo
  df_contingence <- df %>%
    dplyr::filter(nom_taxo %in%vec_taxon_filtre) %>% 
    group_by(code_lac, nom_taxo) %>%
    dplyr::summarise(moy = mean(abondance)) %>%
    spread(nom_taxo, moy, fill = 0)
  return(list(df_descriptif,df_contingence))
}
    

############# Fonction pour le codage flou pour les variables catégorielles
  

fun_codage_flou <- function(liste,nom_df,var,var_name = "AFF_"){
  var_2 <- var
  var <- rlang::syms(var)
  df <- liste[[nom_df]] %>% dplyr::select(c(SBS_name,all_of(var_2))) %>% na.omit()
  df_codage_flou <- df %>% 
    group_by(SBS_name,!!!var) %>%
    dplyr::summarise(n=n()) %>%
    spread(var_2,n,fill=0) %>%
    ungroup() %>% 
    mutate(somme =  dplyr::select_if(., is.numeric) %>% rowSums) %>% 
    mutate_if(is.numeric,~./somme) %>% 
    dplyr::select(-somme) %>% 
    setnames(colnames(.),
             c("nom_taxon",
               paste0(
                 rep(var_name,ncol(.)-1),
                 str_replace_all(tolower(colnames(.)[-1])," ","_")
               )
             )
    )
  
   df_unique <- df %>% 
    group_by(SBS_name,!!!var) %>%
    dplyr::summarise(n=n()) %>%
    group_by(SBS_name) %>% 
    nest() %>%
    mutate(a = map_chr(data,~ifelse(length(unique(.$n))> 1 & max(.$n) >1,
                                    filter(.,n == max(n)) %>% pull(1),
                                    sample_n(.,1) %>% pull(1)))) %>% 
    dplyr::select(SBS_name,a) %>% 
    setnames("a",var_2)
    return(list(df_codage_flou,df_unique))
   
   df_unique_combinaison <- df
} 
# liste<- liste_DIT
# nom_df<- "dispersal_type"
# var<-vec_var[[3]]
# var_name = "AFF_"
# vec_var
# nom_var

######## fonction PCA ########
fun_pca <- function(acp,
                    axes=c(1,2),
                    biplot = TRUE,
                    ind = FALSE,
                    repel = FALSE,
                    tab = FALSE) {
  if (tab == FALSE) {
    if (biplot == TRUE) {
      if (ind == TRUE) {
        fviz_pca_biplot(acp,
                        axes=axes,
                        repel = repel,
                        col.var = "#2E9FDF",
                        # Variables color
                        col.ind = "#696969")  # Individuals color) %>% return()
      } else{
        fviz_pca_biplot(
          acp,
          axes=axes,
          repel = repel,
          col.var = "#2E9FDF",
          geom.ind = "point",
          # Variables color
          col.ind = "#696969"
        )  # Individuals color) %>% return()
      }
    } else{
      a <- fviz_pca_ind(
        acp,
        axes=axes,
        col.ind = "cos2",
        repel = repel,
        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
      )
      b <- fviz_pca_var(
        acp,
        axes=axes,
        col.var = "contrib",
        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
        repel = repel
      )
      ggarrange(plotlist = list(a, b)) %>% return()
    }
  } else{
    vec_inertie_var <-
      inertia.dudi(acp, col.inertia = TRUE)[[1]]$inertia
    
    inertie <-
      round(vec_inertie_var / sum(vec_inertie_var) * 100, 2)
    a <- inertia.dudi(acp, col.inertia = TRUE)[[3]] %>%
      tibble::rownames_to_column(var = "variables") %>%
      as_tibble() %>%
      arrange(desc(Axis1)) %>%
      mutate_if(is.numeric,  ~ round(., 2)) %>%
      setnames(colnames(.)[-1], paste0("axe", 1:(ncol(.) - 1), " (", inertie[1:ncol(.) -
                                                                               1], "%)")) %>% as.data.frame()
    for (i in 2:ncol(a))a[, i] <- color_bar("lightblue")(a[, i])
    tab <- a %>% formattable()
    return(tab)
    cat(paste0(1:length(inertie), ":", cumsum(round(inertie)), "%"))
  }
}

########## is.binaire ######### permet de tester si un vecteur ou une colonne est binaire
is.binaire <- function(df){
  binaire <- c()
  if(is.null(ncol(df))==FALSE){
    for(i in 1:ncol(df)){
      binaire[i] <- all(df[,i] %>% pull() %>% unique() %in%c(0,1),na.rm=TRUE)
    }
  }else{
    binaire <- all(df %>% unique() %in%c(0,1),na.rm=TRUE)
  }
  return(binaire)
}



########### fonction pour filtrer les NA par ligne ou colonnes en applicant un seuil ##########
# le seuil correspond à une proportion de NA par ligne ou colonnes à partir du quelle on ne gardera pas les données
# seuil de données disponible (0.2 = on garde si il y a plus de 20% des données disponible)
fun_NA <- function(df, seuil = 0.2, bycol = TRUE) {
  if (bycol == FALSE) {
    tab <-
      df[(!apply(df, MARGIN = 2, is.na)) %>% rowSums(.) / ncol(df)> seuil, ]
  } else{
    tab <-
      df[, (!apply(df, MARGIN = 2, is.na)) %>% colSums(.) / nrow(df) > seuil]
  }
  return(tab)
}

fun_NA2 <- function(df,seuil_row=0.3,seuil_col=0.3,colfirst=TRUE){
  if(colfirst==TRUE){
    filtre_col <- fun_NA(df,seuil = seuil_col,bycol = TRUE)
    fun_NA(filtre_col,seuil = seuil_row,bycol = FALSE) %>% return()
  }else{
    filtre_row <- fun_NA(df,seuil = seuil_row,bycol = FALSE)
    fun_NA(filtre_row,seuil = seuil_col,bycol = FALSE) %>% return()
  }
}


######################## fonction calcul de diversite
fun_div <- function(df_c){
  
  df_c <- ungroup(df_c)
  esp_0 <- df_c %>% as.data.frame()%>% column_to_rownames() %>%dplyr::select_if( colSums(.)==0) %>% colnames()
  df_com <- df_c%>% dplyr::select(-esp_0)

  
  diversite_spe <- map_dfc(c("shannon","simpson"),
                           ~df_com %>%
                             dplyr::select(-code_lac) %>%
                             diversity(index=.x)) %>% setnames(colnames(.),c("shannon","simpson")) %>% 
    mutate(code_lac = df_c$code_lac) 
  richesse <- df_com %>% dplyr::select(-code_lac) %>% mutate_all(~ifelse(.>0,1,0)) %>% rowSums()
  diversite_spe <- diversite_spe %>%  mutate(richesse=richesse)  %>% 
    return()
}

df <- natu
##################### fonction PCA composantes
fun_comp <- function(df,site=table_physicochimie$code_lac,nax=0,seuil=60,plot=TRUE,nom="PC_"){
  
  acp <- df %>% select_if(is.numeric) %>% na.omit() %>%  dudi.pca(scannf = FALSE,nf=ncol(df))
  
  vec_inertie_var <- inertia.dudi(acp, col.inertia = TRUE)[[1]]$inertia
  
  inertie <- round(vec_inertie_var / sum(vec_inertie_var) * 100, 2) %>% cumsum()
  if(nax==0){
    nombre_axes <- which.max(inertie>seuil)+1
  }else{
    nombre_axes <- nax+1
  }
  
  data <- cbind(code_lac=site,acp$l1) %>% dplyr::select(1:nombre_axes) %>% 
    setnames(colnames(.)[-1],paste0(nom,1:(nombre_axes-1)))
  
  if(plot ==TRUE){
    fun_pca_safe <- possibly(fun_pca,NA)
    plot_12 <- fun_pca(acp,axes=c(1,2))
    plot_34 <- fun_pca_safe(acp,axes=c(3,4))
    plot_56 <- fun_pca_safe(acp,axes=c(5,6))
    return(list(data,plot_12,plot_34,plot_56))
    
  }else{
    return(list(data,acp))
  }
  
}


################### Fonction lasso
fun_lasso <- function(data,y = "phy_FRic",p = 0.7){
  formule <- as.formula(paste(y,"~."))
  training.samples <- data %>% pull(y) %>%  createDataPartition(p = p, list=F) 
  train.data <- data[training.samples,]
  test.data <- data[-training.samples,]
  env <- model.matrix(formule, train.data )[,-1]
  cv_lasso <- glmnet::cv.glmnet(x = env, y = train.data %>% pull(y), alpha = 1)
  lambda <- cv_lasso$lambda.min
  lasso <- glmnet::glmnet(x = env, y = train.data %>% pull(y), alpha = 1, lambda = lambda)
  x.test <- model.matrix(formule, test.data)[,-1]
  predictions <- lasso %>% predict(newx = x.test)
  plot(predictions,test.data %>% pull(y))
  return(list(R2=caret::R2(predictions,test.data %>% pull(y)),
              coef(lasso)))
  
}


################### FOnction reg lin = selection BIC AIC
fun_modele_div_lin <-
  function(data,
           tax = "phy",
           indice = "phy_FRic",
           direction = "both") {
    
    fun_select_tax <- function(df, tax = "mac", ind = "mac_FRic") {
      nom <- colnames(df) %>% str_subset(tax, negate = TRUE)
      nom_var_rep <- colnames(df) %>%  str_subset(ind)
      df %>% dplyr::select(all_of(c(ind, nom))) %>% return()
    }
    
    tab <-
      function(x) {
        tidy(x) %>% mutate_if(is.numeric,  ~ round(., 3)) %>% arrange(desc(abs(estimate)))
      }
    data <-
      fun_select_tax(data, tax = tax, ind = indice) %>% na.omit() %>% dplyr::select(-code_lac)
    formule <- as.formula(paste0(indice , "~ ."))
    ml_lin <- lm(formule, data = data)
    aic_lin <- stepAIC(ml_lin, k = 2, direction = direction)
    bic_lin <- stepAIC(ml_lin, k = log(nrow(data)), direction = direction)
    model <- list(lin = ml_lin, aic = aic_lin, bic = bic_lin)
    param <- map(model,  ~ list(tab(.), glance(.)))

    return(param)
  }

fun_select_tax <- function(df, tax = "mac", ind = "mac_FRic") {
  nom <- colnames(df) %>% str_subset(tax, negate = TRUE)
  nom_var_rep <- colnames(df) %>%  str_subset(ind)
  df %>% dplyr::select(all_of(c(ind, nom))) %>% return()
}


########## XGBOOST ############
fun_cv_xgboost <- function(table_data, indice="fri", booster = "gbtree"){
  train.data_cv <- table_data %>% mutate_if(is.character,as.factor) %>% na.omit()
  traintask <- makeRegrTask(data = as.data.frame(data.matrix(train.data_cv)), target = indice)
  # testtask <- makeRegrTask(data= as.data.frame(data.matrix(test.data)), target = "poi_FRic")
  lrn <- makeLearner("regr.xgboost",predict.type = "response")
  # lrn$par.vals <- list(
  #   objective="reg:squarederror",
  #   eval_metric="rmse",
  #   nrounds=10L,
  #   eta=0.01
  # )
  lrn$par.vals <- list(
    objective="reg:squarederror",
    eval_metric="rmse",
    nthread = 2,
    verbose = 1
  )
  if(booster == "gbtree"){
    params <- makeParamSet(
      makeDiscreteParam("booster",values = booster),
      makeIntegerParam("nrounds",lower = 10, upper = 100), # CHANGE
      makeNumericParam("eta", lower = .1, upper = .5),
      makeIntegerParam("max_depth",lower = 3L,upper = 7L),
      makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
      makeNumericParam("subsample",lower = 0.5,upper = 1),
      makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
      makeIntegerParam("gamma",lower = 0, upper =8),
      makeNumericParam("alpha",lower=-1,upper = 0,trafo = function(x) 10^x))
  }else{ # ajouter alpha pour une régularisation L1 pour virer des variables
    params <- makeParamSet(
      makeDiscreteParam("booster",values = booster),
      makeIntegerParam("nrounds",lower = 10, upper = 100), # CHANGE
      makeNumericParam("eta", lower = .1, upper = .5),
      makeNumericParam("alpha",lower=-1,upper = 0,trafo = function(x) 10^x))
  }
  
  rdesc <- makeResampleDesc("Bootstrap",stratify = FALSE)
  
  ctrl <- makeTuneControlRandom(maxit = 50L)
  
  mytune <- tuneParams(learner = lrn
                       ,task = traintask
                       ,resampling = rdesc
                       ,measures = rmse
                       ,par.set = params
                       ,control = ctrl
                       ,show.info = T)
  return(mytune)
}

fun_xgboost <- function(data,indice='fri'){
  xgb.importance.safe <-possibly(xgb.importance,NULL)
  xgb.plot.importance.safe <- possibly(xgb.plot.importance,NULL)
  # préparation des données
  mod_bic <- fun_aic(data,pos=indice,k="BIC")
  table_data <- mod_bic[[4]] %>% na.omit() %>% mutate_if((is.numeric(.))&(names(.)!=indice),~scale(.))
  BIC <- mod_bic[1:2]
  train.samples <- caret::createDataPartition(table_data %>%
                                                pull(indice) %>%
                                                na.omit(), p = 1, list = FALSE)
  train.data <- table_data[train.samples,]
  # test.data <- data_phy[-train.samples,]
  sparse_matrix <- model.matrix(as.formula(paste(indice, "~ .")), data = train.data)[,-1] %>% return()
  
  
  dtrain <-xgb.DMatrix(data=sparse_matrix, label=train.data[[indice]])
  
  param_estim_linear <- fun_cv_xgboost(table_data=table_data, indice = indice, booster = "gblinear") %>% .$x
  
  param_estim_tree <- fun_cv_xgboost(table_data=table_data,indice = indice,booster = "gbtree") %>% .$x
  
  
  mod_xgb_linear <- xgboost(data = dtrain,
                            params = param_estim_linear,
                            nrounds = param_estim_linear$nrounds,
                            verbose = 1)
  
  mod_xgb_tree <- xgb.train(data = dtrain,
                            tree_method = 'exact',
                            params = param_estim_tree,
                            nrounds = param_estim_tree$nrounds,
                            verbose = 1)
  
  varimp_linear <- xgb.importance (feature_names = colnames(sparse_matrix),model = mod_xgb_linear)
  linear_plot <- xgb.plot.importance (importance_matrix = varimp_linear[1:34])
  
  varimp_tree <- xgb.importance.safe (feature_names = colnames(sparse_matrix),model = mod_xgb_tree)
  tree_plot <- xgb.plot.importance.safe (importance_matrix = varimp_tree[1:34])
  
  
  list_final <- list(tree_plot = tree_plot, 
                     linear_plot = linear_plot,
                     varimp_linear = varimp_linear,
                     varimp_tree = varimp_tree,
                     bic = BIC,
                     param_estim_linear = param_estim_linear,
                     param_estim_tree =param_estim_tree) %>% return()
}


fun_aic <- function(data,pos='fri',k="BIC",res=TRUE,mod=TRUE,categ="typo2"){
  
  var <- names(data[,pos])
  data <- na.omit(data)
  if(pos!="fri"){
    table_data <- data %>% 
      dplyr::select(!matches("comp")) %>% 
      dplyr::select(pos,8:ncol(.)) %>% 
      dplyr::mutate_if(is.character,as.factor) %>% 
      mutate_if(((is.numeric(.))&(names(.)!=pos)),~scale(.))
  }else{
    if(res == FALSE){
      table_data <- data %>% 
        dplyr::select(!matches("comp")) %>% 
        dplyr::select(pos,"ric",8:ncol(.)) %>% 
        dplyr::mutate_if(is.character,as.factor) %>% 
        mutate_if(((is.numeric(.))&(names(.)!=pos)),~scale(.))
    }else{
      data_res <- data %>% 
        na.omit() %>% 
        dplyr::select(!matches("comp")) %>% 
        dplyr::select(pos,"ric",8:ncol(.)) %>% 
        lm(fri ~ ric,data=.) %>% 
        .$res
      table_data <- data %>% 
        na.omit() %>% 
        dplyr::select(!matches("comp")) %>% 
        dplyr::select(8:ncol(.)) %>% 
        dplyr::mutate_if(is.character,as.factor) %>% 
        mutate_if(((is.numeric(.))&(names(.)!=pos)),~scale(.)) %>% 
        dplyr::mutate(fri = data_res)
      
    }
  }
  
  plot <- table_data %>% 
    dplyr::select_if(is.numeric) %>% 
    gather('var','val',-as.symbol(var)) %>%
    ggplot(aes_string(x="val",y=var)) +
    geom_point()+
    facet_wrap(~var,scale="free")+
    geom_smooth(method="lm")
  
  if(mod == TRUE){
    var_num <- dplyr::select_if(table_data,(map_lgl(table_data,~is.numeric(.))& names(table_data)!=pos)) %>% 
      names()
    mod <- as.formula(paste0(pos," ~ (",paste("+",var_num,collapse = " ") %>% str_sub(start = 3,end=-1),") *", categ))
    mod_comp <- lm(mod, data = table_data)
  }else{
    mod_comp <- lm(as.formula(paste(names(data[,pos]) ,'~.')),data=table_data)
    
  }
  
  if (k %in% c("BIC","AIC")) {
    if (k == "AIC") {
      k = 2
    } else{
      k = log(nrow(na.omit(data)))
    }
    
    mod <- MASS::stepAIC(mod_comp, k = k, trace = FALSE)
  }else if(k == "AICc"){
    
    mod <- StepReg::stepwise(data=table_data %>% dplyr::select(-typo),
                             y=pos,
                             Class = "typo2",
                             selection = "bidirection",
                             select="AICc")
  }else{
    cat("Error : k must me 'AIC', 'AICc' or 'BIC'")
  }
  
  model <- broom::tidy(mod) %>%
    arrange(abs(estimate)) %>%
    mutate_if(is.numeric,~round(.,digits = 3))
  qqPlot(mod)
  R2 <- glance(mod)
  
  return(list(model,R2,plot,table_data))
}

####### Transformation box cox ---------------------------------------------------------------------------------------------
fun_boxcox <- function(var,lim = c(-40,40),test = TRUE){
  if(test == TRUE){
    if(shapiro.test(var)$p.value < 0.05){
      res <- boxcox(var~1,lambda = seq(lim[1], lim[2], length = 1000))
      lambda <- res$x[which.max(res$y)]
      var_bc <- ((var^lambda)-1)/lambda %>% return()
    }else{
       var <- var %>% return()
    }
  }else{
    res <- boxcox(var~1,lambda = seq(lim[1], lim[2], length = 1000))
    lambda <- res$x[which.max(res$y)]
    var_bc <- ((var^lambda)-1)/lambda %>% return()
  }
}
###### dataframe to dist to df ---------------------------------------------------------------------------------------------
# permet de calculer une matrice de distance euclidienne à partir d'un tableau
# la première colonne contient les nom des sites
# les autres colonnes sont numériques
# l'argument nom contient le nom des futures colonnes
fun_df_to_dist_to_matrix <- function(df, nom) {
  pmap(
    list(
      df %>% dplyr::select(code_lac) %>% rep(ncol(df) - 1),
      map(df[, -1],  ~ .),
      nom
    ),
    ~ dist(bind_cols(..1, ..2) %>%
             column_to_rownames()) %>%
      matrixConvert(colname = c("code_lac1", "code_lac2", ..3))
  ) %>%
    join_all(type = "inner")%>% return()
}





df <- env_sem
lac <- df$code_lac
fun_prepa_env <- function(df, lac = lac_commun) {
  df <- df %>% dplyr::filter(code_lac %in% lac)
  table_env_boxcox <- df %>% dplyr::select(
    code_lac,
    ammo:perimetre_pla,
    prof_max_pla:alcalinite,
    tps_sej_moy:altitude,
    substrat_shannon:temp_ampli
  ) %>% mutate_if(is.numeric,  ~ fun_boxcox(. + 1))
  
  table_env_pourc <-
    df %>% dplyr::select(AGS:WLS, p_alter) %>% mutate_if(is.numeric,  ~ asin(sqrt(.)))
  
  table_volume <-
    df %>% dplyr::select(volume_pla) %>% mutate(volume_pla = log(log(volume_pla)))
  
  table_spatial <- df %>% dplyr::select(lat_pla, long_pla)
  
  table_env <-
    bind_cols(table_env_boxcox,
              table_env_pourc,
              table_volume,
              table_spatial)
  
  return(table_env)
}

