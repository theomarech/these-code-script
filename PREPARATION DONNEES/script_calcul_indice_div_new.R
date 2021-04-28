
# FONCTIONS POUR LA PREPA DES DONNEES -------------------------------------
# sélectionne les espèces suivant leur fréquence de représentation dans les lacs
fun_tax_freq <- function(df_c,freq=0.05){
  tab_freq <- df_c %>% mutate_if(is.numeric, ~ifelse(.>0,1,0)) %>% summarise_if(is.numeric, ~sum(.)/nrow(df_c)*100)
  vec_sel <- tab_freq %>% dplyr::select_if(.>freq*100) %>% colnames()
  vec_sup <- tab_freq %>% dplyr::select_if(.<freq*100) %>% colnames()
  tab_sel <- df_c %>% dplyr::select(code_lac,all_of(vec_sel)) 
  return(list(tab_tax_sel = tab_sel,vec_tax_supr = vec_sup))
}

# sélectionne les lacs suivant leur richesse minimale (exemple n = 4 signifie que les lacs avec moins de 4 espèces sont supprimés)
fun_lac_numb <- function(df_c,n = 4){
  lac <- df_c %>% 
    mutate_if(is.numeric,~dplyr::if_else(.>0,1,0)) %>% 
    as.data.frame() %>% 
    tibble::column_to_rownames(var="code_lac")%>% 
    t() %>%
    as.data.frame() %>% 
    dplyr::select(where(~sum(.)>(n))) %>% 
    t() %>% 
    as.data.frame() %>%
    dplyr::select_if(~sum(.)!=0) %>% 
    tibble::rownames_to_column("code_lac") %>% .$code_lac
  df_c %>% dplyr::filter(code_lac %in% lac) %>% return()
}

# combine les tableaux de traits et d'abondance/présence absence pour obtenir deux tableaux homogènes

fun_combine_tra_com <- function(df_t,df_c){
 tra <-  df_t %>% dplyr::filter(nom_taxo %in% colnames(df_c)[-1]) %>% arrange(nom_taxo)
 com <-  df_c %>% dplyr::select(code_lac, all_of(tra$nom_taxo))
 return(list(df_t = tra, df_c = com))
}
# supprime les variables avec une variance très faible
fun_zerovar <- function(df){
  df %>% dplyr::select(!nearZeroVar(.)) %>% return()
}

# prend un tableau de trait en entrée et renvoie les axes synthétiques de la PCO (le nom des sites DOIT être inclu ex colonne nom_taxo en première position)
fun_pco_axes <- function(df,nax = 4, seuil_inertie = 60){
  
  dist <- df %>% column_to_rownames() %>% gawdis(w.type = "analytic")

  if(is.euclid(dist) == FALSE & is.euclid(sqrt(dist))==FALSE){
    warning(cat("la matrice de distance n'est pas euclidienne même après une transformation racine carré, la transformation racine sera tout de même appliquée"))
    dist <- sqrt(dist)
  }else{
    dist <- sqrt(dist)
  }
  
  pcoa <- dudi.pco(dist, nf = 10, scannf = FALSE)
  
  if(nax == 0){
     n <- sum(cumsum(pcoa$eig/sum(pcoa$eig)*100) < seuil_inertie)
     tab <- pcoa$li %>% tibble::rownames_to_column(var="nom_taxo") %>% dplyr::select(1:(n + 1))
  
   }else{
     tab <- pcoa$li %>% tibble::rownames_to_column(var="nom_taxo") %>% dplyr::select(1:(nax + 1))
   }
  return(list(trait = tab,inertie_cum = cumsum(pcoa$eig/sum(pcoa$eig)*100)))
}


fun_taxo <- function(df_c){
  df_com <- df_c %>% column_to_rownames()
  
  df_com_rich <- df_com %>% 
    mutate_all(~ifelse(.==0,0,1)) %>% 
    rowSums() %>% 
    data.frame(DIV_richesse=.) %>% 
    tibble::rownames_to_column(var="code_lac")
  
  diversite_spe <- map_dfc(c("shannon","simpson"),
                           ~df_com %>%
                             vegan::diversity(index=.x)) %>% setnames(colnames(.),c("DIV_shannon","DIV_simpson")) %>% 
    bind_cols(df_com_rich) %>% 
    mutate(DIV_pielou = DIV_shannon / log(DIV_richesse)) %>% 
    dplyr::select(code_lac,ric = DIV_richesse, sha = DIV_shannon, sim = DIV_simpson,pie = DIV_pielou) %>% return()
}

fun_func <- function(df_c,df_t){
  dbFD(df_t,df_c %>% column_to_rownames()) %>% 
  bind_cols() %>% 
  mutate(code_lac = df_c$code_lac) %>%
  dplyr::select(code_lac, fri = FRic, fev = FEve, fdi = FDiv, fdis = FDis,rao = RaoQ ) %>% return()
  }

fun_rarity <- function(df_c, dist_t) {
  uniq <-
    funrar::uniqueness(
      df_c %>% column_to_rownames() %>% decostand(method = "total") %>% as.matrix(),
      dist_t %>% as.matrix()
    )
  rest <-
    funrar::restrictedness(df_c %>% column_to_rownames() %>% decostand(method = "total") %>% as.matrix())
  tab <- inner_join(uniq, rest) %>% mutate(rarity = (Ri + Ui) / 2)
  
  df <- matrix(nrow = nrow(df_c), ncol = 4) %>% as.data.frame()
  names(df) <- c("code_lac", colnames(tab)[-1])
  df$code_lac <- df_c$code_lac
  for (i in 1:nrow(df_c)) {
    lac <- df_c[i, -1] %>% select_if(. > 0) %>% colnames()
    df[i, -1] <-
      dplyr::filter(tab, species %in% lac) %>% select_if(is.numeric) %>% summarise_all(mean)
  }
  return(df %>% dplyr::select(code_lac,unique =  Ui, restri = Ri , rarity))
}

# (1) calcul indice div poissons ----------------------------------------------

com_poi <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_poissons.csv")
tra_poi <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_poissons.csv") %>%
  mutate(prot_oeu = as.numeric(prot_oeu)) %>% 
  mutate(zone_ali = ifelse(zone_ali=="ben",0,1)) %>% 
  dplyr::select(c("nom_taxo","zone_ali","zone_rep","pref_hab",type_alim="typealimenttion","biomasse_med"))%>%
  mutate_if(is.character,as.factor)


table_poi <- fun_tax_freq(com_poi,0.01)[[1]] %>% 
  fun_lac_numb(n=4) %>%
  fun_combine_tra_com(tra_poi,.)

t_poi <- table_poi[[1]] %>% mutate(biomasse_med=log(biomasse_med))%>% column_to_rownames()  %>% gawdis(w.type = "analytic") %>% sqrt()
c_poi <- table_poi[[2]]

func_poi <- fun_func(c_poi,t_poi)

rarity_poi <- fun_rarity(c_poi,t_poi)

taxo_poi <- fun_taxo(c_poi)



table_poi <- join_all(list(func_poi,rarity_poi,taxo_poi),type="inner",by="code_lac")%>% setnames(colnames(.)[-1],paste0("poi_",colnames(.)[-1]))




# (2) calcul indice div macrophytes ----------------------------------------------

com_mac <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macrophytes.csv")
tra_mac <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macrophytes.csv") %>% 
  dplyr::select(-c(FRU,DIS,SES,OCC,FDF)) %>% 
  mutate_if(is.numeric,log) %>% 
  mutate_if(is.character,as.factor)


table_mac <- fun_tax_freq(com_mac,0.01)[[1]] %>% 
  fun_lac_numb(n=4+1) %>%
  fun_combine_tra_com(tra_mac,.)

t_mac <- table_mac[[1]] %>%  column_to_rownames()  %>% gawdis(w.type = "analytic") %>% sqrt()
c_mac <- table_mac[[2]]


func_mac <- fun_func(c_mac,t_mac)

rarity_mac <- fun_rarity(c_mac,t_mac)

taxo_mac <- fun_taxo(c_mac)



table_mac <- join_all(list(func_mac,rarity_mac,taxo_mac),type="inner",by="code_lac")%>% setnames(colnames(.)[-1],paste0("mac_",colnames(.)[-1]))



# (3) calcul indice div phytoplancton ----------------------------------------------

com_phy <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_phytoplancton_biovolume.csv",locale = locale(encoding = "latin1"))
tra_phy <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_phyto_ratio.csv",locale = locale(encoding = "latin1"))  %>% 
  mutate_if(is.numeric,log)%>% 
  mutate_if(is.binaire,as.numeric) %>% 
  dplyr::select(-c(Res_env,Organic_carbon_ratio))%>%
  mutate(
    Reproduction = case_when(
      Reproduction  %in% c(
        "Asexual-binary fission",
        "Asexual-binary fission or akinete",
        "Asexual"
      ) ~ "Asexual",
      Reproduction %in% "Sexual and asexual" ~ "SexualAsexual",
      TRUE ~ "Sexual"
    )
  ) %>% 
  mutate_if(is.character,as.factor)

summary(tra_phy)
tra_phy %>% select_if(is.numeric) %>% chart.Correlation()
colnames(tra_phy)
vis_dat(com_phy)



group_phy <- c(1,2,3,4,5,6,6,7,6,8,6,9)


table_phy <- fun_tax_freq(com_phy,0.01)[[1]] %>% 
  fun_lac_numb(n=4) %>%
  fun_combine_tra_com(tra_phy,.)

t_phy <- table_phy[[1]] %>%  column_to_rownames()  %>% gawdis(w.type = "optimized",groups = group_phy) %>% sqrt()
c_phy <- table_phy[[2]]

func_phy <- fun_func(c_phy,t_phy)

rarity_phy <- fun_rarity(c_phy,t_phy)

taxo_phy <- fun_taxo(c_phy)

table_phy <- join_all(list(func_phy,rarity_phy,taxo_phy),type="inner",by="code_lac")%>% setnames(colnames(.)[-1],paste0("phy_",colnames(.)[-1]))



# (4) calcul indice div macroinvertébrés IML ----------------------------------

com_iml <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres_iml.csv")
tra_iml <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macroinvertebres_iml.csv")

group_iml = c(rep(1,4),rep(2,4),rep(3,10),rep(4,2),rep(5,8),rep(6,7),rep(7,3),rep(8,8),rep(9,5),rep(10,4))

table_iml <- fun_tax_freq(com_iml,0.03)[[1]] %>% 
  fun_lac_numb(n=5) %>%
  fun_combine_tra_com(tra_iml,.)


t_iml <- table_iml[[1]] %>%
  column_to_rownames()  %>%
  gawdis(w.type = "analytic",
         groups = group_iml,
         fuzzy = c(unique(group_iml))) %>% 
  sqrt()

c_iml <- table_iml[[2]]


func_iml <- fun_func(c_iml,t_iml)

rarity_iml <- fun_rarity(c_iml,t_iml)

taxo_iml <- fun_taxo(c_iml)


table_iml <- join_all(list(func_iml,rarity_iml,taxo_iml),type="inner",by="code_lac")%>% setnames(colnames(.)[-1],paste0("iml_",colnames(.)[-1]))



# (5) calcul indice div macroinvertébrés IZL ----------------------------------

com_izl <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres_inv_zl.csv")
tra_izl <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macroinvertebres_inv_zl.csv")

group_izl = c(rep(1,4),rep(2,4),rep(3,10),rep(4,2),rep(5,8),rep(6,7),rep(7,3),rep(8,8),rep(9,5),rep(10,4))

table_izl <- fun_tax_freq(com_izl,0.01)[[1]] %>% 
  fun_lac_numb(n=5) %>%
  fun_combine_tra_com(tra_izl,.)


t_izl <- table_izl[[1]] %>%
  column_to_rownames()  %>%
  gawdis(w.type = "analytic",
         groups = group_izl,
         fuzzy = c(unique(group_izl))) %>% 
  sqrt()

c_izl <- table_izl[[2]]


func_izl <- fun_func(c_izl,t_izl)

rarity_izl <- fun_rarity(c_izl,t_izl)

taxo_izl <- fun_taxo(c_izl)


table_izl <- join_all(list(func_izl,rarity_izl,taxo_izl),type="inner",by="code_lac")%>% setnames(colnames(.)[-1],paste0("izl_",colnames(.)[-1]))


# (6) calcul indice div macroinvertébrés IZL ----------------------------------

com_izp <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres_inv_zp.csv")
tra_izp <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/TRAIT/tra_macroinvertebres_inv_zp.csv")

group_izp = c(rep(1,4),rep(2,4),rep(3,10),rep(4,2),rep(5,8),rep(6,7),rep(7,3),rep(8,8),rep(9,5),rep(10,4))

table_izp <- fun_tax_freq(com_izp,0.01)[[1]] %>% 
  fun_lac_numb(n=6) %>%
  fun_combine_tra_com(tra_izp,.)


t_izp <- table_izp[[1]] %>%
  column_to_rownames()  %>%
  gawdis(w.type = "analytic",
         groups = group_izp,
         fuzzy = c(unique(group_izp))) %>% 
  sqrt()

c_izp <- table_izp[[2]]


func_izp <- fun_func(c_izp,t_izp)

rarity_izp <- fun_rarity(c_izp,t_izp)

taxo_izp <- fun_taxo(c_izp)


table_izp <- join_all(list(func_izp,rarity_izp,taxo_izp),type="inner",by="code_lac")%>% setnames(colnames(.)[-1],paste0("izp_",colnames(.)[-1]))





# FUSION DATA ENVIRONNEMENT -----------------------------------------------

data_nouvelle_pri <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/PRET_ANALYSE/data_newenv_pri.csv") 
data_nouvelle_peh <- read_csv("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/PRET_ANALYSE/data_newenv_peh.csv")

# récupération des données dans l'environnement et créations d'une liste
noms <- ls(pattern = "table_")[-1]
list_taxons <- map(noms,~get(.))
names(list_taxons) <- noms %>% str_extract_all("(?<=_)[:alpha:]{1,}")

liste_final <- map(list_taxons,~inner_join(.,data_nouvelle_peh))

a <-liste_final[c("poi","phy","mac","iml")] %>% join_all(by="code_lac",type="inner") %>% dplyr::select(matches("restri")) %>% mutate_all(fun_boxcox)
a %>% chart.Correlation(method="pearson")
lm(mac_restri ~ phy_restri,data=a) %>% summary()


save(object=liste_final,file="C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/PRET_ANALYSE/data_new_complete.RData")
a <- load("C:/Users/marec/OneDrive/Bureau/R/these/publication_1/data/donnees_all_compile.RData")
save(liste_final,file="C:/Users/marec/OneDrive/Bureau/R/these/publication_1/data/donnees_all_compile.RData")

