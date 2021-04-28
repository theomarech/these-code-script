library(ade4)
library(vegan)
library(mlr)
library(xgboost)
rm(list=ls())
gc()

setwd("C:/Users/marec/OneDrive/Bureau/bureau_these_theo_confi2/Data/DATA/DONNEES FONCTIONNELLE/PRET_ANALYSE/")
dir()

data_mixte <- read_csv("table_var_2.csv") %>%
  split(.$libelle) %>% 
  purrr::map(~dplyr::select(.,-libelle)%>% inner_join(var_quali))
data_var_2 <- read_csv("table_var_2.csv") %>%
  dplyr::select(-c(per)) %>% 
  split(.$libelle) %>% 
  purrr::map(~dplyr::select(.,-libelle))
#outliers macrophytes = %>% dplyr::filter(pie>0.9,fev>0.7)
# transformé fri en exp(fri) IZL
#
data_var_2$izl <- data_var_2$izl %>% mutate(fri = log(3-fri))
data_var_2$mac <- data_var_2$mac %>% dplyr::filter(pie>0.9,fev>0.7)

indice_ <- data_var_2$iml %>% colnames() %>% .[2:8]

######## xgboost --------------------------------------------------------------------------------------------------------
list_indice_var_xgboost <- list()
for(i in 1:length(indice_)){
  list_indice_var_xgboost[[i]] <- purrr::map(data_var_2,~fun_xgboost(.,indice=indice_[i]))
  print(i)
}
names(list_indice_var_xgboost) <- indice_

######## linear --------------------------------------------------------------------------------------------------------
list_indice_var_aic <- list()
data_multi_var <- list(mac = data_mac_multi,
                       poi = data_poi_multi,
                       phy = data_phy_multi,
                       iml_poi = data_iml_poi,
                       iml_mac = data_iml_mac,
                       iml_phy = data_iml_phy,
                       inv_poi = data_inv_poi,
                       inv_mac = data_inv_mac,
                       inv_phy = data_inv_phy) %>% 
  purrr::purrr::map(~dplyr::select_if(.,is.numeric))
a <- data_var_2 %>% bind_rows()
data_var_2$iml
hist(sqrt(sqrt(data_var_2$mac$fri)))
data_var_2$poi$pie %>% fun_boxcox()
for(i in 2:8){
  hist(data$mac[[i]],lim=c(-40,40))
}
#### AIC ------------------------------------ AVERAGE
data <- purrr::map(data_var_2,~na.omit(.) %>% dplyr::mutate_at(2:8,~fun_boxcox(.,lim = c(-10,10))))
data_poly <- data_comp %>% select_if(is.numeric) %>%  map2(colnames(.),~fun_poly(.x,name=.y,degree = 2)) %>% bind_cols %>% 
  mutate_all(scale)

fun_data_poly <- function(df,col = c("cond","pal")){
  poly <- df %>% select_if(is.numeric) %>%
    dplyr::select(col[1]:col[2]) %>% 
    map2(colnames(.),~fun_poly(.x,name=.y,degree = 2)) %>% 
    bind_cols %>% 
    mutate_all(scale) 
  bind_cols(df %>% dplyr::select(-c(col[1]:col[2])),poly) %>% return()
}

data <- map(data,~fun_data_poly(.))
list_indice_var_aic <- list()
for(i in 1:length(indice_)){
  list_indice_var_aic[[i]] <- purrr::map(data,~fun_average(., pos=indice_[i], res=FALSE, bootstrap = TRUE, k = "average"))
  print(i)
}
names(list_indice_var_aic) <- indice_
list_indice_var_aic$rao$poi$mod_sel
list_indice_var_aic$fev$poi$table_boot

#### lasso ------------------------------------
list_indice_var_lasso <- list()

for(i in 1:length(indice_)){
  list_indice_var_lasso[[i]] <- purrr::map(data_multi_var,~fun_aic(.,pos=indice_[i],res=TRUE,k = "lasso"))
  print(i)
}

names(list_indice_var_lasso) <- indice_
list_indice_var_lasso$fdi$iml

fun_aic(data_var_2$poi ,res=FALSE)

list.save(list_indice,file="C:/Users/marec/OneDrive/Bureau/xgboost_list_mixte.RData")
list.save(list_indice_var_aic,file="C:/Users/marec/OneDrive/Bureau/list_var_2_average.RData")

res_mixte <- list.load("C:/Users/marec/OneDrive/Bureau/xgboost_resultat/donnee_mixte/xgboost_list_mixte.RData")
res_mixte <-list.load("C:/Users/marec/OneDrive/Bureau/list_var_2_average.RData")
res_mixte <-list.load("C:/Users/marec/OneDrive/Bureau/xgboost_resultat/donnee_var/xgboost_list_var_2.RData")

############## PLOT FOR AVERAGE -----------------------------------------------------------------------------

b <- unlist(res_mixte, recursive = FALSE)

sel_plot <- function(list = res_mixte,
                     tax = "mac",
                     sub = "a",
                     type="importance",
                     r2data = data_var_2) {
  
  b <- unlist(res_mixte, recursive = FALSE)
  
  if (str_split(sub, "") %>% unlist %>% length() == 3) {
    sub <- paste0("(?<!\\_)", sub)
    mac_data <-
      purrr::keep(b, str_detect(names(b), sub)) %>% unlist(recursive = FALSE)
  } else{
    mac_data <-
      purrr::keep(b, str_detect(names(b), sub)) %>% unlist(recursive = FALSE)
  }
  if(type == "importance"){
    data <-
      purrr::keep(mac_data, str_detect(names(mac_data), "imp")) %>%
      compact() %>% 
      purrr::map2(str_sub(names(.),1,3),
           ~ data.frame(coeff = .x, indice = .y) %>%
             tibble::rownames_to_column(var = "var") %>%
             mutate(coeff = as.numeric(coeff))
      ) %>%
      bind_rows() %>%
      mutate(tax = tax)
  }else if(type=="select"){
    data <-
      purrr::keep(mac_data, str_detect(names(mac_data), "best")) %>%
      compact() %>% 
      purrr::map2(str_sub(names(.),1,3), ~ coef(.x) %>% as.data.frame() %>% as_tibble %>% mutate(indice=.y)) %>%
      bind_rows() %>%
      gather(var, coeff, -indice) %>%
      mutate(tax = tax) %>% na.omit()
  }else{
    data <-
      purrr::keep(mac_data, str_detect(names(mac_data), "avg")) %>%
      compact() 
    
    
    
    data <- data %>% 
      purrr::map2(str_sub(names(.),1,3), ~ coef(.x) %>% data.frame(coeff=.,indice=.y) %>% tibble::rownames_to_column(var="var")) %>%
      bind_rows() %>%
      mutate(tax = tax)
  }
  return(data)
}


sub <- names(b) %>% str_sub(5, -1) %>% unique()
table <- purrr::map(sub,~sel_plot(res_mixte,tax = .,sub = .,"average")) %>% bind_rows()
# "fri","fdi","fev","rao"
list_fun_linear_mixte <- table %>%
  as_tibble() %>%
  mutate(col=ifelse(coeff>0,"blue","red"),
         comb=str_to_upper(paste0(tax,"_",indice))) %>% 
  dplyr::filter(indice%in%c("ric","fri"),var!="(Intercept)") %>% 
  mutate()

x11();list_fun_linear_mixte %>% ggplot(aes(x=reorder_within(var,coeff,within=list(tax,indice)),y=coeff,fill=col))+
  geom_bar(stat = "identity")+
  facet_wrap(~comb,scale="free",ncol=2)+
  coord_flip()+
  scale_x_reordered()+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        panel.grid.minor.x = element_line(colour ="gray80",linetype = 8),
        panel.grid.major.x = element_line(colour ="gray80",linetype = 8),
        axis.text.y = element_text(size=rel(1)),
        legend.position = "none")+
  labs(y= "Linear coefficient from average linear models",
       x=NULL)+
  scale_fill_manual(values = c("#34B298","#DD1616"))
nom <- names(data_var_2)
data <- purrr::map2(data_var_2,nom,~mutate(.x,tax = .y)) %>% 
  bind_rows() %>% 
  group_by(tax) %>% 
  nest()

R2_av  <- list_fun_linear_mixte %>% 
  dplyr::select(indice,tax,var) %>% 
  group_by(indice,tax) %>% nest() %>% 
  inner_join(data,by="tax") %>% 
  ungroup() %>% 
  mutate(formula = purrr::map2_chr(indice,data.x,
                            ~paste0(..1,"~",pull(..2,var) %>% paste0(collapse = "+"))),
         mod = purrr::map2(formula,data.y,~lm(formula = .x,data = .y)),
         tidy = purrr::map(mod,~tidy(.) %>% dplyr::select(term,estimate))) %>% 
  dplyr::select(-c(data.x,data.y,mod,formula,data.y))%>% unnest(tidy) %>%
  spread(term,estimate,fill=0) %>% inner_join(minmax_r2adj,copy=TRUE)

R2_sel  <- list_fun_linear_mixte %>% 
  dplyr::select(indice,tax,var) %>% 
  group_by(indice,tax) %>% nest() %>% 
  inner_join(data,by="tax") %>% 
  ungroup() %>% 
  mutate(formula = purrr::map2_chr(indice,data.x,
                            ~paste0(..1,"~",pull(..2,var) %>% paste0(collapse = "+"))),
         mod = purrr::map2(formula,data.y,
                    ~lm(formula = .x,data = .y)),
         R2 = purrr::map_dbl(mod,~glance(.) %>% pull(adj.r.squared)),
         tidy = purrr::map(mod,~tidy(.) %>% dplyr::select(term,estimate))) %>% 
  dplyr::select(-c(data.x,data.y,mod,formula,data.y))%>% unnest(tidy) %>%
  spread(term,estimate,fill=0) %>% mutate(R2 = round(R2,3) %>% as.character)

data_plot <- inner_join(list_fun_linear_mixte,R2_sel %>% dplyr::select(indice,tax,R2)) %>% transmute(indice,var,coeff,tax,col,comb= paste0(comb," r²adj=",R2))


x11();data_plot %>% ggplot(aes(x=reorder_within(var,coeff,within=list(tax,indice)),y=coeff,fill=col))+
  geom_bar(stat = "identity")+
  facet_wrap(~comb,scale="free",ncol=2)+
  coord_flip()+
  scale_x_reordered()+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        panel.grid.minor.x = element_line(colour ="gray80",linetype = 8),
        panel.grid.major.x = element_line(colour ="gray80",linetype = 8),
        axis.text.y = element_text(size=rel(1)),
        legend.position = "none")+
  scale_fill_manual(values = c("#34B298","#DD1616"))+
  labs(y= "Linear coefficient from average linear models",
       x=NULL)

tableau <- bind_rows(R2_sel %>% mutate(mod = "best"),
  R2_av  %>% mutate(mod = "average")) %>% 
  arrange(tax,indice) %>% 
  dplyr::select(tax,indice,mod,`(Intercept)`:tpm,R2) %>%
  mutate_if(is.numeric,~round(.,3)) 

fun_formule <- function(df,df2=data_var_2,ind="fri",tax="mac"){
  pos <- df$msTable %>% 
    tibble::rownames_to_column("modele") %>% 
    .$modele %>% 
    str_split("") %>%
    purrr::map(~as.numeric(.)) %>%
    purrr::map(~purrr::discard(.,is.na)) %>%  compact()
  
  var_env <- r2data$iml[,-c(1:7)]
  
  form <- purrr::map(pos,~ paste0(ind,"~",paste0(dplyr::select(var_env,.) %>% colnames(),collapse="+")))
  res <- purrr::map_dbl(form,~lm(.,data=df2[[tax]]) %>% broom::glance() %>% .$adj.r.squared) %>% return()
}

ind <- names(data) %>% str_sub(1,3)
tax <-names(data) %>% str_sub(5,7)

purrr::map2(ind,tax,~fun_formule(df = data,df2=data_var_2,ind=.x,tax=.y))
a <-purrr::map(list(data,ind,tax),
         ~fun_formule(df = ..1,df2=data_var_2,ind=..2,tax=..3))
minmax_r2adj<- purrr::map(a,function(.)c("[",c(round(min(.),2),";",round(max(.),2),"]")) %>%  paste0(collapse="")) %>%
  bind_rows() %>% gather(var,R2) %>% separate(var,c("indice","tax","type"),sep="\\.") %>% dplyr::select(-type)

write.csv(tableau,"C:/Users/marec/OneDrive/Bureau/publication_1/figures/table_aicc_func.csv",row.names = FALSE)
















############# IMPORTANCE PLOT FOR MIXTE VAR ##################
######### tree_plot
list_fri_tree_mixte <- bind_rows(
  res_mixte$fri$iml$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fri",tax="iml") ,
  res_mixte$fri$poi$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fri",tax="poi") ,
  res_mixte$fri$inv$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fri",tax="inv") ,
  res_mixte$fri$mac$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fri",tax="mac") ,
  res_mixte$fri$phy$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fri",tax="phy") 
)
list_fdi_tree_mixte <- bind_rows(
  res_mixte$fdi$iml$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fdi",tax="iml") ,
  res_mixte$fdi$poi$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fdi",tax="poi") ,
  res_mixte$fdi$inv$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fdi",tax="inv") ,
  res_mixte$fdi$mac$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fdi",tax="mac") ,
  res_mixte$fdi$phy$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fdi",tax="phy") 
)
list_fev_tree_mixte <- bind_rows(
  res_mixte$fev$iml$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fev",tax="iml") ,
  res_mixte$fev$poi$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fev",tax="poi") ,
  res_mixte$fev$inv$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fev",tax="inv") ,
  res_mixte$fev$mac$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fev",tax="mac") ,
  res_mixte$fev$phy$tree_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fev",tax="phy") 
)
list_fun_tree_mixte <- bind_rows(list_fri_tree_mixte,list_fev_tree_mixte,list_fdi_tree_mixte) %>% as_tibble()%>%
  mutate(comb=str_to_upper(paste(tax,"_",var)))


list_fun_tree_mixte %>% ggplot(aes(x=reorder_within(Feature,Importance,within=list(tax,var)),y=Importance))+
  geom_bar(stat = "identity",fill="darkblue")+
  facet_wrap(~comb,scale="free",ncol=3)+
  coord_flip()+
  scale_x_reordered()+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        panel.grid.minor.x = element_line(colour ="gray80",linetype = 8),
        panel.grid.major.x = element_line(colour ="gray80",linetype = 8),
        legend.position = "none")+
  labs(y= "Importance coefficient from xgboost tree learner",
       x=NULL)


###### linear_plot
list_fri_linear_mixte <- bind_rows(
  res_mixte$fri$iml$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fri",tax="iml") ,
  res_mixte$fri$poi$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fri",tax="poi") ,
  res_mixte$fri$inv$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fri",tax="inv") ,
  res_mixte$fri$mac$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fri",tax="mac") ,
  res_mixte$fri$phy$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fri",tax="phy") 
)
list_fdi_linear_mixte <- bind_rows(
  res_mixte$fdi$iml$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fdi",tax="iml") ,
  res_mixte$fdi$poi$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fdi",tax="poi") ,
  res_mixte$fdi$inv$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fdi",tax="inv") ,
  res_mixte$fdi$mac$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fdi",tax="mac") ,
  res_mixte$fdi$phy$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fdi",tax="phy") 
)
list_fev_linear_mixte <- bind_rows(
  res_mixte$fev$iml$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fev",tax="iml") ,
  res_mixte$fev$poi$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fev",tax="poi") ,
  res_mixte$fev$inv$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fev",tax="inv") ,
  res_mixte$fev$mac$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fev",tax="mac") ,
  res_mixte$fev$phy$linear_plot %>% na.omit() %>% dplyr::select(Feature, Importance) %>% mutate(var = "fev",tax="phy") 
)
list_fun_linear_mixte <- bind_rows(list_fri_linear_mixte,list_fev_linear_mixte,list_fdi_linear_mixte) %>%
  as_tibble() %>%
  mutate(col=ifelse(Importance>0,"blue","red"),
         comb=str_to_upper(paste(tax,"_",var)))

x11();list_fun_linear_mixte %>% ggplot(aes(x=reorder_within(Feature,Importance,within=list(tax,var)),y=Importance,fill=col))+
  geom_bar(stat = "identity")+
  facet_wrap(~comb,scale="free",ncol=3)+
  coord_flip()+
  scale_x_reordered()+
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill=NA),
        panel.grid.minor.x = element_line(colour ="gray80",linetype = 8),
        panel.grid.major.x = element_line(colour ="gray80",linetype = 8),
        axis.text.y = element_text(size=rel(0.8)),
        legend.position = "none")+
  labs(y= "Linear coefficient from xgboost linear learner",
       x=NULL)
