
######## packages
library("ordinal")
library("mvabund")
library("labdsv")
library(ecoCopula)
library("tidygraph")
library("ggraph")
rm(list=ls())

poisson <- read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_poissons.csv") %>% na.omit() %>% 
  inner_join(read_csv("/home/theo/Bureau/Data/DATA/DONNEES FONCTIONNELLE/COM/com_macroinvertebres.csv") )


X <- comp_80 %>% dplyr::filter(code_lac %in% poisson$code_lac) %>% mutate_if(is.numeric,~scale(.)) 
poisson <- poisson %>%  dplyr::filter(code_lac %in% X$code_lac)

poisson <- poisson %>% column_to_rownames() %>% select_if(colSums(.)!=0)
dim(poisson)

X <- X %>%  column_to_rownames()

nom_poi <- poisson %>% tibble::rownames_to_column(var="code_lac") %>% 
  gather(var,val,-code_lac) %>% 
  mutate(val = ifelse(val>0,1,0)) %>% 
  group_by(var) %>% 
  dplyr::summarise(sm=sum(val)/nrow(poisson)*100) %>% 
  dplyr::filter(sm>15) %>% 
  pull(var) 
poisson <- poisson %>% dplyr::select_at(nom_poi) 


dim(poisson)

poisson %>% rowSums()

corrplot(cor((poisson)),tl.pos='n')
View(poisson)

p <- poisson %>% decostand(method="hellinger")
a <- poisson %>% gather(var,val) 
ggplot(a,aes(x=val)) +
  geom_density()+
  facet_wrap(~var,scale="free")
plot(Y$mac_FDiv,Y$mac_FRic)
data_comp <- na.omit(data_comp)
X <- data_comp[,2:11]
Y <- data_comp %>% dplyr::select(-c(1:11)) %>% mutate(phy_RaoQ=log(phy_RaoQ+15))
Y %>% gather(var,val) %>% ggplot(aes(x=val))+geom_density()+facet_wrap(~var,scale="free")
poi_abund <- mvabund(Y )
mod <- manylm(as.matrix(Y) ~ as.matrix(X),family = "gaussian")

poi_graph <- cgr(mod,n.samp = 1000, seed = 235,method="AIC")
plot(poi_graph, pad = 1)
?cgr

igraph_out<-poi_graph$best_graph$igraph_out

igraph_out%>% ggraph('fr') + # see ?layout_tbl_graph_igraph
  geom_edge_fan0(aes( colour = partcor, width=partcor)) +
  scale_edge_width(range = c(0.5, 3))+
  scale_edge_color_gradient2(low="red",mid="white",high="#2166ac")+
  geom_node_text(aes(label=name), repel = TRUE)+
  geom_node_point(aes(size=1.3))+
  theme_void() +
  theme(legend.position = 'none')
