beta_poi_com <- otuSummary::matrixConvert(beta)
env_poi <- data_var2$poi %>% na.omit()
acp <- fun_comp( env_poi %>% column_to_rownames(),site=env_poi$code_lac,nax = 5)
tab_acp <- acp[[1]]

pc_1 <- dist(tab_acp$PC_1) %>% as.matrix() %>% as.data.frame()
names(pc_1) <- env_poi$code_lac
rownames(pc_1)<-env_poi$code_lac
pc_1 <- otuSummary::matrixConvert(pc_1) %>% rename("pc_1" = "dist")

pc_2 <- dist(tab_acp$PC_2) %>% as.matrix() %>% as.data.frame()
names(pc_2) <- env_poi$code_lac
rownames(pc_2)<-env_poi$code_lac
pc_2 <- otuSummary::matrixConvert(pc_2) %>% rename("pc_2" = "dist")

pc_3 <- dist(tab_acp$PC_3) %>% as.matrix() %>% as.data.frame()
names(pc_3) <- env_poi$code_lac
rownames(pc_3)<-env_poi$code_lac
pc_3 <- otuSummary::matrixConvert(pc_3) %>% rename("pc_3" = "dist")

pc_4 <- dist(tab_acp$PC_4) %>% as.matrix() %>% as.data.frame()
names(pc_4) <- env_poi$code_lac
rownames(pc_4)<-env_poi$code_lac
pc_4 <- otuSummary::matrixConvert(pc_4) %>% rename("pc_4" = "dist")

data_beta_poi <- join_all(list(beta_poi_com,pc_1,pc_2)) %>% mutate(beta = cut(dist,breaks = 200) %>% factor(ordered = TRUE) %>% as.integer()) 
data_plot <- data_beta_poi%>% group_by(beta) %>% dplyr::summarise(m = mean(pc_1))
ggplot(data_beta_poi,aes(x=m,y=beta))+geom_point()



phy <- beta_phy_com %>% rename("phy" = "dist") %>% mutate(beta_phy = cut(phy,breaks=200) %>% factor(ordered = TRUE) %>% as.integer())
mac<- beta_mac_com %>% rename("mac" = "dist") %>% mutate(beta_mac = cut(mac,breaks=200) %>% factor(ordered = TRUE) %>% as.integer())
izl <-  beta_izl_com %>% rename("izl" = "dist") %>% mutate(beta_izl = cut(izl,breaks=200) %>% factor(ordered = TRUE) %>% as.integer())
izp <-  beta_izp_com %>% rename("izp" = "dist") %>% mutate(beta_izp = cut(izp,breaks=200) %>% factor(ordered = TRUE) %>% as.integer())

a <- join_all(list(izl,izp,mac,data_beta_poi,phy,pc_3,pc_4)) %>% na.omit()

data_plot <- a%>% group_by(beta) %>% dplyr::summarise(m = mean(beta_phy))

ggplot(a,aes(x=pc_1,y=dist))+geom_point()+geom_smooth()
cor(a$phy,a$pc_1)
df <- data.frame(izp=a$izp,izl= a$izl,poi=a$dist,mac=a$mac,phy=a$phy,pc1=a$pc_1,pc2=a$pc_2,pc3 = a$pc_3,pc4=a$pc_4)
glasso_multi <-  df %>% 
  select_if(is.numeric) %>% 
  na.omit() %>% 
  qgraph::cor_auto(forcePD=TRUE) %>%
  qgraph::qgraph(graph = "glasso",
                 sampleSize = nrow(df),
                 threshold = T, 
                 layout = "spring",
                 gamma = 0.5,
                 # refit = TRUE,
                 cut = 0)
centralityPlot(glasso_multi)
