jointure_sans_inv <- inner_join(jointure_sauf_sans_inv,table_divtax_sans_inv)
data_y <- jointure_sans_inv #%>% dplyr::select_at(vars(matches("code_lac|FRic|RaoQ|FDiv|FEve")))
data_x <- comp_80_dummy %>% inner_join(spatial)
data_x <- table_env_filtre

data <- inner_join(data_y,data_x,by="code_lac") %>% mutate_if(is.numeric,~scale(.))
data_y <- data %>% dplyr::select(-code_lac) %>% mutate_if(is.numeric,~scale(.)) %>% na.omit()
write.csv(data,"/home/theo/Bureau/Data/DATA/ENV/ENV_filtre/df_env_filtre.csv",row.names = FALSE)

acp <- data_y %>%select_if(is.numeric) %>%  dudi.pca()
acp %>% fun_pca(axes=c(1,2))
acp %>% fun_pca(axes=c(3,4))

##################################################### RICHESSE FONCTIONNELLE #########################################################
data_macro_rf <- data_y %>% dplyr::select(-c(mac_FDiv,mac_RaoQ,mac_FEve)) %>% dplyr::select_at(vars(!matches("veg")))
data_poisson_rf <- data_y %>% dplyr::select(-c(poi_FDiv,poi_RaoQ,poi_FEve))%>% dplyr::select_at(vars(!matches("veg")))
data_phy_rf <- data_y %>% dplyr::select(-c(phy_FDiv,phy_RaoQ,phy_FEve))%>% dplyr::select_at(vars(!matches("veg")))


data_macro %>% select_if(is.numeric) %>%  chart.Correlation()
data_macro %>% select_if(is.numeric) %>% cor() %>%  corrplot::corrplot(diag=FALSE,type="upper")

df <- data_y
fun_select_tax <- function(df,tax = "mac",ind = "mac_FRic"){
  nom <- colnames(df) %>% str_subset(tax,negate=TRUE)
  nom_var_rep <- colnames(df) %>%  str_subset(ind)
  df %>% dplyr::select(all_of(c(ind,nom))) %>% return()
}

############ macrophytes ############
data_macro %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-mac_FRic) %>% 
  ggplot(aes(x=val,y=mac_FRic))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth(method="gam")
#### selection stepwise AIC
n_var <- ncol(data_macro)-1
mod <- lm(mac_FRic ~ ., data=data_macro)
summary(mod)
mod_aic <- stepAIC(mod,criteria="BIC")
summary(mod_aic)
mod_bic <- stepAIC(mod, k = log(n_var))
summary(mod_bic)
plot(mod_bic)
partition <- varpart(Y=data_macro[,1],
                     X=data_macro[,2:9],
                     X2=data_macro[,10:(ncol(data_macro)-2)],
                     X3 =data_macro[,(ncol(data_macro)-1):ncol(data_macro)])


plot(partition)
#### lassso
fun_lasso(data=data_macro,y="mac_FRic",p=0.8)
#### spls
x <- data_macro %>% dplyr::select(-mac_FRic) %>% select_if(is.numeric) %>% as.matrix()
y <- data_macro$mac_FRic
cv_spls <- spls::cv.spls(x = x, y = y, eta = seq(0.1,0.9,0.1), K = c(1:15),fold = 50 )


mod_spls <- spls(x,y,K=cv_spls$K.opt,eta=cv_spls$eta.opt)
ci.f <- ci.spls( mod_spls, plot.it=TRUE, plot.fix='x', plot.var=1 )
cis <- ci.f$cibeta
cis
coef(mod_spls)
summary(mod_spls)
#### xgboost 
train.samples <- caret::createDataPartition(data_macro %>% pull(mac_FRic), p = 0.9, list = FALSE)
train.data <- data_macro[train.samples,]
test.data <- data_macro[-train.samples,]
sparse_matrix <- model.matrix(mac_FRic ~ ., data = train.data)[,-1]
sparse_matrix_test <- model.matrix(mac_FRic ~ ., data = test.data)[,-1]

dtrain <-xgb.DMatrix(data=sparse_matrix, label=train.data$mac_FRic)
dtest <-xgb.DMatrix(data=sparse_matrix_test, label=test.data$mac_FRic)

xgb_cv <- xgb.cv(data =dtrain,
              eta = 0.02, #learning rate
               nround = 70,
               booster = "gbtree",
               nfold=200, 
               subsample = 0.8,
               colsample_bytree = 0.75,
               gamma = 5,
               min_child_weight = 1,
               max_depth = 8)

fun_plot_xgb(xgb_cv)
xgb <- xgboost(data =dtrain,
               eta = 0.02,
               booster = "gbtree",
               nround = 60,
               subsample = 0.8,
               min_child_weight = 1,
               colsample_bytree = 0.75,
               gamma = 5,
               max_depth = 8)

              
mat <- xgb.importance (feature_names = colnames(sparse_matrix),model = xgb)
xgb.plot.importance (importance_matrix = mat[1:20]) 

pred <- predict(xgb, dtest)
plot(test.data$mac_FRic~pred)
abline(0,1)
data_macro$mac_FRic %>% hist()
############ poissons ############
data_poisson %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-poi_FRic) %>% 
  ggplot(aes(x=val,y=poi_FRic))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth(method="gam")
#### selection stepwise AIC 
mod <- lm(poi_FRic ~ ., data=data_poisson)
mod_aic <- stepAIC(mod)
summary(mod_aic)
mod_bic <- stepAIC(mod, k = log(n_var),direction = "both")
summary(mod_bic)
plot(mod_bic)
partition <- varpart(Y = data_poisson[,9],
                     X = data_poisson[,c(1:8)],
                     X2 = data_poisson[,10:(ncol(data_poisson)-2)],
                     X3 = data_poisson[,(ncol(data_poisson)-1):ncol(data_poisson)])
plot(partition)
data_poisson %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-poi_FRic) %>% 
  ggplot(aes(x=val,y=poi_FRic))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth(method="lm",formula=y~poly(x,2))

a <- lm(poi_FRic ~ mac_RaoQ + phy_FRic + comp_PC_1 + comp_PC_3+ comp_PC_9+ comp_PC_10+ comp_PC_14,data=data_poisson)
summary(lm(a$res~data_poisson$mac_FRic))
#### lasso

fun_lasso(data=data_poisson,y="poi_FRic",p=0.8)[[2]]

#### xgboost 
### xgboost 
train.samples <- caret::createDataPartition(data_poisson %>% pull(poi_FRic), p = 0.8, list = FALSE)
train.data <- data_poisson[train.samples,]
test.data <- data_poisson[-train.samples,]
sparse_matrix <- model.matrix(poi_FRic ~ ., data = train.data)[,-1]
sparse_matrix_test <- model.matrix(poi_FRic ~ ., data = test.data)[,-1]

dtrain <-xgb.DMatrix(data=sparse_matrix, label=train.data$poi_FRic)
dtest <-xgb.DMatrix(data=sparse_matrix_test, label=test.data$poi_FRic)
xgb_cv <- xgb.cv(data =dtrain,nrounds = 150, nfold = 20,gamma=3)
xgb_cv <- xgb.cv(data =dtrain,
                 eta = 0.01, #learning rate
                 nround = 200,
                 nfold=40,
                 booster = "gbtree",
                 subsample = 0.5,
                 gamma = 2,
                 alpha = 1,
                 lambda=0,
                 max_depth = 15)

xgb <- xgboost(data =dtrain,
               eta = 0.01,
               booster = "gbtree",
               nround = 160,
               subsample = 0.5,
               lambda = 0,
               gamma = 2,
               alpha = 1,
               max.depth = 15
)
mat <- xgb.importance (feature_names = colnames(sparse_matrix),model = xgb)
xgb.plot.importance (importance_matrix = mat[1:20]) 

pred <- predict(xgb, dtest)
plot(test.data$mac_FRic~pred)
abline(0,1)

############ phyto ############
data_phy %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-phy_FRic) %>% 
  ggplot(aes(x=val,y=phy_FRic))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth(method="gam")
#### selection stepwise AIC
data_ph <- train.data
n_var <- ncol(data_phy)-1
var_num <- data_phy %>% select_if(is.numeric) %>% dplyr::select(-phy_FRic)%>% colnames() 
formule <- as.formula(paste0("phy_FRic~",paste0("poly(",var_num,",2)",collapse = "+"),collapse=""))
mod <- lm(phy_FRic ~ ., data=data_phy)
summary(mod)
mod_aic <- stepAIC(mod)
summary(mod_aic)
mod_bic <- stepAIC(mod, k = log(n_var))
summary(mod_bic)
qqPlot(mod_bic)
plot(mod_bic)
partition <- varpart(Y=data_phy[,5],
                     X=data_phy[,c(1:4,6:9)],
                     X2=data_phy[,10:(ncol(data_phy)-2)],
                     X3 =data_phy[,(ncol(data_phy)-1):ncol(data_phy)])


plot(partition)
#### lassso
fun_lasso(data=data_macro,y="mac_FRic",p=0.5)

#### xgboost 
train.samples <- caret::createDataPartition(data_phy %>% pull(phy_FRic), p = 0.8, list = FALSE)
train.data <- data_phy[train.samples,]
test.data <- data_phy[-train.samples,]
sparse_matrix <- model.matrix(phy_FRic ~ ., data = train.data)[,-1]
sparse_matrix_test <- model.matrix(phy_FRic ~ ., data = test.data)[,-1]

dtrain <-xgb.DMatrix(data=sparse_matrix, label=train.data$phy_FRic)
dtest <-xgb.DMatrix(data=sparse_matrix_test, label=test.data$phy_FRic)

xgb_cv <- xgb.cv(data =dtrain,
                 eta = 0.02, #learning rate
                 nround = 200,
                 booster = "gbtree",
                 nfold=200, 
                 subsample = 0.8,
                 colsample_bytree = 0.75,
                 gamma = 5,
                 min_child_weight = 1,
                 max_depth = 8)

fun_plot_xgb(xgb_cv)
xgb <- xgboost(data =dtrain,
               eta = 0.02,
               booster = "gbtree",
               nround = 200,
               subsample = 0.8,
               nfold = 200,
               min_child_weight = 1,
               colsample_bytree = 0.75,
               gamma = 5,
               max_depth = 8)


mat <- xgb.importance (feature_names = colnames(sparse_matrix),model = xgb)
xgb.plot.importance (importance_matrix = mat[1:20]) 

pred <- predict(xgb, dtest)
plot(test.data$mac_FRic~pred)
summary(lm(test.data$mac_FRic~pred))
abline(0,1)
data_macro$mac_FRic %>% hist()




############ macroinv ############


data_y <- jointure_tot %>% dplyr::select_at(vars(matches("code_lac|FRic|RaoQ|FDiv|FEve")))
data_x <- comp_80_dummy %>% inner_join(spatial)
data_x <- table_env_filtre

data <- inner_join(data_y,data_x)
data_y <- data %>% dplyr::select(-code_lac) %>% mutate_if(is.numeric,~scale(.)) %>% na.omit()


acp <- dudi.pca(data_x %>% dplyr::select_if(is.numeric))
acp %>% fun_pca(axes=c(5,6))


##################################################### RICHESSE FONCTIONNELLE #########################################################
data_inv <- data_y %>% dplyr::select(-c(inv_FDiv,inv_RaoQ,inv_FEve)) %>% dplyr::select_at(vars(!matches("veg")))

data_inv %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-inv_FRic) %>% 
  ggplot(aes(x=val,y=inv_FRic))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth(method="gam")
#### selection stepwise AIC 
mod <- lm(inv_FRic ~ ., data=data_inv)
mod_aic <- stepAIC(mod)
summary(mod_aic)
mod_bic <- stepAIC(mod, k = log(n_var),direction = "both")
summary(mod_bic)
plot(mod_bic)
partition <- varpart(Y = data_poisson[,9],
                     X = data_poisson[,c(1:8)],
                     X2 = data_poisson[,10:(ncol(data_poisson)-2)],
                     X3 = data_poisson[,(ncol(data_poisson)-1):ncol(data_poisson)])
plot(partition)
data_poisson %>% 
  dplyr::select_if(is.numeric) %>%
  gather(var,val,-poi_FRic) %>% 
  ggplot(aes(x=val,y=poi_FRic))+
  geom_point()+
  facet_wrap(~var,scale="free")+
  geom_smooth(method="lm",formula=y~poly(x,2))
#### lasso

fun_lasso(data=data_inv,y="inv_FRic",p=0.8)[[2]]


















traintask <- makeRegrTask(data = as.data.frame(data.matrix(train.data)), target = "poi_FRic")
testtask <- makeRegrTask(data= as.data.frame(data.matrix(test.data)), target = "poi_FRic")
lrn <- makeLearner("regr.xgboost",predict.type = "response")
lrn$par.vals <- list(
  objective="reg:linear",
  eval_metric="error",
  nrounds=100L,
  eta=0.01
)
params <- makeParamSet(
  makeDiscreteParam("booster",values = c("gbtree","gblinear")),
  makeIntegerParam("max_depth",lower = 3L,upper = 15L),
  makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
  makeNumericParam("subsample",lower = 0.5,upper = 1),
  makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
  makeIntegerParam("gamma",lower = 0, upper =8)
)
rdesc <- makeResampleDesc("LOO",stratify = FALSE)
ctrl <- makeTuneControlRandom(maxit = 10L)
mytune <- tuneParams(learner = lrn
                     ,task = traintask
                     ,resampling = rdesc
                     ,measures = rmse
                     ,par.set = params
                     ,control = ctrl
                     ,show.info = T)
library(mlr)
fun_plot_xgb <- function(data){ggplot(data=data$evaluation_log,aes(x=iter,y=train_rmse_mean))+
    geom_line()+
    geom_line(aes(x=iter,y=test_rmse_mean),col="red")+
    geom_line(aes(x=iter,y=train_rmse_mean-train_rmse_std),linetype="dashed")+
    geom_line(aes(x=iter,y=train_rmse_mean+train_rmse_std),linetype="dashed")+
    geom_line(aes(x=iter,y=test_rmse_mean-train_rmse_std),col="red",linetype="dashed")+
    geom_line(aes(x=iter,y=test_rmse_mean+train_rmse_std),col="red",linetype="dashed") %>% return()}
