source("/home/theo/Bureau/scriptR/librairies.R")


rm(list=ls())
gc()

fun_dim_simplex <- function(df){
  df  %>% na.omit() %>%
    group_by(site, var) %>%
    nest() %>%
    mutate(
      var_diff = map(data,  ~ dplyr::select(., valeurs) %>% pull()),
      lib = map_dbl(var_diff,  ~ round(2 / 3 * length(.))),
      output_simplex = future_map2(var_diff, lib, ~ simplex(
        .x, lib = c(1, .y), pred = c(.y + 1, length(.x))
      ), .progress = TRUE),
      E_rho = map_dbl(output_simplex,  ~ .$"E"[which.max(.$"rho")]),
      E_mae = map_dbl(output_simplex,  ~ .$"E"[which.min(.$"mae")]),
      E_rmse = map_dbl(output_simplex,  ~ .$"E"[which.min(.$"rmse")]),
      output_smap = future_pmap(
        list(var_diff, lib, E_rho),
        ~ s_map(
          time_series = ..1,
          lib = c(1, ..2),
          pred = c(..2+1, length(..1)),
          E = ..3
        ),
        .progress = TRUE
      ),
      theta_max = map_dbl(output_smap,  ~ .$"theta"[which.max(.$"rho")])
    ) %>% return()
}

fun_df_ccm_delay <- function(df1,df2){
  c("conc_od", "keddy") %>%
    expand.grid(
      site = unique(df1$site),
      var1 = .,
      var2 = .,
      time_delay = -30:30
    ) %>%
    unique() %>%
    filter(var1 != var2) %>%
    arrange(site) %>%
    full_join(df1 %>% dplyr::select(site, var, E_rho),
              by = c("var1" = "var", "site" = "site")) %>% tbl_df() %>%
    inner_join(df2 %>% spread(var, valeurs) %>%  group_by(site) %>% nest(),
               by = "site") %>% dplyr::rename(dim_var = E_rho) %>%
    mutate(
      lib_seq = map(data, ~ seq(10, nrow(.), by = round(nrow(
        .
      ) / 7))),
      output_ccm_delay = future_pmap(
        list(data, dim_var, var1, var2, time_delay),
        ~ ccm(
          block = ..1,
          E = ..2,
          lib_column = ..3,
          target_column = ..4,
          lib_sizes = NROW(..1),
          tp = ..5,
          num_samples = 30
        ),
        .progress = TRUE
      ),
      rho_ccm_delay = map_dbl(output_ccm_delay, ~ mean(.$rho))
    ) %>% return()
}
df <- df_ccm_delay
fun_df_ccm <- function(df) {
  df %>% 
    mutate(comb = str_c(site, var2, var1, sep = "-->")) %>%
    split(., .$comb) %>%
    map_df( ~ slice(., which.max(.$rho_ccm_delay))) %>%
    filter(time_delay<=0) %>% 
    mutate(
      output_ccm = future_pmap(
        list(data, dim_var, var1, var2, lib_seq, time_delay),
        ~ ccm(
          block = ..1,
          E = ..2,
          lib_column = ..3,
          target_column = ..4,
          lib_sizes = ..5,
          tp = ..6,
          num_samples = 50
        ),
        .progress = TRUE
      ),
      output_ccm_rho = map(output_ccm, ~filter(.,lib_size==max(.$lib_size)) %>% pull(.,rho)),
      mean_ccm_rho = map_dbl(output_ccm_rho,  ~mean(.)),
      ic_ccm_rho = map_dbl(output_ccm_rho, ~1.96*sd(.)/length(.)),
      plot_ccm = map(output_ccm, ~ ggplot(data = ., aes(
        x = lib_size, y = rho
      )) + geom_point() + geom_smooth())
    ) %>% return()
}

fun_df_ccm_sur<- function(df,num_sur = 50, num_sample = 50){
  df %>% 
    list() %>% 
    rep(.,num_sur) %>%
    bind_rows() %>% 
    arrange(comb) %>%
    mutate(sur_data = pmap(list(data,var1,var2),
                           ~cbind(pull(..1,..2),pull(..1,..3) %>% make_surrogate_ebisuzaki(.,num_surr=1)) %>% as.data.frame()),
           output_sur_ccm = future_pmap(list(sur_data,dim_var,time_delay),
                                        ~ccm(
                                          block = ..1,
                                          E = ..2,
                                          lib_column = 1,
                                          target_column = 2,
                                          lib_sizes = NROW(..1),
                                          tp = ..3,
                                          num_sample = num_sample
                                        )$rho, .progress = TRUE),
           mean_sur = map_dbl(output_sur_ccm,~mean(.))) %>%
    group_by(comb) %>%
    dplyr::summarise(rho_seuil_mean=mean(mean_sur),rho_seuil_ic=1.96*sd(mean_sur)/length(mean_sur)) %>% 
    inner_join(df,by="comb") %>% 
    dplyr::select(comb,time_delay,mean_ccm_rho,ic_ccm_rho,rho_seuil_mean,rho_seuil_ic,plot_ccm) %>% return()
}


df_oxy_keddy <-
  read.csv(
    "/home/theo/Bureau/Vincent/donnees_lac_lacanau_nettoyees.csv",
    header = TRUE,
    sep = ","
  ) %>% as_tibble() %>% mutate(
    date = ymd_hms(date),
    mois = lubridate::month(date, abbr = FALSE, label = TRUE),
    mois2 = paste(
      lubridate::month(date, label = TRUE, abbr = FALSE),
      floor_date(date, unit = "year")
    ) %>% str_extract("[:alpha:]{1,}[:blank:][:digit:]{4}") %>% factor(levels = unique(.))
  )





df_brute_tot <- df_oxy_keddy %>% 
  filter(is.na(site) != TRUE) %>% 
  gather(key = "var", value = "valeurs", c(keddy, conc_od))

mois_test <- unique(df_oxy_keddy$mois2) %>% as.character()

liste_ccm <- list()

indice_mois <- c()

for(i in 1:length(mois_test)){
  
  indice_mois <- mois_test[i]
  
  df_brute <- df_brute_tot %>% filter(mois2 %in% mois_test[10:14]) %>% na.omit()
  
  
  plan(multiprocess)
  
  df_dim_simplex <- fun_dim_simplex(df_brute)
  
  df_ccm_delay <- possibly(fun_df_ccm_delay,"erreur")(df_dim_simplex,df_brute)
  rm()
  df_ccm <- possibly(fun_df_ccm,"erreur")(df_ccm_delay)
  
  df_ccm_sur <- possibly(fun_df_ccm_sur,"erreur")(df_ccm)

  
  liste_ccm[[i]] <- df_ccm_sur
  
  print(i+1)
  
  print(mois_test[i+1])
  
}


      liste_test <- liste_ccm
      names(liste_test)<-mois_test
liste_finale_brute <- bind_rows(liste_test[-1],.id="id") %>% dplyr::rename(mois = id)%>% mutate(mois = fct_inorder(mois, ordered=TRUE))
  
liste_finale_brute$plot_ccm

ggplot(data=data1, aes(x=mois ,y=rho_max,group=site,color=site))+
  geom_point()+
  geom_line()+
  theme(axis.text.x = element_text(angle=45,hjust=1))

data <- write.csv(df_ccm_sur %>% dplyr::select(-plot_ccm),"/home/theo/Bureau/Vincent/results_ccm_mai_septembre_ebi_satod.csv")

data1 <- data %>% mutate(mois = fct_inorder(mois, ordered=TRUE)) %>% filter(causalite =="keddy-->sat_od")




