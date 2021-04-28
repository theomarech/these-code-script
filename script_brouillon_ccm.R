c <- df_ccm %>% inner_join(df_dim_simplex %>% 
                             dplyr::select(E_rho,site,var), by=c("site"="site","var2"="var")) %>% 
  mutate(dim_var2=E_rho) %>%
  dplyr::select(-E_rho)
fun_df_ccm(df_ccm_delay)
df_ccm_delay %>% inner_join(df_dim_simplex, )
df <- c
fun_df_ccm_sur_twins<- function(df,num_sur = 100, num_sample = 100){
  df %>% 
    list() %>% 
    rep(.,num_sur) %>%
    bind_rows() %>% 
    arrange(comb) %>%
    mutate(sur_data = pmap(list(data,var1,var2,dim_var2),
                           ~cbind(pull(..1,..2),make_surrogate_twin(pull(..1,..3),num_surr=1,dim=..4,phase_lock = TRUE,initial_point = "same_season",tau = 1,T_period = 24)) %>%
                             as.data.frame()))
  
  
  ,
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
