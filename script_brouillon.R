################## FONCTION taxonomie #######################
df_code <-
  dbGetQuery(con,
             "SELECT * FROM tsa_appellation_taxon_apt") %>% left_join(df_niveau_taxonomique, by = "cd_niv_apt") %>% dplyr::rename(., niveau_taxo = LbElement)


df <- df_code_taxon

a <- fun_remonter_codetaxon(df_code_taxon, df_code)





ggplot()+
  geom_rect(
    data = data.frame(
      x1 = lubridate::floor_date(ondelette$series$date, unit = "month") %>% unique() %>% +1,
      x2 = lubridate::ceiling_date(ondelette$series$date, unit = "month") %>% unique() -
        1
    ) %>% mutate(
      paire = seq(1, nrow(.)),
      ymin = 0,
      ymax = Inf
    ) %>%
      filter(paire %% 2 != 0) %>% slice(-1) %>% 
      dplyr::select(-paire) %>% tbl_df(),
    mapping = aes(
      xmin = x1,
      xmax = x2,
      ymin = ymin,
      ymax = ymax
    ),
    alpha = 0.2
  )+geom_line(data = variable,aes(x = date, y =sat_od))+geom_line(data = ondelette$series,aes(x=date,y=sat_od.trend),col="red",size=1)+
  theme(
    axis.line = element_line(
      linetype = 1,
      size = 1,
      color = "grey30"),
    panel.background = element_rect(fill = NA)
  )+scale_x_datetime(breaks = "1 months", labels = date_format("%b"))+ylab(NULL)
