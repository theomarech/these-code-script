





pos = "rao"
data <- data_var_2$iml
fun_average <-
  function(data,
           pos = 'fri',
           k = "AIC",
           res = TRUE,
           mod = FALSE,
           categ = "typo2",
           bootstrap = TRUE,
           nboot = 1000) {
    var <- names(data[, pos])
    data <- na.omit(data)
    
    
    
    
    if (pos == "fdi") {
      
      table_data <- data %>%
        dplyr::select(which(
          str_detect(names(data), "code_lac|fri|ric|fev|pie|sha|rao", negate = TRUE)
        )) 
      
      pos_scale <- which(purrr::map_lgl(table_data,~is.numeric(.))&str_detect(names(table_data),paste0("(?<!_)",pos),negate =TRUE))
      
      table_data <- table_data%>%
        dplyr::mutate_if(is.character, as.factor) %>%
        mutate_at(pos_scale,  ~ scale(.))
      
    } else if (pos == "fev") {
      
      table_data <- data %>%
        dplyr::select(which(
          str_detect(names(data), "code_lac|fri|ric|fdi|pie|sha|rao", negate = TRUE)
        ))  
      pos_scale <- which(purrr::map_lgl(table_data,~is.numeric(.))&str_detect(names(table_data),paste0("(?<!_)",pos),negate =TRUE))
      
      table_data <- table_data %>%
        dplyr::mutate_if(is.character, as.factor) %>%
        mutate_at(pos_scale,  ~ scale(.))
    } else if (pos == "rao") {
      
      table_data <- data %>%
        dplyr::select(which(
          str_detect(names(data), "code_lac|fri|ric|fdi|pie|sha|fev", negate = TRUE)
        ))  
      pos_scale <- which(purrr::map_lgl(table_data,~is.numeric(.))&str_detect(names(table_data),paste0("(?<!_)",pos),negate =TRUE))
      
      table_data <- table_data %>%
        dplyr::mutate_if(is.character, as.factor) %>%
        mutate_at(pos_scale,  ~ scale(.))
      
    } else if (pos == "fri") {
      if (res == FALSE) {
        table_data <- data %>%
          dplyr::select(which(
            str_detect(names(data), "code_lac|fev|fdi|pie|sha|rao", negate = TRUE)
          ))
        pos_scale <- which(purrr::map_lgl(table_data,~is.numeric(.))&str_detect(names(table_data),paste0("(?<!_)",pos),negate =TRUE))
        
        table_data <- table_data %>%
          dplyr::mutate_if(is.character, as.factor) %>%
          mutate_at(pos_scale,  ~ scale(.))
      } else{
        data_res <- data %>%
          na.omit() %>%
          dplyr::select(which(
            str_detect(names(data), "code_lac|fev|fdi|pie|sha|rao", negate = TRUE)
          )) %>%
          lm(fri ~ ric, data = .) %>%
          .$res
        
        table_data <- data %>%
          na.omit() %>%
          dplyr::select(which(
            str_detect(names(data), "code_lac|ric|fev|fdi|pie|sha|rao", negate = TRUE)
          ))
        pos_scale <- which(purrr::map_lgl(table_data,~is.numeric(.))&str_detect(names(table_data),paste0("(?<!_)",pos),negate =TRUE))
        
        table_data <- table_data %>%
          dplyr::mutate_if(is.character, as.factor) %>%
          mutate_at(pos_scale,  ~ scale(.)) %>%
          dplyr::mutate(fri = data_res)
      }
      
    } else {
      name <- c("code_lac", "fri", "fev", "fdi", "pie", "sha", "ric","rao") %>%
        str_replace(pos, '') %>%
        stri_remove_empty() %>%
        paste0(collapse = "|")
      table_data <- data %>%
        dplyr::select(which(str_detect(names(data), name, negate = TRUE)))  %>%
        dplyr::mutate_if(is.character, as.factor)
      
      pos_scale <- which(purrr::map_lgl(table_data,~is.numeric(.))&str_detect(names(table_data),paste0("(?<!_)",pos),negate =TRUE))
      
      table_data <- table_data %>%
        mutate_at(pos_scale,  ~ scale(.)) 
    }
    
    # plot <- table_data %>%
    #   dplyr::select_if(is.numeric) %>%
    #   gather('var', 'val', -as.symbol(var)) %>%
    #   ggplot(aes_string(x = "val", y = var)) +
    #   geom_point() +
    #   facet_wrap( ~ var, scale = "free") +
    #   geom_smooth(method = "lm")
    
    
    
    
    if (mod == TRUE) {
      # quelle formule ?
      var_num <-
        dplyr::select_if(table_data, (
          purrr::map_lgl(table_data,  ~ is.numeric(.)) & names(table_data) != pos
        )) %>%
        names()
      mod <-
        as.formula(paste0(pos,
                          " ~ (",
                          paste(var_num, collapse = "+"),
                          ") *",
                          categ))
    } else{
      mod <- as.formula(paste(pos , '~.'))
    }
    
    dmy <- dummyVars("~.", data = table_data)
    table_data <- data.frame(predict(dmy, newdata = table_data)) %>% na.omit()
    
    if (bootstrap == TRUE) {
      # Bootstrap sur les coefficients ?
      
      
      table_data <- table_data %>% mutate_if(names(.)==pos, ~scale(.) %>% as.vector() ,center = TRUE,scale = FALSE)
      full_mod <- lm(mod, data = table_data)
      R2_comp <- glance(full_mod)
      
      full_est <- coef(full_mod)
      full_se <- full_mod %>% tidy() %>% .$std.error
      
      names(full_se) <- names(coef(full_mod))
      EPVglobal <-
        (nrow(table_data) / length(coef(full_mod))) %>% round()
      
      
      
      if (k == "lasso") {
        lambda <-
          glmnet::cv.glmnet(x = table_data %>% dplyr::select(-pos) %>% as.matrix,
                            table_data %>% pull(pos),nfolds = 10)
        sel_mod <-
          glmnet::glmnet(
            x = table_data %>% dplyr::select(-pos) %>% as.matrix,
            table_data %>% pull(pos),
            lambda = lambda$lambda.1se,
            family = "gaussian"
          )
        sel_est <-
          as.vector(coef(sel_mod))
        names(sel_est) <- names(coef(full_mod))
        
        boot_est <-
          matrix(
            0,
            nrow = nboot,
            ncol = length(coef(full_mod)),
            dimnames = list(NULL, names(coef(full_mod)))
          )
        
        for (b in 1:nboot) {
          boot_data <-
            slice_sample(table_data,
                         replace = TRUE,
                         n = nrow(table_data)) %>%
            mutate_at(pos_scale,  ~ scale(.))
          lambda <-
            glmnet::cv.glmnet(
              x = boot_data %>% dplyr::select(-pos) %>% as.matrix,
              boot_data %>% pull(pos),
              nfold = 5
            )
          boot_mod <-
            glmnet::glmnet(
              x = boot_data %>% dplyr::select(-pos) %>% as.matrix,
              boot_data %>% pull(pos),
              lambda = lambda$lambda.1se
            )
          boot_mod <-
            as.vector(coef(boot_mod))
          names(boot_mod) <- names(coef(full_mod))
          boot_est[b, ] <- boot_mod
        }
        
        boot_01 <- (boot_est != 0) * 1
        boot_freq <- apply(boot_01, 2, function(x)
          sum(x) / length(x) * 100)
        
        sqe <- (t(boot_est) - full_est) ^ 2
        rmsd <- apply(sqe, 1, function(x)
          sqrt(mean(x, na.rm = TRUE)))
        rmsdratio <- rmsd / full_se
        boot_mean <- apply(boot_est, 2, mean, na.rm = TRUE)
        boot_median <- apply(boot_est, 2, median)
        boot_025per <- apply(boot_est, 2, quantile, prob = 0.025)
        boot_975per <- apply(boot_est, 2, quantile, prob = 0.975)
        boot_ratio <- boot_mean / full_est
        boot_relbias <- (boot_ratio / (boot_freq / 100) - 1) * 100
        boot_est %>% as.data.frame() %>% .$nitr %>% hist(breaks=40)
        ### table_bootstrap final --------------------------------------------------
        table_boot <-
          round(
            cbind(
              full_est,
              full_se,
              boot_freq,
              sel_est,
              rmsdratio,
              boot_relbias,
              boot_median,
              boot_025per,
              boot_975per
            ),
            6
          ) %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "Predictors") %>%
          as_tibble() %>%
          arrange(desc(abs(sel_est)))
        
        
        
        
      } else if(k %in% c("AIC","BIC")){
        
        if (k == "AIC") {
          k = 2
        }else {
          k = log(nrow(data))
        }
        
        sel_mod <- MASS::stepAIC(
          lm(full_mod, data = table_data),
          k = k,
          direction = "backward",
          trace = FALSE
        )
        sel_est <- coef(sel_mod)[names(coef(full_mod))]
        sel_est[is.na(sel_est)] <- 0
        names(sel_est) <- names(coef(full_mod))
        sel_se <-
          coef(summary(sel_mod))[, "Std. Error"][names(coef(full_mod))]
        names(sel_se) <- names(coef(sel_mod))
        sel_se[is.na(sel_se)] <- 0
        R2_sel <- glance(sel_mod)
        
        
        boot_se <-
          boot_est <-
          matrix(
            0,
            nrow = nboot,
            ncol = length(coef(full_mod)),
            dimnames = list(NULL, names(coef(full_mod)))
          )
        
        for (b in 1:nboot) {
          boot_data <-
            slice_sample(table_data,
                         replace = TRUE,
                         n = nrow(table_data)) %>%
            mutate_if((is.numeric(.) & names(.) != pos),  ~ scale(.))
          mod_comp <- lm(mod, data = boot_data)
          boot_mod <-
            MASS::stepAIC(mod_comp,
                          k = k,
                          direction = "backward",
                          trace = FALSE)
          boot_est[b, names(coef(boot_mod))] <- coef(boot_mod)
          boot_se[b, names(coef(boot_mod))] <-
            coef(summary(boot_mod))[, "Std. Error"]
        }
        
        boot_01 <- (boot_est != 0) * 1
        boot_freq <- apply(boot_01, 2, function(x)
          sum(x) / length(x) * 100)
        
        sqe <- (t(boot_est) - full_est) ^ 2
        rmsd <- apply(sqe, 1, function(x)
          sqrt(mean(x, na.rm = TRUE)))
        rmsdratio <- rmsd / full_se
        boot_mean <- apply(boot_est, 2, mean, na.rm = TRUE)
        boot_median <- apply(boot_est, 2, median)
        boot_025per <- apply(boot_est, 2, quantile, prob = 0.025)
        boot_975per <- apply(boot_est, 2, quantile, prob = 0.975)
        boot_ratio <- boot_mean / full_est
        boot_relbias <- (boot_ratio / (boot_freq / 100) - 1) * 100
        
        ### table_bootstrap final --------------------------------------------------
        table_boot <-
          round(
            cbind(
              full_est,
              full_se,
              boot_freq,
              sel_est,
              sel_se,
              rmsdratio,
              boot_relbias,
              boot_median,
              boot_025per,
              boot_975per
            ),
            6
          ) %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "Predictors") %>%
          as_tibble() %>%
          arrange(desc(abs(sel_est)))
        ### table_frequence model final --------------------------------------------
        pred_ord <- names(boot_freq)[order(boot_freq, decreasing = T)]
        boot_01 <- boot_01[, pred_ord]
        boot_01 <- cbind(boot_01, count = rep(1, times =nboot))
        boot_modfreq <- aggregate(count ~ ., data = boot_01, sum)
        boot_modfreq[, "percent"] <-
          boot_modfreq$count / nboot * 100
        boot_modfreq <-
          boot_modfreq[order(boot_modfreq[, "percent"], decreasing = T),]
        boot_modfreq[, "cum_percent"] <- cumsum(boot_modfreq$percent)
        boot_modfreq <-
          boot_modfreq[boot_modfreq[, "cum_percent"] <= 80,]
        if (dim(boot_modfreq)[1] > 20)
          boot_modfreq <- boot_modfreq[1:20,]
        
        
        boot_modfreq <- cbind("Predictors" = apply(boot_modfreq[, c(2:14)], 1,
                                                   function(x)
                                                     paste(names(x[x == 1]), collapse = " ")),
                              boot_modfreq[, c("count", "percent", "cum_percent")])
        
        
        # Model frequency in % of selected model ----------------------------
        sel_modfreq <-
          sum(apply(boot_01[,-dim(boot_01)[2]], 1, function(x)
            identical(((sel_est != 0) * 1
            )[pred_ord], x))) / nboot * 100
        
      }else{
        model_avg <- possibly(model.avg,NULL)
        imp <- possibly(MuMIn::importance,NULL)
        mod <- MuMIn::dredge(lm(mod,data=table_data))
        best.mod <- subset(mod,delta==0)
        nbest.mod <- subset(mod,delta<=4)
        nest.model <- subset(nbest.mod,!nested(nbest.mod))
        avg.model <- model_avg(nest.model)
        importance <- imp(avg.model)
        rm(mod)
        var_names_ordered <- colnames(nbest.mod)[-1] %>% str_subset(pattern =paste(colnames(table_data),collapse = "|"))
        
        
      }
    }
    
    
    if(k=="lasso"){
      return(
        list(
          mod_sel = sel_mod,
          epv = EPVglobal ,
          table_boot = table_boot,
          boot_freq = boot_modfreq,
          data = table_data,
          R2_sel= R2_sel,
          R2_comp = R2_comp
        )
      )
    }else if (k=="average"){
      list(best=best.mod,avg=avg.model,imp=importance,var_ord = var_names_ordered) %>% return()
    }else{
      return(
        list(
          mod_sel = sel_est,
          epv = EPVglobal ,
          table_boot = table_boot,
          data = table_data
        ))
    }
  }
