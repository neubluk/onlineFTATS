tmp <- readRDS("../../../non i cloud sync/Forecast Updating Sims/sim_res_basic_30092024.rds")

path <- "sims_basic"
library(tidyverse)

est_mapping <- c("bu_bu"="Bottom-Up",
                 "var_full"="Full Cov.",
                 "var_spectral"="Spectral",
                 "var_cov_shrink"="Cov. Shrink",
                 "no_recon"="No Recon.")

# basic boxplots for each scenario, both train and test errors
plot_type <- "med_line" # or "boxplot"
tmp %>%
  group_by(across(c(Nrep, any_of(c("n", "k", "p", "q", "auto", "sigma.sq", "m")), type, level))) %>%
  distinct(train_hat_err, train_tilde_err, test_hat_err, test_tilde_err) %>%
  summarise(s=0,
            train_tilde_now_base_rel_err = train_tilde_err/train_hat_err - 1,
            test_tilde_now_base_rel_err = test_tilde_err/test_hat_err - 1) %>%
  bind_rows(tmp %>%
              group_by(across(c(Nrep, any_of(c("n", "k", "p", "q", "auto", "sigma.sq", "m")), s, type, level))) %>%
              distinct(train_hat_err, train_hat_now_err, test_hat_err, test_hat_now_err) %>%
              summarise(train_tilde_now_base_rel_err = train_hat_now_err/train_hat_err - 1,
                        test_tilde_now_base_rel_err = test_hat_now_err/test_hat_err - 1,
                        type = "no_recon")) %>%
  bind_rows(tmp %>%
              select(Nrep, any_of(c("n", "k", "p", "q", "auto", "s", "sigma.sq", "m")), type, level, train_tilde_now_base_rel_err, test_tilde_now_base_rel_err)) %>%
  rename(train=train_tilde_now_base_rel_err,
         test=test_tilde_now_base_rel_err) %>%
  # filter(s %in% seq(0,length.out=20, by=15)) %>%
  filter(n == 100, 
         # level == 2, 
          type %in% c("var_cov_shrink","no_recon"), 
         p == q) %>%
  mutate(pq = paste0("p=",p,", q=",q)) %>%
  ungroup() %>%
  select(-c(p,q)) %>%
  pivot_longer(c(train,test)) %>%
  group_by( name,
           across(any_of(c("k", "p", "q", "auto", "sigma.sq", "m")))) %>%
  group_walk(~{
     k <- unlist(.y$k)
    .y$k <- paste0(rev(unlist(.y$k)), collapse=",")
    title <- paste(names(.y),
                   .y,
                   collapse=",",
                   sep="=")
    .x <- .x %>%
      mutate(level = factor(level, levels=c(1:(length(k)), "overall")),
             Type=est_mapping[type],
             # name = factor(name, levels=c("train","test"))
             )
    
    lbler <- labeller(name = function(x) ifelse(x=="train","Training","Test"),
                      level=function(x) ifelse(x=="overall","Overall",sprintf("Level %s",x)), 
                      n=function(x)sprintf("n=%s",x), 
                      pq=label_value)
    
    if (plot_type == "boxplot"){
          plot_bounds <- .x %>%
      group_by(level, s, pq, Type) %>%
      mutate(value = ifelse(value==0,NA,value))%>%
      summarise(lower_bound = max(-1,quantile(value,0.25,na.rm=TRUE)-1.5*(quantile(value,0.75,na.rm=TRUE)-quantile(value,0.25,na.rm=TRUE))),
                upper_bound = min(1.5,quantile(value,0.75,na.rm=TRUE)+1.5*(quantile(value,0.75,na.rm=TRUE)-quantile(value,0.25,na.rm=TRUE)))) %>%
      group_by(level) %>%
      summarise(bounds = list(c(min(lower_bound, na.rm=TRUE), max(upper_bound, na.rm=TRUE))))
       print(plot_bounds %>% unnest_wider(bounds, names_sep="_"))
    plot_bounds <- lapply(plot_bounds$bounds, function(x) scale_y_continuous(limits = c(x[1],x[2])))
    
    
    p <-  .x %>%
      ggplot(aes(color=Type,y=value,x=factor(s)))+
      geom_boxplot(outlier.size = 1)+
      facet_grid(level~pq, scales="free", labeller = lbler)+
      ggh4x::facetted_pos_scales(y=plot_bounds)+
      geom_hline(yintercept=0, linetype="dashed")+
      theme(legend.position = "bottom")+
      labs(x="New data available", y="rRMSE")
      
      ggsave(sprintf("../Papers/Online Temporal Hierarchical Forecasting/OFTATS/figs/%s/box_%s.pdf", path, title), plot=p, width=7, height=4)
      
    } else if (plot_type == "med_line") {
      p <- .x %>%
        group_by(Type, level, s, across(any_of(c("n","pq","name")))) %>%
        summarise(value = quantile(value, na.rm=TRUE, 0.5)) %>%
        ggplot(aes(color=Type,y=value,x=s,fill=Type))+
        geom_point(alpha=0.1, size=0.1)+
        # geom_smooth(method="lm", se=FALSE)+
        geom_line()+
        facet_grid(level~pq, scales="free", labeller = lbler)+
        # ggh4x::facetted_pos_scales(y=plot_bounds)+
        geom_hline(yintercept=0, linetype="dashed")+
        theme(legend.position = "bottom")+
        labs(x="New data available", y="rRMSE")+
        ylim(c(-1,1))
      ggsave(sprintf("../Papers/Online Temporal Hierarchical Forecasting/OFTATS/figs/%s/allline_%s.pdf", path, title), plot=p, width=7, height=4)
    }
})

# basic tables to summarise each scenario in mean (se)
tmp %>%
  select(Nrep, n, k, p, q, auto, s, sigma.sq, type, level, train_tilde_now_rel_err, test_tilde_now_rel_err) %>%
  rename(atrain=train_tilde_now_rel_err,
         btest=test_tilde_now_rel_err) %>%
  pivot_longer(c(atrain,btest)) %>%
  group_by(type, name, auto, n, k, p, q, s, sigma.sq, level) %>%
  summarise(mean_err = mean(value, na.rm=TRUE, trim=0.1),
            se_err = chemometrics::sd_trim(value[!is.na(value)], trim=0.1)/sqrt((1-0.1)*sum(!is.na(value)))) %>%
  rowwise() %>%
  mutate(level = factor(level, levels=c(1:(length(k)), "overall")),
         
         type=est_mapping[type],
         value = paste0(format(round(mean_err,2),nsmall=2)," (", format(round(se_err,2),nsmall=2), ")")) %>%
  group_by(sigma.sq, auto, k, p, q) %>%
  group_walk(~{
    
    .y2 <- .y %>%
      rowwise() %>%
      mutate(k = paste0("(",paste(k,collapse = ","),")"))
    
    title <- paste(
      names(.y2),
      unlist(.y2),
      sep = "=",
      collapse = ","
    )
    
    caption <- "$10\\%$-trimmed mean rRMSE for "
    
    math_cap <- c("sigma.sq"="\\sigma^2","p"="p","q"="q","k"="k")
    seps <- c("=", "\\in", "=", "=")
    
    .y2 <- .y %>%
      select(-any_of(c("auto" ,"name"))) %>%
      rowwise() %>%
      mutate(k = paste0("\\{",paste(rev(k),collapse = ","),"\\}"))
    
    caption <- paste(caption,
                     "$",
                     paste0(
                       math_cap[names(.y2)],
                       seps,
                       unlist(.y2),
                       collapse = ", "
                     ),
                     "$",
                     collapse=" ")
    
    if (.y$auto){
      caption <- paste(caption,
                       "and auto-selected models.")
    } else {
      caption <- paste(caption,
                       "and fixed order of the used models.")
    }
    
    caption <- paste(caption,
                     "The standard errors are given in parentheses and the best results are highlighted in bold.")
    x2 <- .x %>%
      select(level, n, type, name, s, value) %>%
      mutate(level = factor(level, levels = c(1:(length(unique(.x$level))-1), "overall"),
                            labels = c(paste("Level",1:(length(unique(.x$level))-1)), "Overall"))
      ) %>%
      pivot_wider(names_from=c(name,s), values_from=value) %>%
      arrange(level, n, type)
    
    dat_min_ind <- .x %>%
      select(level, n, type, name, s, mean_err) %>%
      mutate(level = factor(level, levels = c(1:(length(unique(.x$level))-1), "overall"),
                            labels = c(paste("Level",1:(length(unique(.x$level))-1)), "Overall"))
      )  %>%
      group_by(level, n, name, s) %>%
      reframe(type,
              is_min = mean_err == min(mean_err, na.rm=TRUE)) %>%
      pivot_wider(names_from=c(name, s), values_from=is_min) %>%
      arrange(level, n, type) %>%
      mutate(level=FALSE,
             n=FALSE,
             type=FALSE,
             across(where(is.logical), replace_na, FALSE)) %>%
      as.matrix()
    
    dat <- as.matrix(x2)
    
    for (i in 1:ncol(dat_min_ind)){
      dat[, i] <- kableExtra::cell_spec(dat[,i],format="latex", bold=dat_min_ind[,i])
    }
    
    unique_s <- unique(.x$s)
    
    kab <- dat%>%
      knitr::kable(align=c(rep("l",3),rep("r",2*length(unique(.x$s)))),
                   booktabs=TRUE,
                   format="latex",
                   caption=caption, 
                   label=title,
                   col.names = c("Level", "n", "Type", paste0(c("z=",rep("",length(unique_s)-1)), unique(.x$s)), paste0(c("z=",rep("",length(unique_s)-1)), unique(.x$s))),
                   escape=FALSE) %>%
      kableExtra::collapse_rows(columns = 1:2, latex_hline="custom", custom_latex_hline = 1:2) %>%
      kableExtra::add_header_above(c(" "=3, "Training rRMSE"=length(unique(.x$s)), "Test rRMSE"=length(unique(.x$s)))) %>%
      kableExtra::kable_styling(latex_options = "scale_down")
    
    kab <- gsub("\\raggedleft\\arraybackslash", "", kab, fixed=TRUE)
    kab <- gsub("\\raggedright\\arraybackslash", "", kab, fixed=TRUE)
    
    writeLines(kab, sprintf("../Papers/Online Temporal Hierarchical Forecasting/OFTATS/tables/%s/%s.tex", path, title))
  })









 tmp %>%
  select(Nrep, n, k, p, q, s, type, level, train_tilde_now_rel_err) %>%
  rowwise() %>%
  filter(n == 100, level == 1) %>%
  group_by(level, n, k) %>%
  mutate(pq = paste0("p=",p,", q=",q)) %>%
  group_walk(~{
    title <- paste(names(.y),
                   .y,
                   collapse=",",
                   sep="=")
    p <- .x %>%
      ggplot(aes(color=factor(type),y=train_tilde_now_rel_err,x=pq))+
      geom_boxplot()+
      facet_grid(s~., scales="free", labeller = labeller(p=function(x)sprintf("p=%s",x), q=function(x)sprintf("q=%s",x)))+
      geom_hline(yintercept=0, linetype="dashed")+
      ggtitle(title)
    print(p)
  })


tmp %>%
  select(-c(test_hat_err, test_tilde_err, test_hat_now_err, test_tilde_now_err,contains("train"))) %>%
  pivot_longer(contains("err")) %>%
  # bind_rows(tmp %>% select(rep,s,level,test_now_rel_err) %>% mutate(type="base_now") %>% rename(value=test_now_rel_err) %>% expand_grid(name=c("test_tilde_rel_err"))) %>%
  ggplot(aes(color=type,y=value,x=interaction(s)))+
  geom_boxplot()+
  facet_grid(level~name, scales="free")+
  geom_hline(yintercept=0, linetype="dashed")+
  ggh4x::facetted_pos_scales(
    y = list(level == "1" ~ scale_y_continuous(limits = c(-1,1)),
             level == "2" ~ scale_y_continuous(limits = c(-0.5,0.5)),
             level == "overall" ~ scale_y_continuous(limits = c(-1,1)))
  )

tmp %>%
  mutate(type=paste(Var1,Var2,sep="-")) %>%
  select(any_of(c("p","q","Source")), s, type, level, train_tilde_now_rel_err, test_tilde_now_rel_err) %>%
  #filter(level=="1") %>%
  #mutate(order = paste0("p=",p,", q=",q)) %>%
  pivot_longer(contains("err")) %>%
  group_by(s, type, level, name) %>%
  mutate(mean = mean(value, trim=0.1),
         se = chemometrics::sd_trim(value,trim=0.1)/(1-0.1)/sqrt(n())) %>%
  # bind_rows(tmp %>% select(rep,s,level,test_now_rel_err) %>% mutate(type="base_now") %>% rename(value=test_now_rel_err) %>% expand_grid(name=c("test_tilde_rel_err"))) %>%
  ggplot(aes(color=type,y=mean,x=s))+
  # geom_boxplot()+
  geom_point()+
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se))+
  geom_smooth(method = "loess")+
  facet_grid(name~level, scales="free")+
  # ggh4x::facetted_pos_scales(
  #   y = list(name == "train_tilde_now_rel_err" ~ scale_y_continuous(limits = c(-1,0.5)),
  #            name == "test_tilde_now_rel_err" ~ scale_y_continuous(limits = c(-1,1)))
  # )+
  geom_hline(yintercept=0, linetype="dashed")
