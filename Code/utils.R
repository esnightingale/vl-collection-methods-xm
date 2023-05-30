
make_df <- function(mod){
  
  df <- tidy(mod) %>% 
    filter(effect == "fixed") %>% 
    bind_cols(m1.ci[-1,]) %>% 
    mutate(across(c(estimate,conf.low,conf.high), function(x) round(exp(x),3)),
           IRR = paste0(estimate," (", conf.low, ", ", conf.high, ")")) %>% 
    select(term, IRR, statistic, p.value)
  
  return(df)
  
}

compare_methods <- function(var){
  
  mod <- glmer.nb(paste(var,"~ Collection_Method + (1|House_ID)"),
                  dat)
  
  summ.df <- make_df(mod)
  print(summ.df)
  
  # Marginal means per method 
  emm <- emmeans(mod,
                 pairwise ~ "Collection_Method",
                 type = "response")
  
  # Make df with CI
  emm.df <-
    emm$emmeans %>%
    broom::tidy() %>% 
    left_join(broom::tidy(confint(emm$emmeans)))
  
  # Pairwise contrasts
  emm.contr <- emm$contrasts %>%
    broom::tidy() %>% 
    mutate(outcome = var) %>% 
    select(outcome, contrast, ratio, std.error, adj.p.value)
  
  print(emm.contr)
  
  # Plot EMMs
  emm.df %>% 
    ggplot(aes(Collection_Method, response, 
               ymin=conf.low, ymax=conf.high,
               colour = Collection_Method)) +
    geom_errorbar(width = 0.2) +
    geom_point() +
    guides(colour = "none") +
    labs(x = "Collection Method",
         y = "Marginal mean per collection",
         title = var) -> p
  
  return(list(fit = mod,
              summary = summ.df,
              emm = emm.df,
              contrasts = emm.contr,
              plot = p))
}