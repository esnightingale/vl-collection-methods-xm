library(tidyverse)
library(lme4)
library(emmeans)
library(broom.mixed)
library(gridExtra)

theme_set(theme_classic())

# dat_raw <- readxl::read_xlsx(here::here("pilotstudy_07022020_mmc.xlsx"))
dat_raw <- readxl::read_xlsx(here::here("Data/PilotDatawithMosOct2020.xlsx"))

# ---------------------------------------------------------------------------- #
# Check missing data entries and tidy

dat_raw %>% 
  select(Fly_Total:`MOSQUITO MALE`) %>%
  mutate(across(everything(), function(x) (x == "." | is.na(x)))) %>%
  summary() 

dat_all <- dat_raw %>% 
  mutate(across(Fly_Total:`MOSQUITO MALE`, function(x) as.numeric(na_if(na_if(x, "-"),"."))),
         Month = factor(Month, levels = c("June","July","August","September")), 
         Date = as.Date(Date),
         across(where(is.character), as.factor)) %>% 
  rename(Fly_Male = Male,
         Mosquito_Female = `MOSQUITO FEMALE`)

summary(dat_all) 

# Excluding fourth replicate
dat <- dat_all %>% 
  filter(Replicate != 4)

# ---------------------------------------------------------------------------- #
# Summarise P. arg per collection by method

dat %>% 
  ggplot(aes(Collection_Method, P_arg,
             fill = Collection_Method)) +
  geom_boxplot() +
  labs(x = "Collection Method", y = "P. argentipes per collection") +
  guides(fill = F) 

ggsave(here::here("Figures","boxplot_p_arg.png"), height = 5, width = 7)

# ---------------------------------------------------------------------------- #
# Fit models for four outcomes

mods <- lapply(c("Fem_Total", "P_arg","Fly_Male","Mosquito_Female"),
               compare_methods)
# Note that the model fit to female mosquitoes does not converge

# Summary of fitted IRRs between MVA/PKP and CDC
lapply(mods, function(mod) print(mod$summary))

# Pairwise comparisons between methods
lapply(mods, function(mod) print(mod$contrasts))

# Marginal mean counts (averaging over household variation) by method
plot.list <- lapply(mods, function(mod) print(mod$p))
plot.grid <- grid.arrange(grobs = plot.list)
ggsave(here::here("Figures","emmeans_all_outcomes.png"), plot.grid, height = 8, width = 8)

# ---------------------------------------------------------------------------- #
# Compare proportion bloodfed between methods

mod2 <- glmer(cbind(Arg_Blood,P_arg) ~ Collection_Method + (1|House_ID),
                weights = P_arg,
                family = "binomial",
                dat)

summ.df <- make_df(mod2)
print(summ.df)

# Marginal means per method 
emm <- emmeans(mod2,
               "Collection_Method",
               type = "response")

# Make df with CI
emm.df <-
  emm %>%
  broom::tidy() %>% 
  left_join(broom::tidy(confint(emm)))

# Pairwise contrasts
emm.contr <- emm %>%
  contrast(method = "pairwise") %>% 
  broom::tidy() %>% 
  mutate(outcome = "Blood fed p") %>% 
  select(outcome, contrast, odds.ratio, std.error, adj.p.value)

print(emm.contr)

# Plot EMMs
emm.df %>% 
  ggplot(aes(Collection_Method, prob, 
             ymin=conf.low, ymax=conf.high,
             colour = Collection_Method)) +
  geom_errorbar(width = 0.2) +
  geom_point() +
  guides(colour = "none") +
  labs(x = "Collection Method",
       y = "Marginal mean",
       title = "Proportion of caught female P. argentipes found to be blood-fed") 

ggsave(here::here("Figures","emmeans_prop_bloodfed.png"), height = 5, width = 7)

################################################################################

