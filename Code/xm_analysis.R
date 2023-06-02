library(tidyverse)
library(lme4)
library(emmeans)
library(broom.mixed)
library(gridExtra)

theme_set(theme_classic())

source(here::here("Code/utils.R"))

dat_raw <- read_csv(here::here("Data/PilotDatawithMosOct2020.csv"))

# ---------------------------------------------------------------------------- #
# Check missing data entries and tidy

dat_raw %>% 
  select(Fly_Total:`MOSQUITO MALE`) %>%
  summarise(across(everything(), 
                   # sum NA and . entries across outcomes
                   function(x) sum(x == "." | is.na(x))))

dat_all <- dat_raw %>% 
  mutate(across(Fly_Total:`MOSQUITO MALE`, 
                function(x) as.numeric(na_if(na_if(x, "-"),"."))),
         Month = factor(Month, 
                        levels = c("June","July","August","September")), 
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
ggsave(here::here("Figures","emmeans_all_outcomes.png"), 
       plot.grid, height = 8, width = 8)

################################################################################
################################################################################

