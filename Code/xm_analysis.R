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

mods <- lapply(c("Fly_Total", "P_arg","Fly_Male","Mosquito_Female"),
               compare_methods)

# Note that the model fit to female mosquitoes does not converge

lapply(mods, function(mod) print(mod$contrasts))
lapply(mods, function(mod) print(mod$emm))

plot.list <- lapply(mods, function(mod) print(mod$p))
grid.arrange(grobs = plot.list)

################################################################################

