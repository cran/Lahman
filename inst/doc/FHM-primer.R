## ----nomessages, echo = FALSE-------------------------------------------------
# set some default options for chunks
knitr::opts_chunk$set(
  warning = FALSE,   # avoid warnings and messages in the output
  message = FALSE,
  collapse = TRUE,   # collapse all output into a single block
  tidy = FALSE,      # don't tidy our code-- assume we do it ourselves
  fig.height = 5,
  fig.width = 7
)
options(digits=4)    # number of digits to display in output; can override with chunk option R.options=list(digits=)

set.seed(1234)       # reproducibility

## -----------------------------------------------------------------------------
B = 1e3
u = pnorm(rnorm(B))

## -----------------------------------------------------------------------------
u_true = 1:B / (B+1) # almost true uniform quantiles (slight bias in tails)
plot(u_true, sort(u), pch = 19, cex = 0.25, 
     main = "Uniform Q-Q Plot",
     xlab = "Theoretical Quantiles", 
     ylab = "Observed Quantiles")
lines(u_true, u_true, lty = 2, col = "red")

## -----------------------------------------------------------------------------
n = 100
u_obs = sort(unlist(replicate(B, pnorm(max(rnorm(n)))^100)))
plot(u_true, u_obs, pch = 19, cex = 0.25, 
     main = "Uniform Q-Q Plot for Maximum of 100 Normals",
     xlab = "Theoretical Quantiles", 
     ylab = "Observed Quantiles")
lines(u_true,u_true, lty = 2, col = "red")

## ----message = FALSE, warning = FALSE-----------------------------------------
# software
library(Lahman)
library(tidyverse)
#install.packages("devtools")
#library(devtools)
#devtools::install_github(repo = "DEck13/fullhouse")
#library(fullhouse)

# version numbers
packageVersion("Lahman")
packageVersion("tidyverse")
#packageVersion("fullhouse")

# data preprocessing
foo = Batting %>% 
  # get years 1911 and 1997
  filter(yearID %in% c(1911, 1997)) %>% 
  # compute batting average
  mutate(BA = round(H/AB, 3)) %>% 
  # get player names
  left_join(People %>% mutate(name = paste(nameFirst, nameLast, sep = " "))) %>% 
  # subset columns
  select(name, yearID, AB, BA) %>% 
  # only consider full-time players
  filter(AB >= 320)

## -----------------------------------------------------------------------------
max_BA = foo %>% 
  summarise(name = name[which(BA == max(BA))], max_BA = max(BA), .by = yearID)
max_BA

## -----------------------------------------------------------------------------
means_sds = foo %>% 
  summarise(mean_BA = mean(BA), sd_BA = sd(BA), .by = yearID)
means_sds

## -----------------------------------------------------------------------------
foo %>% 
  ggplot() + 
  aes(x = BA, color = factor(yearID)) + 
  geom_density() + 
  theme_minimal() + 
  labs(color = "yearID")

## -----------------------------------------------------------------------------
foo %>% 
  summarise(name = name[which(scale(BA) == max(scale(BA)))], 
            Z_max = max(scale(BA)), 
            .by = yearID)

## ----message = FALSE----------------------------------------------------------
# talent pool size (from fullhouse package)
N_1911 = 1694866
N_1997 = 9669798

# number of full-time players
ns = foo %>% summarise(n = n(), .by = yearID)
n_1911 = ns %>% filter(yearID == 1911) %>% pull(n)
n_1997 = ns %>% filter(yearID == 1997) %>% pull(n)

## -----------------------------------------------------------------------------
# means and standard deviations for BA from 1911 
means_sds_1911 = means_sds %>% 
  filter(yearID == 1911) %>% select(mean_BA, sd_BA)
# means and standard deviations for BA from 1997
means_sds_1997 = means_sds %>% 
  filter(yearID == 1997) %>% select(mean_BA, sd_BA)

## -----------------------------------------------------------------------------
# max BA from 1911 
max_BA_1911 = max_BA %>% 
  filter(yearID == 1911) %>% pull(max_BA)
# max BA from 1997 
max_BA_1997 = max_BA %>% 
  filter(yearID == 1997) %>% pull(max_BA)
# 1911 Ty Cobb's extreme batting average percentile
F_max_1911 = 
  pnorm(max_BA_1911, 
        mean = means_sds_1911$mean_BA, 
        sd = means_sds_1911$sd_BA)^n_1911
F_max_1911
# 1997 Tony Gwynn's extreme batting average percentile
F_max_1997 = 
  pnorm(max_BA_1997, 
        mean = means_sds_1997$mean_BA, 
        sd = means_sds_1997$sd_BA)^n_1997
F_max_1997

## -----------------------------------------------------------------------------
# aptitude for 1911 Ty Cobb
qnorm(F_max_1911^(1/N_1911))
# aptitude for 1997 Tony Gwynn
qnorm(F_max_1997^(1/N_1997))

## -----------------------------------------------------------------------------
# input for 1997 Tony Gwynn
sprintf("%.10f", F_max_1997^(1/N_1997))
# input for 1911 Ty Cobb
sprintf("%.10f", F_max_1911^(1/N_1911))

