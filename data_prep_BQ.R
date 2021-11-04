library(tidyverse)
library(zoo)
library(vars)
rm(list=ls())
theme_set(theme_bw())



#-------Data prep-------

# UNRATE into quarterly

unrate <- read_csv("./data/UNRATE.csv") %>%
  mutate(UNRATE = as.numeric(UNRATE)) %>% 
  mutate(DATE = as.Date(DATE)) %>% 
  group_by(Time = format(as.yearqtr(DATE, "%b-%Y"), "%YQ%q")) %>%   # Convert monthly to quarterly
  summarise_all(mean) %>% 
  filter(DATE >as.Date("1948-03-31") & DATE < as.Date("1988-01-01")) # Data from 1948 Q2 to 1987 Q4


# Detrend UNRATE

unrate <- ts(unrate$UNRATE, start = c(1948,2), end = c(1987,4), frequency = 4)
unrate <-  ts(resid(lm(unrate ~ time(unrate))), start = c(1948,2), end = c(1987,4), frequency = 4)


# Detrend GDP with break at 1974 q1

gdp <- read_csv("./data/GNPC96.csv") %>% 
  mutate(GNPC96 = as.numeric(GNPC96)) %>% 
  mutate(dlgdp = 100*c(0,diff(log(GNPC96)))) %>% 
  filter(DATE > as.Date("1948-03-31") & DATE < as.Date("1988-01-01")) %>% # Data from 1948 Q2 to 1987 Q4
  mutate(breakq = 1*(DATE < as.Date("1974-01-01")))

gdp <- resid(lm(gdp$dlgdp ~ c(1:length(gdp$dlgdp)) + gdp$breakq))
gdp <- ts(gdp, start = c(1948,2), end = c(1987,4), frequency = 4)



# VAR estimated by BQ
var_results <- VAR(cbind(gdp, unrate), p=8, type="none")

# LR restriction 
svar_results <- BQ(var_results)

