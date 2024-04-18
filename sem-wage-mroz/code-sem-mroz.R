## ==== load other necessary packages ====
library("tidyverse")
library("magrittr")
library("wooldridge")  # data sets
library("systemfit")

mroz <- wooldridge::mroz %>%
  as_tibble() %>%
  select(wage, educ,exper, 
         fatheduc,motheduc,everything()) %>%
  filter(!is.na(wage), hours >0) # keep in workforce

## ==== OLS directly ====
eq.S <- hours ~ lwage +educ +age + kidslt6 + nwifeinc
eq.D <- lwage ~ hours +educ +exper + expersq

fit_s <- lm(formula = eq.S, data = mroz)
smry_s <- summary(fit_s)

fit_d <- lm(formula = eq.D, data = mroz)
smry_d <- summary(fit_d)

## ==== Two-stage least squares (2SLS) regression results ====


sys <- list(eq.D, eq.S)
instr <- ~ age +kidslt6 + nwifeinc +educ +exper + expersq
mroz.sys <- systemfit(formula = sys, inst=instr, 
                      method="2SLS", data=mroz)
mroz.smry <- summary(mroz.sys) 
