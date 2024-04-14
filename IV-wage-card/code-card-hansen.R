
#==== data prepare ====
require(magrittr)
require(tidyverse)
require(broom)
require(haven)
require(systemfit)
require(AER)
#renv::install("ivmodel")
require(ivmodel) # for LIML fit

# read data
file_path <- "IV-wage-card/card1995.dta"
card1995 <- haven::read_dta(file_path) 

tbl_tidy <- card1995 |>
  mutate(lwage = lwage76,
         edu = ed76,
         exp = age76 - ed76 - 6,
         exp2 = (exp^2)/100,
         age = age76,
         age2 = (age^2)/100,
         south = reg76r,
         urban = smsa76r,
         college = nearc4,
         public = nearc4a,
         private = nearc4b
  ) |>
  filter(!is.na(lwage)) %>%
  rename_at(all_of(c("daded","momed")), 
            ~ all_of(c("dadedu","momedu")))

vars_list <- c("lwage", "edu", "exp", "exp2", 
               "age", "age2", "black","south", "urban",
               "college","public","private","momedu","dadedu")
tbl_reg <- tbl_tidy |>
  select(all_of(vars_list))

# write out, run only once
#file_path <- here::here("data/hansen21-chpt12-iv-card1995.xlsx")
#openxlsx::write.xlsx(tbl_reg, file_path)

# ==== model fit: OLS, IV, and 2SLS  ====
# model ols
mod_ls <- formula("lwage ~ edu + exp + exp2 + black + south + urban")
fit_lm <- lm(formula = mod_ls, data = tbl_reg)
smry_lm <- summary(fit_lm)

# iv model with instrument of college
mod_iv_cl <- formula("lwage ~ edu + exp + exp2 + black + south + urban |college + exp + exp2 + black + south + urban")
fit_iv_cl <- AER::ivreg(formula = mod_iv_cl,data = tbl_reg )
smry_iv_cl <- summary(fit_iv_cl)

# iv model with instrument of college and age
mod_iv_ca <- formula("lwage ~ edu + exp + exp2 + black + south + urban |college + age + age2 + black + south + urban")
fit_iv_ca <- AER::ivreg(formula = mod_iv_ca,data = tbl_reg )
smry_iv_ca <- summary(fit_iv_ca)  

# tsls model with instrument of public and private
mod_tsls_pp <- formula("lwage ~ edu + exp + exp2 + black + south + urban |public +private + exp + exp2 + black + south + urban")
fit_tsls_pp <- AER::ivreg(formula = mod_tsls_pp,data = tbl_reg )
smry_tsls_pp <- summary(fit_tsls_pp)

# tsls model with instrument of public, private and age
mod_tsls_ppa <- formula("lwage ~ edu + exp + exp2 + black + south + urban |public +private + age + age2 + black + south + urban")
fit_tsls_ppa <- AER::ivreg(formula = mod_tsls_ppa,data = tbl_reg )
smry_tsls_ppa <- summary(fit_tsls_ppa)

# tsls model with instrument of public, private, momedu, dadedu, and age
## Subset Overid test Test (Section 12.32)
mod_tsls_ppmda <- formula("lwage ~ edu + exp + exp2 + black + south + urban |public +private + momedu + dadedu +age + age2 + black + south + urban")
fit_tsls_ppmda <- AER::ivreg(formula = mod_tsls_ppmda,data = tbl_reg )
smry_tsls_ppmda <- summary(fit_tsls_ppmda)

# ==== model fit: liml estimation ====
library(ivmodel)
# see url: https://github.com/hyunseungkang/ivmodel
### Use Card (1995) data to estimate the effect of education on log earnings ###
# n: sample size; L: # of IVs; p: # of covariates 
# Y: n by 1 vector of outcomes (must be continuous)
# D: n by 1 vector of treatments (continuous or discrete)
# Z: n by L vector of instruments (continuous or discrete)
# X: n by L vector of instruments (continuous or discrete)

# two Instrument Anaylsis with Proximity to 4yr College as IV#
Y <-  tbl_reg[,"lwage"] |> as.matrix()
D <-  tbl_reg[,"edu"] |> as.matrix()
Z <-  tbl_reg[,c("public", "private")] |> as.matrix()
Xname <-  c("exp", "exp2", "black", "south", "urban")
X <-  tbl_reg[,Xname] |> as.matrix()

# fit model
card.model2IV <- ivmodel(Y=Y, D=D, Z=Z, X=X)

# get the result of LIML
fit_liml_pp <- card.model2IV$LIML
vars_eng <- c("edu", "exp", "exp2", "black", "south", "urban", "(Intercept)")
vars_fct <- c("(Intercept)","edu", "exp", "exp2", "black", "south", "urban")

## table of estimation
est_liml_pp <- tibble(
  term = vars_eng,
  estimate = c(fit_liml_pp$point.est,
               fit_liml_pp$point.est.other),
  std.error = c(fit_liml_pp$std.err,
                fit_liml_pp$std.err.other),
  statistic = c(fit_liml_pp$test.stat,
                fit_liml_pp$test.stat.other),
  p.value = c(fit_liml_pp$p.value,
              fit_liml_pp$p.value.other)
) |>
  mutate(term = factor(term, levels = vars_fct),
         model = "LIML_pp") %>%
  arrange(term)

# ==== custom function ====

#' Tidy model with broom and add model names
#'
#' @param x Object. An model fit/result object to be converted into a tidy tibble.
#' @param y Character. to label the model fit/result object.
#'
#' @return tibble
#' @export
#'
#' @examples
#' 
modfit2dt <- function(x, y){
  out <- broom::tidy(x) |>
    mutate(model = y)
}

#' Compare and format multiple model fit/results with DT::datatable() 
#'
#' @param df tibble, long format tibble contains  multiple model fit/results.
#' @param fct.level, character vector, specify the common variables' factor levels 
#' for all  model fit/results.
#' @param caption character, the DT caption. 
#' @param isEscaped Logical, turn on/ off DT cell newline, 
#' default `isEscaped = FALSE` to turn on newline
#' @param opt_len integer, control table length with default `opt_len=15`
#'
#' @return DT objects
#' @export
#'
#' @examples
#' 
tidy.dt <- function(df, fct.level, 
                    caption, isEscaped=FALSE,
                    opt_len=15){
  out_str <- df |>
    select(model, term, estimate, std.error) %>%
    mutate(
      result = str_c(
        scales::number(estimate,0.0001), "<br/>",
        "(",scales::number(std.error,0.0001), ")"
      )
    ) |>
    select(model, term, result) |>
    pivot_wider(names_from = model,
                values_from = result) |>
    mutate(term = factor(term, levels = fct.level)) |>
    arrange(term)
  
  dt_tab <- DT::datatable(
    out_str, 
    escape = isEscaped, # default FALSE to keep newline
    caption = caption,
    options = list(
      dom = "t", 
      pageLength = opt_len,
      columnDefs = list(
        list(width = 10, targets = c(2:7))
      )
    )
  )
  
  return(dt_tab)
}


# ==== result compare: six model fit ====
## combine all result 
library(broom)

mod_type <- list("OLS", "IV_c", "IV_ca",
                 "2SLS_pp", "2SLS_ppa")
mod_list <- list(fit_lm, fit_iv_cl, fit_iv_ca, 
                 fit_tsls_pp, fit_tsls_ppa) 
# purrr map
out_stage2 <- map2_df(.x = mod_list, 
               .y = mod_type, 
               .f = modfit2dt) |>
  bind_rows(est_liml_pp)  |> # bind LIML
  select(model, everything())

# ==== result show: table 12.1 ====
## 
## note 1: get rid margin for word output, set chunk argument:'screenshot.opts=list(selector=".dataTables_wrapper")'
## note 2: create DT cell newline: first glue string with `"<br/>"`, 
## and then use `DT::datatable(yourtable,escape = FALSE)`

terms_all <-c('(Intercept)','edu','exp','exp2','black','south','urban') 

tab12.1 <- tidy.dt(df = out_stage2, fct.level = terms_all,
        caption = "第二阶段回归估计结果",
        isEscaped = FALSE,
        opt_len = 15)

# ==== fit instrument ====
iv_wg_c <- formula("lwage ~ exp + exp2 + black + south + urban + college")
fit_wg_c <- lm(iv_wg_c, tbl_reg)
smry_wg_c <- summary(fit_wg_c)

# K=L, one exendogeneity
## iv for edu: with {college}
iv_edu_c <- formula("edu ~ exp + exp2 + black + south + urban + college")
fit_edu_c <- lm(iv_edu_c, tbl_reg)
smry_edu_c <- summary(fit_edu_c)

# K=L, two endogeneity
## iv for edu: with {college} and {age + age2}
iv_edu_ca <- formula("edu ~ age + age2 + black + south + urban + college")
fit_edu_ca <- lm(iv_edu_ca, tbl_reg)
smry_edu_ca <- summary(fit_edu_ca)

## iv for exp: with {college} and {age + age2} 
iv_exp_ca <- formula("exp ~ age + age2 + black + south + urban + college")
fit_exp_ca <- lm(iv_exp_ca, tbl_reg)
smry_exp_ca <- summary(fit_exp_ca)

## iv for exp2: with {college} and {age + age2}  
iv_exp2_ca <- formula("exp2 ~ age + age2 + black + south + urban + college")
fit_exp2_ca <- lm(iv_exp2_ca, tbl_reg)
smry_exp2_ca <- summary(fit_exp2_ca)

# K > L, one endogeneity
## iv for edu with {public +private} 
iv_edu_pp <- formula("edu ~ exp + exp2 + black + south + urban + public + private")
fit_edu_pp <- lm(iv_edu_pp, tbl_reg)
smry_edu_pp <- summary(fit_edu_pp)

## iv for edu: with {public +private + momedu + dadedu}
iv_edu_ppmd <- formula("edu ~ public +private + momedu + dadedu +exp + exp2 + black + south + urban")
fit_edu_ppmd <- lm(iv_edu_ppmd, tbl_reg)
smry_edu_ppmd <- summary(fit_edu_ppmd)

# K > L, two endogeneity
## iv for edu: with {public +private} and {age + age2} 
iv_edu_ppa <- formula("edu ~ age + age2 + black + south + urban + public + private")
fit_edu_ppa <- lm(iv_edu_ppa, tbl_reg)
smry_edu_ppa <- summary(fit_edu_ppa)

## iv for exp: with {public +private} and {age + age2} 
iv_exp_ppa <- formula("exp ~ age + age2 + black + south + urban + public + private")
fit_exp_ppa <- lm(iv_exp_ppa, tbl_reg)
smry_exp_ppa <- summary(fit_exp_ppa)

## iv for exp2: with {public +private} and {age + age2} 
iv_exp2_ppa <- formula("exp2 ~ age + age2 + black + south + urban + public + private")
fit_exp2_ppa <- lm(iv_exp2_ppa, tbl_reg)
smry_exp2_ppa <- summary(fit_exp2_ppa)

# ==== result compare: first stage for iv reg ====

library(broom)
mod_type <- list("lwage_c", "edu_c", "edu_ca",
                 "exp_ca", "exp2_ca",
                 "edu_pp","edu_ppmd")
mod_list <- list(
  fit_wg_c, fit_edu_c, fit_edu_ca, 
  fit_exp_ca, fit_exp2_ca, fit_edu_pp,
  fit_edu_ppmd) 

# purrr map
out_stage1 <- map2_df(
  .x = mod_list, 
  .y = mod_type, 
  .f = modfit2dt # costom function 
) |>
  select(model, everything())

# ==== result show: table 12.2 ====
## note: see code notation in section table 12.1
terms_all <-c('(Intercept)','exp','exp2','black','south','urban','college','public','private','age','age2',"momedu","dadedu") 

tab12.2 <- tidy.dt(df = out_stage1, fct.level = terms_all,
                   caption = "第一阶段回归估计结果",
                   isEscaped = FALSE,
                   opt_len = 15)
