source(here::here("R","set-global.R"))
require("wooldridge")

mroz <- wooldridge::mroz %>%
  as_tibble() %>%
  select(wage, educ,exper, 
         fatheduc,motheduc,everything()) %>%
  filter(!is.na(wage))


# write.xlsx(mroz,"data/Table-11-mroz-filter.xlsx")
n <- nrow(mroz)

# ==== 17.2 Estimation problem with endogeneity====

### ====wage example: All variables in dataset ======

vars_label<- read.delim(here::here("data","mroz-var-label.txt"), header = T,sep=":") %>%as_tibble()
vars <- names(mroz) 
tab_vars <- vars_label

tab_vars %>%
  mutate(group = if_else(vars %in% c("wage","lwage", "educ","fatheduc", "motheduc", "exper","expersq"), "prior", "secondary")) %>%
  arrange(group,vars) %>%
  add_column(index = 1:nrow(.), .before = "vars") %>%
  select(-group) %>%
  #filter(vars %in% c("wage", "fatheduc", "educ")) %>%
  datatable(caption = "variables and labels",
            rownames = T,
            options = list(
              pageLength=7,
              dom="tip",
              columnDefs = list(
                list(className = 'dt-center',targets = "_all")),
              list(visible=T,targets=0)
            )
  )

### ==== Wage example: Raw dataset====
mroz <- mroz %>%
  select(lwage, educ, exper, expersq, fatheduc, motheduc) %>%
  add_column(id=1:n, .before = "lwage") 

mroz %>% datatable(caption = str_c("dataset n=(",n,")"),
                   rownames = T,
                   options = list(
                     pageLength=8,
                     dom="tip",
                     scrollX = TRUE,
                     scrollCollapse = TRUE,
                     columnDefs = list(
                       list(className = 'dt-center',targets = "_all"))
                   )
) %>%
  formatRound(c("lwage"), 2)

### ==== Wage example: the scatter ====

mroz %>%
  ggplot(aes(educ, lwage))+
  geom_point(size=3) +
  labs(x= "educ", y="log(wage)") +
  theme(text = element_text(size=16))

### ==== Wage example: use OLS method directly====

mod_origin <- formula(lwage ~ educ +exper+expersq)
ols_origin <- lm(formula = mod_origin, data = mroz
                 )


## ==== ## 17.4 Two-stage least squares method  ====

### ====Step-by-step solution ====

### stage 1 OLS estimate ====
mod_step1 <- formula(educ~exper + expersq + motheduc)  # modle setting
ols_step1 <- lm(formula = mod_step1, data = mroz)  # OLS estimation

### stage 1 OLS predicted values ====
mroz_add <- mroz %>% mutate(educHat = fitted(ols_step1)) # add fitted educ to data set

### stage 2 model ====
mod_step2 <- formula(lwage~educHat + exper + expersq)
ols_step2 <- lm(formula = mod_step2, data = mroz_add)

## ==== Integrated solution ====

### ==== IV `mothereduc`:  using `systemfit::systemfit()` ====
# load pkg
require(systemfit)
# set two models
eq_1 <- educ ~  exper + expersq + motheduc
eq_2 <- lwage ~ educ + exper + expersq 
sys <- list(eq1 = eq_1, eq2 = eq_2)
# specify the instruments
instr <- ~  exper + expersq + motheduc
# fit models 
fit.sys <- systemfit(
  sys, inst=instr, 
  method="2SLS", data = mroz)
# summary of model fit
smry.system_m <- summary(fit.sys) 


### ==== IV `mothereduc`:  using `ARE::ivreg()` ====
# load pkg 
require(AER)
# specify model
mod_iv_m <- formula(lwage ~ educ + exper + expersq
                    | motheduc + exper + expersq)# fit model
lm_iv_m <- ivreg(formula = mod_iv_m, data = mroz)
# summary of model fit
smry.ivm <- summary(lm_iv_m)


### ==== IV `fatheduc` :  using `systemfit::systemfit()` ====
# set two models
eq_1 <- educ ~  exper + expersq + fatheduc
eq_2 <- lwage ~ educ + exper + expersq 
sys <- list(eq1 = eq_1, eq2 = eq_2)
# specify the instruments
instr <- ~ exper + expersq + fatheduc
# fit models 
fit.sys <- systemfit(
  sys, inst=instr, 
  method="2SLS", data = mroz)
# summary of model fit
smry.system_f <- summary(fit.sys) 

### ==== IV `fatheduc`:  using `ARE::ivreg()` ====
# specify model
mod_iv_f <- formula(lwage ~ educ + exper + expersq
                    | fatheduc + exper + expersq)
# fit model
lm_iv_f <- ivreg(formula = mod_iv_f, data = mroz)
# summary of model fit
smry.ivf <- summary(lm_iv_f)

### ==== IV `mothereduc` & `fatheduc` :  using `systemfit::systemfit()` ====
# set two models
eq_1 <- educ ~ exper + expersq + motheduc + fatheduc
eq_2 <- lwage ~ educ + exper + expersq 
sys <- list(eq1 = eq_1, eq2 = eq_2)
# specify the instruments
instr <- ~ exper + expersq + motheduc + fatheduc
# fit models 
fit.sys <- systemfit(
  sys, inst=instr, 
  method="2SLS", data = mroz)
# summary of model fit
smry.system_mf <- summary(fit.sys) 

### ==== IV `mothereduc` & `fatheduc` :  using `ARE::ivreg()` ====
# specify model
mod_iv_mf <- formula(
  lwage ~ educ + exper + expersq
  | motheduc + fatheduc + exper + expersq)
# fit model
lm_iv_mf <- ivreg(formula = mod_iv_mf, data = mroz)
# summary of model fit
smry.ivmf <- summary(lm_iv_mf)

### ==== Solutions comparison ====

#install.packages("stargazer")
library("stargazer")
tbl_compare <- stargazer(
  ols_origin, ols_step2, lm_iv_m, lm_iv_f, lm_iv_mf,
  #title="lwage equation: OLS, 2SLS, and IV models compared",
  header=FALSE, 
  type="html", # "html" or "latex" (in index.Rmd) 
  #keep.stat="n",  # what statistics to print
  omit.table.layout="n",
  #star.cutoffs=NA,
  digits=4, 
  single.row=FALSE,
  font.size = "tiny",
  intercept.bottom=FALSE, #moves the intercept coef to top
  column.labels=c("OLS","explicit 2SLS", "IV mothereduc", "IV fathereduc",
                  "IV mothereduc and fathereduc"),
  dep.var.labels.include = FALSE,
  model.numbers = TRUE,
  dep.var.caption="Dependent variable: lwage",
  model.names=FALSE,
  notes = c("standard errorin parentheses"),
  notes.append = TRUE
  #star.char=NULL #supresses the stars
)


## ==== 17.5 Testing Instrument validity ====

### ==== three OLS models  ====

library("car")
mod_relevance1 <- formula(educ ~ exper +expersq + motheduc)
mod_relevance2 <- formula(educ ~ exper +expersq  + fatheduc)
mod_relevance3 <- formula(educ ~ exper +expersq + motheduc + fatheduc)

ols_relevance1 <- lm(formula = mod_relevance1, data = mroz)
ols_relevance2 <- lm(formula = mod_relevance2, data = mroz)
ols_relevance3 <- lm(formula = mod_relevance3, data = mroz)


### ==== restricted F-test (model 1) ====
# restricted F-test
constrain_test1 <- linearHypothesis(
  ols_relevance1, c("motheduc=0")
  )
# obtain F statistics
F_r1 <- constrain_test1$F[[2]]

### ==== classic F-test (model 1) ====
smry_ols1 <- summary(ols_relevance1)
F_c1 <- smry_ols1$fstatistic[[1]]

### ==== restricted F-test (model 2) ====
constrain_test2 <- linearHypothesis(
  ols_relevance2, c("fatheduc=0")
  )
# obtain F statistics
F_r2 <- constrain_test2$F[[2]]

### ==== restricted F-test (model 3) ====
constrain_test3 <- linearHypothesis(
  ols_relevance3, c("motheduc=0", "fatheduc=0")
  )
# obtain F statistics
F_r3 <- constrain_test3$F[[2]]

### ==== Cragg-Donald test (code) ====
# filter samples
mroz1 <- wooldridge::mroz %>%
  filter(wage>0, inlf==1)
# set parameters
N <- nrow(mroz1); G <- 2; B <- 2; L <- 2 
# for endogenous variables
x1 <- resid(lm( mtr ~ kidslt6 + nwifeinc, data = mroz1))
x2 <- resid(lm( educ ~ kidslt6 + nwifeinc, data = mroz1))
# for instruments
z1 <-resid(lm(motheduc ~ kidslt6 + nwifeinc, data = mroz1))
z2 <-resid(lm(fatheduc ~ kidslt6 + nwifeinc, data=mroz1))
# column bind
X <- cbind(x1,x2)
Y <- cbind(z1,z2)
# calculate Canonical correlation
rB <- min(cancor(X,Y)$cor)
# obtain the F statistics
CraggDonaldF <- ((N-G-L)/L)/((1-rB^2)/rB^2)

### ==== Cragg-Donald test (result) ====
out <- tribble(
  ~"G", ~"L", ~"B", ~"N", ~"rb", ~"CraggDonaldF",
  G, L, B, N, number(rB, 0.0001), number(CraggDonaldF, 0.0001)
)
tbl_out <- kable(out, align = "c", caption = "Cragg-Donald test results")


### ==== J-test (2SLS residuals) ==== 
mroz_resid <- mroz %>%
  mutate(resid_iv_mf = residuals(lm_iv_mf)) 

### ==== J-test (run auxiliary regression) ====
# set model formula
mod_jtest <- formula(resid_iv_mf ~ exper +expersq +motheduc +fatheduc)
# OLS estimate
lm_jtest <- lm(formula = mod_jtest, data = mroz_resid)

### ==== J-test (Restricted F-test) ====
# restricted F-test
restricted_ftest <- linearHypothesis(lm_jtest, c("motheduc = 0", "fatheduc = 0"), test = "F")
# obtain the F statistics
restricted_f <- restricted_ftest$F[[2]]

### ==== J-test (calculate J-statistic by hand) ====
# numbers of instruments
m <- 2
# calculate J statistics
(jtest_calc <- m*restricted_f)

### ==== J-test (obtain J-statistic with tools) ====

# chi square test directly
jtest_chitest <- linearHypothesis(
  lm_jtest,  c("motheduc = 0", "fatheduc = 0"), 
  test = "Chisq")
# obtain the chi square value
jtest_chi <- jtest_chitest$Chisq[2]

### ====J-test (adjust the freedoms) ====

# correct freedoms
f <- m -1
# compute correct p-value for J-statistic
(pchi <- pchisq(jtest_chi, df = f, lower.tail = FALSE))

## ==== 17.6 Testing Regressor endogeneity====

### ==== Wage example: Hausman test (full model diagnose) ====

summary(lm_iv_mf, diagnostics = TRUE)


