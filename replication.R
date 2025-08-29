###############################################################################
#     Replication: Improving the Design of Conditional Transfer Programs: 
#         Evidence from a Randomized Education Experiment in Colombia
#
#                             Miguel Camacho
#                               David Guio
#                               Unai Oyón
###############################################################################

rm(list = ls())

#setwd("/Users/dguio/Documents/APE/M2/S2/Machine/Final_project/Replication")

#################
##### SETUP #####
#################

# Installs packages if not already installed, then loads packages 
list.of.packages <- c("SuperLearner", "ggplot2", "glmnet", "clusterGeneration",
                      "mvtnorm", "xgboost", "haven", "dplyr", "sandwich",
                      "lmtest", "stargazer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))

######################################################################
############# Replication Original Results (Table 3) #################
######################################################################

data <- read_dta("Public_Data_AEJApp_2010-0132.dta")

data <- data %>%
  filter( !(suba == 1 & grade < 9)) %>%
  filter( survey_selected==1) %>%
  filter( !(grade == 11))

#Regressions of enrollment for San Cristobal

data_san_cristobal <- data %>%
  filter(suba==0)

C1 <- lm(at_msamean ~ T1_treat + T2_treat, data = data_san_cristobal)
vcov1 <- vcovCL(C1, cluster = data_san_cristobal$school_code)

summary(C1, vcov = vcov1)
coeftest(C1, vcov = vcov1)

robust.se1 <- sqrt(diag(vcov1))

C2 <- lm(at_msamean ~ T1_treat + T2_treat + s_teneviv + s_utilities +
           s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 +
           s_years_back + s_sexo + s_estcivil + s_single + s_single + 
           s_edadhead + s_yrshead + s_tpersona + s_num18 + s_estrato +
           s_puntaje + s_ingtotal + grade + suba + s_over_age
         , data = data_san_cristobal)
vcov2 <- vcovCL(C2, cluster = data_san_cristobal$school_code)

summary(C2, vcov = vcov2)
coeftest(C2, vcov = vcov2)

robust.se2 <- sqrt(diag(vcov2))


C3 <- lm(at_msamean ~ T1_treat + T2_treat + school_code + s_teneviv + s_utilities +
           s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 +
           s_years_back + s_sexo + s_estcivil + s_single + s_single + 
           s_edadhead + s_yrshead + s_tpersona + s_num18 + s_estrato +
           s_puntaje + s_ingtotal + grade + suba + s_over_age
         , data = data_san_cristobal)
vcov3 <- vcovCL(C3, cluster = data_san_cristobal$school_code)

summary(C3, vcov = vcov3)
coeftest(C3, vcov = vcov3)


robust.se3 <- sqrt(diag(vcov3))


#Regressions of enrollment for Suba
data_suba <- data %>%
  filter(suba==1 & grade > 8)

C4 <- lm(at_msamean ~ T3_treat , data = data_suba)
vcov4 <- vcovCL(C4, cluster = data_suba$school_code)

summary(C4, vcov = vcov4)
coeftest(C4, vcov = vcov4)

robust.se4 <- sqrt(diag(vcov4))


C5 <- lm(at_msamean ~ T3_treat + s_teneviv + s_utilities +
           s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 +
           s_years_back + s_sexo + s_estcivil + s_single + s_single + 
           s_edadhead + s_yrshead + s_tpersona + s_num18 + s_estrato +
           s_puntaje + s_ingtotal + grade + suba + s_over_age
         , data = data_suba)
vcov5 <- vcovCL(C5, cluster = data_suba$school_code)

summary(C5, vcov = vcov5)
coeftest(C5, vcov = vcov5)

robust.se5 <- sqrt(diag(vcov5))

C6 <- lm(at_msamean ~ T3_treat + school_code + s_teneviv + s_utilities +
           s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 +
           s_years_back + s_sexo + s_estcivil + s_single + s_single + 
           s_edadhead + s_yrshead + s_tpersona + s_num18 + s_estrato +
           s_puntaje + s_ingtotal + grade + suba + s_over_age
         , data = data_suba)
vcov6 <- vcovCL(C6, cluster = data_suba$school_code)

summary(C6, vcov = vcov6)
coeftest(C6, vcov = vcov6)

robust.se6 <- sqrt(diag(vcov6))


#Regression of T3 vs. T1;
data_adj <- data %>%
  filter(grade > 8)

C7 <- lm(at_msamean ~ T1_treat + T2_treat + T3_treat + school_code + suba + s_teneviv + s_utilities +
           s_durables + s_infraest_hh + s_age_sorteo + s_age_sorteo2 +
           s_years_back + s_sexo + s_estcivil + s_single + s_single + 
           s_edadhead + s_yrshead + s_tpersona + s_num18 + s_estrato +
           s_puntaje + s_ingtotal + grade + suba + s_over_age
         , data = data_adj)
vcov7 <- vcovCL(C7, cluster = data_adj$school_code)

summary(C7, vcov = vcov7)
coeftest(C7, vcov = vcov7)

robust.se7 <- sqrt(diag(vcov7))


#### Produce table

stargazer(C1, C2, C3, C4, C5, C6, C7, se= list(robust.se1,robust.se2,robust.se3,robust.se4,
                                               robust.se5,robust.se6,robust.se7),
          align=TRUE, dep.var.labels=c("(1)","(2)", "(3)","(4)","(5)", "(6)", "(7)"),
          keep = c("T1_treat", "T2_treat", "T3_treat"),
          covariate.labels=c("Basic Treatment","Savings Treatment", "Tertiary Treatment"), 
          omit.stat=c("LL","ser","f"), no.space=TRUE)






