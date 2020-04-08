##################
# CCES Analysis ##
##################
setwd("C:/Users/wooki/Dropbox/tapsDisasters")
ccesdisaster <- read.csv("data/cces_panel/ccesdisaster_longformat.csv")
ccesdisaster <- read.csv("C:/Users/wooki/Desktop/cces_final.csv")# - done
ccesdisaster <- ccesdisaster[,-c(1,2)]
names(ccesdisaster)

# Note: Response variables: 
# 1) Obama's approval rating 
# 2) Governor's approval rating 
# 3) turnout in election (10-12-14)
# 4) Vote for Obama in 2012
# 5) Vote change ~ receiving disaster declaration



library(dplyr)
# Create lagged variables
ccesdisaster <- ccesdisaster %>%
  group_by(caseid) %>%
  mutate(lag.apprObama_year = lag(apprObama_year),
         lag.apprGov_year = lag(apprGov_year),
         lag.turnout_year = lag(turnout_year)
  )
ccesdisaster$chng.apprObama_year <- ccesdisaster$apprObama_year-ccesdisaster$lag.apprObama_year
ccesdisaster$chng.apprGov_year <- ccesdisaster$apprGov_year-ccesdisaster$lag.apprGov_year
ccesdisaster$chng.turnout_year <- ccesdisaster$turnout_year-ccesdisaster$lag.turnout_year


## change vars as binary vars
ccesdisaster$chng.apprObama.3point <- NA
ccesdisaster$chng.apprObama.3point[ccesdisaster$chng.apprObama_year >0 ] <- 1
ccesdisaster$chng.apprObama.3point[ccesdisaster$chng.apprObama_year <0 ] <- -1
ccesdisaster$chng.apprObama.3point[ccesdisaster$chng.apprObama_year ==0 ] <- 0
table(ccesdisaster$chng.apprObama.3point)



ccesdisaster$chng.apprGov.3point <- NA
ccesdisaster$chng.apprGov.3point[ccesdisaster$chng.apprGov_year >0 ] <- 1
ccesdisaster$chng.apprGov.3point[ccesdisaster$chng.apprGov_year <0 ] <- -1
ccesdisaster$chng.apprGov.3point[ccesdisaster$chng.apprGov_year ==0 ] <- 0
table(ccesdisaster$chng.apprGov.3point)


#variables summary
table(ccesdisaster$apprObama_year)


a1<-barplot(table(ccesdisaster$apprObama_year), xlab = "Approval Rate")
a2<-barplot(table(ccesdisaster$turnout_year), xlab = "Turnout")
a3<-barplot(table(ccesdisaster$dec_3month), xlab = "Number of Disaster Declaration in 3 Months")

nozero<-subset(ccesdisaster,ccesdisaster$damage_3month>0)
nozero$Groups <- cut(x=nozero$damage_3month, breaks=seq(from=0, to=ceiling(max(nozero$damage_3month)), by = 2))
Bygroup = tapply(nozero$damage_3month, nozero$Groups, sum)
a4<-barplot(height = Bygroup, xlab = "Logged $ Weather Damage in 3 Months", ylab = "")

par(mfrow=c(2,2))
a1

axis(1, at = seq(0, 25, 5), las = 1)
table(ccesdisaster$dec_6month)
table(ccesdisaster$dem)
table(ccesdisaster$rep)
table(ccesdisaster$female)
table(ccesdisaster$white)
table(ccesdisaster$educ)
table(ccesdisaster$age_2010)
table(ccesdisaster$income)
table(ccesdisaster$newsint)
table(ccesdisaster$ideo3)
table(ccesdisaster$turnout_year)

# create pid var
table(as.factor(ccesdisaster$ideo5_10))
ccesdisaster$ideo5 <- rep(NA, nrow(ccesdisaster))
ccesdisaster$ideo5[ccesdisaster$ideo5_10=="Very liberal"] <- 1
ccesdisaster$ideo5[ccesdisaster$ideo5_10=="Liberal"] <- 2
ccesdisaster$ideo5[ccesdisaster$ideo5_10=="Moderate"] <- 3
ccesdisaster$ideo5[ccesdisaster$ideo5_10=="Conservative"] <- 4
ccesdisaster$ideo5[ccesdisaster$ideo5_10=="Very Conservative"] <- 5

table(ccesdisaster$ind)


# panel analysis
library(plm)
library(stargazer)

plm.mod.obama1 <- plm(apprObama_year ~ dec_3month + damage_3month +dem + rep+ female + white + educ + age_2010 + income+ newsint + factor(state10)-1, 
                      index = c("caseid", "year"), data=ccesdisaster, model = 'within')


plm.mod.obama2 <- plm(apprObama_year ~ dec_3month*total + damage_3month +dem + rep+ female + white + educ + age_2010 + income+ newsint + factor(state10)-1, 
                       index = c("caseid", "year"), data=ccesdisaster, model = 'within')

plm.mod.obama3 <- plm(apprObama_year ~ dec_3month*newsint + damage_3month +dem + rep+ female + white + educ + age_2010 + income + newsint+ factor(state10)-1, 
                       index = c("caseid", "year"), data=ccesdisaster, model = 'within')

plm.mod.obama4 <- plm(apprObama_year ~ dec_3month+ damage_3month*educ  +dem + rep+female + white + educ + age_2010 + income + newsint+ factor(state10)-1, 
                        index = c("caseid", "year"), data=ccesdisaster, model = 'within')
plm.mod.obama5 <- plm(apprObama_year ~ dec_3month*educ+ damage_3month  +dem + rep+female + white + educ + age_2010 + income + newsint+ factor(state10)-1, 
                        index = c("caseid", "year"), data=ccesdisaster, model = 'within')


difference_models <- list(plm.mod.obama1, plm.mod.obama2,plm.mod.obama3,
                          plm.mod.obama4,plm.mod.obama5)

stargazer(difference_models,
          type="latex",
          style="apsr",
          title="Panel Analyses on the Effect of Issuing Disaster Declaration (Fixed Effects of Individuals)",
          keep.stat=c("n","rsq"),
          label="tab:differences")


#partisan
library(pglm)
plm.mod.obama21 <- plm(apprObama_year ~ dec_3month*dem + damage_3month + female + white + educ + age_2010 + income + newsint+ factor(state10)-1, 
                       index = c("caseid", "year"), data=ccesdisaster, model = 'within')

plm.mod.obama21 <- plm(apprObama_year ~ dec_1month*income +dem +damage_3month + female + white + educ + age_2010 + newsint+ factor(state10)-1, 
                       index = c("caseid", "year"), data=ccesdisaster, model = 'within')

summary(plm.mod.obama21)
ccesdisaster$dec_1month

plm.mod.obama21 <- plm(apprObama_year ~ dec_3month*dem + damage_3month + female + white + educ + age_2010 + income + newsint, 
                       index = c("caseid"), data=ccesdisaster, model = 'within')

library(sjPlot)
plot_model(plm.mod.obama21, type = "int")

library(sjmisc)
library(ggplot2)
data(efc)
theme_set(theme_sjplot())

# make categorical
efc$c161sex <- to_factor(efc$c161sex)

# fit model with interaction
fit <- lm(apprObama_year ~ dec_3month*dem + damage_3month + female + white + educ + age_2010 + income + newsint+ factor(caseid), data = ccesdisaster)
summary(fit)
plot_model(fit, type = "pred", terms = c("barthtot", "c161sex"),pred.type = "fe")


plm.mod.obama22 <- plm(apprObama_year ~ dec_3month*rep + damage_3month + female + white + educ + age_2010 + income + newsint+ factor(state10)-1, 
                       index = c("caseid", "year"), data=ccesdisaster, model = 'within')


plm.mod.obama23 <- plm(apprObama_year ~ dec_3month*ind+ damage_3month+ female + white + educ + age_2010 + income + newsint+ factor(state10)-1, 
                       index = c("caseid", "year"), data=ccesdisaster, model = 'within')

plm.mod.obama24 <- plm(apprObama_year ~ dec_3month + damage_3month*rep  + female + white + educ + age_2010 + income + newsint+ factor(state10)-1, 
                       index = c("caseid", "year"), data=ccesdisaster, model = 'within')

plm.mod.obama25 <- plm(apprObama_year ~ dec_3month + damage_3month*dem + female + white + educ + age_2010 + income + newsint+ factor(state10)-1, 
                       index = c("caseid", "year"), data=ccesdisaster, model = 'within')


difference_models <- list(plm.mod.obama21, plm.mod.obama22,plm.mod.obama23,
                          plm.mod.obama24,plm.mod.obama25)

stargazer(difference_models,
          type="latex",
          style="apsr",
          title="Panel Analyses on the Effect of Issuing Disaster Declaration (Fixed Effects of Individuals)",
          keep.stat=c("n","rsq"),
          label="tab:differences")


plm.mod.turnout1 <- plm(turnout_year ~ dec_3month + damage_3month +dem + rep+ 
                          female + white + educ + age_2010 + income + newsint+ factor(state10)-1, 
                        index = c("caseid", "year"), data=ccesdisaster, model = 'within')

plm.mod.turnout2 <- plm(turnout_year ~ dec_3month*newsint + damage_3month +dem + rep+ 
                          female + white + educ + age_2010 + income + factor(state10)-1, 
                        index = c("caseid", "year"), data=ccesdisaster, model = 'within')

plm.mod.turnout3 <- plm(turnout_year ~ dec_3month*total + damage_3month +dem + rep+ 
                           female + white + educ + age_2010 + income + factor(state10)-1, 
                         index = c("caseid", "year"), data=ccesdisaster, model = 'within')

difference_models <- list(plm.mod.turnout1, plm.mod.turnout2,plm.mod.turnout3
                         )

stargazer(difference_models,
          type="latex",
          style="apsr",
          title="Panel Analyses on the Effect of Issuing Disaster Declaration (Fixed Effects of Individuals)",
          keep.stat=c("n","rsq"),
          label="tab:differences")