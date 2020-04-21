#document format
library(knitr) 
library(DT) 
#graphics
library(GGally) 
library(grid) 
library(ggpubr) 
library(ggExtra) 
#modeling
library(fitdistrplus) 
library(Hmisc)
library(caret) 
library(gbm) 
library(Amelia)
#general
library(tidyverse)
library(broom)
library(forcats)


car <- dataCar %>%
  mutate_at(c("clm", "agecat", "X_OBSTAT_", "veh_body"), as.factor) %>%
  #because ther are only a few roadsters, these are being grouped together as sports cars
  mutate(veh_body = as.character(veh_body),
         veh_body = as.factor(ifelse(veh_body %in% c("RDSTR", "CONVT"),
                                     yes = "SPRT", no = veh_body)))
car <- car %>%
  mutate(deductible = as.factor(ifelse(round(claimcst0, 0) %in% c(200, 354, 390, 345) 
                                       , "deductible","no_deductible")))

car <- car %>%
  mutate_if(is.factor, fct_infreq)


car$ID <- seq.int(nrow(car))


car.train<- subset(car, car$ID %in% dataCar.train.id$ID)

car.test<- subset(car, car$ID %in% dataCar.test.id$ID)


train_severity <- car.train %>% filter(claimcst0>0,claimcst0 / numclaims < avg_loss_cap)

train_severity <- train_severity %>% filter(deductible == "no_deductible")

train_severity <- train_severity %>% mutate(avg_capped_losss = claimcst0 / numclaims)




glm_severity <- glm(
  avg_capped_losss ~ log(veh_value + 0.001) + veh_body + veh_age + gender + area + agecat,
  family = Gamma(link = "log"),
  data = train_severity,
  weights = train_severity$numclaims)

summary(glm_severity)

glm_sev_pred<-predict(glm_severity,train_severity,type="response")

mean(glm_sev_pred)
sum(train_severity$claimcst0)/sum(train_severity$numclaims)


####################### Train GLM Frequency #######################



train_freq<-car.train %>% filter( (numclaims == 0) | (claimcst0 / numclaims < avg_loss_cap))

test_freq<-car.test %>% filter( (numclaims == 0) | (claimcst0 / numclaims < avg_loss_cap))

test_freq_features<-test_freq %>% dplyr::select (veh_value , veh_body , veh_age , gender , area , agecat)


glm_freq_1 <- glm(
  numclaims/exposure ~ log(veh_value + 0.001) + veh_body + veh_age + gender + area + agecat,
  family = poisson(link = "log"),
  data = train_freq,
  weights = train_freq$exposure)


glm_freq_pred<-predict(glm_freq_1,train_freq,type="response")

sum(glm_freq_pred*train_freq$exposure)

sum(train_freq$numclaims)

####################### Predict GLM #######################


glm_freq_test<-predict(glm_freq_1,test_freq_features,type="response")

sum(glm_freq_test*test_freq$exposure)

sum(test_freq$numclaims)


glm_sev_test<-predict(glm_severity,test_freq_features,type="response")


pure_prem_pred_glm<-glm_freq_test*glm_sev_test

single_lift(test_freq$claimcst0
            ,pure_prem_pred_glm*test_freq$exposure,20,test_freq$exposure)



