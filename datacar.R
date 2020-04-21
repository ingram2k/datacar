library("insuranceData")

library("dplyr")
library("mltools")
library("data.table")
library("xgboost")

library("ggplot2")


data("dataCar")

set.seed(1)

dataCar$ID <- seq.int(nrow(dataCar))

dataCar.train.id <- data.frame(ID=dataCar$ID) %>% dplyr::sample_frac(0.70)

dataCar.test.id  <- dplyr::anti_join(data.frame(ID=dataCar$ID), dataCar.train.id, by = 'ID')


dataCar_1h <- one_hot(as.data.table(dataCar))


pre.train<- subset(dataCar_1h, dataCar_1h$ID %in% dataCar.train.id$ID)

pre.test<- subset(dataCar_1h, dataCar_1h$ID %in% dataCar.test.id$ID)

###################################### Fit Severity ######################################

severity <- dataCar %>%
  filter(claimcst0 > 0) %>%
  mutate(
    avg_loss = claimcst0 / numclaims)

avg_loss_cap <- quantile(severity$avg_loss, 0.95)

pre.train_severity<-pre.train %>% filter(claimcst0>0,claimcst0 / numclaims < avg_loss_cap)
pre.test_severity<-pre.test %>% filter(claimcst0>0,claimcst0 / numclaims < avg_loss_cap)


features_train_sev <- as.matrix(pre.train_severity[c("veh_value"
                                           , "veh_body_CONVT"
                                           , "veh_body_HBACK"
                                           , "veh_body_MCARA"
                                           ,"veh_body_PANVN","veh_body_RDSTR"                
                                           ,"veh_body_SEDAN","veh_body_STNWG"                
                                           ,"veh_body_TRUCK","veh_body_UTE"                  
                                           ,"veh_age","gender_F"                      
                                           ,"gender_M","area_A"                        
                                           ,"area_B","area_C"                        
                                           ,"area_D","area_E"                        
                                           ,"area_F","agecat"        
                                           )])
features_test_sev <- as.matrix(pre.test_severity[c("veh_value"
                                                     , "veh_body_CONVT"
                                                     , "veh_body_HBACK"
                                                     , "veh_body_MCARA"
                                                     ,"veh_body_PANVN","veh_body_RDSTR"                
                                                     ,"veh_body_SEDAN","veh_body_STNWG"                
                                                     ,"veh_body_TRUCK","veh_body_UTE"                  
                                                     ,"veh_age","gender_F"                      
                                                     ,"gender_M","area_A"                        
                                                     ,"area_B","area_C"                        
                                                     ,"area_D","area_E"                        
                                                     ,"area_F","agecat"        
)])

label_train_sev <- pre.train_severity$claimcst0/pre.train_severity$numclaims

dmat_train_sev <- xgb.DMatrix(features_train_sev, info = list(label = label_train_sev, weight = pre.train_severity$numclaims))

mean_sev<-mean(label_train_sev)


model_severity<-xgb.train(data=dmat_train_sev
                 ,objective = "reg:gamma"
                 ,booster ='gbtree'
                 ,base_score = mean_sev
                 ,eval_metric = "mae"
                 ,verbose = 1
                 ,eta = 0.002
                 ,nround= 5000
                 ,min_child_weight = 75
                 ,max_depth = 5
                 ,colsample_bytree = 0.6
                 ,subsample = 0.6
                 ,early_stopping_rounds = 20
                 ,watchlist = list(train=dmat_train_sev, test=dmat_train_sev))


###################################### Fit Frequency ######################################

pre.train_freq<-pre.train %>% filter( (numclaims == 0) | (claimcst0 / numclaims < avg_loss_cap))
pre.test_freq<-pre.test  %>% filter( (numclaims == 0) | (claimcst0 / numclaims < avg_loss_cap))

features_train_freq <- as.matrix(pre.train_freq %>% dplyr::select (veh_value
                                                     , veh_body_CONVT
                                                     , veh_body_HBACK
                                                     , veh_body_MCARA
                                                     ,veh_body_PANVN,veh_body_RDSTR                
                                                     ,veh_body_SEDAN,veh_body_STNWG                
                                                     ,veh_body_TRUCK,veh_body_UTE                  
                                                     ,veh_age,gender_F                      
                                                     ,gender_M,area_A                        
                                                     ,area_B,area_C                       
                                                     ,area_D,area_E                        
                                                     ,area_F,agecat        
))


features_test_freq <- as.matrix(pre.test_freq %>% dplyr::select (veh_value
                                                                   , veh_body_CONVT
                                                                   , veh_body_HBACK
                                                                   , veh_body_MCARA
                                                                   ,veh_body_PANVN,veh_body_RDSTR                
                                                                   ,veh_body_SEDAN,veh_body_STNWG                
                                                                   ,veh_body_TRUCK,veh_body_UTE                  
                                                                   ,veh_age,gender_F                      
                                                                   ,gender_M,area_A                        
                                                                   ,area_B,area_C                       
                                                                   ,area_D,area_E                        
                                                                   ,area_F,agecat        
))


label_train_freq <- pre.train_freq$numclaims

offset_freq<-log(pre.train_freq$exposure)

dmat_train_freq <- xgb.DMatrix(features_train_freq, info = 
                                 list(label = label_train_freq
                                      , base_margin = offset_freq ))


model_freq<-xgb.train(data=dmat_train_freq
                          ,objective = "count:poisson"
                          ,booster ='gbtree'
                          ,base_score = 1
                          ,eval_metric = "mae"
                          ,verbose = 1
                          ,eta = 0.002
                          ,nround= 10000
                          ,min_child_weight = 100
                          ,max_depth = 5
                          ,colsample_bytree = 0.6
                          ,subsample = 0.6
                          ,early_stopping_rounds = 20
                          ,watchlist = list(train=dmat_train_freq, test=dmat_train_freq))

############################### Predit ##############################

#Predict severity on the same test set as frequency since every policy should
#have the same expected severity , hence sev & freq for each row

sev_pred<-predict(model_severity,features_test_freq,type="response")

mean(sev_pred)
sum(pre.test_freq$claimcst0)/sum(pre.test_freq$numclaims)




freq_pred<-predict(model_freq,features_test_freq,type="response")

sum(freq_pred*pre.test_freq$exposure)/sum(pre.test_freq$exposure)
sum(pre.test_freq$numclaims)/sum(pre.test_freq$exposure)

mean(freq_pred*pre.test_freq$exposure)
mean(pre.test_freq$numclaims)

sum(freq_pred*pre.test_freq$exposure)
sum(pre.test_freq$numclaims)

pure_prem_pred<-freq_pred*sev_pred

actual_pure_prem<-pre.test_freq$claimcst0/pre.test_freq$exposure

mean(pure_prem_pred)
sum(pre.test_freq$claimcst0)/sum(pre.test_freq$exposure)

sum(pre.test_freq$claimcst0)/sum(pre.test_freq$exposure)
sum(pure_prem_pred*pre.test_freq$exposure)/sum(pre.test_freq$exposure)
