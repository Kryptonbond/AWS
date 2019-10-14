### 
#' ---
#' title: "Test_Data_Prep"
#' author: "SM"
#' date: "Oct 10, 2019"
#' ---

### Taking full test data

model_02_dataset = rbind(failed_ids_ALL_test, not_failed_ids_ALL_test %>% sample_frac( 1.0 ))  

#### Verifying all data rows
nrow(model_02_dataset) == nrow(failed_ids_ALL_test) + nrow(not_failed_ids_ALL_test %>% sample_frac( 1.0 ))  

###### names(model_02_dataset)
###### not_failed_FE_data1
###### failed_FE_data1


### Selected rows only
model_03_dataset = sqldf('select a.*  
                         FROM not_failed_FE_data1 a
                         left outer join model_02_dataset b
                         using(device)
                         where a.device = b.device
                         ')

#### checking if rows are reduced
nrow(not_failed_FE_data1)
nrow(model_03_dataset)

model_04_dataset = sqldf('select a.*  
                         FROM failed_FE_data1 a
                         left outer join model_02_dataset b
                         using(device)
                         where a.device = b.device
                         ')

#### checking if rows are reduced
nrow(not_failed_FE_data1)
nrow(model_04_dataset)

model_05_dataset = rbind(model_03_dataset, model_04_dataset)
nrow(model_05_dataset)
nrow(failure)

model_final_dataset_test = model_05_dataset 
names(model_final_dataset_test)
############################################################ 
###  Next Day Predcition Model  ::: Binary Class Classification Model
############################################################ 
require(xgboost)
require(Matrix)

feature.names <- names(model_final_dataset_test)
feature.names <- feature.names[-grep('^failure$', feature.names)]   ### Revmoing Predictor Variable
feature.names <- feature.names[-grep('^date$', feature.names)]   ### Revmoing Date Variable
feature.names <- feature.names[-grep('^device$', feature.names)]   ### Revmoing device Variable

main=paste(feature.names, collapse = '+')
feature.formula <- formula(paste('failure~', paste0(main),sep =''))

### GBM: Xgboost Modelling##
set.seed(2001000324)

#### select 80% dataset as training data  but 100% data from test set
indexes<-sample(seq_len(nrow(model_final_dataset_test)),floor(nrow(model_final_dataset_test)*1.0))

#### create xgb.matrix for xgb.train   
#### training data (80%)
options(na.action='na.pass')

test_spm = as.matrix(model_final_dataset_test[indexes, feature.names])
#head(test_spm); dim(test_spm)

dtest <- xgb.DMatrix(data= test_spm,
                      label = model_final_dataset_test[indexes, 'failure'])



###Test Results 

pred.test<-predict(modelgbm, dtest)
pred.test2<-ifelse(pred.test>=0.5,1,0)

##create confusion matrix to know how good of prediction of churn with actual outcome in July
confusionMatrix(as.factor(pred.test2),as.factor(model_final_dataset_test$failure))


model_final_dataset_test_pred = cbind(model_final_dataset_test, as.data.frame(pred.test2) )

### No of unique devices predicted that could fail
length(unique(model_final_dataset_test_pred[ model_final_dataset_test_pred$pred.test2 ==1, c("device")]))

### Total No of unique devices in test set
length(unique(model_final_dataset_test_pred[ , c("device")]))




