### 
#' ---
#' title: "Main Model Code PAM"
#' author: "SM"
#' date: "Oct 10, 2019"
#' ---

##### Loading common library
require(desctable)

library(tidyverse)
library(ggthemes)
library(corrplot)
library(GGally)
library(DT)
library(caret)
library(pastecs)

if (!require(DBI)) install.packages('DBI')
if (!require(RSQLite)) install.packages('RSQLite')
if (!require(dplyr)) install.packages('dplyr')
if (!require(dbplyr)) install.packages('dbplyr')

library(corrgram)
library(janitor)
library(lubridate)
library(corrplot)
library(maps)
library(ggrepel)
library(sqldf)

########################################
getwd()
setwd('C:\\\\Users\\saurabh.mangal\\OneDrive - Accenture\\R-code-for-RDataMining-book\\Aws')

# Set the blank spaces to NA's
failure = read_csv("device_failure.csv" , col_names = T , na = "")

dim(failure)
colnames(failure)

failure$date = as.Date(failure$date, "%m/%d/%Y")  ## View(failure[failure$device== "W1F11ZG9",])

## Check missing data
sapply(failure , function(x) sum(is.na(x)))
summary(failure)
stat.desc(failure)   ## library(pastecs)

## row
#sum(rowSums(is.na(colSums(is.na(failure)) )) > 0)  # Number of missing per row

# Number of missing per column/variable
colSums(is.na(failure)) 

# Convert to missing data
# failure[failure$colname____name=="& ","age"] <- NA # NOTE: Notice hidden spaces.
# failure[failure$colname____age==999,"age"] <- NA

failure[!complete.cases(failure),]

##########################################################################
##  Descriptive Statistics
##########################################################################

head(failure,1)

#### Totol #  of devices
length(unique(failure$device))

#### Total # of failure 
length(unique(failure$failure))

#### Balance of falure
hist(failure$failure)

library(plyr)
ddply(failure, .(failure), nrow)   ## gives total number of failures


#### Total # of unique failure 
unique(failure[failure$failure == 1,]$device)
length(unique(failure[failure$failure == 1,]$device))

### Frequency table of how many time it has failed
failure01 <- failure[failure$failure == 1, ]

failure01 %>% frequency(failure$device, failure, nmax = 15)

library(gmodels)
CrossTable(failure01$device)
######################################################
### GENERAL PLOTS
######################################################

#hist(failure$attribute1, col="green")
#hist(failure$attribute2, breaks="FD", col="green")
#hist(failure$attribute3, col="green")

#### Understanding the Data

ggplot(failure, aes(x = failure, y = attribute1, group = failure)) +   geom_boxplot()
ggplot(failure, aes(x = failure, y = attribute2, group = failure)) +   geom_boxplot()
ggplot(failure, aes(x = failure, y = attribute3, group = failure)) +   geom_boxplot()
ggplot(failure, aes(x = failure, y = attribute4, group = failure)) +   geom_boxplot()
ggplot(failure, aes(x = failure, y = attribute5, group = failure)) +   geom_boxplot()
ggplot(failure, aes(x = failure, y = attribute6, group = failure)) +   geom_boxplot()
ggplot(failure, aes(x = failure, y = attribute7, group = failure)) +   geom_boxplot()
ggplot(failure, aes(x = failure, y = attribute8, group = failure)) +   geom_boxplot()
ggplot(failure, aes(x = failure, y = attribute9, group = failure)) +   geom_boxplot()

##### removing outlier

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  return(y)
}

y <- remove_outliers(failure$attribute4)
summary(y)
boxplot(y)

##############################
### Also creating two data sets of Failed and not failed sensors to understand their behavior
##############################

length(unique(failure$device))

failed_devices_ids= failure[failure$failure==1, ]$device

failed_devices= failure[(failure$device %in% failed_devices_ids),]

nrow(failed_devices)
length(unique(failed_devices_ids))

`%notin%` <- Negate(`%in%`)
not_failed_devices= failure[(failure$device %notin% failed_devices_ids),]
nrow(not_failed_devices)
length(unique(not_failed_devices$device))
not_failed_devices_ids = unique(not_failed_devices$device)

### Verify all matches
length(unique(failure$device))  == length(failed_devices_ids) + length(not_failed_devices_ids)

nrow(failure) == nrow(not_failed_devices) + nrow(failed_devices)

##############################
### Days Before failure. 
##############################


failed_devices_01 = sqldf("SELECT device,
                          COUNT(device) as days_before_failure
                          FROM failed_devices group by device")

a1 <- failed_devices %>% group_by(device)


mean(failed_devices_01$days_before_failure)
boxplot(failed_devices_01$days_before_failure)

##############################
#### Looking at prevvious days for failure, 

nrow(failed_devices)
failed_devices_02 = failed_devices[failed_devices$failure!=1, ]
failed_devices_02x = failed_devices[failed_devices$failure==1, ]
nrow(failed_devices_02)

nrow(failed_devices_02)
ncol(failed_devices_02)

failed_devices_03 =  sqldf("SELECT failed_devices_02.* , 
                            failed_devices_02x.date as date_of_failure 
                            FROM failed_devices_02 
                            left outer join failed_devices_02x
                            using(device)
                           ")

nrow(failed_devices_03)
ncol(failed_devices_03)

failed_devices_03$date_of_failure = as.Date(failed_devices_03$date_of_failure,origin =  origin)
failed_devices_03$days_before_failure = as.numeric(failed_devices_03$date_of_failure -  failed_devices_03$date)

nrow(failed_devices_03)
ncol(failed_devices_03)

failed_devices_03$failure = 1

### make sure it sorted correctly
failed_devices_03 = failed_devices_03[  with(failed_devices_03,
                                             order(device, -days_before_failure)),  ]

failed_devices_03 = failed_devices_03[  with(failed_devices_03, 
                                             order(device, -days_before_failure)),  ]

nrow(failed_devices_03)
ncol(failed_devices_03)

############################################################ 
############################################################ 
### FEATURE ENGINEERING 
############################################################
############################################################ 


############################################################ 
### Create all day Aggregate variables 
############################################################

failed_devices_03_all = 
  sqldf("select 
    		  a.device,  
    		  a.failure,
    		  a.date_of_failure as date_of_failure ,
    		  max(a.days_before_failure) as days_before_failure_max, 
          count(device) as count_records, 
          sum(attribute1) as attribute1_all_sum , 
          sum(attribute2) as attribute2_all_sum , 
          sum(attribute3) as attribute3_all_sum , 
          sum(attribute4) as attribute4_all_sum , 
          sum(attribute5) as attribute5_all_sum , 
          sum(attribute6) as attribute6_all_sum , 
          sum(attribute7) as attribute7_all_sum , 
          sum(attribute8) as attribute8_all_sum , 
          sum(attribute9) as attribute9_all_sum ,
            
          avg(attribute1) as attribute1_all_avg , 
          avg(attribute2) as attribute2_all_avg , 
          avg(attribute3) as attribute3_all_avg , 
          avg(attribute4) as attribute4_all_avg , 
          avg(attribute5) as attribute5_all_avg , 
          avg(attribute6) as attribute6_all_avg , 
          avg(attribute7) as attribute7_all_avg , 
          avg(attribute8) as attribute8_all_avg , 
          avg(attribute9) as attribute9_all_avg 
          from failed_devices_03 a group by  a.device 
        ")

nrow(failed_devices_03_all)

############################################################ 
### Create 7 day Aggregate variables 
############################################################

failed_devices_03_7days = 
  sqldf("select 
    		  a.device,  
    		  a.failure,
    		  a.date_of_failure as date_of_failure ,
    		  a.days_before_failure, 
          count(device) as count_records, 
          sum(attribute1) as attribute1_sev_sum , 
          sum(attribute2) as attribute2_sev_sum , 
          sum(attribute3) as attribute3_sev_sum , 
          sum(attribute4) as attribute4_sev_sum , 
          sum(attribute5) as attribute5_sev_sum , 
          sum(attribute6) as attribute6_sev_sum , 
          sum(attribute7) as attribute7_sev_sum , 
          sum(attribute8) as attribute8_sev_sum , 
          sum(attribute9) as attribute9_sev_sum ,
          
          avg(attribute1) as attribute1_sev_avg , 
          avg(attribute2) as attribute2_sev_avg , 
          avg(attribute3) as attribute3_sev_avg , 
          avg(attribute4) as attribute4_sev_avg , 
          avg(attribute5) as attribute5_sev_avg , 
          avg(attribute6) as attribute6_sev_avg , 
          avg(attribute7) as attribute7_sev_avg , 
          avg(attribute8) as attribute8_sev_avg , 
          avg(attribute9) as attribute9_sev_avg 
          
          from failed_devices_03 a 
          where a.days_before_failure < 8
          group by  a.device 
          
        ")

nrow(failed_devices_03_7days)
ncol(failed_devices_03_7days)

############################################################ 
### Create lag variables 
############################################################
library(data.table)
library(dplyr)

lag_variable <- failed_devices_03 %>%
  arrange(device, -days_before_failure) %>% 
  group_by(device) %>%
  mutate(attribute1_lag1_value = dplyr::lag(attribute1, n = 1, default = NA))%>%
  mutate(attribute1_lag2_value = dplyr::lag(attribute1, n = 2, default = NA))%>%
  mutate(attribute1_lag3_value = dplyr::lag(attribute1, n = 3, default = NA))%>%
  
  mutate(attribute2_lag1_value = dplyr::lag(attribute2, n = 1, default = NA))%>%
  mutate(attribute2_lag2_value = dplyr::lag(attribute2, n = 2, default = NA))%>%
  mutate(attribute2_lag3_value = dplyr::lag(attribute2, n = 3, default = NA))%>%
  
  mutate(attribute3_lag1_value = dplyr::lag(attribute3, n = 1, default = NA))%>%
  mutate(attribute3_lag2_value = dplyr::lag(attribute3, n = 2, default = NA))%>%
  mutate(attribute3_lag3_value = dplyr::lag(attribute3, n = 3, default = NA))%>%
  
  mutate(attribute4_lag1_value = dplyr::lag(attribute4, n = 1, default = NA))%>%
  mutate(attribute4_lag2_value = dplyr::lag(attribute4, n = 2, default = NA))%>%
  mutate(attribute4_lag3_value = dplyr::lag(attribute4, n = 3, default = NA))%>%
  
  mutate(attribute5_lag1_value = dplyr::lag(attribute5, n = 1, default = NA))%>%
  mutate(attribute5_lag2_value = dplyr::lag(attribute5, n = 2, default = NA))%>%
  mutate(attribute5_lag3_value = dplyr::lag(attribute5, n = 3, default = NA))%>%
  
  mutate(attribute6_lag1_value = dplyr::lag(attribute6, n = 1, default = NA))%>%
  mutate(attribute6_lag2_value = dplyr::lag(attribute6, n = 2, default = NA))%>%
  mutate(attribute6_lag3_value = dplyr::lag(attribute6, n = 3, default = NA))%>%
  
  mutate(attribute7_lag1_value = dplyr::lag(attribute7, n = 1, default = NA))%>%
  mutate(attribute7_lag2_value = dplyr::lag(attribute7, n = 2, default = NA))%>%
  mutate(attribute7_lag3_value = dplyr::lag(attribute7, n = 3, default = NA))

nrow(lag_variable)
ncol(lag_variable)

############################################################ 
### Rolling aggregates variables 
############################################################

rolling_variables <- failed_devices_03 %>%   
  arrange(device, -days_before_failure) %>% 
  group_by(device) %>%
  mutate(attribute1_rol1_value = 
           dplyr::lag(attribute1, n = 4, default = NA) + 
           dplyr::lag(attribute1, n = 5, default = NA) + 
           dplyr::lag(attribute1, n = 6, default = NA)) %>%
  mutate(attribute1_rol2_value = 
           dplyr::lag(attribute1, n = 5, default = NA) + 
           dplyr::lag(attribute1, n = 6, default = NA) + 
           dplyr::lag(attribute1, n = 7, default = NA)) %>%
  
  mutate(attribute2_rol1_value = 
           dplyr::lag(attribute2, n = 4, default = NA) + 
           dplyr::lag(attribute2, n = 5, default = NA) + 
           dplyr::lag(attribute2, n = 6, default = NA)) %>%
  mutate(attribute3_rol1_value = 
           dplyr::lag(attribute3, n = 4, default = NA) + 
           dplyr::lag(attribute3, n = 5, default = NA) + 
           dplyr::lag(attribute3, n = 6, default = NA)) %>%
  mutate(attribute4_rol1_value = 
           dplyr::lag(attribute4, n = 4, default = NA) + 
           dplyr::lag(attribute4, n = 5, default = NA) + 
           dplyr::lag(attribute4, n = 6, default = NA)) %>%
  mutate(attribute5_rol1_value = 
           dplyr::lag(attribute5, n = 4, default = NA) + 
           dplyr::lag(attribute5, n = 5, default = NA) + 
           dplyr::lag(attribute5, n = 6, default = NA)) %>%
  mutate(attribute6_rol1_value = 
           dplyr::lag(attribute6, n = 4, default = NA) + 
           dplyr::lag(attribute6, n = 5, default = NA) + 
           dplyr::lag(attribute6, n = 6, default = NA)) %>%
  mutate(attribute6_rol2_value = 
           dplyr::lag(attribute6, n = 5, default = NA) + 
           dplyr::lag(attribute6, n = 6, default = NA) + 
           dplyr::lag(attribute6, n = 7, default = NA)) %>%
  
  mutate(attribute7_rol1_value = 
           dplyr::lag(attribute7, n = 4, default = NA) + 
           dplyr::lag(attribute7, n = 5, default = NA) + 
           dplyr::lag(attribute7, n = 6, default = NA))

nrow(rolling_variables)
ncol(rolling_variables)
############################################################ 
### Tumbline aggregates variables 
############################################################

tumbling_variables <- failed_devices_03 %>%   
  arrange(device, -days_before_failure) %>% 
  group_by(device) %>%
  mutate(attribute1_tum1_value = 
           dplyr::lag(attribute1, n = 7, default = NA) + 
           dplyr::lag(attribute1, n = 8, default = NA) + 
           dplyr::lag(attribute1, n = 9, default = NA)) %>%
  mutate(attribute1_tum2_value = 
           dplyr::lag(attribute1, n = 10, default = NA) + 
           dplyr::lag(attribute1, n = 11, default = NA) + 
           dplyr::lag(attribute1, n = 12, default = NA)) 


nrow(tumbling_variables)
ncol(tumbling_variables)
############################################################ 
### CREATE MODELLING RECORD 
############################################################

model_01_dataset1 =  sqldf("SELECT a.device,  a.Date, a.failure,
                      b.attribute1_all_sum , b.attribute2_all_sum , b.attribute3_all_sum , b.attribute4_all_sum ,
                      b.attribute5_all_sum , b.attribute6_all_sum , b.attribute7_all_sum , b.attribute8_all_sum ,
                      b.attribute9_all_sum ,
                      b.attribute1_all_avg , b.attribute2_all_avg , b.attribute3_all_avg , b.attribute4_all_avg ,
                      b.attribute5_all_avg , b.attribute6_all_avg , b.attribute7_all_avg , b.attribute8_all_avg ,
                      b.attribute9_all_avg 
                      FROM failed_devices_02 a
                      left outer join failed_devices_03_all b
                      using(device) 
                           ")
nrow(model_01_dataset1)
model_01_dataset1$failure = 1

model_01_dataset2 =  sqldf("SELECT a.*,
                      attribute1_sev_sum , attribute2_sev_sum , attribute3_sev_sum , attribute4_sev_sum ,		  
                      attribute5_sev_sum , attribute6_sev_sum , attribute7_sev_sum ,attribute8_sev_sum ,
                      attribute9_sev_sum ,
                      
                      attribute1_sev_avg ,attribute2_sev_avg ,attribute3_sev_avg ,attribute4_sev_avg ,
                      attribute5_sev_avg ,attribute6_sev_avg ,attribute7_sev_avg ,attribute8_sev_avg ,
                      attribute9_sev_avg                      
                      FROM model_01_dataset1 a
                      left outer join failed_devices_03_7days b
                      using(device) 
                           ") 

nrow(model_01_dataset2)
ncol(model_01_dataset2)

model_01_dataset3 =  sqldf("SELECT a.*,
                      attribute1_lag1_value,attribute1_lag2_value,attribute1_lag3_value,
                      attribute2_lag1_value,attribute2_lag2_value,attribute2_lag3_value,
                      attribute3_lag1_value,attribute3_lag2_value,attribute3_lag3_value,
                      attribute4_lag1_value,attribute4_lag2_value,attribute4_lag3_value,
                      attribute5_lag1_value,attribute5_lag2_value,attribute5_lag3_value,
                      attribute6_lag1_value,attribute6_lag2_value,attribute6_lag3_value,
                      attribute7_lag1_value,attribute7_lag2_value,attribute7_lag3_value
                      FROM model_01_dataset2 a
                      left outer join lag_variable b
                      using(device,Date) 
                           ") 

nrow(model_01_dataset3)
ncol(model_01_dataset3)

model_01_dataset4 =  sqldf("SELECT a.*,
                      attribute1_rol1_value, attribute1_rol2_value,
                      attribute2_rol1_value, attribute3_rol1_value,
                      attribute4_rol1_value, attribute5_rol1_value,
                      attribute6_rol1_value, attribute6_rol2_value,
                      attribute7_rol1_value
                      FROM model_01_dataset3 a
                      left outer join rolling_variables b
                      using(device,Date) 
                           ") 

nrow(model_01_dataset4)
ncol(model_01_dataset4)

model_01_dataset5 =  sqldf("SELECT a.*,
                      attribute1_tum1_value, 
                      attribute1_tum2_value
                      FROM model_01_dataset4 a
                      left outer join tumbling_variables b
                      using(device,Date) 
                           ") 

nrow(model_01_dataset5)
ncol(model_01_dataset5)

failed_FE_data1 = model_01_dataset5

############################################################ 
###  Merge And create test and train data sets
############################################################ 


### Checkpoint04 -- 
save.image(file =  "Saurabh_20191010V1.RData")

#### Selecting devices for training/ testing the data -- based on 70% & 30% logic    --- Device that Failed
failed_FE_data1_device = as.data.frame(unique(failed_FE_data1$device))
names(failed_FE_data1_device) = c("device")

failed_ids_ALL_test = failed_FE_data1_device %>% sample_frac( 0.3 )
failed_ids_ALL_train =  failed_FE_data1_device[(failed_FE_data1_device$device %notin% failed_ids_ALL_test$device),]
failed_ids_ALL_train = ldply (failed_ids_ALL_train, data.frame)
names(failed_ids_ALL_train) = c("device")


#### Selecting devices for training/ testing the data -- based on 70% & 30% logic --- Device that did Not Failed
not_failed_FE_data1_device = as.data.frame(unique(not_failed_FE_data1$device))
names(not_failed_FE_data1_device) = c("device")

not_failed_ids_ALL_test = not_failed_FE_data1_device %>% sample_frac( 0.3 )
not_failed_ids_ALL_train =  not_failed_FE_data1_device[(not_failed_FE_data1_device$device %notin% not_failed_ids_ALL_test$device),]
not_failed_ids_ALL_train = ldply (not_failed_ids_ALL_train, data.frame)
names(not_failed_ids_ALL_train) = c("device")

###################################################################################################################

nrow(failed_FE_data1)
nrow(failed_ids_ALL_train)

model_02_dataset = rbind(failed_ids_ALL_train, not_failed_ids_ALL_train %>% sample_frac( 0.3 ))  

nrow(model_02_dataset) == nrow(failed_ids_ALL_train) + nrow(not_failed_ids_ALL_train %>% sample_frac( 0.3 ))  

#names(model_02_dataset)
#not_failed_FE_data1
#failed_FE_data1

model_03_dataset = sqldf('select a.*  
                         FROM not_failed_FE_data1 a
                         left outer join model_02_dataset b
                         using(device)
                         where a.device = b.device
                         ')

model_04_dataset = sqldf('select a.*  
                         FROM failed_FE_data1 a
                         left outer join model_02_dataset b
                         using(device)
                         where a.device = b.device
                         ')

model_05_dataset = rbind(model_03_dataset, model_04_dataset)
nrow(model_05_dataset)
nrow(failure)

model_final_dataset = model_05_dataset 
#names(model_final_dataset)

############################################################ 
###  Next Day Predcition Model  ::: Binary Class Classification Model
############################################################ 
require(xgboost)
require(Matrix)

feature.names <- names(model_final_dataset)
feature.names <- feature.names[-grep('^failure$', feature.names)]   ### Revmoing Predictor Variable
feature.names <- feature.names[-grep('^date$', feature.names)]   ### Revmoing Date Variable
feature.names <- feature.names[-grep('^device$', feature.names)]   ### Revmoing device Variable

main=paste(feature.names, collapse = '+')
feature.formula <- formula(paste('failure~', paste0(main),sep =''))

##GBM: Xgboost Modelling##
set.seed(2001000324)

##select 80% dataset as training data
indexes<-sample(seq_len(nrow(model_final_dataset)),floor(nrow(model_final_dataset)*0.8))

#creat xgb.matrix for xgb.train   
## training data (80%)
options(na.action='na.pass')

train_spm = as.matrix(model_final_dataset[indexes, feature.names])
head(train_spm,1); dim(train_spm)

dtrain <- xgb.DMatrix(data= train_spm,
                      label = model_final_dataset[indexes, 'failure'])

## validation data (20%)  
valid_spm = as.matrix(model_final_dataset[-indexes, feature.names])
head(valid_spm,1);dim(valid_spm)

dvalid <- xgb.DMatrix(data = valid_spm,
                      label = model_final_dataset[-indexes, 'failure'])


##full dataset for prediction
data_F <- sparse.model.matrix(feature.formula, data =model_final_dataset[,-1])
dim(data_F)

## start to run model
watchlist <- list(valid = dvalid, train = dtrain)

params <- list(booster = "gbtree", objective = "binary:logistic",tree_method="auto",
               max_depth =10, eta = 0.0015,gamma=2,
               max_delta_step=20,colsample_bytree = 0.8,subsample = 0.8,alpha = 0.0001, 
               lambda=1.5)

modelgbm<- xgb.train(params = params, data = dtrain,
                     nrounds = 1000, early.stop.round = 800,
                     eval_metric = 'auc', maximize = T,
                     watchlist = watchlist, print.every.n = 20)

xgb.save(modelgbm, "xgboost.model")     #added by SM to save trained model to local file for later prediction


pred.valid<-predict(modelgbm, dvalid)
pred.valid2<-ifelse(pred.valid>=0.5,1,0)

pred.train<-predict(modelgbm, dtrain)

##create confusion matrix to know how good of prediction of churn with actual outcome in July
confusionMatrix(as.factor(pred.valid2),as.factor(model_final_dataset[-indexes,]$failure))


############################################################ 
### Variable Imprtance
############################################################ 


modelgbm2 <- xgb.dump(modelgbm, with.stats = T)

importance_matrix <- xgb.importance(feature.names, model = modelgbm)

xgb.plot.importance(importance_matrix[1:10,])

test <- chisq.test(failure$attribute6, failure$failure)
print(test)
test <- chisq.test(failure$attribute1, failure$failure)
print(test)
test <- chisq.test(failure$attribute4, failure$failure)
print(test)

############################################################ 
###  7 Day Predcition Model  ::: Binary Class Classification Model
############################################################ 

## Not planned ##
