### 
#' ---
#' title: "Feature Engineering on Not Failed Data"
#' author: "SM"
#' date: "Oct 10, 2019"
#' ---


############################################################ 
############################################################ 
### FEATURE ENGINEERING 
############################################################
############################################################ 

##View(not_failed_devices)
nrow(not_failed_devices)
ncol(not_failed_devices)

general_devices_03 = not_failed_devices

general_devices_03$date_of_failure = max(not_failed_devices$date)
general_devices_03$days_before_failure = as.numeric(general_devices_03$date_of_failure -  general_devices_03$date)

############################################################ 
### Create all day Aggregate variables 
############################################################

general_devices_03_all = 
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
          from general_devices_03 a group by  a.device 
        ")

nrow(general_devices_03_all)
ncol(general_devices_03_all)

############################################################ 
### Create 7 day Aggregate variables 
############################################################

general_devices_03_7days = 
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
          
          from general_devices_03 a 
          where a.days_before_failure < 8
          group by  a.device 
          
        ")

nrow(general_devices_03_7days)
ncol(general_devices_03_7days)

############################################################ 
### Create lag variables 
############################################################
library(data.table)
library(dplyr)

lag_variable <- general_devices_03 %>%
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

rolling_variables <- general_devices_03 %>%   
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

tumbling_variables <- general_devices_03 %>%   
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
                      FROM general_devices_03 a
                      left outer join general_devices_03_all b
                      using(device) 
                           ")
nrow(model_01_dataset1);nrow(general_devices_03)

model_01_dataset2 =  sqldf("SELECT a.*,
                      attribute1_sev_sum , attribute2_sev_sum , attribute3_sev_sum , attribute4_sev_sum ,		  
                      attribute5_sev_sum , attribute6_sev_sum , attribute7_sev_sum ,attribute8_sev_sum ,
                      attribute9_sev_sum ,
                      
                      attribute1_sev_avg ,attribute2_sev_avg ,attribute3_sev_avg ,attribute4_sev_avg ,
                      attribute5_sev_avg ,attribute6_sev_avg ,attribute7_sev_avg ,attribute8_sev_avg ,
                      attribute9_sev_avg                      
                      FROM model_01_dataset1 a
                      left outer join general_devices_03_7days b
                      using(device) 
                           ") 

nrow(model_01_dataset2);nrow(general_devices_03)
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

nrow(model_01_dataset3);nrow(general_devices_03)
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

nrow(model_01_dataset4);nrow(general_devices_03)
ncol(model_01_dataset4)

model_01_dataset5 =  sqldf("SELECT a.*,
                      attribute1_tum1_value, 
                      attribute1_tum2_value
                      FROM model_01_dataset4 a
                      left outer join tumbling_variables b
                      using(device,Date) 
                           ") 

nrow(model_01_dataset5);nrow(general_devices_03)
ncol(model_01_dataset5)
names(model_01_dataset5)

not_failed_FE_data1 = model_01_dataset5

### Checkpoint04 -- 
save.image(file =  "Saurabh_20191010V2.RData")



############### 
## Compare Colname are same across -- to double confirm 
###############

names(failed_FE_data1) == names(not_failed_FE_data1) 

############################################################ 




