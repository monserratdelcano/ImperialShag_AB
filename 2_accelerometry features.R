#2 Calculate features using a 1-second moving window

library(caTools)
library(zoo)
library(PerformanceAnalytics)
library(stats)

#Load data
load(file="C:/Users/monse/Punta Leon 2019_2021 y 2022/Analisis se�ales acc pichones/Random Forest analisis/Output/acc_observed_events_withFrequencies_07052025.rData")

ls()
head(data2)
table(data2$Event)
unique(data2$Event)
table(data2$Behaviour)
tail(data2)

#Add columns

data3=data.frame(data2,Mean_x=NA,Mean_y=NA,Mean_z=NA,Mean_stat_x=NA,Mean_stat_y=NA,Mean_stat_z=NA,Mean_dyn_x=NA, Mean_dyn_y=NA,Mean_dyn_z=NA,Mean_VEDBA=NA,Mean_Pitch=NA,Mean_Roll=NA,Mean_Freq_heave_H=NA,Mean_Freq_sway_H=NA,Mean_Freq_surge_H=NA,Min_x=NA,Min_y=NA,Min_z=NA,Min_stat_x=NA,Min_stat_y=NA,Min_stat_z=NA,Min_dyn_x=NA, Min_dyn_y=NA,Min_dyn_z=NA,Min_VEDBA=NA,Min_Pitch=NA,Min_Roll=NA,Min_Freq_heave_H=NA,Min_Freq_sway_H=NA,Min_Freq_surge_H=NA,Max_x=NA,Max_y=NA,Max_z=NA,Max_stat_x=NA,Max_stat_y=NA,Max_stat_z=NA,Max_dyn_x=NA,Max_dyn_y=NA,Max_dyn_z=NA,Max_VEDBA=NA,Max_Pitch=NA,Max_Roll=NA,Max_Freq_heave_H=NA,Max_Freq_sway_H=NA,Max_Freq_surge_H=NA,SD_x=NA,SD_y=NA,SD_z=NA,SD_stat_x=NA,SD_stat_y=NA,SD_stat_z=NA,SD_dyn_x=NA, SD_dyn_y=NA,SD_dyn_z=NA,SD_VEDBA=NA,SD_Pitch=NA,SD_Roll=NA,SD_Freq_heave_H=NA,SD_Freq_sway_H=NA,SD_Freq_surge_H=NA)


mean_col= c(which(colnames(data3)=="Mean_x"):which(colnames(data3)=="Mean_Freq_surge_H"))
min_col=c(which(colnames(data3)=="Min_x"):which(colnames(data3)=="Min_Freq_surge_H"))
max_col=c(which(colnames(data3)=="Max_x"):which(colnames(data3)=="Max_Freq_surge_H"))
sd_col=c(which(colnames(data3)=="SD_x"):which(colnames(data3)=="SD_Freq_surge_H"))

length(mean_col); length(min_col); length(max_col); length(sd_col)

data_col=c(2:4,7:12,17:22) 

names(data3[data_col])
length(data_col)

ID=unique(data3$ID)

names(data3)
length(ID)


for(i in 1:length(ID)) {

  idx1<- which(data3$ID==ID[i]) 
  
  
  for(d in c(1: length(data_col)))      {
    col_data <- data_col[d]
    col_mean_data <- mean_col[d]
    col_min_data <- min_col[d]
    col_max_data <- max_col[d]
    col_sd_data <- sd_col[d]
    

    
    #mean
    data3[,col_mean_data][idx1] <- rollapply(data3[,col_data][idx1],50, function (x)mean (x), by = 1, partial = FALSE, align ="center", fill=NA) 
    #max
    data3[,col_max_data][idx1] <-rollapply(data3[,col_data][idx1],50, function(x) max (x), by = 1, partial = FALSE, align ="center", fill=NA)  
    #min
    data3[,col_min_data][idx1] <- rollapply(data3[,col_data][idx1],50, function (x) min (x), by = 1, partial = FALSE, align ="center", fill=NA)  
    #sd
    data3[,col_sd_data][idx1] <-rollapply(data3[,col_data][idx1],50, function (x) sd (x), by = 1, partial = FALSE, align ="center", fill=NA)   
    

          }
 }


# Calculate median and mode for frequencies
#Add columns
data3=data.frame(data3,Median_Freq_heave_H=NA,Median_Freq_sway_H=NA,Median_Freq_surge_H=NA,Moda_Freq_heave_H=NA,Moda_Freq_sway_H=NA,Moda_Freq_surge_H=NA)

median_col= c(which(colnames(data3)=="Median_Freq_heave_H"):which(colnames(data3)=="Median_Freq_surge_H"))
moda_col= c(which(colnames(data3)=="Moda_Freq_heave_H"):which(colnames(data3)=="Moda_Freq_surge_H"))

length(median_col); length(moda_col)

data_col=c(20:22) 

names(data3[data_col])
length(data_col)

ID=unique(data3$ID)

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


for(i in 1:length(ID)) {
  
  idx1<- which(data3$ID==ID[i]) 
  
  for(d in c(1: length(data_col)))      {
    col_data <- data_col[d]
    col_median_data <- median_col[d]
    col_moda_data <- moda_col[d]
    
    data3[,col_median_data][idx1] <- rollapply(data3[,col_data][idx1],50, function (x)median (x), by = 1, partial = FALSE, align ="center", fill=NA) 
    #moda
    data3[,col_moda_data][idx1] <-rollapply(data3[,col_data][idx1],50, get_mode, by = 1, partial = FALSE, align ="center", fill=NA)  
    
    
    
    
  }
}

names(data3)
data5=data3

#Save table
save(data5,file="D:/Punta Leon 2019_2021 y 2022/Analisis señales acc pichones/Random Forest analisis/Output/acc_senales pichones_features 1seg_withFrequencies_10062025.RData")


save(data3, file="C:/Users/monse/Punta Leon 2019_2021 y 2022/Analisis señales acc pichones/Random Forest analisis/Output/acc_senales pichones_features 1seg_withFrequencies_07052025.RData")

#Vemos algunos valores:
tapply(data3$Mean_Pitch,list(data3$ID,data3$Category),mean,na.rm=T)
tapply(data3$Mean_Freq_sway_H,list(data3$Category),mean,na.rm=T)
tapply(data3$Mean_Freq_heave_H,list(data3$ID,data3$Behaviour),mean,na.rm=T)
tapply(data3$Mean_VEDBA,list(data3$ID,data3$Behaviour),mean,na.rm=T)
tapply(data3$Mean_stat_x,list(data3$ID,data3$Category),mean,na.rm=T)

