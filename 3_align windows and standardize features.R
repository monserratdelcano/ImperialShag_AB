
#3 Aling windows and standarized features

#Load data
load(file="D:/Punta Leon 2019_2021 y 2022/Analisis señales acc pichones/Random Forest analisis/Output/acc_senales pichones_features 1seg_withFrequencies_10062025.RData")

data3=data5
names(data3)
rownames(data3)=NULL

#Align behavioural and chick-related labels with the corresponding feature windows by shifting labels 24 rows to account for the centered rolling window.

data3=data.frame(data3,"Hora_corr"=NA) 
data3=data.frame(data3,"Category_corr"=NA)
data3=data.frame(data3,"Behaviour_corr"=NA) 
data3=data.frame(data3,"Event_corr"=NA)
data3=data.frame(data3,"Chick_age_days_corr"=NA)
data3=data.frame(data3,"Chick_age_category_corr"=NA)
data3=data.frame(data3,"Chick_hierarchy_corr"=NA)
data3=data.frame(data3,"Row_number_corr"=NA)
names(data3)

( IDs <- unique(as.character(data3$ID)) )
class(data3$ID)

length(IDs)

for (i in 1:length(IDs)) {  
  
  idx1 = which(data3$ID==IDs[i]) 
  xx=c(rep(NA,24),data3$Hora[idx1]) 
  data3$Hora_corr[idx1]=xx[1:(length(xx)-24)] 
  yy=c(rep(NA,24),data3$Category[idx1]) 
  data3$Category_corr[idx1]=yy[1:(length(yy)-24)]
  zz=c(rep(NA,24),data3$Behaviour[idx1]) 
  data3$Behaviour_corr[idx1]=zz[1:(length(zz)-24)]
  xx1=c(rep(NA,24),data3$Event[idx1])
  data3$Event_corr[idx1]=xx1[1:(length(xx1)-24)]
  Chick_age_days=c(rep(NA,24),data3$Chick_age_days[idx1])
  data3$Chick_age_days_corr[idx1]=Chick_age_days[1:(length(Chick_age_days)-24)]
  Chick_age_category=c(rep(NA,24),data3$Chick_age_category[idx1])
  data3$Chick_age_category_corr[idx1]=Chick_age_category[1:(length(Chick_age_category)-24)]
  Chick_hierarchy=c(rep(NA,24),data3$Chick_hierarchy[idx1])
  data3$Chick_hierarchy_corr[idx1]=Chick_hierarchy[1:(length(Chick_hierarchy)-24)]
  Row_number=c(rep(NA,24),data3$Row_number[idx1])
  data3$Row_number_corr[idx1]=Row_number[1:(length(Row_number)-24)]
}

# Convert corrected time to POSIXct format using the local time zone
data3$Hora_corr <- as.POSIXct(data3$Hora_corr, origin="1970-01-01",tz="America/Argentina/Buenos_Aires") 

#Standarised features

names=paste(names(data3[29:94]),"Stan",sep="_")
data3[c(names)] <- NA

sd_colum=c(which(colnames(data3)=="Mean_x_Stan"): which(colnames(data3)=="Moda_Freq_surge_H_Stan"))

raw_colum=c(which(colnames(data3)=="Mean_x"): which(colnames(data3)=="Moda_Freq_surge_H"))


IDs=unique(data3$ID)

for(i in 1:length(IDs)){
  
  idx1<- which(data3$ID==IDs[i]) 
  
  for(a in c(1: length(raw_colum))){
    col_raw <- raw_colum[a]
    col_sd <- sd_colum[a]

    data3[,col_sd][idx1] <- (data3[,col_raw][idx1] - min(data3[,col_raw][idx1], na.rm=T))/(max(data3[,col_raw][idx1], na.rm=T) - min(data3[,col_raw][idx1], na.rm=T))
    
  }
  
}

#Delete NAs
ss=which(is.na(data3$Mean_x))
data4=data3[-ss,]


#Save table
save(data4, file="D:/Punta Leon 2019_2021 y 2022/Analisis señales acc pichones/Random Forest analisis/Output/DataParameters_features 1seg_withFrequencies_10062025.RData")

