#1 data preparation

library(spectral)
library(chron)
library(igraph)
library(zoo)
library(doBy) 

#Load behavioural annotations
#This dataset contains the observed behaviours and their corresponding start and end row numbers. These row numbers will be used to match each behavioural observation with the corresponding accelerometer records.

ifilex = "D:/paper_04/Data/Behavioural_data.csv"


x =read.table(file=ifilex,
              as.is=TRUE, dec=".",sep=";", header=T)

head(x)
names(x)
tail(x)
unique(x$event)
unique(x$Category)
unique(x$Behaviour)
which(x$Number_rows_events=="NA")

#Load acceleration data
load("D:/paper_04/Data/Accelerometry_data.Rdata")
head(xx)

#Calculate pitch, roll, and VeDBA
xx=cbind(xx, StaticX=NA, StaticY=NA, StaticZ=NA,
  DynamicX=NA, DynamicY=NA, DynamicZ=NA,
  ODBA=NA, VEDBA=NA,Pitch=NA, Roll=NA) 

head(xx)
tail(xx)

IDs=unique(xx$ID_chick) 

for (i in 1:length(IDs)){
  
  idx1=which(xx$ID==IDs[i])
  
  xx$StaticX[idx1] =rollapply(xx$Acc_x[idx1],100, function (x) mean (x), by =1,  						partial = FALSE, align ="center", fill=NA) 
  
  xx$DynamicX[idx1]=abs(xx$Acc_x[idx1]-xx$StaticX[idx1])		
  

  xx$StaticY[idx1] =rollapply(xx$Acc_y[idx1],100, function (x) mean (x), by = 1, 						partial = FALSE, align ="center", fill=NA)
  
  xx$DynamicY[idx1]=abs(xx$Acc_y[idx1]-xx$StaticY[idx1])
  
  xx$StaticZ[idx1] =rollapply(xx$Acc_z[idx1],100, function (x) mean (x), by = 1, 						partial = FALSE, align ="center", fill=NA)
  
  xx$DynamicZ[idx1]=abs(xx$Acc_z[idx1]-xx$StaticZ[idx1])
  
  xx$ODBA[idx1]=(xx$DynamicX[idx1]+xx$DynamicY[idx1]+xx$DynamicZ[idx1])

  xx$VEDBA[idx1]=sqrt(xx$DynamicX[idx1]^2+
      xx$DynamicY[idx1]^2+xx$DynamicZ[idx1]^2)
  
  xx$Roll[idx1]=atan2(xx$StaticY[idx1],sqrt(xx$StaticX[idx1]^2+xx$StaticZ[idx1]^2))*180/pi
  
  xx$Pitch[idx1]=atan2(xx$StaticX[idx1],sqrt(xx$StaticZ[idx1]^2+xx$StaticY[idx1]^2))*180/pi
  
}


#Smooth VeDVA, pitch and roll----
xx$VEDBA_smooth_1sec=NA
xx$Pitch_smooth_1sec=NA
xx$Roll_smooth_1sec=NA

for(i in 1:length(IDs)){
  
  idx1=which(xx$ID==IDs[i]) 			
  xx$VEDBA_smooth_1sec[idx1] <- rollapply(xx$VEDBA[idx1],50, function (x)mean 						(x), by = 1, partial = FALSE, align ="center", fill=NA)
  xx$Pitch_smooth_1sec[idx1] <- rollapply(xx$Pitch[idx1],50, function (x)mean 						(x), by = 1, partial = FALSE, align ="center", fill=NA)
  xx$Roll_smooth_1sec[idx1] <- rollapply(xx$Roll[idx1],50, function (x)mean 						(x), by = 1, partial = FALSE, align ="center", fill=NA)
  
}

summary(xx)

#Calculate frequency
xx$Freq_heave_H=NA
xx$Freq_sway_H=NA
xx$Freq_surge_H=NA

detach("package:igraph", unload=TRUE)

fq=50


power_spec = function(y,samp.freq, ...){
  
  N <- length(y)
  fk <- fft(y)
  fk <- fk[2:length(fk)/2+1]
  fk <- 2*fk[seq(1, length(fk), by = 2)]/N
  freq <- (1:(length(fk)))* samp.freq/(2*length(fk))
  data.frame(amplitude = Mod(fk), freq = freq)
}


for(i in 1:length(IDs)){
  
  idx1=which(xx$ID==IDs[i])
  
  xx$Freq_heave_H[idx1]<- rollapply(xx$DynamicZ[idx1],100, function(x) power_spec(x,samp.freq = 50)$freq[which.max(power_spec(x,samp.freq = 50)$amplitude)],by=1,partial=FALSE, align="center",fill=NA) 
  xx$Freq_sway_H[idx1]<- rollapply(xx$DynamicY[idx1],100, function(x) power_spec(x,samp.freq = 50)$freq[which.max(power_spec(x,samp.freq = 50)$amplitude)],by=1,partial=FALSE, align="center",fill=NA)
  xx$Freq_surge_H[idx1]<- rollapply(xx$DynamicX[idx1],100, function(x) power_spec(x,samp.freq = 50)$freq[which.max(power_spec(x,samp.freq = 50)$amplitude)],by=1,partial=FALSE, align="center",fill=NA)
  
}
summary(xx)
rownames(xx) <- NULL 
data1=xx

#save table
save(data1, file="C:/Users/XXX/Punta Leon 2019_2021 y 2022/Analisis señales acc pichones/Random Forest analisis/Output/acc_smooth_withFrequencies_07052025.rData")


#Match behavioural annotations with the corresponding accelerometer records
unique(data1$ID_chick)
head(data1)
names(x)

x=orderBy(~ID_chick+ Date+Row_event_start, data=x) 
head(x)
tail(x)
table(x$event,x$ID_chick)
names(data1)

#Add columns to data1
data1$Chick_age_days=rep(NA,nrow(data1))
data1$Chick_age_category=rep(NA,nrow(data1))
data1$Chick_hierarchy=rep(NA,nrow(data1))
data1$Category=rep(NA,nrow(data1))
data1$Behaviour=rep(NA, nrow(data1))
data1$Event=rep(NA,nrow(data1))


( IDsx <- unique(as.character(x$ID_chick)) )
( IDs<- unique(as.character(data1$ID_chick)) )
class(data1$ID_chick)

length(IDsx) 
length(IDs)  

names(data1)
names(x)

#Loop through each individual to assign behavioural labels to the corresponding accelerometer records
for(i in 1:length(IDsx)){

  idx1=which(data1$ID==IDsx[i]) #datos del file de aceleracion del ID=1
  idx2=which(x$ID_chick==IDsx[i]) #datos del file x 
  
  for (d in c(idx2[1]:(idx2[1]+length(idx2)-1))) { 																				
    idx3 = which(data1$Row_number[idx1] >=x[d,"Row_event_start"] & 
        data1$Row_number[idx1] <= x[d,"Row_event_end"])
    
      #LABELS
    data1$Chick_age_days[idx1][idx3]=as.character(x$Chick_age_days[d])
    data1$Chick_age_category[idx1][idx3]=as.character(x$Chick_age_category[d])
    data1$Chick_hierarchy[idx1][idx3]=as.character(x$Chick_hierarchy[d])
    data1$Category[idx1][idx3]=as.character(x$Category[d])
    data1$Behaviour[idx1][idx3]<-as.character(x$Behaviour[d])
    data1$Event[idx1][idx3]<-as.character(x$event[d])
    
  }
  
}


#Convert date and time information to POSIXct format

names(data1)
mydates=strptime(data1$Fecha, "%d/%m/%Y")
mydates
mytime1 = data1$Hora
TIME1=paste(mydates,mytime1)

TIME1 = as.POSIXct(strptime(TIME1,"%Y-%m-%d %H:%M:%OS")) 
data.class(TIME1)

data1$Hora = TIME1


data1 = data1[,-match("Fecha",names(data1))]#Eliminamos la columna de la fecha 

head(data1)

rownames(data1)=NULL
table(data1$Category)
summary(data1)

idx4=which(data1$Behaviour!="NA") 
data2=data1[idx4,]

head(data2)
tail(data2)
rownames(data2)=NULL


#Save table
save(data2, file="C:/Users/monse/Punta Leon 2019_2021 y 2022/Analisis señales acc pichones/Random Forest analisis/Output/acc_observed_events_withFrequencies_07052025.rData")
