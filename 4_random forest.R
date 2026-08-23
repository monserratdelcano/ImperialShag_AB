#Random Forest

library(randomForest)
library(datasets)
library(caret)
library(dplyr)
library(ggplot2)
library(scales)


#Load data
load(file="D:/Punta Leon 2019_2021 y 2022/Analisis se�ales acc pichones/Random Forest analisis/Output/DataParameters_features 1seg_withFrequencies_10062025.RData")


#Select 80% of individuals for Random Forest training-validation and leave 20% out for test 

#Balance the training data for each individual based on the number of observations of the least represented behaviour

#N1_PA_2022----
n1=data4[data4$ID_chick=="N1_PA_2022",]

table(n1$Category_corr)
begN1=n1[n1$Category_corr=="Begging",]
eatN1=n1[n1$Category_corr=="Eating",]
othN1=n1[n1$Category_corr=="Active_Others",]
rn1=n1[n1$Category_corr=="Resting",]

begN1_bal=begN1[sample(1:nrow(begN1),6306,replace = F),]
eatN1_bal=eatN1
othN1_bal=othN1[sample(1:nrow(othN1),6306,replace = F),]
rn1_bal=rn1[sample(1:nrow(rn1),6306,replace = F),]


n1_bal=rbind(begN1_bal,eatN1_bal,othN1_bal,rn1_bal)
table(n1_bal$Category_corr)

#N2_PA_2022----
n2=data4[data4$ID=="N2_PA_2022",]

table(n2$Category_corr)
begN2=n2[n2$Category_corr=="Begging",]
eatN2=n2[n2$Category_corr=="Eating",]
othN2=n2[n2$Category_corr=="Active_Others",]
rN2=n2[n2$Category_corr=="Resting",]

begN2_bal=begN2[sample(1:nrow(begN2),6548,replace = F),]
eatN2_bal=eatN2
othN2_bal=othN2[sample(1:nrow(othN2),6548,replace = F),]
rN2_bal=rN2[sample(1:nrow(rN2),6548,replace = F),]

n2_bal=rbind(begN2_bal,eatN2_bal,othN2_bal,rN2_bal)
table(n2_bal$Category_corr)

#N3_PB_2022----

n3=data4[data4$ID=="N3_PB_2022",]

table(n3$Category_corr)
begN3=n3[n3$Category_corr=="Begging",]
eatN3=n3[n3$Category_corr=="Eating",]
othN3=n3[n3$Category_corr=="Active_Others",]
rN3=n3[n3$Category_corr=="Resting",]

begN3_bal=begN3[sample(1:nrow(begN3),8418,replace = F),]
eatN3_bal=eatN3
othN3_bal=othN3[sample(1:nrow(othN3),8418,replace = F),] 
rN3_bal=rN3[sample(1:nrow(rN3),8418,replace = F),]

n3_bal=rbind(begN3_bal,eatN3_bal,othN3_bal,rN3_bal)
table(n3_bal$Category_corr)

#N6_PA_2022----
n6=data4[data4$ID_chick=="N6_PA_2022",]

table(n6$Category_corr)
begn6=n6[n6$Category_corr=="Begging",]
eatn6=n6[n6$Category_corr=="Eating",]
othn6=n6[n6$Category_corr=="Active_Others",]
rn6=n6[n6$Category_corr=="Resting",]


begn6_bal=begn6[sample(1:nrow(begn6),5166,replace = F),]
eatn6_bal=eatn6
othn6_bal=othn6[sample(1:nrow(othn6),5166,replace = F),]
rn6_bal=rn6[sample(1:nrow(rn6),5166,replace = F),]


n6_bal=rbind(begn6_bal,eatn6_bal,othn6_bal,rn6_bal)
table(n6_bal$Category_corr)

#N7_PB_2022----
n7_b=data4[data4$ID_chick=="N7_PB_2022",]

table(n7_b$Category_corr)
begn7_b=n7_b[n7_b$Category_corr=="Begging",]
eatn7_b=n7_b[n7_b$Category_corr=="Eating",]
othn7_b=n7_b[n7_b$Category_corr=="Active_Others",]
r7_b=n7_b[n7_b$Category_corr=="Resting",]


begn7_bal_b=begn7_b[sample(1:nrow(begn7_b), 3564 ,replace = F),]
eatn7_bal_b=eatn7_b
othn7_bal_b=othn7_b[sample(1:nrow(othn7_b), 3564 ,replace = F),]
r7_bal_b=r7_b[sample(1:nrow(r7_b), 3564 ,replace = F),]


n7_bal_b=rbind(begn7_bal_b,eatn7_bal_b,othn7_bal_b,r7_bal_b)
table(n7_bal_b$Category_corr)

#N8_PA_2022----
n8=data4[data4$ID_chick=="N8_PA_2022",]

table(n8$Category_corr)
begn8=n8[n8$Category_corr=="Begging",]
eatn8=n8[n8$Category_corr=="Eating",]
othn8=n8[n8$Category_corr=="Active_Others",]
r8=n8[n8$Category_corr=="Resting",]


begn8_bal=begn8[sample(1:nrow(begn8), 4609 ,replace = F),]
eatn8_bal=eatn8
othn8_bal=othn8[sample(1:nrow(othn8), 4609 ,replace = F),]
r8_bal=r8[sample(1:nrow(r8), 4609 ,replace = F),]


n8_bal=rbind(begn8_bal,eatn8_bal,othn8_bal,r8_bal)
table(n8_bal$Category_corr)

#N9_PA_2022----
n9=data4[data4$ID_chick=="N9_PA_2022",]

table(n9$Category_corr)
begn9=n9[n9$Category_corr=="Begging",]
eatn9=n9[n9$Category_corr=="Eating",]
othn9=n9[n9$Category_corr=="Active_Others",]
rn9=n9[n9$Category_corr=="Resting",]


begn9_bal=begn9[sample(1:nrow(begn9),5500 ,replace = F),]
eatn9_bal=eatn9
othn9_bal=othn9[sample(1:nrow(othn9),5500 ,replace = F),]
rn9_bal=rn9[sample(1:nrow(rn9),5500 ,replace = F),]


n9_bal=rbind(begn9_bal,eatn9_bal,othn9_bal,rn9_bal)
table(n9_bal$Category_corr)


#N10_PA_2022----
n10=data4[data4$ID_chick=="N10_PA_2022",]

table(n10$Category_corr)
begn10=n10[n10$Category_corr=="Begging",]
eatn10=n10[n10$Category_corr=="Eating",]
othn10=n10[n10$Category_corr=="Active_Others",]
rn10=n10[n10$Category_corr=="Resting",]


begn10_bal=begn10[sample(1:nrow(begn10),10482 ,replace = F),]
eatn10_bal=eatn10
othn10_bal=othn10[sample(1:nrow(othn10),10482 ,replace = F),]
rn10_bal=rn10[sample(1:nrow(rn10),10482 ,replace = F),]


n10_bal=rbind(begn10_bal,eatn10_bal,othn10_bal,rn10_bal)
table(n10_bal$Category_corr)

#N12_PA_2022----
n12=data4[data4$ID_chick=="N12_PA_2022",]

table(n12$Category_corr)
begn12=n12[n12$Category_corr=="Begging",]
eatn12=n12[n12$Category_corr=="Eating",]
othn12=n12[n12$Category_corr=="Active_Others",]
rn12=n12[n12$Category_corr=="Resting",]


begn12_bal=begn12[sample(1:nrow(begn12),6068 ,replace = F),]
eatn12_bal=eatn12
othn12_bal=othn12[sample(1:nrow(othn12),6068 ,replace = F),]
rn12_bal=rn12[sample(1:nrow(rn12),6068 ,replace = F),]


n12_bal=rbind(begn12_bal,eatn12_bal,othn12_bal,rn12_bal)
table(n12_bal$Category_corr)

#N12_PB_2022----
n12b=data4[data4$ID_chick=="N12_PB_2022",]

table(n12b$Category_corr)
begn12b=n12b[n12b$Category_corr=="Begging",]
eatn12b=n12b[n12b$Category_corr=="Eating",]
othn12b=n12b[n12b$Category_corr=="Active_Others",]
rn12b=n12b[n12b$Category_corr=="Resting",]


begn12b_bal=begn12b[sample(1:nrow(begn12b),1918 ,replace = F),]
eatn12b_bal=eatn12b[sample(1:nrow(eatn12b),1918 ,replace = F),]
othn12b_bal=othn12b[sample(1:nrow(othn12b),1918 ,replace = F),]
rn12b_bal=rn12b


n12b_bal=rbind(begn12b_bal,eatn12b_bal,othn12b_bal,rn12b_bal)
table(n12b_bal$Category_corr)


#Create data sets

#Test 
N5=data4[data4$ID_chick=="N5_PA_2022",]
N7=data4[data4$ID_chick=="N7_PA_2022",]
N11=data4[data4$ID_chick=="N11_PA_2022",]
test=rbind(N5,N7,N11)
names(test)
test=test[,-c(1:5,7:95,97:102,115:117,163:165)]  
names(test)
test_f=test[,c(3:62)] 
names(test_f)

#Train (balanced)
train= rbind(n1_bal,n2_bal,n3_bal,n6_bal,n7_bal_b,n8_bal,n9_bal,n10_bal,n12_bal,n12b_bal)
rownames(train)=NULL
unique(train$ID_chick)
table(train$Category_corr,train$ID)
names(train)
train_1=train[,-c(1:5,7:95,97:102,115:117,163:165)]   
names(train_1)
train_1$Category_corr=as.factor(train_1$Category_corr)


#Validation (unbalanced)
val=rbind(n1,n2,n3,n6,n7_b,n8,n9,n10,n12,n12b)
rownames(val)=NULL
unique(val$ID_chick)
table(val$Category_corr,val$ID)
names(val)
val_1=val[,-c(1:5,7:95,97:102,115:117,163:165)]   
names(val_1)
val_1$Category_corr=as.factor(val_1$Category_corr)

#Model RF
model.rf <- NULL
predichos <- NULL 
tab <- NULL
importance_var <- NULL
confusion_matrices=list()

unique(val_1$ID_chick)

id <- unique(train_1$ID)
id

for(i in 1:length(id)) {
  idx<-which(train_1$ID!=id[i]) 
  trainset= train_1[idx,]
model.rf [[i]]<- randomForest(Category_corr ~ Mean_x_Stan + Mean_y_Stan + Mean_z_Stan +
    Mean_stat_x_Stan + Mean_stat_y_Stan + Mean_stat_z_Stan +
    Mean_dyn_x_Stan + Mean_dyn_y_Stan + Mean_dyn_z_Stan +
    Mean_VEDBA_Stan + Mean_Pitch_Stan + Mean_Roll_Stan +
    Min_x_Stan + Min_y_Stan + Min_z_Stan +
    Min_stat_x_Stan + Min_stat_y_Stan + Min_stat_z_Stan +
    Min_dyn_x_Stan + Min_dyn_y_Stan + Min_dyn_z_Stan +
    Min_VEDBA_Stan + Min_Pitch_Stan + Min_Roll_Stan +
    Min_Freq_heave_H_Stan + Min_Freq_sway_H_Stan + Min_Freq_surge_H_Stan +
    Max_x_Stan + Max_y_Stan + Max_z_Stan +
    Max_stat_x_Stan + Max_stat_y_Stan + Max_stat_z_Stan +
    Max_dyn_x_Stan + Max_dyn_y_Stan + Max_dyn_z_Stan +
    Max_VEDBA_Stan + Max_Pitch_Stan + Max_Roll_Stan +
    Max_Freq_heave_H_Stan + Max_Freq_sway_H_Stan + Max_Freq_surge_H_Stan +
    SD_x_Stan + SD_y_Stan + SD_z_Stan +
    SD_stat_x_Stan + SD_stat_y_Stan + SD_stat_z_Stan +
    SD_dyn_x_Stan + SD_dyn_y_Stan + SD_dyn_z_Stan +
    SD_VEDBA_Stan + SD_Pitch_Stan + SD_Roll_Stan +
    SD_Freq_heave_H_Stan + SD_Freq_sway_H_Stan + SD_Freq_surge_H_Stan+
    Moda_Freq_heave_H_Stan+Moda_Freq_sway_H_Stan+Moda_Freq_surge_H_Stan,
  data=trainset,ntree=500,importance=T, na.action=na.exclude)
  idx2=which(val_1$ID==id[i]) 
  valset_f=val_1[idx2,c(3:62)]    #validation set features
  valset_l=val_1[idx2,2]  #validation labels 
  predichos[[i]] <- predict(model.rf[[i]],valset_f)
  tab[[i]] <- table(predict(model.rf[[i]],valset_f), true=valset_l)
  confusion_matrices[[i]] <- confusionMatrix(tab[[i]])
  importance_var[[i]] <- importance(model.rf[[i]])
 }

#Save data model results
getwd()
setwd("D:/Punta Leon 2019_2021 y 2022/Analisis se�ales acc pichones/Random Forest analisis/Output/RF_LOIO_CV_Balanceado_Category_ModaFreq_11062025/")

save(model.rf, file="rf.rdata")
save(confusion_matrices, file="CM.rdata")
save(importance_var, file="importance_var.rdata")
save(predichos, file="predichos.rdata")

#Summarise LOIO-CV results using aggregated confusion matrices and classification metrics for each behavioural class and validation individual.----
 
confusion_agregada <- confusion_matrices[[1]]$table
confusion_agregada[,] <- 0


for (i in 1:length(confusion_matrices)) {
  confusion_agregada <- confusion_agregada + confusion_matrices[[i]]$table
}

print(confusion_agregada)

resultados <- list()

for (i in 1:length(confusion_matrices)) {
  metrics <- as.data.frame(confusion_matrices[[i]]$byClass)

  metrics$class <- rownames(metrics)

  metrics$ID <- i
  
  resultados[[i]] <- metrics
}

tabla_final <- do.call(rbind, resultados)
rownames(tabla_final)=NULL
print(tabla_final)


resumen <- tabla_final %>%
  group_by(class) %>%
  summarise(
    Precision_mean = mean(Precision, na.rm = TRUE),
    Precision_sd   = sd(Precision, na.rm = TRUE),
    Recall_mean    = mean(Recall, na.rm = TRUE),
    Recall_sd      = sd(Recall, na.rm = TRUE),
    F1_mean        = mean(F1, na.rm = TRUE),
    F1_sd          = sd(F1, na.rm = TRUE),
    .groups = "drop"
  )


recSen=ggplot(resumen, aes(x = Precision_mean, y = Recall_mean, label = class)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Recall_mean - Recall_sd,
    ymax = Recall_mean + Recall_sd),
    width = 0.01) +
  geom_errorbarh(aes(xmin = Precision_mean - Precision_sd,
    xmax = Precision_mean + Precision_sd),
    height = 0.01) +
  geom_text(vjust = -1, size = 4) +
  labs(x = "Precisión", y = "Sensibilidad") +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1),
    labels = function(x) gsub("\\.", ",", sprintf("%.2f", x))
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1),
    labels = function(x) gsub("\\.", ",", sprintf("%.2f", x))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color="black"),
    axis.ticks = element_line(color="black"),
    axis.text = element_text(color="black"),
    axis.title = element_text(color="black")
  )
recSen


#Assess and visualise variable importance across all LOIO-CV Random Forest models.Variable importance scores are averaged across models, and the 20 most important

importances_list <- lapply(model.rf, function(m) {
  as.data.frame(m$importance)
})

for (i in seq_along(importances_list)) {
  importances_list[[1]]$Variable <- rownames(importances_list[[i]])
}

all_importances <- bind_rows(importances_list, .id = "Fold")

summary_importance <- all_importances %>%
  group_by(Variable) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop") %>%
  arrange(desc(MeanDecreaseGini))  # o la métrica que estés usando
print(summary_importance)


library(patchwork) 

top_accuracy <- summary_importance %>%
  slice_max(order_by = MeanDecreaseAccuracy, n = 20)

p1 <- ggplot(top_accuracy, aes(x = MeanDecreaseAccuracy, 
  y = reorder(Variable, MeanDecreaseAccuracy))) +
  geom_col(fill = "gray40") +
  labs(title = "Top 20 - Mean Decrease Accuracy",
    x = "MeanDecreaseAccuracy",
    y = "Atributo") +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())


top_gini <- summary_importance %>%
  slice_max(order_by = MeanDecreaseGini, n = 20)

p2 <- ggplot(top_gini, aes(x = MeanDecreaseGini, 
  y = reorder(Variable, MeanDecreaseGini))) +
  geom_col(fill = "gray40") +
  labs(title = "Top 20 - Mean Decrease Gini",
    x = "MeanDecreaseGini",
    y = NULL) +  # quita solo el título del eje Y, mantiene las etiquetas
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())


p2 <- ggplot(top_gini, aes(x = reorder(Variable, -MeanDecreaseGini), 
  y = MeanDecreaseGini)) +
  geom_col(fill = "gray40") +
  labs(title = "Top 20 - Mean Decrease Gini", 
    x = "Atributo", 
    y = "MeanDecreaseGini") +
  scale_y_continuous(expand = c(0, 0)) +  # <--- elimina espacio entre barras y eje
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.line = element_line(color = "black"))
p2


p3=p1 + p2

p4=p2 + recSen
p4




#Predicciones sobre set Test------
#Vemos las predicciones sobre los 3 animales que quedaron afuera
pred= predict(model.rf[[1]],newdata = test_f)
c2=confusionMatrix(data=pred, as.factor(test$Category_corr))
table(pred)
table(test$Category_corr)

#Vemos las predicciones para el N5_PA_2022------
names(N5)
N5t=test[test$ID_chick=="N5_PA_2022",]
names(N5t)
N5T_f=N5t[,c(3:62)]
names(N5T_f)

#Forma lenta 
pred_N5= predict(model.rf[[1]],newdata = N5T_f)
cm_N5=confusionMatrix(data=pred_N5, as.factor(N5t$Category_corr))
table(pred_N5)
table(N5t$Category_corr)

#Vemos las predicciones para N7_PA----
names(N7)
N7t=test[test$ID_chick=="N7_PA_2022",]
names(N7t)
N7T_f=N7t[,c(3:62)] #Nos quedamo con las features estandarizadas 
names(N7T_f)

#Vemos las predicciones para N11----
names(N11)
N11t=test[test$ID_chick=="N11_PA_2022",]
names(N11t)
N11T_f=N11t[,c(3:62)] #Nos quedamo con las features estandarizadas 
names(N11T_f)

#Forma lenta 
pred_N11= predict(model.rf[[2]],newdata = N11T_f)
cm_n11=confusionMatrix(data=pred_N11, as.factor(N11t$Category_corr))
table(pred_N11)
table(N11t$Category_corr)

#Automatizamos con un loop, segun con que pichón estemos cambiamos N por el numero del ID del pichon correspondiente N5. N17 o N11----
conf_matrices_N11 <- list()

for (i in seq_along(model.rf)) {
  pred_N11 <- predict(model.rf[[i]], newdata = N11T_f)
  cm_N11 <- confusionMatrix(data = pred_N11, reference = as.factor(N11t$Category_corr))
  conf_matrices_N11[[i]] <- cm_N11
}

conf_matrices_N11[[2]]
cm_N11

#Automatizamos para N11 las metricas 
resultados_N11 <- list()

# Iterar sobre cada matriz de confusión
for (i in seq_along(conf_matrices_N11)) {
  # Calcular las métricas de interés
  metrics_N11 <- as.data.frame(conf_matrices_N11[[i]]$byClass)
  
  # Agregar columnas para el nombre de la clase y el modelo
  metrics_N11$class <- rownames(metrics_N11)
  metrics_N11$model <- paste0("model_", i)
  
  # Almacenar los resultados en la lista
  resultados_N11[[i]] <- metrics_N11
}

# Unir todo en una sola tabla final
tabla_final_N11 <- do.call(rbind, resultados_N11)

# Combinar los resultados en un único data frame
tabla_final_N11 <- do.call(rbind, resultados_N11)

rownames(tabla_final_N11)=NULL
# Imprimir la tabla final
print(tabla_final_N11)

tapply(tabla_final_N11$`Balanced Accuracy`,tabla_final_N11$class,mean)
tapply(tabla_final_N11$`Balanced Accuracy`,tabla_final_N11$class,sd)

tapply(tabla_final_N11$Sensitivity,tabla_final_N11$class,mean)
tapply(tabla_final_N11$Sensitivity,tabla_final_N11$class,sd)

tapply(tabla_final_N11$Precision,tabla_final_N11$class,mean)
tapply(tabla_final_N11$Precision,tabla_final_N11$class,sd)

tapply(tabla_final_N11$F1,tabla_final_N11$class,mean)
tapply(tabla_final_N11$F1,tabla_final_N11$class,sd)


#Graficos de predicciones sobre los animales Test----
#N5_PA_20222

n5=test[test$ID_chick=="N5_PA_2022",]  
n5_f=n5[,c(3:62)]
predichos=predict(model.rf[[2]],newdata = n5_f)
cm=confusionMatrix(data=predichos, as.factor(n5$Category_corr))

#Agregamos los predichos a N5
n5_2=data4[data4$ID_chick=="N5_PA_2022",]
n5_2$predichos=predichos
n5_2$Time2=seq(1:nrow(n5_2))


#Graficamos los reales
library(ggplot2)
library(ggpubr)
library(cowplot)
library(grid)
library(gridExtra)
library(ggplot2)
names(n5_2)
n5_2$Row_number_corr
nn=n5_2[n5_2$Category_corr=="Active_Others" ,]
nn=nn[nn$Event_corr=="44",]

p1=ggplot(nn, aes(x=Row_number_corr,color = Event_corr)) + 
  geom_line(aes(y = Acc_x),size=0.7) + 
  geom_line(aes(y = Acc_y),size=0.7) +
  geom_line(aes(y = Acc_z),size=0.7) +
  scale_y_continuous(name= "")+
  ggtitle("Observados")


#Graficamos los predichos
p2=ggplot(nn, aes(x=Row_number_corr,color = predichos)) + 
  geom_line(aes(y = Acc_x),size=0.7) + 
  geom_line(aes(y = Acc_y),size=0.7) +
  geom_line(aes(y = Acc_z),size=0.7) +
  scale_y_continuous(name= "")+
  ggtitle("Predichos")

p3 <-plot_grid(p1,p2, scale =1,nrow = 2,align = "v")
p3


#Graficamos el VeDBA
names(n5_2)

p4=ggplot(n5_2, aes(x=Time2,color = Category_corr))+
  geom_line(aes(y = VEDBA_smooth_1sec),size=0.7)+
  scale_x_continuous(name= "")+
  ggtitle("Observados")

p5=ggplot(n5_2, aes(x=Time2,color = predichos))+
  geom_line(aes(y = VEDBA_smooth_1sec),size=0.7)+
  scale_x_continuous(name= "")+
  ggtitle("Predichos")

p6 <-plot_grid(p4,p5, scale =1,nrow = 2,align = "v")
p6

#Hacemos un geom raster 

n5_3=n5_2[,c("Category_corr","predichos","Row_number_corr")]
str(n5_3)
library(tidyr)
library(dplyr)
library(tidyr)


#N7_PA
n7=test[test$ID_chick=="N7_PA_2022",]  
n7_f=n7[,c(3:62)]
predichos=predict(model.rf[[8]],newdata = n7_f)
cm=confusionMatrix(data=predichos, as.factor(n7$Category_corr))

#Agregamos los predichos a N7
n7_2=data4[data4$ID_chick=="N7_PA_2022",]
n7_2$predichos=predichos
n7_2$Time2=seq(1:nrow(n7_2))

n5_2$Row_number_corr

p7=ggplot(n7_2, aes(x=Time2,color = Category_corr)) + 
  geom_line(aes(y = Acc_x),size=0.7) + 
  geom_line(aes(y = Acc_y),size=0.7) +
  geom_line(aes(y = Acc_z),size=0.7) +
  scale_y_continuous(name= "")+
  ggtitle("Observados")


#Graficamos los predichos
p8=ggplot(n7_2, aes(x=Time2,color = predichos)) + 
  geom_line(aes(y = Acc_x),size=0.7) + 
  geom_line(aes(y = Acc_y),size=0.7) +
  geom_line(aes(y = Acc_z),size=0.7) +
  scale_y_continuous(name= "")+
  ggtitle("Predichos")

p9 <-plot_grid(p7,p8, scale =1,nrow = 2,align = "v")
p9

#N11_PA
n11=test[test$ID_chick=="N11_PA_2022",]  
n11_f=n11[,c(3:62)]
predichos=predict(model.rf[[8]],newdata = n11_f)
cm=confusionMatrix(data=predichos, as.factor(n11$Category_corr))

#Agregamos los predichos a N7
n11_2=data4[data4$ID_chick=="N11_PA_2022",]
n11_2$predichos=predichos
n11_2$Time2=seq(1:nrow(n11_2))


p10=ggplot(n11_2, aes(x=Time2,color = Category_corr)) + 
  geom_line(aes(y = Acc_x),size=0.7) + 
  geom_line(aes(y = Acc_y),size=0.7) +
  geom_line(aes(y = Acc_z),size=0.7) +
  scale_y_continuous(name= "")+
  ggtitle("Observados")


#Graficamos los predichos
p11=ggplot(n11_2, aes(x=Time2,color = predichos)) + 
  geom_line(aes(y = Acc_x),size=0.7) + 
  geom_line(aes(y = Acc_y),size=0.7) +
  geom_line(aes(y = Acc_z),size=0.7) +
  scale_y_continuous(name= "")+
  ggtitle("Predichos")

p12 <-plot_grid(p10,p11, scale =1,nrow = 2,align = "v")
p12

#Plot matriz-----
# Obtener matriz de confusión
library(caret)
library(ggplot2)
library(dplyr)


cm <- confusionMatrix(data = predichos, reference = as.factor(n5$Category_corr))

# Tabla de frecuencias
cm_table <- as.data.frame(cm$table) %>%
  mutate(Correct = Prediction == Reference)

# Calcular Recall por clase (columna: Reference)
recall_df <- cm_table %>%
  group_by(Reference) %>%
  summarise(Recall = sum(Freq[Prediction == Reference]) / sum(Freq) * 100)

# Calcular Precision por clase (fila: Prediction)
precision_df <- cm_table %>%
  group_by(Prediction) %>%
  summarise(Precision = sum(Freq[Prediction == Reference]) / sum(Freq) * 100)

# Cantidad de clases
n_clases <- length(unique(cm_table$Reference))

# Gráfico
ggplot(cm_table, aes(x = Reference, y = Prediction)) +
  geom_point(aes(size = Freq, color = Correct), alpha = 0.8) +
  geom_text(aes(label = Freq), color = "black", size = 3) +
  
  # Recall (%)
  geom_text(data = recall_df, aes(x = Reference, y = n_clases + 0.6,
                                  label = sprintf("%.1f", Recall)),
            inherit.aes = FALSE, size = 3.5, fontface = "bold") +
  
  # Precision (%)
  geom_text(data = precision_df, aes(x = n_clases + 0.6, y = Prediction,
                                     label = sprintf("%.1f", Precision)),
            inherit.aes = FALSE, size = 3.5, fontface = "bold") +
  
  # Etiquetas de ejes externos
  annotate("text", x = mean(1:n_clases), y = n_clases + 1.2,
           label = "Recall (%)", size = 4, fontface = "bold") +
  annotate("text", x = n_clases + 1.3, y = mean(1:n_clases),
           label = "Precision (%)", angle = 270, size = 4, fontface = "bold") +
  
  scale_color_manual(values = c("TRUE" = "green4", "FALSE" = "brown1")) +
  scale_size_continuous(range = c(2, 10)) +
  labs(
    title = "Classification confusion table plot",
    x = "Observations (True Labels)",
    y = "Predictions (Predicted Labels)"
  ) +
  coord_cartesian(xlim = c(1, n_clases + 1.8), ylim = c(1, n_clases + 1.8)) +
  theme_gray(base_size = 12) +
  theme(
    #panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )
