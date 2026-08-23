#Filter
library(randomForest)
library(datasets)
library(caret)

#Load data
load(file="D:/Punta Leon 2019_2021 y 2022/Analisis se�ales acc pichones/Random Forest analisis/Output/DataParameters_features 1seg_withFrequencies_10062025.RData")

load("D:/Punta Leon 2019_2021 y 2022/Analisis se�ales acc pichones/Random Forest analisis/Output/RF_LOIO_CV_Balanceado_Category_ModaFreq_11062025/val_1.rdata")

#Add predicted values----
#animal1
valset_l=val_1[,2]
i1=val_1[val_1$ID_chick=="N1_PA_2022",]
i1=data4[data4$ID_chick=="N1_PA_2022",]
i1$predichos=predichos[[1]]
confusionMatrix(predichos[[1]],as.factor(i1$Category_corr))
confusionMatrix(i1$predichos,as.factor(i1$Category_corr))

i1$Hora_corr=n1$Hora_corr
i1$Row_number_corr=n1$Row_number_corr

#animal2
i2=val_1[val_1$ID_chick=="N2_PA_2022",]
i2$predichos=predichos[[2]]
confusionMatrix(table(predichos[[2]],true=i2$Category_corr))
confusionMatrix(table(i2$predichos,true=i2$Category_corr))
confusion_matrices[[2]]

i2$Hora_corr=n2$Hora_corr
i2$Row_number_corr=n2$Row_number_corr

#animal3

i3=val_1[val_1$ID_chick=="N3_PB_2022",]
i3$predichos=predichos[[3]]
confusionMatrix(table(predichos[[3]],true=i3$Category_corr))
confusionMatrix(table(i3$predichos,true=i3$Category_corr))
confusion_matrices[[3]]

i3$Hora_corr=n3$Hora_corr
i3$Row_number_corr=n3$Row_number_corr

#animal 4
table(val_1$ID_chick)
length(predichos[[4]])
i4=val_1[val_1$ID_chick=="N6_PA_2022",]
i4$predichos=predichos[[4]]
confusionMatrix(table(predichos[[4]],true=i4$Category_corr))
confusionMatrix(table(i4$predichos,true=i4$Category_corr))
confusion_matrices[[4]]

i4$Hora_corr=n6$Hora_corr
i4$Row_number_corr=n6$Row_number_corr

#animal5
table(val_1$ID_chick)
length(predichos[[5]])
i5=val_1[val_1$ID_chick=="N7_PB_2022",]
i5$predichos=predichos[[5]]
confusionMatrix(table(predichos[[5]],true=i5$Category_corr))
confusionMatrix(table(i5$predichos,true=i5$Category_corr))
confusion_matrices[[5]]

i5$Hora_corr=n7_b$Hora_corr
i5$Row_number_corr=n7_b$Row_number_corr

#animal6
table(val_1$ID_chick)
length(predichos[[6]])
i6=val_1[val_1$ID_chick=="N8_PA_2022",]
i6$predichos=predichos[[6]]
confusionMatrix(table(predichos[[6]],true=i6$Category_corr))
confusionMatrix(table(i6$predichos,true=i6$Category_corr))
confusion_matrices[[6]]

i6$Hora_corr=n8$Hora_corr
i6$Row_number_corr=n8$Row_number_corr

#animal7
table(val_1$ID_chick)
length(predichos[[7]])
i7=val_1[val_1$ID_chick=="N9_PA_2022",]
i7$predichos=predichos[[7]]
confusionMatrix(table(predichos[[7]],true=i7$Category_corr))
confusionMatrix(table(i7$predichos,true=i7$Category_corr))
confusion_matrices[[7]]

i7$Hora_corr=n9$Hora_corr
i7$Row_number_corr=n9$Row_number_corr

#Animal8
table(val_1$ID_chick)
length(predichos[[8]])
i8=val_1[val_1$ID_chick=="N10_PA_2022",]
i8$predichos=predichos[[8]]
confusionMatrix(table(predichos[[8]],true=i8$Category_corr))
confusionMatrix(table(i8$predichos,true=i8$Category_corr))
confusion_matrices[[8]]

i8$Hora_corr=n10$Hora_corr
i8$Row_number_corr=n10$Row_number_corr

#Animal 9
table(val_1$ID_chick)
length(predichos[[9]])
i9=val_1[val_1$ID_chick=="N12_PA_2022",]
i9$predichos=predichos[[9]]
confusionMatrix(table(predichos[[9]],true=i9$Category_corr))
confusionMatrix(table(i9$predichos,true=i9$Category_corr))
confusion_matrices[[9]]

i9$Hora_corr=n12$Hora_corr
i9$Row_number_corr=n12$Row_number_corr

#Animal 10
table(val_1$ID_chick)
length(predichos[[10]])
i10=val_1[val_1$ID_chick=="N12_PB_2022",]
i10$predichos=predichos[[10]]
confusionMatrix(table(predichos[[10]],true=i10$Category_corr))
confusionMatrix(table(i10$predichos,true=i10$Category_corr))
confusion_matrices[[10]]

i10$Hora_corr=n12b$Hora_corr
i10$Row_number_corr=n12b$Row_number_corr

animales=rbind(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
names(animales)
unique(animales$ID_chick)

# Assign unique event IDs to consecutive rows with the same label in both observed and predicted data
names(animales)

ii=animales[,c(1,2,63,64,65)]
names(ii)

head(ii)


library(dplyr)

df <- ii %>%
  arrange(ID_chick, Row_number_corr) %>%
  group_by(ID_chick) %>%  # Agrupar por pichón
  mutate(
    evento_real = cumsum(Category_corr != lag(Category_corr, default = first(Category_corr))),
    evento_predicho = cumsum(predichos != lag(predichos, default = first(predichos)))
  ) %>%
  ungroup()  # Desagrupar al final si vas a seguir trabajando
head(df$evento_real)
table(df$evento_real,df$Category_corr)



# count events
conteo_reales <- df %>%
  group_by(ID_chick, Category_corr) %>%
  summarise(n_eventos = n_distinct(evento_real), .groups = "drop")
conteo_reales

conteo_predichos <- df %>%
  group_by(ID_chick, predichos) %>%
  summarise(n_eventos = n_distinct(evento_predicho), .groups = "drop")
conteo_predichos

names(df)

#Retain only the start and end times, and the first and last row numbers of each predicted event

df2 <- df %>%
  group_by(ID_chick, evento_predicho) %>%
  summarise(
    predichos   = first(predichos),   # etiqueta del comportamiento predicho
    Hora_start  = first(Hora_corr),
    Hora_end    = last(Hora_corr),
    Row_start   = first(Row_number_corr),
    Row_end     = last(Row_number_corr),
    .groups = "drop"
  )


#Add columns to df2 containing event duration in number of rows and seconds
df2 <- df2 %>%
  group_by(ID_chick) %>%
  mutate(
    Dif_row = Row_end - Row_start,
    Duration_sec = Dif_row / 50
  ) %>%
  ungroup()



# Apply post-processing rules to predicted behavioural events by merging short events with neighbouring events and smoothing isolated misclassifications
library(zoo)

df2 <- df2 %>%
  arrange(ID_chick, Row_start) %>% 
  mutate(predichos = as.character(predichos)) %>%  # Convertir factor a character
  group_by(ID_chick) %>%
  mutate(
    filtrados = case_when(
      # Si Eating es muy corto (<3s) y esta entre dos Begg, lo cambiamos a Begg 
      predichos == "Eating" & Duration_sec < 3 &
        lag(predichos, default = first(predichos)) == "Begging" &
        lead(predichos, default = last(predichos)) == "Begging" ~ "Begging",
      
      # Si Begging es muy corto (<3s) y esta entre dos Eating lo cambiamos a Eating
      predichos == "Begging" & Duration_sec < 3 &
        lag(predichos, default = first(predichos)) == "Eating" &
        lead(predichos, default = last(predichos)) == "Eating" ~ "Eating",
      
      # Si Active_Others es muy corto (<4s) y esta entre dos Begg los cambiamos a Begg
      predichos == "Active_Others" & Duration_sec < 4 &
        lag(predichos, default = first(predichos)) == "Begging" &
        lead(predichos, default = last(predichos)) == "Begging" ~ "Begging",
      
      # Cualquier evento muy corto (<3s) esta rodeado por el mismo comportamiento (B,E o R), lo fusionamos con ese comportamiento
      Duration_sec < 3 &
        lag(predichos, default = first(predichos)) == lead(predichos, default = last(predichos)) &
        lag(predichos, default = first(predichos)) %in% c("Begging", "Eating", "Resting") ~ lag(predichos, default = first(predichos)),
      
      # Si ninguna regla aplica → mantener predichos
      TRUE ~ predichos
    )
  ) %>%
  # Recalcular bloques para absorber microeventos.  Crea un block_id que identifica segmentos continuos del mismo comportamientos (filtrados) y calcula la duracion de ese bloque 
  mutate(block_id = cumsum(filtrados != lag(filtrados, default = first(filtrados)))) %>%
  group_by(ID_chick, block_id) %>%
  mutate(block_duration = sum(Duration_sec)) %>%
  ungroup() %>%
  # Bloques muy cortos → fusionar con comportamiento adyacente. Si el bloque dura < 4 s y esta entre dos bloques del mismo tipo lo absorbemos para que bloques peque�os queden fusionados con sus vecinos. 
  mutate(
    filtrados = ifelse(block_duration < 4 & 
        lag(filtrados, default = first(filtrados)) == lead(filtrados, default = last(filtrados)),
      lag(filtrados, default = first(filtrados)), filtrados)
  ) %>%
  group_by(ID_chick) %>%
  # Ventana movil más amplia para Begging. Usa una ventana de 11 eventos para calcular prop_begging. Si prop_begging > 0.8 (es decir que Begging domina esa zona) : si el evento actual es active_others y dura < 4 s lo cambiamso a begging y si active others dura < 2 segundos siempre lo cambiamso a begging 
  #Esto sirve para ver si la zona es mayoritariamente Begging , eliminamos picos de active_others
  mutate(
    window_events = 11,
    prop_begging = rollapply(filtrados == "Begging", width = window_events, FUN = mean, fill = NA, align = "center"),
    
    # Regla 1: Si Begging domina y el evento es Active_Others corto (<4) → Begging
    filtrados = ifelse(!is.na(prop_begging) & prop_begging > 0.8 &
        filtrados == "Active_Others" & Duration_sec < 4, "Begging", filtrados),
    
    # NUEVA REGLA: Si Begging domina y Active_Others es MUY corto (<2) → Begging SIEMPRE
    filtrados = ifelse(!is.na(prop_begging) & prop_begging > 0.8 &
        filtrados == "Active_Others" & Duration_sec < 2, "Begging", filtrados)
  ) %>%
  ungroup() %>%
  select(-block_id, -block_duration, -window_events, -prop_begging)



#Merge the filtered predicted behavioural labels with the original data based on chick and event IDs
df3 <- df %>%
  left_join(
    df2 %>% select(ID_chick, evento_predicho, filtrados),
    by = c("ID_chick", "evento_predicho")
  )


#Matrices before and after filter

library(purrr)
ids <- unique(df3$ID_chick)

get_metrics <- function(id){
  d <- df3[df3$ID_chick == id, ]
  cm <- confusionMatrix(table(d$predichos, d$Category_corr))
  
  m <- as.data.frame(cm$byClass)
  m$Class <- rownames(cm$byClass)
  m$ID <- id
  
  # calculamos F1 = 2 * (precision * recall)/(precision + recall)
  m$F1 <- with(m, 2 * (Sensitivity * `Pos Pred Value`) / 
      (Sensitivity + `Pos Pred Value`))
  
  return(m)
}


all_metrics <- map_dfr(ids, get_metrics)


summary_metrics <- all_metrics %>%
  group_by(Class) %>%
  summarise(across(where(is.numeric),
    list(mean = mean, sd = sd),
    .names = "{.col}_{.fn}"),
    .groups="drop")
summary_metrics



ids <- unique(df3$ID_chick)
get_metrics_f <- function(id){
  d <- df3[df3$ID_chick == id, ]
  cm <- confusionMatrix(table(d$filtrados, d$Category_corr))
  
  # pasamos a dataframe
  m <- as.data.frame(cm$byClass)
  m$Class <- rownames(cm$byClass)
  m$ID <- id
  
  # calculamos F1 = 2 * (precision * recall)/(precision + recall)
  m$F1 <- with(m, 2 * (Sensitivity * `Pos Pred Value`) / 
      (Sensitivity + `Pos Pred Value`))
  
  return(m)
}

all_metrics_f <- map_dfr(ids, get_metrics_f)

summary_metrics_f <- all_metrics_f %>%
  group_by(Class) %>%
  summarise(across(where(is.numeric),
    list(mean = mean, sd = sd),
    .names = "{.col}_{.fn}"),
    .groups="drop")
summary_metrics_f

#Plot metrics

traduccion <- c(
  "Class: Eating" = "Adquision",
  "Class: Begging" = "Solicitud",
  "Class: Active_Others" = "Activo general",
  "Class: Resting" = "Descanso"
)

summary_metrics <- summary_metrics %>%
  mutate(Class = recode(Class, !!!traduccion))

summary_metrics_f <- summary_metrics_f %>%
  mutate(Class = recode(Class, !!!traduccion))

recSen_p <- ggplot(summary_metrics, aes(x = Precision_mean, y = Recall_mean, label = Class)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Recall_mean - Recall_sd,
    ymax = Recall_mean + Recall_sd),
    width = 0.01) +
  geom_errorbarh(aes(xmin = Precision_mean - Precision_sd,
    xmax = Precision_mean + Precision_sd),
    height = 0.01) +
  geom_text(vjust = -1, size = 12) +
  labs(
    title = "A) Rendimiento del RF",
    x = NULL,
    y = "Sensibilidad"
  )+   # <---- sin "Precisión"
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
  theme(plot.title=element_text(size=37,face='bold'),
    panel.grid = element_blank(),
    axis.line = element_line(color="black"),
    axis.ticks = element_line(color="black"),
    axis.text = element_text(color="black"),
    axis.title = element_text(color="black"),
    axis.title.y = element_text(size = 40),
    axis.text.x=element_text(colour="black",size=40),
    axis.text.y=element_text(colour="black",size=40)
  )

# --- recSen_f 
recSen_f <- ggplot(summary_metrics_f, aes(x = Precision_mean, y = Recall_mean, label = Class)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Recall_mean - Recall_sd,
    ymax = Recall_mean + Recall_sd),
    width = 0.01) +
  geom_errorbarh(aes(xmin = Precision_mean - Precision_sd,
    xmax = Precision_mean + Precision_sd),
    height = 0.01) +
  geom_text(vjust = -1, size = 12) +
  labs(
    title = " B) Rendimiento del RF posterior a la correccion logica",
    x = NULL,
    y = "Sensibilidad"
  )  +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1),
    labels = function(x) gsub("\\.", ",", format(round(x, 2), nsmall = 2))
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1),
    labels = function(x) gsub("\\.", ",", format(round(x, 2), nsmall = 2))
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title=element_text(size=37,face='bold'),
    axis.text.y = element_text(colour="white",size=40),
    panel.grid = element_blank(),
    axis.line = element_line(color="black"),
    axis.ticks = element_line(color="black"),
    axis.text = element_text(color="black"),
    axis.title = element_text(color="white"),
    axis.text.x=element_text(colour="black",size=40)
  )


recSen_pyf <- recSen_p + recSen_f
recSen_pyf

library(cowplot)
recSen_p <- recSen_p + labs(x = NULL)
recSen_f <- recSen_f + labs(x = NULL)

# unir los gráficos
recSen_pyf <- plot_grid(
  recSen_p, recSen_f,
  ncol = 2,
  align = "v"
)


final_plot <- ggdraw(recSen_pyf) +
  draw_label("Precision", 
    x = 0.5, y = 0.02,   # centrado abajo
    angle = 0, 
    vjust = 1.5, hjust = 0.5, 
    size = 40)+
  theme(plot.margin = margin(b = 30))

final_plot

