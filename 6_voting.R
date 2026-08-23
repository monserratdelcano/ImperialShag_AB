## Apply post-processing rules and perform majority voting for one test individual.
# The same procedure is repeated separately for each test animal.

library(randomForest)
library(datasets)
library(caret)

#Load data
load(file="D:/Punta Leon 2019_2021 y 2022/Analisis se�ales acc pichones/Random Forest analisis/Output/DataParameters_features 1seg_withFrequencies_10062025.RData")

load("D:/Punta Leon 2019_2021 y 2022/Analisis se�ales acc pichones/Random Forest analisis/Output/RF_LOIO_CV_Balanceado_Category_ModaFreq_11062025/rf.rdata")

load("D:/Punta Leon 2019_2021 y 2022/Analisis se�ales acc pichones/Random Forest analisis/Output/RF_LOIO_CV_Balanceado_Category_ModaFreq_11062025/CM.rdata")

load("D:/Punta Leon 2019_2021 y 2022/Analisis se�ales acc pichones/Random Forest analisis/Output/RF_LOIO_CV_Balanceado_Category_ModaFreq_11062025/predichos.rdata")

load("D:/Punta Leon 2019_2021 y 2022/Analisis se�ales acc pichones/Random Forest analisis/Output/RF_LOIO_CV_Balanceado_Category_ModaFreq_11062025/test.rdata")


#Test 1 N5_PA_2022
names(N5)
N5t=test[test$ID_chick=="N5_PA_2022",]
names(N5t)
N5t_f=N5t[,c(3:62)]
names(N5t_f)

#Predicted
N5t_p1= predict(model.rf[[1]],newdata = N5t_f)
N5t_p2= predict(model.rf[[2]],newdata = N5t_f)
N5t_p3= predict(model.rf[[3]],newdata = N5t_f)
N5t_p4= predict(model.rf[[4]],newdata = N5t_f)
N5t_p5= predict(model.rf[[5]],newdata = N5t_f)
N5t_p6= predict(model.rf[[6]],newdata = N5t_f)
N5t_p7= predict(model.rf[[7]],newdata = N5t_f)
N5t_p8= predict(model.rf[[8]],newdata = N5t_f)
N5t_p9= predict(model.rf[[9]],newdata = N5t_f)
N5t_p10= predict(model.rf[[10]],newdata = N5t_f)

confusionMatrix(data=N5t_p10, as.factor(N5t$Category_corr))
confusionMatrix(data=N5t$p10, as.factor(N5t$Category_corr))

N5t$p1=N5t_p1
N5t$p2=N5t_p2
N5t$p3=N5t_p3
N5t$p4=N5t_p4
N5t$p5=N5t_p5
N5t$p6=N5t_p6
N5t$p7=N5t_p7
N5t$p8=N5t_p8
N5t$p9=N5t_p9
N5t$p10=N5t_p10

N5t$Hora_corr=N5$Hora_corr
N5t$Row_number_corr=N5$Row_number_corr


ii=N5t[,c(1,2,63:74)]
names(ii)
head(ii)

#Add event IDs
library(dplyr)

df <- ii %>%
  group_by(ID_chick) %>% 
  mutate(
    evento_real = cumsum(Category_corr != lag(Category_corr, default = first(Category_corr))),
    ep1 = cumsum(p1 != lag(p1, default = first(p1))),
    ep2 = cumsum(p2 != lag(p2, default = first(p2))),
    ep3 = cumsum(p3 != lag(p3, default = first(p3))),
    ep4 = cumsum(p4 != lag(p4, default = first(p4))),
    ep5 = cumsum(p5 != lag(p5, default = first(p5))),
    ep6 = cumsum(p6 != lag(p6, default = first(p6))),
    ep7 = cumsum(p7 != lag(p7, default = first(p7))),
    ep8 = cumsum(p8 != lag(p8, default = first(p8))),
    ep9 = cumsum(p9 != lag(p9, default = first(p9))),
    ep10 = cumsum(p10 != lag(p10, default = first(p10))),
  ) %>%
  ungroup() 


#Agregamos los eventos
df <- ii %>%
  arrange(ID_chick, Row_number_corr) %>%   
  group_by(ID_chick) %>%
  mutate(
    evento_real = cumsum(Category_corr != lag(Category_corr, default = first(Category_corr))),
    across(p1:p10, ~ cumsum(.x != lag(.x, default = first(.x))), .names = "e{col}")
  ) %>%
  ungroup()
head(df)

#Count events
resumen_eventos <- df %>%
  group_by(ID_chick, Category_corr) %>%
  summarise(
    n_bloques_reales = n_distinct(evento_real), 
    .groups = "drop"
  )
resumen_eventos

resumen_p <- df %>%
  group_by(ID_chick) %>%  
  summarise(
    across(starts_with("ep"), 
      ~ n_distinct(.x), 
      .names = "n_{col}"),
    .groups = "drop"
  )
resumen_p


sapply(1:10, function(i) {
  tapply(df[[paste0("ep", i)]], df[[paste0("p", i)]], function(x) length(unique(x)))
})


library(tidyr)

# Reshape predicted event labels and probabilities from wide to long format
df_largo <- df %>%
  select(ID_chick, Row_number_corr, starts_with("ep"), starts_with("p")) %>%
  pivot_longer(
    cols = c(starts_with("ep"), starts_with("p")),
    names_to = c(".value", "num"),
    names_pattern = "(ep|p)(\\d+)"
  )

# Ahora df_largo tiene columnas: ID_chick, Row_number_corr, ep, p, num

# Calculate row_start y row_end for each evento
df2 <- df_largo %>%
  group_by(ID_chick, num, ep, p) %>%
  summarise(
    row_start = first(Row_number_corr),
    row_end   = last(Row_number_corr),
    .groups = "drop"
  ) %>%
  arrange(ID_chick, num, row_start)

df2

unique(df2$num)
tapply(df2$ep, df2$num, function(x) length(unique(x)))

# Calculate the duration of each predicted event based on the number of rows

df2 <- df2 %>%
  group_by(ID_chick) %>%
  mutate(
    Dif_row = row_end - row_start,
    Duration_sec = Dif_row / 50
  ) %>%
  ungroup()


#Apply filter (see script "5_Filter")

library(zoo)

df3 <- df2 %>%
  arrange(ID_chick, num, row_start) %>%  
  group_by(ID_chick, num) %>%          
  mutate(
    p = as.character(p),         
    filtrados = case_when(
      # Eating corto entre Begging → Begging
      p == "Eating" & Duration_sec < 3 &
        lag(p, default = first(p)) == "Begging" &
        lead(p, default = last(p)) == "Begging" ~ "Begging",
      
      # Begging corto entre Eating → Eating
      p == "Begging" & Duration_sec < 3 &
        lag(p, default = first(p)) == "Eating" &
        lead(p, default = last(p)) == "Eating" ~ "Eating",
      
      # Active_Others corto entre Begging → Begging
      p == "Active_Others" & Duration_sec < 4 &
        lag(p, default = first(p)) == "Begging" &
        lead(p, default = last(p)) == "Begging" ~ "Begging",
      
      # Evento muy corto rodeado del mismo tipo → absorber
      Duration_sec < 3 &
        lag(p, default = first(p)) == lead(p, default = last(p)) &
        lag(p, default = first(p)) %in% c("Begging","Eating","Resting") ~ lag(p, default = first(p)),
      
      TRUE ~ p
    )
  ) %>%
  # Recalcular bloques
  mutate(block_id = cumsum(filtrados != lag(filtrados, default = first(filtrados)))) %>%
  group_by(ID_chick, num, block_id) %>%
  mutate(block_duration = sum(Duration_sec)) %>%
  ungroup() %>%
  # Fusionar bloques muy cortos
  mutate(
    filtrados = ifelse(block_duration < 4 &
        lag(filtrados, default = first(filtrados)) == lead(filtrados, default = last(filtrados)),
      lag(filtrados, default = first(filtrados)), filtrados)
  ) %>%
  group_by(ID_chick, num) %>%
  # Ventana móvil para Begging
  mutate(
    window_events = 11,
    prop_begging = rollapply(filtrados == "Begging", width = window_events, FUN = mean, fill = NA, align = "center"),
    
    filtrados = ifelse(!is.na(prop_begging) & prop_begging > 0.8 &
        filtrados == "Active_Others" & Duration_sec < 4, "Begging", filtrados),
    
    filtrados = ifelse(!is.na(prop_begging) & prop_begging > 0.8 &
        filtrados == "Active_Others" & Duration_sec < 2, "Begging", filtrados)
  ) %>%
  ungroup() %>%
  select(-block_id, -block_duration, -window_events, -prop_begging)


#Merge the filtered behavioural labels from each of the 10 predictions into the original data frame

library(rlang)


df_final <- df

for (i in 1:10) {
  ep_col <- paste0("ep", i)   # columna de df
  f_col  <- paste0("f", i)    # columna nueva destino
  
  # armar el by con el nombre correcto
  by_cols <- c("ID_chick" = "ID_chick", setNames("ep", ep_col))
  
  df_final <- df_final %>%
    left_join(
      df3 %>%
        filter(num == i) %>%
        select(ID_chick, ep, filtrados),
      by = by_cols
    ) %>%
    rename(!!f_col := filtrados)
}


# Summarise classification metrics across the 10 filtered predictions
library(purrr)

f_cols <- paste0("f", 1:10)


metricas_list <- map(f_cols, function(col){
  cm <- confusionMatrix(table(df_final[[col]], df_final$Category_corr))
  
  df <- as.data.frame(cm$byClass)
  df$Clase <- rownames(cm$byClass)
  
  # Calcular F1-score por clase
  df$F1 <- 2 * (df$Sensitivity * df$`Pos Pred Value`) / 
    (df$Sensitivity + df$`Pos Pred Value`)
  
  df$pred <- col
  df
})


metricas_df <- bind_rows(metricas_list)


resumen_metricas_filtradas <- metricas_df %>%
  group_by(Clase) %>%
  summarise(across(where(is.numeric),
    list(mean = mean, sd = sd),
    .names = "{.col}_{.fn}"),
    .groups = "drop")
resumen_metricas_filtradas

# Assign unique event IDs to consecutive rows with the same label for each filtered prediction

f_cols <- paste0("f", 1:10)  # columnas f1:f10

df_final <- df_final %>%
  group_by(ID_chick) %>%
  mutate(
    across(
      all_of(f_cols),
      ~ cumsum(.x != lag(.x, default = first(.x))),
      .names = "e{.col}"   # genera ef1, ef2, ..., ef10
    )
  ) %>%
  ungroup()

resumen_f <- df_final %>%
  group_by(ID_chick) %>%  
  summarise(
    across(starts_with("ef"), 
      ~ n_distinct(.x),  
      .names = "n_{col}"),
    .groups = "drop"
  )
resumen_f
sapply(1:10, function(i) {
  tapply(df_final[[paste0("ef", i)]], df_final[[paste0("f", i)]], function(x) length(unique(x)))
})
resumen_eventos
resumen_p

#Votation
#Combine the 10 filtered predictions using majority voting
f_cols <- paste0("f", 1:10)

df_votacion <- df_final  

votacion <- character(nrow(df_final))

for (i in seq_len(nrow(df_final))) {
  
  fila <- unlist(df_final[i, f_cols])
  
  counts <- table(fila)
  max_count <- max(counts)
  candidatos <- names(counts[counts == max_count])
  
  if (length(candidatos) == 1) {
 
    votacion[i] <- candidatos
    
  } else {

    asignado <- NA
    
    # 1. mirar fila anterior
    if (i > 1 && votacion[i-1] %in% candidatos) {
      asignado <- votacion[i-1]
    }
    
  
    if (is.na(asignado) && i < nrow(df_final)) {
      fila_post <- unlist(df_final[i+1, f_cols])
      counts_post <- table(fila_post)
      ganador_post <- names(counts_post[which.max(counts_post)])
      
      if (ganador_post %in% candidatos) {
        asignado <- ganador_post
      }
    }
    

    if (is.na(asignado)) {
      asignado <- candidatos[1]
    }
    
    votacion[i] <- asignado
  }
}

# agregamos al df_final
df_votacion$votacion <- votacion


#Assign unique event IDs to consecutive rows with the same majority-voted behavioural label
df_votacion <- df_votacion %>%
  group_by(ID_chick) %>%
  mutate(
    evento_votacion = cumsum(votacion != lag(votacion, default = first(votacion)))
  ) %>%
  ungroup()
names(df_votacion)

#count events
resumen_votacion <- df_votacion %>%
  group_by(ID_chick, votacion) %>%
  summarise(
    n_eventos = n_distinct(evento_votacion),
    .groups = "drop"
  )

resumen_votacion 
resumen_f
resumen_eventos
resumen_p



#plots
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(grid)
library(gridExtra)

colores1 <- c(
  "Active_Others" = "#446F8C",  # verde azulado
  "Begging"       = "#C4B882",  # coral/naranja
  "Eating"        = "#2C6E3D",  # azul lavanda
  "Resting"       = "#D5A448"   # rosado vibrante
)
#a1=df3[df3$ID_chick=="N1_PA_2022",]+
n1=df_votacion
#n1_2=n1
n1$Time2=seq(1:nrow(n1))
#n1_$predichos=predichos[[1]]
names(df_final)
dim(n1)
n1_2=n1[c(1:90000),] #nos quedamos con 30 minutos para redondear
#Graficos reales 
p1 = ggplot(n1_2, aes(x = Time2, y = Category_corr, fill = Category_corr)) +
  geom_tile(height = 0.7, width = 1) +
  scale_fill_manual(values = colores1) +
  ggtitle("Observado N5_PA") +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 90000),
    breaks = seq(0, 90000, 15000),
    labels = c("0", "5", "10", "15", "20", "25", "30"),
    name = "Minutos (min)"
  ) +
  scale_y_discrete(
    expand = c(0, 0),
    name = "",
    labels = c(
      "Eating"        = "Adquisición",
      "Begging"       = "Solicitud",
      "Active_Others" = "Activo general",
      "Resting"       = "Descanso"
    )
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(family = '', vjust = 0.5, hjust = 0.5, size = 24),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 0.5, color = "white"),
    axis.title.x = element_text(size = 24, color = "white"),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length = unit(4, "pt"),
    plot.margin = ggplot2::margin(0, 2, 0, 0, unit = "cm")
  )

p1



#Grafico con predichos
p2 = ggplot(n1_2, aes(x = Time2, y = p1, fill = p1)) +
  geom_tile(height = 0.7, width = 1) +
  scale_fill_manual(values = colores1) +
  ggtitle("Predichos N5_PA") +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 90000),
    breaks = seq(0, 90000, 15000),
    labels = c("0", "5", "10", "15", "20", "25", "30"),
    name = "Minutos (min)"
  ) +
  scale_y_discrete(
    expand = c(0, 0),
    name = "",
    labels = c(
      "Eating"        = "Adquisición",
      "Begging"       = "Solicitud",
      "Active_Others" = "Activo general",
      "Resting"       = "Descanso"
    )
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(family = '', vjust = 0.5, hjust = 0.5, size = 24),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 0.5, color = "white"),
    axis.title.x = element_text(size = 24, color = "white"),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length = unit(4, "pt"),
    plot.margin = ggplot2::margin(0, 2, 0, 0, unit = "cm")
  )

p2

# Gráfico con filtrados
p3 = ggplot(n1_2, aes(x = Time2, y = f1, fill = f1)) +
  geom_tile(height = 0.7, width = 1) +
  scale_fill_manual(values = colores1) +
  ggtitle("Corrección lógica N5_PA") +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 90000),
    breaks = seq(0, 90000, 15000),
    labels = c("0", "5", "10", "15", "20", "25", "30"),
    name = "Minutos (min)"
  ) +
  scale_y_discrete(
    expand = c(0, 0),
    name = "",
    labels = c(
      "Eating"        = "Adquisición",
      "Begging"       = "Solicitud",
      "Active_Others" = "Activo general",
      "Resting"       = "Descanso"
    )
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(family = '', vjust = 0.5, hjust = 0.5, size = 24),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 24, color = "black"),
    axis.title.x = element_text(size = 24, color = "black"),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length = unit(4, "pt"),
    plot.margin = ggplot2::margin(0, 2, 0, 0, unit = "cm")
  )

p3


#Unimos los reales, predichos y filtrados en un mismo panel

p4 <-plot_grid(p1,p2,p3, scale =1,nrow = 3,align = "v")
p4

#Grafico para la votacion----
p5 = ggplot(n1_2, aes(x = Time2, y = votacion, fill = votacion)) +
  geom_tile(height = 0.7, width = 1) +
  scale_fill_manual(values = colores1) +
  ggtitle("Votación N5_PA") +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 90000),
    breaks = seq(0, 90000, 15000),
    labels = c("0", "5", "10", "15", "20", "25", "30"),
    name = "Minutos (min)"
  ) +
  scale_y_discrete(
    expand = c(0, 0),
    name = "",
    labels = c(
      "Eating"        = "Adquisición",
      "Begging"       = "Solicitud",
      "Active_Others" = "Activo general",
      "Resting"       = "Descanso"
    )
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(family = '', vjust = 0.5, hjust = 0.5, size = 24),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 24),
    axis.text.x = element_text(size = 24, color = "black"),
    axis.title.x = element_text(size = 24, color = "black"),
    axis.line = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.length = unit(4, "pt"),
    plot.margin = ggplot2::margin(0, 2, 0, 0, unit = "cm")
  )

p5



p6=plot_grid(p1,p5, scale =1,nrow = 2,align = "v")
p6


