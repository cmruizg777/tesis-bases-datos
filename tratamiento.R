#se carga la base de datos
train = read.csv('trainning.csv')

#se elimina las columnas innecesarias
deleted_columns <- c("id","luz", "nivel_id", "fecha")
trainning <- train[ , !(names(train) %in% deleted_columns)]
View(trainning)
summary(trainning)

library(caret)
process <-preProcess(as.data.frame(trainning), method=c("range"))

norm_scale <- predict(process, as.data.frame(trainning))


#se normaliza la horas de luz
hmax = 24;
hmin = 0;
trainning[trainning$horas_luz<24,1] = round((trainning[trainning$horas_luz<24,1]-hmin)/(hmax-hmin),2)
View(trainning)

#se normaliza la temperatura
tmax= 30;
tmin = 0;
trainning[trainning$temperatura<30,2] = round((trainning[trainning$temperatura<tmax,2]-tmin)/(tmax-tmin),2)
View(trainning)

#se normaliza la humedad relativa
hum_max= 100;
hum_min = 0;
trainning[trainning$humedad<100,3] = round((trainning[trainning$temperatura<hum_max,3]-hum_min)/(hum_max-hum_min),2)
View(trainning)

#se normaliza la humedad suelo
hums_max= 1024;
hums_min = 0;
trainning[trainning$humedad_suelo<1024, 4] = round((trainning[trainning$humedad_suelo<1024, 4]-hums_min)/(hums_max-hums_min),2)
View(trainning)

#eliminando filas duplicadas
trainning = trainning[!duplicated(trainning), ]

#guardando los datos en un nuevo archivo
write.csv(trainning, 'normalized.csv', row.names = FALSE)
View(trainning)



train = read.csv('normalized.csv')
summary(train)
train[train$horas_luz<24,1] <- as.integer(train[train$horas_luz<24,1])
View(train)
View(train)
train[train$temperatura<30,2] <- as.integer(train[train$temperatura<30,2])
train[train$humedad_suelo<1024,4] = train[train$humedad_suelo<1024,4]/100;
train[train$humedad_suelo<1024,4] <- as.integer(train[train$humedad_suelo<1024,4])
train = train[!duplicated(train), ]
train = read.csv('entrenamiento.csv')
train[train$humedad>90,3]=train[train$humedad>90,3] - 7
write.csv(train, 'trainning.csv', row.names = FALSE)
train = read.csv('entrenamiento.csv')
train[train$humedad>90,3]=train[train$humedad>90,3] - 7
write.csv(train, 'trainning.csv', row.names = FALSE)
train = read.csv('trainning.csv')
train[train$horas_luz<24,1] <- as.integer(train[train$horas_luz<24,1])
train[train$temperatura<30,2] <- as.integer(train[train$temperatura<30,2])
train[train$humedad_suelo<1024,4] = train[train$humedad_suelo<1024,4]/100;
train[train$humedad_suelo<1024,4] <- as.integer(train[train$humedad_suelo<1024,4])
train[train$humedad_suelo<1024,4] = train[train$humedad_suelo<1024,4]*100;
train = train[!duplicated(train), ]
write.csv(train, 'reduced.csv', row.names = FALSE)
plot(train)
summary(train)
View(train)
sumary(train)
summary(train)
