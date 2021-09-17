library(dplyr)
library(na.tools)
library(dlookr)
library(lubridate)
library(readr)
library(measurements)
library(stats)
library(ggplot2)
library(corrplot)
library(devtools)
library(sjmisc)
library(rpart)
library(rpart.plot)
library(ggpubr)
library(gmodels)
library(ROCR)
library(caret)
library(glmnet)

############### Read the files into a tibble ######################
database<- read_csv("fires2015_train.csv", na= c("NA","", "-"), col_names = TRUE)
datatest<- read_csv("fires2015_test.csv", na= c("NA","", "-"), col_names = TRUE)


database <- mutate(database, region=tolower(region),
                   district=tolower(district),
                   parish=tolower(parish),
                   municipality=tolower(municipality)
)
datatest <- mutate(datatest, region=tolower(region),
                   district=tolower(district),
                   parish=tolower(parish),
                   municipality=tolower(municipality)
)


############### Handle missing values ######################
database %>% any_na()
str(database)
summary(database)
database<- database[-15] #alert_source only has NA's 
datatest<- datatest[-15]

database<- database[complete.cases(database$extinction_date),] # como são apenas 9 casos que correspondem a 0.0012 da base de dados original removemos os casos em que extinction date/hour era NA
database<- database[complete.cases(database$firstInterv_date),] # como acrescentar 207 casos , utilizando por exemplo, a data mais utilizada iria fazer com que essa dita iria ficar muito maior do que anteriormente, por exemplo, 17:00:00 iria fiar com 251 casos o que correspondia a 570% do valor original nessa hora 
database<- database[complete.cases(database$total_area),]


database$firstInterv_hour[is.na(database$firstInterv_hour)] <- "17:00:00"
datatest$firstInterv_hour[is.na(datatest$firstInterv_hour)] <- "17:00:00"

n<- length(database$id)
n_test <- length(datatest$id)

# delete NAs in Region database
for(z in 1:n){
  test=database$region[z]
  municipio=database$municipality[z]
  if(is.na(test)) {
    for(y in 1:n){
      if((isTRUE(tolower(database$municipality[y])==tolower(municipio))) & (!(is.na(database$region[y])))){
        database$region[z]<-database$region[y]
        break
      }
      if(y==n){
        for(p in 1:n){
          if((isTRUE(tolower(database$district[p])==tolower(database$district[z]))) & (!(is.na(database$region[p])))){
            database$region[z]<-database$region[p]
            break
          }
        }
      }
    }
  }
}
# delete NAs in Region datatest
for(z in 1:n_test){
  test=datatest$region[z]
  municipio=datatest$municipality[z]
  if(is.na(test)) {
    for(y in 1:n){
      if((isTRUE(tolower(datatest$municipality[y])==tolower(municipio))) & (!(is.na(datatest$region[y])))){
        datatest$region[z]<-datatest$region[y]
        break
      }
      if(y==n){
        for(p in 1:n){
          if((isTRUE(tolower(datatest$district[p])==tolower(datatest$district[z]))) & (!(is.na(datatest$region[p])))){
            datatest$region[z]<-datatest$region[p]
            break
          }
        }
      }
    }
  }
}

######################################################################################################################


#fire duration
database<- mutate(database,fire_duration = difftime(strptime(paste(database$extinction_date,database$extinction_hour), "%Y-%m-%d %H:%M:%S"),strptime(paste(database$alert_date,database$alert_hour), "%Y-%m-%d %H:%M:%S")))
datatest<- mutate(datatest,fire_duration = difftime(strptime(paste(datatest$extinction_date,datatest$extinction_hour), "%Y-%m-%d %H:%M:%S"),strptime(paste(datatest$alert_date,datatest$alert_hour), "%Y-%m-%d %H:%M:%S")))
database <- filter(database,database$fire_duration >= 0)
n<- length(database$id)
n_test <- length(datatest$id)

temp<- as.factor(binning(as.numeric(database$fire_duration),8,type="kmeans",labels = c("curto","medio curto","medio","medio alto","alto","muito alto","desastroso","catastrófico")))

database <- mutate(database, fire_duration=temp)

temp<-c()
for(x in 1:n_test) {
  if(isTRUE(datatest$fire_duration[x]<6.27e+03)){
    temp[x]="curto"
  } 
  else if(isTRUE(datatest$fire_duration[x]<1.23e+04)){
    temp[x]="medio curto"
  } 
  else if(isTRUE(datatest$fire_duration[x]<2.49e+04)){
    temp[x]="medio"
  } 
  else if(isTRUE(datatest$fire_duration[x]<5.43e+04)){
    temp[x]="medio alto"
  }
  else if(isTRUE(datatest$fire_duration[x]<1.11e+05)){
    temp[x]="alto"
  }
  else if(isTRUE(datatest$fire_duration[x]<2.67e+05)){
    temp[x]="muito alto"
  }
  else if(isTRUE(datatest$fire_duration[x]<7.9e+05)){
    temp[x]="desastroso"
  }
  else{
    temp[x]="catastrófico"
  }
  
}
datatest <- mutate(datatest, fire_duration=temp)

#####################################################################################################################
#burned_area
temp<- as.factor(binning(as.numeric(database$total_area),4,type="kmeans",labels = c("1","2","3","4")))
database <- mutate(database, burned_area=temp)

temp<-c()
for(x in 1:n_test) {
  if(isTRUE(datatest$total_area[x]<=75.3)){
    temp[x]="1"
  } 
  else if(isTRUE(datatest$total_area[x]<=457)){
    temp[x]="2"
  } 
  else if(isTRUE(datatest$total_area[x]<=1.38e+03)){
    temp[x]="3"
  } 
  else {
    temp[x]="4"
  }
}

datatest$burned_area = temp

####################################################################################################################

#vegetation burned

temp<- as.factor(binning(as.numeric(database$vegetation_area),4,type="kmeans",labels = c("1","2","3","4")))
database <- mutate(database, vegetation_burned=temp)


temp<-c()
for(x in 1:n_test) {
  if(isTRUE(datatest$vegetation_area[x]<=82.8)){
    temp[x]="1"
  } 
  else if(isTRUE(datatest$vegetation_area[x]<=460)){
    temp[x]="2"
  } 
  else if(isTRUE(datatest$vegetation_area[x]<=1.58e+03)){
    temp[x]="3"
  } 
  else {
    temp[x]="4"
  }
}
datatest$vegetation_burned = temp


####################################################################################################################



#size
n<- length(database$id)
n_test <- length(datatest$id)


#firebefore
list<- c()
for(x in 1:n){
  for(y in 1:n){
    if(isTRUE((database$parish[x]==database$parish[y]) & (database$municipality[x]==database$municipality[y]))){
      tempo<-difftime(strptime(paste(database$alert_date[x],database$alert_hour[x]), "%Y-%m-%d %H:%M:%S"),strptime(paste(database$extinction_date[y],database$extinction_hour[y]), "%Y-%m-%d %H:%M:%S"),units = "hours")
      if(x != y & tempo<=36 & tempo >2){
        list[x]<-1
      }
    }
    if(is_empty(list[x])){
      list[x]<-0
    }
  }
}




database<-mutate(database,firebefore=list)


list<- c()
for(x in 1:n_test){
  for(y in 1:n_test){
    if(isTRUE((datatest$parish[x]==datatest$parish[y]) & (datatest$municipality[x]==datatest$municipality[y]))){
      tempo<-difftime(strptime(paste(datatest$alert_date[x],datatest$alert_hour[x]), "%Y-%m-%d %H:%M:%S"),strptime(paste(datatest$extinction_date[y],datatest$extinction_hour[y]), "%Y-%m-%d %H:%M:%S"),units = "hours")
      if(isTRUE(x != y & tempo<=36 & tempo >2)){
        list[x]<-1
      }
    }
    if(is_empty(list[x])){
      list[x]<-0
    }
  }
}

datatest<-mutate(datatest,firebefore=list)


####################################################################################################################


#Get Temperatures
source("getTemperatureNOAA.R")
#round numbers function
round.to <- function(x, b) {
  round(x/b)*b
}

for(x in 1:n) {
  database$lat[x]<-(ConvUnit(database$lat[x]))
  database$lon[x]<-(ConvUnit(database$lon[x]))
}
for(x in 1:n_test) {
  datatest$lat[x]<-(ConvUnit(datatest$lat[x]))
  datatest$lon[x]<-(ConvUnit(datatest$lon[x]))
}

temp<-c()
districts<-unique(select(database, district))
temps <- as.data.frame(matrix(ncol=19, nrow=365))
for(x in 1:18){
  colnames(temps)[x] = districts$district[x]
  case<-database %>% filter(district==districts$district[x] , !is.na(lat) , !is.na(lon)) %>% slice(1)
  print(case$id)
  temperatures<-getTemp(case$district,case$lat,case$lon,case$alert_date)
  temp<-temperatures[["tmax"]][["tmax"]]
  size<-365-length(temp)
  if(length(temp)!=365){
    for(y in 1:size){
      temp<-append(temp,NA)
    }
  }
  temps[x]<-temp
}
dates<-temperatures[["tmax"]][["date"]]
colnames(temps)[19] = "dates"
temps[19]<-dates

temp<-c()
for(x in 1:18){
  for(y in 1:365){
    if(is.na(temps[[x]][[y]])){
      temps[[x]][[y]]=round.to(mean(as.numeric(select(slice(temps,y:y),1:18)),na.rm=TRUE),1)
    }
  }
}

for(x in 1:n){
  temp[x]=as.numeric(filter(temps, dates==database$alert_date[x]) %>% select(database$district[x]))
}
database$TMAX<-temp 

temp<-c()
for(x in 1:n_test){
  temp[x]=as.numeric(filter(temps, dates==datatest$alert_date[x]) %>% select(datatest$district[x]))
}
datatest$TMAX<-temp


######################################################################################################################


temp <- c()

#Daytime
for(x in 1:n) {
  if(isTRUE(database$alert_hour[x]<as.difftime("06:00:00"))){
    temp[x]="Madrugada"
  } 
  else if(isTRUE(database$alert_hour[x]<as.difftime("12:00:00"))){
    temp[x]="Manhã"
  } 
  else if(isTRUE(database$alert_hour[x]<as.difftime("18:00:00"))){
    temp[x]="Tarde"
  } 
  else if(isTRUE(database$alert_hour[x]<=as.difftime("23:59:59"))){
    temp[x]="Noite"
  } 
}
database <- mutate(database, daytime=temp)

temp<-c()
for(x in 1:n_test) {
  if(isTRUE(datatest$alert_hour[x]<as.difftime("06:00:00"))){
    temp[x]="Madrugada"
  } 
  else if(isTRUE(datatest$alert_hour[x]<as.difftime("12:00:00"))){
    temp[x]="Manhã"
  } 
  else if(isTRUE(datatest$alert_hour[x]<as.difftime("18:00:00"))){
    temp[x]="Tarde"
  } 
  else if(isTRUE(datatest$alert_hour[x]<=as.difftime("23:59:59"))){
    temp[x]="Noite"
  } 
}
datatest <- mutate(datatest, daytime=temp)

######################################################################################################################

temp <- c()
# month
for(x in 1:n) {
  months = month(database$alert_date[x])
  temp[x] = months
}
tempx=cut(temp,4, labels =c("1º trimestre","2º trimestre","3º trimestre","4º trimestre"))
database <- mutate(database, alert_month = tempx)
database <- relocate(database,alert_month,.after=8)


temp <- c()

for(x in 1:n_test) {
  months = month(datatest$alert_date[x])
  temp[x] = months
}
tempx=cut(temp,4, labels =c("1º trimestre","2º trimestre","3º trimestre","4º trimestre"))
datatest <- mutate(datatest, alert_month = tempx)
datatest <- relocate(datatest, alert_month, .after=8)



######################################################################################################################

#TMAX 
temp<- as.factor(binning(as.numeric(database$TMAX),4,type="kmeans",labels = c("1","2","3","4")))
database <- mutate(database, TMAX=temp)

temp<-c()
for(x in 1:n_test) {
  if(datatest$TMAX[x]<=as.numeric("190")){
    temp[x]="1"
  } 
  else if(datatest$TMAX[x]<=as.numeric("252")){
    temp[x]="2"
  } 
  else if(datatest$TMAX[x]<=as.numeric("306")){
    temp[x]="3"
  } 
  else{
    temp[x]="4"
  }  
}
datatest$TMAX<-temp

##################################################################################


#######   dimenseonality reduction and data organization   ######################

database <- relocate (database, burned_area, .before = 21)
database <- relocate (database, daytime, .before = 21)
database <- relocate (database, firebefore, .before = 21)
database <- relocate (database, fire_duration, .before = 21)
database <- relocate (database, vegetation_burned, .before = 21)
database <- relocate(database,TMAX,.after=9)

datatest <- relocate (datatest, TMAX, .after = 9)
datatest <- relocate (datatest, vegetation_burned, .after = 21)
datatest <- relocate (datatest, fire_duration, .after = 22)
datatest <- relocate (datatest, firebefore, .after = 23)
datatest <- relocate (datatest, daytime, .after = 24)
datatest <- relocate (datatest, burned_area, .after = 25)


information <- prcomp(database[c(17:21)], center = TRUE, scale = TRUE)
summary(information)
str(information)
database <- database[- c(20,21)] # apagar total_area e farming_village_ares
datatest <- datatest[- c(20,21)] 

#database <- database [-c(11:13,15)]
#datatest <- datatest [-c(11:13,15)] # apagar variaveis continuas que foram anteriormente transportadas para novas variaveis categoricas

datatest$TMAX <- as.factor(datatest$TMAX)
datatest$vegetation_burned <- as.factor(datatest$vegetation_burned)
datatest$fire_duration <- as.factor(datatest$fire_duration)
datatest$burned_area <- as.factor(datatest$burned_area)



###### DATA EXPLORATORY ANALYSIS ########

CrossTable(database$daytime,database$cause_type,expected=TRUE)
CrossTable(database$TMAX,database$cause_type,expected=TRUE)
CrossTable(database$alert_month,database$cause_type,expected=TRUE)
CrossTable(database$region,database$cause_type,expected=TRUE)
CrossTable(database$origin,database$cause_type,expected=TRUE)
CrossTable(database$fire_duration,database$cause_type,expected=TRUE)
CrossTable(database$firebefore,database$cause_type,expected=TRUE)
CrossTable(database$burned_area,database$cause_type,expected=TRUE)
CrossTable(database$vegetation_burned,database$cause_type,expected=TRUE)


ggplot(database, aes(x =cause_type))+geom_bar(aes(fill = daytime))+facet_wrap(~alert_month)
ggplot(database, aes(x =cause_type))+geom_bar(aes(fill = TMAX))+facet_wrap(~alert_month)
ggplot(database, aes(x =cause_type))+geom_bar(aes(fill = origin))+facet_wrap(~alert_month)
ggplot(database, aes(x =cause_type))+geom_bar(aes(fill = fire_duration))+facet_wrap(~alert_month)
ggplot(database, aes(x =cause_type))+geom_bar(aes(fill = firebefore))+facet_wrap(~alert_month)
ggplot(database, aes(x =cause_type))+geom_bar(aes(fill = vegetation_burned))+facet_wrap(~alert_month)
ggplot(database, aes(x =cause_type, y=vegetation_area)) + geom_boxplot()
ggplot(database, aes(x =cause_type, y=farming_area)) + geom_boxplot()
ggplot(database, aes(x =cause_type, y=village_area)) + geom_boxplot()
ggplot(database, aes(x =cause_type))+geom_bar(aes(fill = district))
#######  Modelling ######



database <- database[-c(1,5:7,11:19)] #reduzimos variaveis que atravês da data visualization considerarmos não ter imortância
treedatabase <- rpart(cause_type ~ .,database[-3])
rpart.plot(treedatabase)
prp(treedatabase)
prediction <- predict(treedatabase, datatest, type = "class")


############## Datatest prediction ################
id <- c(7512:10729)

submission <- data.frame("id" = id, "cause_type" = prediction)






