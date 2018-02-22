
#data nyc taxi 
#lubridate extensively used
library(ggplot2)

TaxiTrainData = read.csv(file.choose())
TaxiTestData = read.csv(file.choose())
library(stringr)

str(TrainTax_Mut)
str(TaxiTestData)
TrainTax_Mut = TaxiTrainData
TestTax_Mut = TaxiTestData
library(lubridate)
library(dplyr)
library(ggmap)

TrainTax_Mut = TrainTax_Mut %>% select(-c(id))
TestTax_Mut = TestTax_Mut %>% select(-c(id))

library(lubridate)
?ymd_hms

TrainTax_Mut$DropTimeInDate = ymd_hms(TrainTax_Mut$dropoff_datetime)
TestTax_Mut$DropTimeInDate = ymd_hms(TestTax_Mut$dropoff_datetime)


?select
colnames(TrainTax_Mut)
colnames(TestTax_Mut)
TrainTax_Mut$InDate = ymd_hms(TrainTax_Mut$pickup_datetime)
TestTax_Mut$InDate = ymd_hms(TestTax_Mut$pickup_datetime)

class(TrainTax_Mut$InDate)
?date
date(head(TrainTax_Mut$InDate))

# get date
TrainTax_Mut$Date = date(TrainTax_Mut$InDate)
TestTax_Mut$Date = date(TestTax_Mut$InDate)


TrainTax_Mut$Day = day(TrainTax_Mut$InDate)
TestTax_Mut$Day = day(TestTax_Mut$InDate)
head(TrainTax_Mut$Day)

# get monday tuesday
TrainTax_Mut$WeekDay = wday(TrainTax_Mut$InDate,label = TRUE) 
TestTax_Mut$WeekDay = wday(TestTax_Mut$InDate,label = TRUE)

head(TrainTax_Mut$WeekDay)
?wday
?mday
mday(head(TrainTax_Mut$InDate))

# get hours 
TrainTax_Mut$Hour = hour(TrainTax_Mut$InDate)
TestTax_Mut$Hour = hour(TestTax_Mut$InDate)
head(TrainTax_Mut$Hour)
?hour
?duration

# calculate the time differec betweeb pick up and drop
TrainTax_Mut$TimeDiff = difftime(TrainTax_Mut$DropTimeInDate,TrainTax_Mut$InDate)
colnames(TrainTax_Mut)
colnames(TestTax_Mut)
?rename()

# rename variables accordingly 
TrainTax_Mut = TrainTax_Mut %>% rename(PickupDate = Date, PickupDay = Day ,PickupWDay = WeekDay ,PickupHour = Hour)
TestTax_Mut = TestTax_Mut %>% rename(PickupDate = Date, PickupDay = Day ,PickupWDay = WeekDay ,PickupHour = Hour)

colnames(TrainTax_Mut)


#calculate distaance and average speed as duration alone would depend on distance travelled would be wrong metric
?weekdays
?mutate
library(geosphere)
distm (c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
plot1 = head(TrainTax_Mut) %>% ggplot(aes(x= PickupHour,y = AvgSpeed))+ geom_bar()
str(TrainTax_Mut)
plot1
###bin to office time and non office time since NYC is a financial capital
# most of the people are office going
TrainTax_Mut = mutate(TrainTax_Mut,Office_NonOffice =if_else(PickupHour < 9 | PickupHour > 17 ,"Office hours","Non-Office hours" ))
TestTax_Mut = mutate(TestTax_Mut,Office_NonOffice =if_else(PickupHour < 9 | PickupHour > 17 ,"Office hours","Non-Office hours" )) 

TestTax_Mut = TestTax_Mut %>% rename(pickUpDateObj = InDate)
colnames(TrainTax_Mut)
colnames(TestTax_Mut)

str(TrainTax_Mut)
str(TestTax_Mut)
?strptime
?time
class(TrainTax_Mut$pickUpDateObj)
TrainTax_Mut$pickUpDateObj
summary(TrainTax_Mut)
?if_else


?mutate
TestTax_Mut = TestTax_Mut %>% mutate(Office_NonOffice = as.factor(Office_NonOffice))
TrainTax_Mut$Office_NonOffice = as.factor(TrainTax_Mut$Office_NonOffice)
str(TrainTax_Mut)
### dealing with the problem
TrainTax_Mut = select(TrainTax_Mut,-PickupTime)

TrainTax_Mut$TimeDiff
head(TrainTax_Mut)
head(TestTax_Mut)
###calculating average speed of  the taxi during these hours
TrainTax_Mut = mutate(TrainTax_Mut, AvgSpeed = TripDistance/trip_duration)
ggplot(TrainTax_Mut,mapping = aes(x=Office_NonOffice,y= AvgSpeed))+geom_bar()
tapply(TrainTax_Mut$AvgSpeed,TrainTax_Mut$Office_NonOffice,FUN = mean)
str(TrainTax_Mut)

# grep("..:..:..",head(as.character(TrainTax_Mut$pickup_datetime)),value = TRUE)
 
times>strptime("00:43:35",format = "%H:%M:%S")
times = strptime(str_extract(head(as.character(TrainTax_Mut$pickup_datetime)),"..:..:.."),format = "%H:%M:%S")
TrainTax_Mut$PickUpTimeInSeconds = seconds(hms(str_extract(as.character(TrainTax_Mut$pickup_datetime),"..:..:..")))
TestTax_Mut$PickUpTimeInSeconds = seconds(hms(str_extract(as.character(TestTax_Mut$pickup_datetime),"..:..:..")))


# ?grepl

TrainTax_Mute$PickUpTimeInSeconds = 

# y = strptime("12:01:59.23",format = "%H:%M:%S" )

# x <- hms("12:01:59.23")
seconds(x)

?format
?cut
?bin
?vapply
?mapply
?distm

# calculating distance for each row 
TrainTax_Mut$TripDistance = distHaversine(select(TrainTax_Mut,pickup_longitude,pickup_latitude),
      select(TrainTax_Mut,dropoff_longitude,dropoff_latitude))

TestTax_Mut$TripDistance = distHaversine(select(TestTax_Mut,pickup_longitude,pickup_latitude),
                                          select(TestTax_Mut,dropoff_longitude,dropoff_latitude))


str(TrainTax_Mut)
str(TestTax_Mut)
?mapdist

TrainTax_Mut = TrainTax_Mut %>% mutate(PickUpMonth = month(pickUpDateObj))
TestTax_Mut = TestTax_Mut %>% mutate(PickUpMonth = month(pickUpDateObj))


write.csv(TrainTax_Mut,file = "C:\\Users\\ntihish\\Documents\\Analytics Training Institute\\Base analytics\\Base Analyticcs practice\\Kaggle NYC Taxi data\\TrainClean.csv")
write.csv(TestTax_Mut,file = "C:\\Users\\ntihish\\Documents\\Analytics Training Institute\\Base analytics\\Base Analyticcs practice\\Kaggle NYC Taxi data\\TestClean.csv")


####selecting required variables
FinalTrainTaxi = select(TrainTax_Mut,c(vendor_id,passenger_count,store_and_fwd_flag,PickUpMonth,PickupDay,PickupWDay,PickupHour,Office_NonOffice,TripDistance,PickUpTimeInSeconds,trip_duration))
head(FinalTrainTaxi)
FinalTestTaxi = select(TestTax_Mut,c(vendor_id,passenger_count,store_and_fwd_flag,PickUpMonth,PickupDay,PickupWDay,PickupHour,Office_NonOffice,TripDistance,PickUpTimeInSeconds))
head(FinalTestTaxi)
?as.ordered
str(FinalTrainTaxi)
str(FinalTestTaxi)


### converting to factors and numeric
FinalTrainTaxi = FinalTrainTaxi %>% mutate(vendor_id = as.factor(vendor_id),
                                           passenger_count = as.ordered(passenger_count),
                                           PickUpTimeInSeconds = as.numeric(PickUpTimeInSeconds),
                                           PickUpMonth = as.factor(PickUpMonth)
                                           
                                    
                                           )

FinalTestTaxi = FinalTestTaxi %>% mutate( vendor_id = as.factor(vendor_id),
                                          passenger_count = as.ordered(passenger_count),
                                          PickUpTimeInSeconds = as.numeric(PickUpTimeInSeconds),
                                          PickUpMonth = as.factor(PickUpMonth)
                                         )

#


#plot avg speed vs time of the day 
ggplot(someTab,mapping = aes(x=PickupHour,y=HWiseAvgSpeed))+geom_line()
colnames(TrainTax_Mut)
someTab = TrainTax_Mut%>%group_by(PickupHour)%>%summarise(HWiseAvgSpeed=mean(AvgSpeed))
someTab = as.data.frame(someTab)
someTab


# mapdist(from = select(TrainTax_Mut,pickup_longitude,pickup_latitude), to =select(TrainTax_Mut,dropoff_longitude,dropoff_latitude)  )
sampleDf = head(TrainTax_Mut)
# sampleDf %>% mutate(pickupDateAgain = as.Date(pickup_datetime))
# duration(head(TrainTax_Mut$TimeDiff),units = "minutes")

# class(TrainTax_Mut$TimeDiff)

##### caret package 


library(caret)
set.seed(825)

index = createDataPartition(FinalTrainTaxi$trip_duration,p = 0.7,list = FALSE,times = 1)
# nrow(FinalTrainTaxi)
# nrow(index)

FinalTrainTaxi_Train = FinalTrainTaxi[index,]
FinalTrainTaxi_Test = FinalTrainTaxi[-index,]


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)



TaxigbmFit1 <- train(trip_duration ~ ., data = FinalTrainTaxi_Train, 
                 method = "gbm", 
                 trControl = fitControl,
                 
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)

saveRDS(TaxigbmFit1,"C:\\Users\\ntihish\\Documents\\Analytics Training Institute\\Base analytics\\Base Analyticcs practice\\Kaggle NYC Taxi data\\gbmModel.rds")


TaxigbmFit1

FinalTrainTaxi_Test$PredValues = predict(TaxigbmFit1,newdata = FinalTrainTaxi_Test)

R2(pred = FinalTrainTaxi_Test$PredValues,obs = FinalTrainTaxi_Test$trip_duration)

names(FinalTrainTaxi)

# readRDS()
length(unique(FinalTrainTaxi_Train$PickUpTimeInSeconds))
nrow(FinalTestTaxi)
FinalTestTaxi$predValues = predict(TaxigbmFit1,newdata = FinalTestTaxi)
write.csv(SubmitDF,file = "C:\\Users\\ntihish\\Documents\\Analytics Training Institute\\Base analytics\\Base Analyticcs practice\\Kaggle NYC Taxi data\\FinalSubmission.csv",row.names = FALSE)
SubmitDF = cbind(TaxiTestData$id,FinalTestTaxi$predValues)
?cbind
SubmitDF = bind_cols(as.data.frame(TaxiTestData$id),as.data.frame(FinalTestTaxi$predValues))
class(FinalTestTaxi$predValues)
class(TaxiTestData$id)
bind_cols(TaxiTestData$id,FinalTestTaxi$predValues)
nrow(SubmitDF)
