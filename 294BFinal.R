library("dplyr")  #for dplyr
library("RSQLite") #for sqllite
library("nycflights13") #for data
library("ggplot2")
my_db <- src_sqlite("my_db.sqlite10", create = T)
flights_sqlite <- copy_to(
  my_db, flights, temporary = FALSE, 
  indexes = list(
    c("year", "month", "day"), 
    "carrier", 
    "tailnum")
)

airlines_sqlite <- copy_to(
  my_db, airlines, temporary = FALSE, 
  indexes = list("carrier")
)

airports_sqlite <- copy_to(
  my_db, airports, temporary = FALSE, 
  indexes = list("faa")
)

planes_sqlite <- copy_to(
  my_db, planes, temporary = FALSE, 
  indexes = list("tailnum")
)

weather_sqlite <- copy_to(
  my_db, weather, temporary = FALSE, 
  indexes = list(
    c("year", "month","day","hour"),
    "origin")
)
nycflights13_sqlite()
my_db
flights_sqlite <- tbl(my_db, "flights")
flights_sqlite
airliness_sqlite <- tbl(my_db, "airlines")
airliness_sqlite
airports_sqlite <- tbl(my_db, "airports")
airports_sqlite
planes_sqlite <- tbl(my_db, "planes")
planes_sqlite
weather_sqlite <- tbl(my_db, "weather")




flights_weather <- inner_join (
  tbl(my_db, "flights"),
  tbl(my_db, "weather"),
  by = c("year","month","day","hour")) %>%
  collect() 
fw<-data.frame(flights_weather)
D1<-(lm(flights_weather$dep_delay~flights_weather$temp+
             flights_weather$dewp+flights_weather$humid+
             flights_weather$wind_speed+flights_weather$pressure))
summary(D1)

D2<-(lm(flights$dep_delay~flights$year+ flights$month+
           flights$day+flights$hour))
flights_airports<- inner_join (
  tbl(my_db, "flights"),
  tbl(my_db, "airports"),
  by = c("dest" = "faa")) %>%
  collect() 

D3<-(lm(flights_airports$dep_delay~flights_airports$lat+
             flights_airports$lon+flights_airports$alt))

flights_planes <- inner_join (
  tbl(my_db, "flights"),
  tbl(my_db, "planes"),
  by = "tailnum") %>%
  collect() 
D4<- (lm(flights_planes$dep_delay~
             flights_planes$engines+flights_planes$engine))
             
             
             dd<-data.frame(planes)
             
flights_planes <- inner_join (
               tbl(my_db, "flights"),
               tbl(my_db, "planes"),
               by = "tailnum") %>%
               collect()             
             
             
             
flights= tbl(my_db, "flights") %>% 
               collect() %>%
               mutate(canceled = is.na(arr_time))
              
             
canceled.weather<-flights  %>% 
  inner_join(weather)             

C1<-(glm(canceled.weather$canceled~canceled.weather$temp+
             canceled.weather$dewp+canceled.weather$humid+
             canceled.weather$wind_speed+canceled.weather$pressure,family="binomial"(link="logit")))

C2<-(glm(flights$canceled~flights$year+ flights$month+
             flights$day+flights$hour,family="binomial"(link="logit")))



canceled.airports<-flights  %>% 
  inner_join(airports,by=c("dest" = "faa"))

C3<-(glm(canceled.airports$canceled~canceled.airports$lat+
              canceled.airports$lon+canceled.airports$alt,family="binomial"(link="logit")))



canceled.planes<-flights  %>% 
  inner_join(planes,"tailnum")

C4<-(glm(canceled.planes$canceled~
               canceled.planes$engines+canceled.planes$engine,family="binomial"(link="logit")))

summary(C4)


             
               
             
