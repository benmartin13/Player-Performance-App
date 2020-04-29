#Data Cleaning and Prep, Model Creation
load(file = "statcast_data.Rdata")
sc <- statcast_bind
head(sc)

sc <- sc %>% 
  filter(pitch_type != "null")

br2016 <- daily_batter_bref(t1="2016-01-01", t2 = "2016-12-01")
br2017 <- daily_batter_bref(t1="2017-01-01", t2 = "2017-12-01")
br2018 <- daily_batter_bref(t1="2018-01-01", t2 = "2018-12-01")
br2019 <- daily_batter_bref(t1="2019-01-01", t2 = "2019-12-01")
bref <- rbind(br2016, br2017, br2018, br2019)

woba2016 <- woba_plus(br2016)
woba2017 <- woba_plus(br2017)
woba2018 <- woba_plus(br2018)
woba2019 <- woba_plus(br2019)

sc2016 <- sc%>%
  filter(game_date > "2016-01-01" & game_date < "2016-12-01")

sc2017 <- sc%>%
  filter(game_date > "2017-01-01" & game_date < "2017-12-01")


sc2018 <- stat2018 %>% 
  filter(pitch_type != "null")

sc2019 <- stat2019 %>% 
  filter(pitch_type != "null")



sc2016bip <- sc2016 %>% 
  filter(description == "hit_into_play" | description == "hit_into_play_out" | description =="hit_into_play_score"
         | description == "hit_into_play_no_out")

sc2017bip <- sc2017 %>% 
  filter(description == "hit_into_play" | description == "hit_into_play_out" | description =="hit_into_play_score"
         | description == "hit_into_play_no_out")

sc2018bip <- sc2018 %>% 
  filter(description == "hit_into_play" | description == "hit_into_play_out" | description =="hit_into_play_score"
         | description == "hit_into_play_no_out")

sc2019bip <- sc2019 %>% 
  filter(description == "hit_into_play" | description == "hit_into_play_out" | description =="hit_into_play_score"
         | description == "hit_into_play_no_out")

##2016
plyr::count(sc2016, vars = "batter") -> batters2016

qualified_hitters2016 <- batters2016 %>% 
  filter(freq > 500)

num_batters2016 <- nrow(batters2016)

for(i in 1:num_batters2016){
  temp <- sc2016bip %>% 
    filter(batter == qualified_hitters2016$batter[i])
  
  temphh <- temp %>% 
    filter(launch_speed > 95)
  
  qualified_hitters2016$hhrate[i] <- nrow(temphh)/nrow(temp)
  qualified_hitters2016$la_hh[i] <- mean(temphh$launch_angle)
  qualified_hitters2016$std_la[i] <- sd(temp$launch_angle)
  qualified_hitters2016$maxEV[i] <- max(na.omit(temp$launch_speed))
  qualified_hitters2016$avgEV[i] <- mean(na.omit(temp$launch_speed))
}

for(i in 1:num_batters2016){
  playername_lookup(qualified_hitters2016$batter[i])[,1] -> qualified_hitters2016$name_first[i]
  playername_lookup(qualified_hitters2016$batter[i])[,2] -> qualified_hitters2016$name_last[i]
  playername_lookup(qualified_hitters2016$batter[i])[,11] -> qualified_hitters2016$fangraphs[i]
  playername_lookup(qualified_hitters2016$batter[i])[,6] -> qualified_hitters2016$birth_year[i]
  playername_lookup(qualified_hitters2016$batter[i])[,7] -> qualified_hitters2016$rookie_year[i]
}

qualified_hitters2016$fullname <- with(qualified_hitters2016, paste(name_first, name_last))

qualified_hitters2016$year <- 2016

as.numeric(qualified_hitters2016$birth_year) -> qualified_hitters2016$birth_year
as.numeric(qualified_hitters2016$rookie_year) -> qualified_hitters2016$rookie_year

qualified_hitters2016$year - qualified_hitters2016$birth_year -> qualified_hitters2016$age
(qualified_hitters2016$year - qualified_hitters2016$rookie_year) +1 -> qualified_hitters2016$tenure

##2017
plyr::count(sc2017, vars = "batter") -> batters2017

qualified_hitters2017 <- batters2017 %>% 
  filter(freq > 500)

num_batters2017 <- nrow(batters2017)

for(i in 1:num_batters2017){
  temp <- sc2017bip %>% 
    filter(batter == qualified_hitters2017$batter[i])
  
  temphh <- temp %>% 
    filter(launch_speed > 95)
  
  qualified_hitters2017$hhrate[i] <- nrow(temphh)/nrow(temp)
  qualified_hitters2017$la_hh[i] <- mean(temphh$launch_angle)
  qualified_hitters2017$std_la[i] <- sd(temp$launch_angle)
  qualified_hitters2017$maxEV[i] <- max(na.omit(temp$launch_speed))
  qualified_hitters2017$avgEV[i] <- mean(na.omit(temp$launch_speed))
  
}

for(i in 1:num_batters2017){
  playername_lookup(qualified_hitters2017$batter[i])[,1] -> qualified_hitters2017$name_first[i]
  playername_lookup(qualified_hitters2017$batter[i])[,2] -> qualified_hitters2017$name_last[i]
  playername_lookup(qualified_hitters2017$batter[i])[,11] -> qualified_hitters2017$fangraphs[i]
  playername_lookup(qualified_hitters2017$batter[i])[,6] -> qualified_hitters2017$birth_year[i]
  playername_lookup(qualified_hitters2017$batter[i])[,7] -> qualified_hitters2017$rookie_year[i]
}

qualified_hitters2017$fullname <- with(qualified_hitters2017, paste(name_first, name_last))

qualified_hitters2017$year <- 2017

as.numeric(qualified_hitters2017$birth_year) -> qualified_hitters2017$birth_year
as.numeric(qualified_hitters2017$rookie_year) -> qualified_hitters2017$rookie_year

qualified_hitters2017$year - qualified_hitters2017$birth_year -> qualified_hitters2017$age
(qualified_hitters2017$year - qualified_hitters2017$rookie_year) +1 -> qualified_hitters2017$tenure

##2018
plyr::count(sc2018, vars = "batter") -> batters2018

qualified_hitters2018 <- batters2018 %>% 
  filter(freq > 500)

num_batters2018 <- nrow(batters2018)

for(i in 1:num_batters2018){
  temp <- sc2018bip %>% 
    filter(batter == qualified_hitters2018$batter[i])
  
  temphh <- temp %>% 
    filter(launch_speed > 95)
  
  qualified_hitters2018$hhrate[i] <- nrow(temphh)/nrow(temp)
  qualified_hitters2018$la_hh[i] <- mean(temphh$launch_angle)
  qualified_hitters2018$std_la[i] <- sd(temp$launch_angle)
  qualified_hitters2018$maxEV[i] <- max(na.omit(temp$launch_speed))
  qualified_hitters2018$avgEV[i] <- mean(na.omit(temp$launch_speed))
  
}

for(i in 1:num_batters2018){
  playername_lookup(qualified_hitters2018$batter[i])[,1] -> qualified_hitters2018$name_first[i]
  playername_lookup(qualified_hitters2018$batter[i])[,2] -> qualified_hitters2018$name_last[i]
  playername_lookup(qualified_hitters2018$batter[i])[,11] -> qualified_hitters2018$fangraphs[i]
  playername_lookup(qualified_hitters2018$batter[i])[,6] -> qualified_hitters2018$birth_year[i]
  playername_lookup(qualified_hitters2018$batter[i])[,7] -> qualified_hitters2018$rookie_year[i]
}

qualified_hitters2018$fullname <- with(qualified_hitters2018, paste(name_first, name_last))

qualified_hitters2018$year <- 2018

as.numeric(qualified_hitters2018$birth_year) -> qualified_hitters2018$birth_year
as.numeric(qualified_hitters2018$rookie_year) -> qualified_hitters2018$rookie_year

qualified_hitters2018$year - qualified_hitters2018$birth_year -> qualified_hitters2018$age
(qualified_hitters2018$year - qualified_hitters2018$rookie_year) +1 -> qualified_hitters2018$tenure

##2019
plyr::count(sc2019, vars = "batter") -> batters2019

qualified_hitters2019 <- batters2019 %>% 
  filter(freq > 500)

num_batters2019 <- nrow(batters2019)

for(i in 1:num_batters2019){
  temp <- sc2019bip %>% 
    filter(batter == qualified_hitters2019$batter[i])
  
  temphh <- temp %>% 
    filter(launch_speed > 95)
  
  qualified_hitters2019$hhrate[i] <- nrow(temphh)/nrow(temp)
  qualified_hitters2019$la_hh[i] <- mean(na.omit(temphh$launch_angle))
  qualified_hitters2019$std_la[i] <- sd(na.omit(temp$launch_angle))
  qualified_hitters2019$maxEV[i] <- max(na.omit(temp$launch_speed))
  qualified_hitters2019$avgEV[i] <- mean(na.omit(temp$launch_speed))
  
}

for(i in 1:num_batters2019){
  playername_lookup(qualified_hitters2019$batter[i])[,1] -> qualified_hitters2019$name_first[i]
  playername_lookup(qualified_hitters2019$batter[i])[,2] -> qualified_hitters2019$name_last[i]
  playername_lookup(qualified_hitters2019$batter[i])[,11] -> qualified_hitters2019$fangraphs[i]
  playername_lookup(qualified_hitters2019$batter[i])[,6] -> qualified_hitters2019$birth_year[i]
  playername_lookup(qualified_hitters2019$batter[i])[,7] -> qualified_hitters2019$rookie_year[i]
}

qualified_hitters2019$fullname <- with(qualified_hitters2019, paste(name_first, name_last))

qualified_hitters2019$year <- 2019

as.numeric(qualified_hitters2019$birth_year) -> qualified_hitters2019$birth_year
as.numeric(qualified_hitters2019$rookie_year) -> qualified_hitters2019$rookie_year

qualified_hitters2019$year - qualified_hitters2019$birth_year -> qualified_hitters2019$age
(qualified_hitters2019$year - qualified_hitters2019$rookie_year) +1 -> qualified_hitters2019$tenure


qualified_hitters2016 <- qualified_hitters2016 %>% 
  rename(Name = fullname)
qualified_hitters2017 <- qualified_hitters2017 %>% 
  rename(Name = fullname)
qualified_hitters2018 <- qualified_hitters2018 %>% 
  rename(Name = fullname)
qualified_hitters2019 <- qualified_hitters2019 %>% 
  rename(Name = fullname)

qualified_hitters2016 <- merge(qualified_hitters2016, woba2016[,c("Name", "wOBA", "wOBA_CON")], by = "Name", all.x = TRUE)
qualified_hitters2017 <- merge(qualified_hitters2017, woba2017[,c("Name", "wOBA", "wOBA_CON")], by = "Name", all.x = TRUE)
qualified_hitters2018 <- merge(qualified_hitters2018, woba2018[,c("Name", "wOBA", "wOBA_CON")], by = "Name", all.x = TRUE)
qualified_hitters2019 <- merge(qualified_hitters2019, woba2019[,c("Name", "wOBA", "wOBA_CON")], by = "Name", all.x = TRUE)

qualified_hitters2016$barrel_consistency <- qualified_hitters2016$avgEV/qualified_hitters2016$maxEV
qualified_hitters2017$barrel_consistency <- qualified_hitters2017$avgEV/qualified_hitters2017$maxEV
qualified_hitters2018$barrel_consistency <- qualified_hitters2018$avgEV/qualified_hitters2018$maxEV
qualified_hitters2019$barrel_consistency <- qualified_hitters2019$avgEV/qualified_hitters2019$maxEV



qualified_hitters2016$nextwOBA <- woba2017$wOBA[match(qualified_hitters2016$Name, woba2017$Name)]
qualified_hitters2017$nextwOBA <- woba2018$wOBA[match(qualified_hitters2017$Name, woba2018$Name)]
qualified_hitters2018$nextwOBA <- woba2019$wOBA[match(qualified_hitters2018$Name, woba2019$Name)]

qualified_hitters2016$est_nextwOBA <- predict(fit, qualified_hitters2016)
qualified_hitters2017$est_nextwOBA <- predict(fit, qualified_hitters2017)
qualified_hitters2018$est_nextwOBA <- predict(fit, qualified_hitters2018)
qualified_hitters2019$est_nextwOBA <- predict(fit, qualified_hitters2019)



qualified_hitters <- rbind(qualified_hitters2016, qualified_hitters2017, qualified_hitters2018)

save(qualified_hitters, file = "qualified_hitters2016-2018.Rdata")
save(qualified_hitters2019, file = "qualified_hitters2019.Rdata")
save(sc2018, file = "statcast2018.Rdata")
save(sc2019, file = "statcast2019.Rdata")






#Model fitting
library(caret)

#Data Partitioning
new=createDataPartition(y=na.omit(qualified_hitters$nextwOBA),p=.7,list=FALSE)
train=qualified_hitters[new,]
test=qualified_hitters[-new,]

#Resampling Method
tc <- trainControl(method="boot",number=20)

#Model Creation
fit <- train(nextwOBA~hhrate + la_hh + std_la + maxEV + avgEV + poly(age,2) +
                tenure + barrel_consistency, data=train, method="lm",trControl=tc, na.action = na.omit, 
                metric = "RMSE")

#Model Results
summary(fit)
fit$results

