
#PRE-PROCESSING

  #Data input
  deploy_set <- read.csv(file = 'deployStations.csv', head =T, sep =",")
  test_set <- read.csv(file = 'testStations.csv', head =T, sep =",")

  #NA replaced by mean
  test_set$bikes_3h_ago[which(is.na(test_set$bikes_3h_ago))] <- mean(test_set$bikes_3h_ago,na.rm = TRUE)
  #Deploy_set has not NA data

  #Dummies/new variables_Train data set
  isRainning <- ifelse(deploy_set$precipitation.l.m2 > 0, 1, 0)
  isWindy <- ifelse(deploy_set$windMeanSpeed.m.s > 12, 1, 0)
  sumBikesFull <- deploy_set$bikes_3h_ago + deploy_set$full_profile_3h_diff_bikes
  sumBikesShort <-deploy_set$short_profile_3h_diff_bikes + deploy_set$short_profile_bikes
  deploy_set <- cbind(deploy_set, isRainning, isWindy, sumBikesFull, sumBikesShort)
  included <- names(deploy_set) %in% c('bikes','weekday','weekhour','isHoliday','station','numDocks','isRainning', 'isWindy', 'full_profile_bikes','short_profile_3h_diff_bikes','short_profile_bikes','sumBikesFull')
 
   model_train <-deploy_set[included]

  #Dummies/new variables_Test data set
  isRainning <- ifelse(test_set$precipitation.l.m2 > 0, 1, 0)
  isWindy <- ifelse(test_set$windMeanSpeed.m.s > 12, 1, 0)
  sumBikesFull <- test_set$bikes_3h_ago + test_set$full_profile_3h_diff_bikes
  sumBikesShort <-test_set$short_profile_3h_diff_bikes + test_set$short_profile_bikes

  test_set <- cbind(test_set, isRainning, isWindy, sumBikesFull, sumBikesShort)


#PREDICTION 
  train_control <- trainControl(method = 'repeatedcv', number = 10, repeats = 3)
  set.seed(123)
  modelo_KNN<-train(bikes~.,data=model_train,method="knn",trControl=train_control)
  print(modelo_KNN)

  bikes <- predict(modelo_KNN, test_set)

#SAVE RESULTS
  solution <- cbind(test_set, bikes)
  solution <- solution %>% mutate(bikes = ifelse(numDocks < bikes, numDocks, round(bikes,0)))
  solution <- select(solution, c(Id, bikes))
  write.csv(solution, "C:\\Users\\Marina\\Desktop\\submision1\\submission5.csv", row.names=FALSE)

