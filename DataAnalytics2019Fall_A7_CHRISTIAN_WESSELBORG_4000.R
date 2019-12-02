#### White Wine Data ####

#loading in data
rm(list =ls())

winew <- read.csv("C:/Classes/4F19/Data Analytics/Assignment 7/winequality-white.csv", sep = ";")

#EDA

library(ggplot2)

summary(winew)

boxplot(winew$quality)
hist(winew$quality, main = "White Wine Quality")
shapiro.test(winew$quality)

winew_mat <- cor(winew)
round(winew_mat, 3)

plot(winew$pH ~ winew$alcohol)

plot(winew$alcohol ~ winew$quality)
ggplot() +
  geom_hex(aes(y = winew$quality, x = winew$alcohol), bins = 7) +
  labs(title = "Density Plot of White Wine Quality vs. Alcohol")

plot(winew$volatile.acidity ~ winew$quality)

#PCA

det(winew_mat)

#KMO test

kmo = function( data ){
  library(MASS) 
  X <- cor(as.matrix(data)) 
  iX <- ginv(X) 
  S2 <- diag(diag((iX^-1)))
  AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
  IS <- X+AIS-2*S2                         # image covariance matrix
  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a) 
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b/(b+a)                        # indiv. measures of sampling adequacy
  AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the correlation matrix. That is the  negative of the partial correlations, partialling out all other variables.
  kmo <- BB/(AA+BB)                     # overall KMO statistic
  # Reporting the conclusion 
  if (kmo >= 0.00 && kmo < 0.50){test <- 'The KMO test yields a degree of common variance unacceptable for FA.'} 
  else if (kmo >= 0.50 && kmo < 0.60){test <- 'The KMO test yields a degree of common variance miserable.'} 
  else if (kmo >= 0.60 && kmo < 0.70){test <- 'The KMO test yields a degree of common variance mediocre.'} 
  else if (kmo >= 0.70 && kmo < 0.80){test <- 'The KMO test yields a degree of common variance middling.' } 
  else if (kmo >= 0.80 && kmo < 0.90){test <- 'The KMO test yields a degree of common variance meritorious.' }
  else { test <- 'The KMO test yields a degree of common variance marvelous.' }
  
  ans <- list( overall = kmo,
               report = test,
               individual = MSA,
               AIS = AIS,
               AIR = AIR )
  return(ans)
} 

kmo(winew)

#remove residual sugar
winew2 <- winew[, -4]

kmo(winew2)

#model

library(psych)

#including residual.sugar
pca1 <- principal(winew, nfactors = 6, rotate = "none")
pca1
plot(pca1$values, type = "b")

pca2 <- principal(winew, nfactors = 4, rotate = "none")
pca2

pca2r <- principal(winew, nfactors = 4, rotate = "varimax")
pca2r
print.psych(pca2r, cut = 0.5, sort = T)

#less factors
pca1 <- principal(winew, nfactors = 6, rotate = "none")
pca1
plot(pca1$values, type = "b")

pca2 <- principal(winew, nfactors = 3, rotate = "none")
pca2

pca2r <- principal(winew, nfactors = 3, rotate = "varimax")
pca2r
print.psych(pca2r, cut = 0.5, sort = T)

#random forrest

library(randomForest)

#seed

set.seed(12)

#make quality a factor

winew$quality <- factor(winew$quality)

#training and validation sets

train <- sample(nrow(winew), nrow(winew) * 0.8, replace = F)

Train <- winew[train, ]
Valid <- winew[-train, ]

#model

model1 <- randomForest(quality ~ ., data = Train, ntree = 500, mtry = 11, importance = T)
model1
model1$importance
#validation

predvalid <- predict(model1, Valid, type = "class")
summary(predvalid)

mean(predvalid == Valid$quality)
table(predvalid, Valid$quality)

table <- table(predvalid, Valid$quality)
sum(diag(table))
sum(table)

sum(diag(table))/sum(table)

#tuning
set.seed(12)

modlist <- list(1:11)
for(i in 1:11){
  mod <- randomForest(quality ~ ., data = Train, ntree = 500, mtry = i, importance = T)
  table <- mod$confusion
  
  modlist[i] <- sum(diag(table))/sum(table)
}

plot(x = 1:11, y = modlist, main = "Error Rate vs. Increasing mtry", xlab = "mtry", ylab = "Error Rate")

#### Red Wine Data ####
#loading in data
rm(list = ls())

wine <- read.csv("C:/Classes/4F19/Data Analytics/Assignment 7/winequality-red.csv", sep = ";")

#EDA

summary(wine)

boxplot(wine$quality)
hist(wine$quality, main = "Red Wine Quality")
shapiro.test(wine$quality)

wine_mat <- cor(wine)
wine_mat
round(wine_mat, 3)

plot(wine$pH ~ wine$alcohol)

plot(wine$alcohol ~ wine$quality)

plot(wine$volatile.acidity ~ wine$quality)

#PCA

det(wine_mat)

#KMO test

kmo = function( data ){
  library(MASS) 
  X <- cor(as.matrix(data)) 
  iX <- ginv(X) 
  S2 <- diag(diag((iX^-1)))
  AIS <- S2%*%iX%*%S2                      # anti-image covariance matrix
  IS <- X+AIS-2*S2                         # image covariance matrix
  Dai <- sqrt(diag(diag(AIS)))
  IR <- ginv(Dai)%*%IS%*%ginv(Dai)         # image correlation matrix
  AIR <- ginv(Dai)%*%AIS%*%ginv(Dai)       # anti-image correlation matrix
  a <- apply((AIR - diag(diag(AIR)))^2, 2, sum)
  AA <- sum(a) 
  b <- apply((X - diag(nrow(X)))^2, 2, sum)
  BB <- sum(b)
  MSA <- b/(b+a)                        # indiv. measures of sampling adequacy
  AIR <- AIR-diag(nrow(AIR))+diag(MSA)  # Examine the anti-image of the correlation matrix. That is the  negative of the partial correlations, partialling out all other variables.
  kmo <- BB/(AA+BB)                     # overall KMO statistic
  # Reporting the conclusion 
  if (kmo >= 0.00 && kmo < 0.50){test <- 'The KMO test yields a degree of common variance unacceptable for FA.'} 
  else if (kmo >= 0.50 && kmo < 0.60){test <- 'The KMO test yields a degree of common variance miserable.'} 
  else if (kmo >= 0.60 && kmo < 0.70){test <- 'The KMO test yields a degree of common variance mediocre.'} 
  else if (kmo >= 0.70 && kmo < 0.80){test <- 'The KMO test yields a degree of common variance middling.' } 
  else if (kmo >= 0.80 && kmo < 0.90){test <- 'The KMO test yields a degree of common variance meritorious.' }
  else { test <- 'The KMO test yields a degree of common variance marvelous.' }
  
  ans <- list( overall = kmo,
               report = test,
               individual = MSA,
               AIS = AIS,
               AIR = AIR )
  return(ans)
} 

kmo(wine)

#remove residual sugar
wine2 <- wine[, -4]

kmo(wine2)

#model

library(psych)

pca1 <- principal(wine2, nfactors = 6, rotate = "none")
pca1
plot(pca1$values, type = "b")

pca2 <- principal(wine2, nfactors = 4, rotate = "none")
pca2

pca2r <- principal(wine2, nfactors = 4, rotate = "varimax")
pca2r
print.psych(pca2r, cut = 0.5, sort = T)

#including residual.sugar
pca1 <- principal(wine, nfactors = 6, rotate = "none")
pca1
plot(pca1$values, type = "b", main = "Scree Plot")

pca2 <- principal(wine, nfactors = 4, rotate = "none")
pca2

pca2r <- principal(wine, nfactors = 4, rotate = "varimax")
pca2r
print.psych(pca2r, cut = 0.5, sort = T)

#random forrest

library(randomForest)

#seed

set.seed(12)

#make quality a factor

wine$quality <- factor(wine$quality)

#training and validation sets

train <- sample(nrow(wine), nrow(wine) * 0.8, replace = F)

Train <- wine[train, ]
Valid <- wine[-train, ]

#model

model1 <- randomForest(quality ~ ., data = Train, ntree = 500, mtry = 11, importance = T)
model1
model1$importance

#validation

predvalid <- predict(model1, Valid, type = "class")
summary(predvalid)

mean(predvalid == Valid$quality)
table(predvalid, Valid$quality)

table <- table(predvalid, Valid$quality)
sum(diag(table))
sum(table)

sum(diag(table))/sum(table)
#### Absentee Data ####
#loading data
rm(list = ls())

absentee <- read.csv("C:/Classes/4F19/Data Analytics/Assignment 7/Absenteeism_at_work_AAA/Absenteeism_at_work.csv", sep = ";")

#EDA

#converting factor values to factors
absentee$ID <- as.factor(absentee$ID)
absentee$Reason.for.absence <- as.factor(absentee$Reason.for.absence)
absentee$Month.of.absence <- as.factor(absentee$Month.of.absence)
absentee$Day.of.the.week <- as.factor(absentee$Day.of.the.week)
absentee$Seasons <- as.factor(absentee$Seasons)
absentee$Disciplinary.failure <- as.factor(absentee$Disciplinary.failure)
absentee$Education <- factor(absentee$Education, ordered = T)
absentee$Social.drinker <- as.factor(absentee$Social.drinker)
absentee$Social.smoker <- as.factor(absentee$Social.smoker)

#summary

summary(absentee)

a <- na.omit(absentee)

#plots

plot(absentee$Absenteeism.time.in.hours ~ absentee$Day.of.the.week)

plot(absentee$Absenteeism.time.in.hours ~ absentee$Distance.from.Residence.to.Work, main = "Absentee time vs. distance from work", xlab = "Distance from work", ylab = "Hours absent")

plot(absentee$Absenteeism.time.in.hours ~ absentee$Son)

plot(absentee$Absenteeism.time.in.hours ~ absentee$Age)

hist(absentee$Absenteeism.time.in.hours, main = "Length of Absence", xlab = "Hours")
#random forrest

library(randomForest)

#seed

set.seed(12)

#training and testing

train <- sample(nrow(absentee), size = nrow(absentee) * .7, replace = F)

Train <- absentee[train, ]
Valid <- absentee[-train, ]
#model

model1 <- randomForest(Absenteeism.time.in.hours ~ ., data = Train, ntree = 500, mtry = 10, importance = T)
model1
plot(model1)

mean(sqrt(model1$mse))
#validation
predvalid <- predict(model1, Valid)
summary(predvalid)

mean((Valid[, 21] - as.numeric(predvalid))^2)

#model with limited variables

model2 <- randomForest(Absenteeism.time.in.hours ~ Distance.from.Residence.to.Work + Age + Education + Son + Pet + Weight + Height + Body.mass.index, data = Train, ntree = 500, mtry = 8, importance = T)
model2
plot(model2)

model2$importance

mean(sqrt(model2$mse))
#validation
predvalid <- predict(model2, Valid)
summary(predvalid)

mean((Valid[, 21] - as.numeric(predvalid))^2)

#knn

library(class)

#model
id <- Train$ID

knn <- knn(Train, Valid, cl = id, k = 3)
knn
summary(knn)

table(knn, Valid$ID)

mean(knn == Valid$ID)

#model best k value
id <- Train$ID
knnlist <- data.frame(Valid$ID)

for(i in 1:5){
  knnlist[, (i + 1)] <- knn(Train, Valid, cl = id, k = (i * 2) - 1)
}

accuracy <- list(1:5)
for(i in 1:5){
  accuracy[i] <- mean(knnlist[, (i + 1)] == Valid$ID)
}

plot(x = 1:5, y = accuracy, main = "Optimal K value", xlab = "K value test")

#model reducing variables
knn <- knn(Train[, -c(8, 9, 18, 19, 20, 21)], Valid[, -c(8, 9, 18, 19, 20, 21)], cl = id, k = 1)
knn
summary(knn)

mean(knn == Valid$ID)

#model reducing variables
knn <- knn(Train[, -c(7, 9, 15, 14, 18, 19, 20)], Valid[, -c(7, 9, 15, 14, 18, 19, 20)], cl = id, k = 1)
knn
summary(knn)

mean(knn == Valid$ID)

#model reducing variables
knn <- knn(Train[, -c(1, 6, 7, 8, 9, 10, 11, 12, 13, 15, 14, 18, 19, 20)], Valid[, -c(1, 6, 7, 8, 9, 10, 11, 12, 13, 15, 14, 18, 19, 20)], cl = id, k = 1)
knn
summary(knn)

mean(knn == Valid$ID)

#model reducing variables .2027
knn <- knn(Train[, c(2, 3, 4, 5, 21)], Valid[, c(2, 3, 4, 5, 21)], cl = id, k = 1)
knn
summary(knn)

mean(knn == Valid$ID)

#model best predictors .1351
knn <- knn(Train[, c( 3, 4, 5, 21)], Valid[, c( 3, 4, 5, 21)], cl = id, k = 1)
knn
summary(knn)

mean(knn == Valid$ID)

#model reducing variables .1981
knn <- knn(Train[, c(2, 4, 5, 21)], Valid[, c(2,4, 5, 21)], cl = id, k = 1)
knn
summary(knn)

mean(knn == Valid$ID)

#model reducing variables .2117
knn <- knn(Train[, c(2, 3, 5, 21)], Valid[, c(2, 3, 5, 21)], cl = id, k = 1)
knn
summary(knn)

mean(knn == Valid$ID)

#model reducing variables .2162
knn <- knn(Train[, c(2, 3, 4, 21)], Valid[, c(2, 3, 4, 21)], cl = id, k = 1)
knn
summary(knn)

mean(knn == Valid$ID)

#model reducing variables .1846
knn <- knn(Train[, c(2, 3, 4, 5)], Valid[, c(2, 3, 4, 5)], cl = id, k = 1)
knn
summary(knn)

mean(knn == Valid$ID)

#model reducing variables removing 4 and 5 .2072
knn <- knn(Train[, c(2, 3, 21)], Valid[, c(2, 3, 21)], cl = id, k = 1)
knn
summary(knn)

mean(knn == Valid$ID)

#model reducing variables removing 4 and 5 .2072
knn <- knn(Train[, c(2, 3, 21)], Valid[, c(2, 3, 21)], cl = id, k = 1)
knn
summary(knn)

mean(knn == Valid$ID)

#finding optimal k

set.seed(12)

for(i in 1:10){
  knnlist[, (i + 1)] <- knn(Train[, c(2, 3, 21)], Valid[, c(2, 3, 21)], cl = id, k = (i * 2) - 1)
}

accuracy <- list(1:10)
for(i in 1:10){
  accuracy[i] <- mean(knnlist[, (i + 1)] == Valid$ID)
}

plot(x = 1:10, y = accuracy, main = "Optimal K value", xlab = "K value test")

#ID summary
summary(absentee$ID)

#individual absences

plot(absentee$Absenteeism.time.in.hours ~ absentee$ID)

barplot(table(absentee$ID))
