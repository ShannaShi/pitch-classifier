#Program: mariners q.R
#Date: November 2018
#Programmer: Shanna Shi
#The programmer retains all rights to this code.

library(dplyr)
library(tree)
library(randomForest)

setwd("~/Documents/!baseball")

train <- read.csv("train.csv", stringsAsFactors = TRUE)

#let's do some data cleaning - finding obviously misentered data
table(train$type)
table(train$pitcher)
arrange(train,pitch_id)
table(train$pitcher_side)
table(train$batter_side)
table(train$inning)
table(train$half)
table(train$outs)
table(train$balls)
table(train$strikes)

#removing the blanks and the...ones with the weird numbers, converting some variables;
train_clean <- filter(train,train$type %in% c("CH","CU","FA","FC","FS","SI","SL")) %>%
  filter(pitcher !="-8.15618991851807" | pitcher != "SI") %>%
  filter(pitch_id != "-0.307924002408981" | pitch_id != "1.99430000782013") %>%
  filter(pitcher != "SI") %>%
  filter(pitcher_side %in% c("Left","Right")) %>%
  filter(batter_side %in% c("Left","Right")) %>%
  filter(inning %in% c(1:20)) %>%
  droplevels.data.frame() %>%
  mutate(half=as.factor(half),pfx_x=as.character(pfx_x),pfx_z=as.character(pfx_z),inning=as.character((inning))) %>%
  mutate(pfx_x=as.numeric(pfx_x),pfx_z=as.numeric(pfx_z),inning=as.integer(inning))

#oh gotta clean the testing set
test <- read.csv("test.csv", stringsAsFactors = TRUE)

arrange(test,pitch_id)
table(test$pitcher)
table(test$inning)

test_clean <- mutate(test,half=as.factor(half),inning=as.character(inning)) %>%
  mutate(inning=as.integer(inning))

#i keep forgetting which pitch types are included
table(train_clean$type)

#creates a decision tree using all predictors except pitcher and pitch_id)
tree.pitchtype=tree(type~.-pitcher -pitch_id,train_clean)

tree.pitchtype

summary(tree.pitchtype)

plot(tree.pitchtype)
text(tree.pitchtype,pretty=0) #well then

#we have a tree but do i need so many nodes for fastballs???
#also FC and FS not included - small proportion of them relative to the data set?
#let's uh try and validate this from training data that we actually know the outcome on

train_clean2 <- select(train_clean,-pitcher,-pitch_id)

set.seed(420)
subset <- sample(1:nrow(train_clean1),1500)
tc.test=train_clean[-subset,]

tree.pitchtype <- tree(type~. -pitcher -pitch_id,train_clean,subset=subset)
tree.pred <- predict(tree.pitchtype,tc.test,type="class")

table(tree.pred,tc.test$type)

(1207+1747+8152+73+166+251)/16348

#70.9% accuracy - probably would be better if we dealt with all the FC and FS SOMEHOW

#cross-validation for tree complexity
cv.pitchtype <- cv.tree(tree.pitchtype,FUN=prune.misclass)
cv.pitchtype

par(mfrow=c(1,2))
plot(cv.pitchtype$size,cv.pitchtype$dev,type="b")
plot(cv.pitchtype$k,cv.pitchtype$dev,type="b")


#our current tree already has the lowest amount of classification errors. good to know.

#i got to the part in my textbook that describes random forests & bagging so let's try that

bag.pitchtype <- randomForest(type~.-pitcher -pitch_id,train_clean,subset=subset)
bag.pred <- predict(bag.pitchtype,tc.test)

table(bag.pred,tc.test$type)
(1313+1701+8177+103+13+718+2650)/16348 #89.7% accuracy

#wait let's add an actual random forest now

rf.pitchtype <- randomForest(type~.-pitcher -pitch_id,train_clean,subset=subset,mtry=4)
#default mtry is sqrt #of predictors, or approx. 6
#that's what was used in the bagged model above
#but a lot of them are probably correlated so we're going to try a slightly lower number
rf.pred <- predict(rf.pitchtype,tc.test)

table(rf.pred,tc.test$type)
(1318+1697+8185+101+11+706+2651)/16348 #89.7% accuracy - no meaningful difference

#let's...go ahead and actually predict these pitch types now

#predicts pitch types
test.pred <- predict(rf.pitchtype,test_clean)

#adds predicted pitch types to the final table
predicted <- mutate(test_clean,predicted=test.pred)

varImpPlot(rf.pitchtype)
