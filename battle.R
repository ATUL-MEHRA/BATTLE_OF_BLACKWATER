

train <- read.csv("train.csv")
test <- read.csv("test.csv")


sapply(train, function(x) sum(is.na(x))) 

train <- na.omit(train)

indices <- sample(1:nrow(train), 0.1*nrow(train))

train1 = train[indices,]

test_indices = sample(1:nrow(train), 0.1*nrow(train))

test1 = train[test_indices, ]
########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(bestSoldierPerc ~ ., data = train1, family = "binomial")
summary(model_1) 

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


# Removing multicollinearity through VIF check
library(car)
vif(model_2)

model_2<- glm(formula = bestSoldierPerc ~ soldierId + assists + greekFireItems + healthLost + 
                knockedOutSoldiers + throatSlits + killRank + killPoints + 
                enemiesKilled + killingStreaks + farthermostKill + numShips + 
                numSaves + horseRideKills + swimmingDistance + friendlyKills + 
                castleTowerDestroys + onFootDistance + weaponsUsed + respectEarned,
              family = "binomial", data = train1) 

summary(model_2)

vif(model_2)


# remove horseRideKills

model_3<- glm(formula = bestSoldierPerc ~ soldierId + assists + greekFireItems + healthLost + 
                knockedOutSoldiers + throatSlits + killRank + killPoints + 
                enemiesKilled + killingStreaks + farthermostKill + numShips + 
                numSaves  + swimmingDistance + friendlyKills + 
                castleTowerDestroys + onFootDistance + weaponsUsed + respectEarned,
              family = "binomial", data = train1) 

summary(model_3) 
# remove castleTowerDestroys

model_4<- glm(formula = bestSoldierPerc ~ soldierId + assists + greekFireItems + healthLost + 
                knockedOutSoldiers + throatSlits + killRank + killPoints + 
                enemiesKilled + killingStreaks + farthermostKill + numShips + 
                numSaves  + swimmingDistance + friendlyKills + 
                 onFootDistance + weaponsUsed + respectEarned,
              family = "binomial", data = train1) 

summary(model_4) 

# remove farthermostKill

model_5 <- glm(formula = bestSoldierPerc ~ soldierId + assists + greekFireItems + healthLost + 
                knockedOutSoldiers + throatSlits + killRank + killPoints + 
                enemiesKilled + killingStreaks  + numShips + 
                numSaves  + swimmingDistance + friendlyKills + 
                onFootDistance + weaponsUsed + respectEarned,
              family = "binomial", data = train1) 

summary(model_5) 


# remove throatSlits

model_6 <- glm(formula = bestSoldierPerc ~ soldierId + assists + greekFireItems + healthLost + 
                 knockedOutSoldiers  + killRank + killPoints + 
                 enemiesKilled + killingStreaks  + numShips + 
                 numSaves  + swimmingDistance + friendlyKills + 
                 onFootDistance + weaponsUsed + respectEarned,
               family = "binomial", data = train1) 

summary(model_6) 
vif(model_6)


final_model <- model_6



#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of bestSoldierPerc for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test1[,-25])


# Let's see the summary 

summary(test_pred)

test1$prob <- test_pred
View(test1)
# Let's use the probability cutoff of 50%.

test_pred_soldier <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_soldier <- factor(ifelse(test1$bestSoldierPerc>= 0.50,"Yes","No"))


table(test_actual_soldier,test_pred_soldier)


#######################################################################
test_pred_soldier <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

#install.packages("e1071")
library(e1071)

library(caret)

test_conf <- confusionMatrix(test_pred_soldier, test_actual_soldier, positive = "Yes")
test_conf
#######################################################################


# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_soldier <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_soldier, test_actual_soldier, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=15,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.457 for final model

test_cutoff_soldier <- factor(ifelse(test_pred >=0.457, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_soldier, test_actual_soldier, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec


### KS -statistic - Test Data ######

test_cutoff_soldier <- ifelse(test_cutoff_soldier=="Yes",1,0)
test_actual_soldier <- ifelse(test_actual_soldier=="Yes",1,0)

install.packages("ROCR")
library(ROCR)


#on testing  data
pred_object_test<- prediction(test_cutoff_soldier, test_actual_soldier)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_soldier, test_pred, groups = 10)




#predicted probabilities of bestSoldierPerc for original test data

test_pred = predict(final_model, type = "response", 
                    newdata = test)


test_cutoff_soldier <- factor(ifelse(test_pred >=0.457,1,0))


final_submission <- data.frame(test$soldierId,test_cutoff_soldier)

summary(final_submission)


# SO we have 5083 Soldier considered best and awarded by the king.

