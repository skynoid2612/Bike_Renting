############# Project 2 - Bike Renting #############
# Assinging all the directories libraries
library(measures)
cal_result = function(test,predict){  # Calculate all the result of regression model
  r1=measures::RSQ(test,predict)
  r2=measures::MAE(test,predict)
  r3=measures::RMSE(test,predict)
  r4=measures::RMSLE(test,predict)
  result= c(r1,r2,r3,r4)
  
  return(result)
}

setwd('D:/Data Science/EDWISOR/2_PORTFOLIO/project 2')
df <- read.csv(file = 'day.csv',header = TRUE)
# AFTER CHECK THE DATA SET WE ARE GOING TO REMOVE VARIABLES WHICH PROVIDING THE SAME INFORMATION
# Reason for dropping the features
# 1. dteday --> this feature consist of the date, moreover we already has year and month in our data set.
# 2. casual & registered --> addition of casual and registered is equal to cnt
# 3. Instant --> We don't want the result to be bias based on the instant, ML algorithm will treat instant as numerical data.
new_dataset<- subset(x = df,select = -c(instant,dteday,casual,registered))
new_dataset <- as.data.frame(new_dataset)
categorical_columns = c('season', 'yr', 'mnth', 'holiday', 'weekday','workingday', 'weathersit')
for(i in categorical_columns){new_dataset[,i] = as.factor(new_dataset[,i])} # converting in categorical variable
# Let's check the data set and which inference can be taken out by summerising the dataset
summary(new_dataset)
#season  yr           mnth     holiday weekday   workingday    weathersit      temp             atemp              hum           windspeed            cnt
#1:181   0:365   1      : 62   0:710   0:105   Min.   :0.000   1:463      Min.   :0.05913   Min.   :0.07907   Min.   :0.0000   Min.   :0.02239  Min.   :  22
#2:184   1:366   3      : 62   1: 21   1:105   1st Qu.:0.000   2:247      1st Qu.:0.33708   1st Qu.:0.33784   1st Qu.:0.5200   1st Qu.:0.13495  1st Qu.:3152 
#3:188           5      : 62           2:104   Median :1.000   3: 21      Median :0.49833   Median :0.48673   Median :0.6267   Median :0.18097  Median :4548
#4:178           7      : 62           3:104   Mean   :0.684              Mean   :0.49538   Mean   :0.47435   Mean   :0.6279   Mean   :0.19049  Mean   :4504
#                8      : 62           4:104   3rd Qu.:1.000              3rd Qu.:0.65542   3rd Qu.:0.60860   3rd Qu.:0.7302   3rd Qu.:0.23321  3rd Qu.:5956
#                10     : 62           5:104   Max.   :1.000              Max.   :0.86167   Max.   :0.84090   Max.   :0.9725   Max.   :0.50746  Max.   :8714
#                (Other):359           6:105

# BY STUDING THE ABOVE SUMMARY OF THE DATA SET WE CAN COME UP WITH THE RESPECTIVE INFERENCE
# SEASON, YR, MNTH, WEEKDAY  - VARIABLE IS EQUALLY DISTRIBUTION
# HOLIDAY - BIKE RENTING IS MORE WHEN THE HOLIDAY VARAIBLE IS '0'
# WEATHERSIT - IT'S OBVIOUS THAT BIKE RENT IS HIGH IN CLEAR WEATHER

########################## EDA ##########################
############# MISSING VALUE CHECK #############
anyNA(new_dataset)
# RESULT IS FALSE NO MISSING VALUE PRESENT IN THE RESPECTIVE DATA SET

############# OUTLIER ANALYSIS #############
library(ggplot2) 
 
numeric_index = sapply(new_dataset,is.numeric) #selecting only numeric
numeric_data = new_dataset[,numeric_index] # creating the dataset with numerical varaible only
cnames = colnames(numeric_data) # fetching column names
for (i in 1:length(cnames)) # Ploting the boxplot
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(new_dataset))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of cnt for",cnames[i])))
}

#gridExtra::grid.arrange(gn1,gn2,gn3, ncol = 3) # this boxplot doesn't have any outliear
gridExtra::grid.arrange(gn4,gn5,gn6, ncol = 3) 
# BY OBSERVING THE BOX PLOT
# gn4 : hum and gn5 : windspeed HAS THE OUTLIERS
# SO WE HAVE TWO CHOICE TO DEAL WITH THE OUTLIERS
# 1. DELETE THE OUTLIERS FROM THE DATA SET AND PERFORM THE FURTHER ANALYSIS
# 2. IMPUTE THE OUTLIER WITH IMPTATION TECHNIQUE OR ANY STATISTICAL TECHNIQUE

# Process1 will have the deletation technique 
Process1 =  new_dataset
for(i in c('hum', 'windspeed'))
{
  val = Process1[,i][Process1[,i] %in% boxplot.stats(Process1[,i])$out]
  Process1 = Process1[which(!Process1[,i] %in% val),]
} # all the outliear got removed after applying this loop

# Process2 will have the imputation method
Process2 = new_dataset
for(i in c('hum', 'windspeed'))
{
  val = Process2[,i][Process2[,i] %in% boxplot.stats(Process2[,i])$out]
  Process2[,i][Process2[,i] %in% val] = NA
}

#Process2 = knnImputation(Process2,k=1)
#anyNA(Process2)
# In above code we have tried to impute the missing value with knnimputation method but due to insufficicy in model, KNN imputation
# has generate the error.
    #Error in knnImputation(Process2, k = 1) : 
      #Not sufficient complete cases for computing neighbors.
# Moreover, only 0.02 data was has the missing value. so dropping the varaible and performing the further anaylsis will be the best option
# rather then imputution it with some false data.
# further analysis and generating of machine learning algorithm is done on Process1 data frame (by dropping the outliers)


############# FEATURE SELECTION #############
library(corrplot)
numerical_columns = c('temp','atemp','hum','windspeed','cnt')
corrplot(corr = cor(new_dataset[,numerical_columns]),method = 'number') # ploting the correlation plot
# BY CHECKING THE CORRELATION CHART atemp AND temp IS HIGHLY CORRELATED WITH EACHOTHER
# MOREOVER, BY CHECKING THE LAST PLOTS, temp, atemp HAS SOME AMOUNT OF DEPENCENCE WITH RESPECT TO cnt 
# AND HUM AND WINDSPEED HAS NEGATIVE DEPENDENCY TOWARDS cnt

# Deleting the temp feature
Process1 = subset(Process1, select = -temp)
#Process2 = subset(Process2, select = -temp)

############# FEATURE SCALING #############
# we have four numerical variable, let's check the histogram of the respective feature
hist(Process1$atemp)
hist(Process1$hum)
hist(Process1$windspeed)
hist(Process1$cnt)
# INFERENCE:
# As THE DATA IS ALREADY NORMALIZED SO THERE IS NO NEED OF SCALING
########################## MODEL DEVELOPMENT ##########################

############# Spliting the dataset in test and train data #############
set.seed(123)
train_index = sample(1:nrow(Process1),0.8*nrow(Process1)) #Featching the 80% index of the data set
train = Process1[train_index,] 
test = Process1[-train_index,]

##############################################################################
#Following model will be taken into considaration:
#1. Linear Regression
#2. Decision Tree
#3. Random Forest
#4. K-Nearest Neighbors


############# LINEAR REGRESSION #############
library(usdm)
new_col = c('atemp','hum','windspeed')
vifcor(train[,new_col], th = 0.9)

Linear1 = lm(cnt~., data = train) # Linear Regression model generation
Linear_predict1 = predict(Linear1, test[,-11]) # Prediction of the varaible for cross validation of an algorithm
lr_result = cal_result(test$cnt,Linear_predict1) # storing the generated variable for further corss varification with other ML models
summary(Linear1)
# Model is highly significance as the Adj R Square is above 80%
# Moreover all the variable are significant
############# DECISION TREE #############
library(rpart)
Dtree1 = rpart(cnt ~., data = train, method = 'anova') # Decision Tree generation
Dtree_predict1 = predict(Dtree1, test[,-11])# Prediction of the varaible for cross validation of an algorithm
Dtree_result = cal_result(test$cnt,Dtree_predict1)# storing the generated variable for further corss varification with other ML models
summary(Dtree1)
############# RANDOM FOREST #############
library('randomForest')
rf1 = randomForest(x = train[,-11], y = train$cnt, importance = TRUE, ntree = 500) # RANDOM FOREST generation
rf_predict1 = predict(rf1, test[,-11])# Prediction of the varaible for cross validation of an algorithm
rf_result = cal_result(test$cnt,rf_predict1)# storing the generated variable for further corss varification with other ML models
rf1
# % Var explained: 87.2
############# K-NEAREST NEIGHBORS #############
library(caret)
knn1 = knnreg(train[,-11], train$cnt, k = 3) # K-NEAREST NEIGHBORS generation
knn_predict1 = predict(knn1, test[,-11])# Prediction of the varaible for cross validation of an algorithm
knn_result = cal_result(test$cnt,knn_predict1)# storing the generated variable for further corss varification with other ML models
summary(knn1)
########################## ACCUARACY CHECK ##########################
result = data.frame(knn_result,Dtree_result,rf_result,lr_result) # genreating the accuracy chart with respect to all the ML result
row.names(result) = c('rsq','mae','rmse','rmsle') # Naming the rows for clearing

