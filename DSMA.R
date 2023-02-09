#### source & packages ####
rm(list = ls()) 

setwd('~/Desktop/23WS/DSMA/Tables right/')
source("DailyLevelData_analysis_functions.r")
library(dplyr)
library(doParallel)
library(caret)
library(smotefamily)
library(Hmisc)

#### gender ####
rm(list = ls()) #clear workspace

library(Hmisc)
library(dplyr)

setwd('~/Desktop/23WS/DSMA/Tables right/')
DailyLevel_data <- read.csv('all01.csv', na.strings = c('NA','NULL')) #read csv file. Be aware of the parameters, such as the string for NA's. Usually either "" or "\\NA".
cum_u_names=DailyLevel_data$cum_u_names
Nobs=length(cum_u_names)
DailyLevel_data <- DailyLevel_data[,-1]

# store the date in date column and remove the old name
DailyLevel_data$date <- as.Date(DailyLevel_data$date_tip) 
DailyLevel_data$date_tip <- NULL

cum_u_names=DailyLevel_data$cum_u_names
Nobs=length(cum_u_names)

install.packages("remotes")
install.packages("gender")
remotes::install_github("ropensci/gender-data-pkg")
remotes::install_github("lmullen/genderdata")
library(genderdata)
library(gender)
library(doParallel)

# replacing the cum_u_names with the count of reviewers' genders
if(1){ # takes some time to run, if you prefer not to wait too long and you have not changed the data, you can use "namelist.RData" to load the results.
  install.packages("gender")
  library(gender)
  remotes::install_github("ropensci/gender-data-pkg")
  library(genderdata)
  # best is to do gender extractin in parallel
  library(doParallel)
  
  
  gendersplit=function(x){
    a=max.col(gender(unlist(strsplit(x,",")))[,2:3])
    return(c(male=sum(a==1,na.rm=T),female=sum(a==2,na.rm=T)))
  }
  
  cl=makeCluster(detectCores()/2+2)
  registerDoParallel(cl)
  nameslist=NULL
  for(k in 1:20){
    whichrun=floor(Nobs/20*(k-1)+1):floor(Nobs/20*k)
    a=foreach(i=whichrun,.packages=c("gender"),.noexport = c("DailyLevel_data"),.combine=rbind) %dopar%
      {gendersplit(cum_u_names[i])}
    rownames(a)=NULL
    nameslist=rbind(nameslist,a)
    print(k)
  }
  stopImplicitCluster()
  stopCluster(cl)
  save(file="nameslist_rev.RData",list="nameslist")
}else{
  load("nameslist_rev.RData")  
}

DailyLevel_data=cbind(DailyLevel_data,nameslist)  
DailyLevel_data$cum_u_names=NULL

write.csv(DailyLevel_data,"daily+gender.csv")
#### missings ####
dailygender=read.csv("dailygender.csv",header=TRUE, na.strings = c('NA','NULL'))
dailygender<-dailygender[,-1]

business_data1 <- subset(dailygender,select = -c(bsid,name,ch_in,cum_max_friends,cum_max_u_elite,cum_max_us_fans,cum_n_tips,cum_max_review_count,date,male,female,american,french,japanese,chinese,mexican,thai)) 
# removed this because MICE does not like imputing factors with more than 50 levels

business_data1$n_photo[is.na(business_data1$n_photo)]=0

library(mice)

#inspect pattern of missings
md.pattern(business_data1)
write.csv(yelp_data,"yelp_data_noNA.csv")

#Below, the predictormatrix is specified.
#It is a square matrix of size ncol(data) containing 0/1 data specifying the set of predictors to be used for each target column. 
#Rows correspond to target variables (i.e. variables to be imputed), in the sequence as they appear in data. 
#A value of '1' means that the column variable is used as a predictor for the target variable (in the rows). 
#The diagonal of predictorMatrix must be zero.
predictorMatrix <- matrix(0,nrow = ncol(business_data1), ncol = ncol(business_data1)) # Make a matrix of zeros
colnames(predictorMatrix)=colnames(business_data1)
row.names(predictorMatrix)=colnames(business_data1)
predictorMatrix[c("price"),] <- 1 #variables "business_price" can be explained by all other variables
diag(predictorMatrix) <- 0 #diagonal must be zero

#impute data
business_data1_data_imputed <- mice(business_data1, predictorMatrix = predictorMatrix, m=5, maxit = 50, seed = 500)

summary(business_data1_data_imputed)

#get one of the complete data sets ( 2nd out of 5)
business_data_complete_data <- complete(business_data1_data_imputed,2)

# bring back the business_id
business_data_complete_data=cbind(bsid=dailygender$bsid,business_data_complete_data)

dailygenger2<-subset(dailygender,select = c(bsid,name,ch_in,cum_max_friends,cum_max_u_elite,cum_max_us_fans,cum_n_tips,cum_max_review_count,date,male,female,american,french,japanese,chinese,mexican,thai))
DailyLevel2=cbind(dailygenger2,business_data_complete_data)
DailyLevel3<-DailyLevel2[,-1]


write.csv(DailyLevel3,"DailyLevel3.csv")

DailyLevel_data<-DailyLevel3
DailyLevel_data<-subset(DailyLevel_data,select = -c(postal_code))

# n_photos==NA and cum_max_u_elite==NA are actually zeros, let's replace them with 0 before imputing.
DailyLevel_data$cum_max_u_elite[is.na(DailyLevel_data$cum_max_u_elite)]=0

#

describe(DailyLevel_data)
#missing里有14 postal code



#the complete data sets can be used to estimate your model of choice
#and the results of all 5 models can be combined as in the earlier example
write.csv(DailyLevel_data, file="DailyLevel_data_tip_Imputed.csv")

#### weather ####
#PRCP = Precipitation (tenths of mm)
#SNOW = Snowfall (mm)
#SNWD = Snow depth (mm)
#TMAX = Maximum temperature (tenths of degrees C)
#TMIN = Minimum temperature (tenths of degrees C)
#TOBS = Temperature at the time of observation (tenths of degrees C)

rm(list = ls()) #clear workspace

library(Hmisc)
library(dplyr)

setwd('~/Desktop/23WS/DSMA/Tables right/')

library(dplyr)
library(doParallel)
library(caret)
library(smotefamily)

source("DailyLevelData_analysis_functions.r")

weatherdatafromprof<-load("~/Desktop/23WS/DSMA/Tables right/weather_data.RData")

# This loop extracts the weather data, and add it to the yelp data.
if(0){
  #load data
  yelp_data <- read.csv("DailyLevel_data_tip_Imputed.csv",header=TRUE,skipNul = T) #read csv file
  yelp_data$date <- as.Date(yelp_data$date)
  yelp_data$X=NULL
  
  #---- read the temperature data
  wear=extractweather(yelp_data,resol=.25)
  
  # take the averages across stations for each coordinate
  weather=weardailyavg(wear)
  
  
  
  dates=sort(unique(yelp_data$date))
  weatherstations=as.data.frame(t(sapply(weather,function(x){colMeans(x$range)})))
  
  # adding weather data to yelp_data
  if(1){
    stations_by=t(apply(yelp_data[,c("latitude","longitude")],1,
                        function(x){a=sort((x[1]-weatherstations$rangelat)^2+
                                             (x[2]-weatherstations$rangelong)^2,index.return=T)
                        return(a$ix[1:50])})) # finding the 50 closest stations
    
    # add for example, temperature forecasts to the weather data
    for(i in 1:length(weather)){
      if(nrow(weather[[i]]$data)==0)
        next
      store_weather=weather[[i]]$data
      store_weather$TOBS_1=c(store_weather$TOBS[2:nrow(store_weather)],NA)
      store_weather$TOBS_2=c(store_weather$TOBS[3:nrow(store_weather)],NA,NA)
      store_weather$TOBS_3=c(store_weather$TOBS[4:nrow(store_weather)],NA,NA,NA)
      store_weather$TOBS_4=c(store_weather$TOBS[5:nrow(store_weather)],NA,NA,NA,NA)
      weather[[i]]$data=store_weather
    }
    weatherinf=colnames(store_weather)[-1] # which weather variables are available?
    
    yelp_data_weather=NULL
    for(i in 1:length(weather)){
      k=1 # start with the closest station
      stores_in=stations_by[,k]==i
      if(sum(stores_in)==0)
        next
      store_weather=weather[[i]]$data
      
      temp=yelp_data[stores_in,]
      temp=merge(temp,store_weather,by.x="date",by.y="DATE",all.x=T)
      yelp_data_weather=rbind(yelp_data_weather,temp)
      print(i)
    }
    
    # now deal with the missings, by going to the next possible station
    temp_indx=is.na(yelp_data_weather[,"TOBS"])|is.na(yelp_data_weather[,"PRCP"])
    k_changed=NULL
    for(i in which(temp_indx)){
      temp_date=yelp_data_weather[i,]$date
      for(k in 2:ncol(stations_by)){
        temp=weather[[stations_by[i,k]]]$data
        if(!is.na(as.numeric(temp[temp$DATE==temp_date,"TOBS"]))&!is.na(as.numeric(temp[temp$DATE==temp_date,"PRCP"])))
          break
      }
      k_changed=c(k_changed,k)
      
      yelp_data_weather[i,weatherinf]=temp[temp$DATE==temp_date,-1]
      #print(i)
    }
    
    # add weekends and quarters
    temp=weekdays(yelp_data_weather$date,abbreviate = T)
    yelp_data_weather$WE=temp=="Sat"|temp=="Sun"
    
    yelp_data_weather$Quarter=as.factor(quarters(yelp_data_weather$date))
    
    #save(file="yelp_data_weather.RData",list=c("yelp_data_weather"))
    write.csv(yelp_data_weather,file="yelp_data_tip_weather.csv")
    
  }
  
}
write.csv(yelp_data_weather,"yelp_data_tip_weather.csv")

######### yelp_data 
yelp_data_weather=read.csv(file="yelp_data_tip_weather.csv")

#### some adjustments to the imported data ####
yelp_data=yelp_data_weather

yelp_data$date = as.Date(yelp_data$date)
yelp_data$ch_in_string[yelp_data$ch_in>=1]="ch_in"
yelp_data$ch_in_string[yelp_data$ch_in==0]="Noch_in"
yelp_data$ch_in_string <- as.factor(yelp_data$ch_in_string)
#yelp_data$ch_in_string <- relevel(yelp_data$ch_in_string,ref="ch_in") # since the performance evaluations are mainly made
# to check for the minority class - in our case Noch_in


yelp_data$parking=as.factor(yelp_data$parking)
yelp_data$is_open=as.factor(yelp_data$is_open)
yelp_data$WE=as.factor(yelp_data$WE)
yelp_data$Quarter=as.factor(yelp_data$Quarter)

yelp_data <- subset(yelp_data,select = -c(business_open)) 

write.csv(yelp_data,"yelp_data.csv")

#### correlation check ####
# check correlations with all numerical cars
correlations <- round(cor(yelp_data[, c("WE","price","parking","TMIN","SNOW",
                                        "fast_food","brunch","bars","TOBS","PRCP","n_photo","female","male","cum_n_tips",
                                        "cum_max_friends","cum_max_u_elite","cum_max_us_fans","cum_max_review_count",
                                        "american","french","japanese","chinese","mexican","thai","is_trendy",
                                        "latitude","longitude","stars","is_open","caters","wifi","reservation",
                                        "alcohol","noise","credit_card")]),2)
print(correlations)

# Make a nice chart
#install.packages("corrplot")
library(corrplot)
# select only the numeric variables
# sapply tests for each column, whether it is numeric or not
yelp_numeric<-yelp_data[sapply(yelp_data, is.numeric)]
correl_matrix<-cor(yelp_numeric,use="pairwise.complete.obs") # correlation matrix
corrplot(correl_matrix) # correlation plot
#### train & evaluate - imbalance ####
yelp_data<-read.csv("yelp_data.csv")
yelp_data<-yelp_data[,-1]
# Split randomly
set.seed(66)
yelp_data_na=yelp_data

yelp_data_na$WE <- as.integer(as.logical(yelp_data_na$WE))
yelp_data$WE <- as.integer(as.logical(yelp_data$WE))

summary(yelp_data_na)

yelp_data <- na.omit(yelp_data)

# list of variables in your model
varsin=c("ch_in_string","ch_in","WE","price","parking","TMIN","SNOW",
         "fast_food","brunch","bars","TOBS","PRCP","n_photo","female","male","cum_n_tips",
         "cum_max_friends","cum_max_u_elite","cum_max_us_fans","cum_max_review_count",
         "american","french","japanese","chinese","mexican","thai","is_trendy",
         "latitude","longitude","stars","is_open","caters","wifi","reservation",
         "alcohol","noise","credit_card")
yelp_data=subset(yelp_data,select=varsin)
datasetsize=nrow(yelp_data)/1 # would you like to work only  on a subset of your data? 
x <- yelp_data[sample(1:nrow(yelp_data), datasetsize, replace = F),]
x.train <- x[1:floor(nrow(x)*.75), ]
x.evaluate <- x[(floor(nrow(x)*.75)+1):nrow(x), ]

BaseFormula <- as.formula(paste0("ch_in_string~",paste(varsin[-c(1,2)],collapse = "+")))
BaseFormula1 <- as.formula(paste0("ch_in~",paste(varsin[-c(1,2)],collapse = "+")))
summary(yelp_data)

# create dummies (required for SMOTE)
x.traindum=cbind(x.train[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.train),newdata = x.train))
x.evaluatedum=cbind(x.evaluate[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.evaluate),newdata = x.evaluate))
library(caret)

# class imbalance check.
temp=table(x.train[,"ch_in_string"])
print(temp)
# if yes, maybe you want do random over-sampling:
if(0){
  oversampled=x.train[x.train$ch_in_string==names(temp)[sort.int(temp,index.return=T,decreasing = T)$ix[1]],]
  minclass=names(temp)[sort.int(temp,index.return=T)$ix[1]]
  for(m in 1:(length(temp)-1)){
    minchclass=names(temp)[sort.int(temp,index.return=T)$ix[m]]
    minclassdat=x.train[x.train$ch_in_string==minchclass,]
    minclassdat=minclassdat[sample(1:nrow(minclassdat), sort(temp,decreasing = T)[1] , replace = T),]
    oversampled=rbind(oversampled,minclassdat)
  }
  x.train=oversampled
}

summary(x.traindum)

# or do SMOTE:
if(1){
  x.traindum_smote=SMOTE(x.traindum[,-c(1,2)],x.traindum[,2])$data
  names(x.traindum_smote)[ncol(x.traindum_smote)]="ch_in_string"
  x.traindum_smote$ch_in=ifelse(x.traindum_smote$ch_in_string=="ch_in",1,0)
  x.traindum_smote$ch_in_string=as.factor(x.traindum_smote$ch_in_string)
  x.traindum=x.traindum_smote
  rm(x.traindum_smote)
}
temp=table(x.traindum[,"ch_in_string"])
print(temp)


#### normalize data (very important for ML techniques, but not for logistic regression) ####
x.trainnorm=predict(preProcess(x.traindum, method = "range"), newdata=x.traindum)
x.evaluatenorm=predict(preProcess(x.evaluatedum, method = "range"), newdata=x.evaluatedum)

# adjust Baseformulea to the dummy version of the data
varsin_dum=varsin[1:2]
for(i in 3:length(varsin)){
  if(!is.null(levels(x[,varsin[i]]))){
    for(j in 2:nlevels(x[,varsin[i]])){ # first level will be considered as the base-level
      varsin_dum=c(varsin_dum,paste(varsin[i],levels(x[,varsin[i]])[j],sep="."))
    }
  }else{
    varsin_dum=c(varsin_dum,varsin[i])
  }
}
x.traindum$ch_in_string<-as.factor(x.traindum$ch_in_string)
x.evaluatedum$ch_in_string<-as.factor(x.evaluatedum$ch_in_string)
x.trainnorm$ch_in_string<-as.factor(x.trainnorm$ch_in_string) 
x.evaluatenorm$ch_in_string<-as.factor(x.evaluatenorm$ch_in_string)

# redo the releveling:
x.traindum$ch_in_string=relevel(x.traindum$ch_in_string,ref="Noch_in") 
x.evaluatedum$ch_in_string=relevel(x.evaluatedum$ch_in_string,ref="Noch_in")
x.trainnorm$ch_in_string=relevel(x.trainnorm$ch_in_string,ref="Noch_in") 
x.evaluatenorm$ch_in_string=relevel(x.evaluatenorm$ch_in_string,ref="Noch_in")

BaseFormula_dum <- as.formula(paste0("ch_in_string~",paste(varsin_dum[-c(1,2)],collapse = "+")))
BaseFormula1_dum <- as.formula(paste0("ch_in~",paste(varsin_dum[-c(1,2)],collapse = "+")))

# set threshold probability: usually .5, but better is to set it to the portion of 1's. 
probthres=mean(x.traindum$ch_in)

#### 1. the analyses -LOGIT ####
ptm <- proc.time()
x.modelLogit <- glm(BaseFormula_dum , data = x.traindum, family = "binomial") # estimating the probability of "checkin"

summary(x.modelLogit)

x.evaluate$predictionlogit <- predict(x.modelLogit, newdata=x.evaluatedum, type = "response")
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit>probthres] <- "ch_in"
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit<=probthres] <- "Noch_in"

x.evaluate$correctlogit <- x.evaluate$predictionlogitclass == x.evaluate$ch_in_string
x.evaluate$correctlogit <- as.integer(as.logical(x.evaluate$correctlogit))

source("DailyLevelData_analysis_functions.r")

print(paste("% of predicted classifications correct", mean(x.evaluate$correctlogit)))
LogitOutput <- makeLiftPlot(x.evaluate$predictionlogit,x.evaluate,"Logit")

TimeAux <- proc.time() - ptm 
#LogitOutput$summary=summary(x.modelLogit)
LogitOutput$TimeElapsed <- TimeAux[3]
LogitOutput$PercCorrect <- mean(x.evaluate$correctlogit)*100
Logitconfmatrix <- table(x.evaluate$predictionlogitclass,x.evaluate$ch_in_string)
rm(TimeAux)
Logitconfmatrix

#### 2. SVM ####
cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
# fast trainer
x.modelSVM <- train(BaseFormula_dum, data = x.trainnorm, method="svmRadial", cachesize=12000, tolerance=.01,
                    trControl = trainControl(classProbs =  TRUE))

x.evaluate$predictionSVM <- predict(x.modelSVM, newdata=x.evaluatenorm, type="prob")


x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctSVM <- x.evaluate$predictionSVMclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctSVM)))

# for fast trainer you can also get the variable importance
print(varImp(x.modelSVM))

# Extract the class probabilities.
x.evaluate$predictionSVM <- x.evaluate$predictionSVM[,'ch_in']

SVMOutput <- makeLiftPlot(x.evaluate$predictionSVM,x.evaluate,"SVM")

TimeAux <- proc.time() - ptm 
SVMOutput$TimeElapsed <- TimeAux[3]
SVMOutput$PercCorrect <- mean(x.evaluate$correctSVM)*100
SVMconfmatrix <- table(x.evaluate$predictionSVMclass,x.evaluate$ch_in_string)
SVMconfmatrix
rm(TimeAux)
stopCluster(cl)

#### 3. Neural network ####
cl <- makeCluster(detectCores())
registerDoParallel(cl)

library(NeuralNetTools) # required for plotting
# fast trainer using parallel computations
ptm <- proc.time()
mlp_grid = expand.grid(layer1 = 5,
                       layer2 = 0,
                       layer3 = 0)
x.modelNNet <- train(BaseFormula_dum, data=x.trainnorm, method='mlpML',tuneGrid=mlp_grid) 

x.evaluate$predictionNNet <- predict(x.modelNNet, newdata = x.evaluatenorm, type="prob")

x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$correctNNet <- x.evaluate$predictionNNetclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctNNet)))

print(varImp(x.modelNNet))
# plot NNet
if(0){
  NeuralNetTools::plotnet(x.modelNNet$finalModel)
}
x.evaluate$predictionNNet <- x.evaluate$predictionNNet[,"ch_in"]

NNetOutput <- makeLiftPlot(x.evaluate$predictionNNet,x.evaluate,"Neural Network")

TimeAux <- proc.time() - ptm 
#NNetOutput$summary=varImp(x.modelNNet)
NNetOutput$TimeElapsed <- TimeAux[3]
NNetOutput$PercCorrect <- mean(x.evaluate$correctNNet)*100
NNetconfmatrix <- table(x.evaluate$predictionNNetclass,x.evaluate$ch_in_string)
NNetconfmatrix
rm(TimeAux)

stopCluster(cl)

#### 4. TREE ####
# fast model using parallel computation
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
x.modelTree <- train(BaseFormula_dum, data=x.trainnorm, method='ctree') 


x.evaluate$predictionTree <- predict(x.modelTree, newdata = x.evaluatenorm, type = "prob")

x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionTreeClass <- factor(x.evaluate$predictionTreeClass, levels=c("Noch_in","ch_in"))

x.evaluate$correctTree <- x.evaluate$predictionTreeClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctTree)))

x.evaluate$predictionTree <- x.evaluate$predictionTree[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelTree))

# plot tree, if desired 
if(0){
  plot(x.modelTree$finalModel)
}

TreeOutput <- makeLiftPlot(x.evaluate$predictionTree,x.evaluate,"Tree")

TimeAux <- proc.time() - ptm 
#TreeOutput$summary <- varImp(x.modelTree)
TreeOutput$TimeElapsed <- TimeAux[3]
TreeOutput$PercCorrect <- mean(x.evaluate$correctTree)*100
Treeconfmatrix <- table(x.evaluate$predictionTreeClass,x.evaluate$ch_in_string)
Treeconfmatrix
rm(TimeAux)

stopCluster(cl)


#### 5. Bagging ####
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# fast training using parallel computation
x.modelBagging  <- train(BaseFormula_dum, data=x.trainnorm, method="treebag",importance=T)

# Use the model to predict the evaluation.
x.evaluate$predictionBagging <- predict(x.modelBagging, newdata=x.evaluatenorm, type="prob")

x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionBaggingClass <- factor(x.evaluate$predictionBaggingClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctBagging <- x.evaluate$predictionBaggingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBagging)))

# Extract the class probabilities.
x.evaluate$predictionBagging <- x.evaluate$predictionBagging[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelBagging))

BaggingOutput <- makeLiftPlot(x.evaluate$predictionBagging,x.evaluate,"Bagging")

TimeAux <- proc.time() - ptm
#BaggingOutput$summary <- varImp(x.modelBagging)
BaggingOutput$TimeElapsed <- TimeAux[3]
BaggingOutput$PercCorrect <- mean(x.evaluate$correctBagging)*100
Baggingconfmatrix <- table(x.evaluate$predictionBaggingClass,x.evaluate$ch_in_string)
Baggingconfmatrix
rm(TimeAux)
stopCluster(cl)



#### 6. Boosting ####
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# Create a model using boosting ensemble algorithms
# fast trainer using parallel computation
x.modelBoosting  <- train(BaseFormula_dum, data=x.trainnorm, method = 'blackboost')#,  method = 'bstTree')

# Use the model to predict the evaluation.
x.evaluate$predictionBoosting <- predict(x.modelBoosting, newdata=x.evaluatenorm,type="prob")

x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionBoostingClass <- factor(x.evaluate$predictionBoostingClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctBoosting <- x.evaluate$predictionBoostingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBoosting)))

# Extract the class probabilities.
x.evaluate$predictionBoosting <- x.evaluate$predictionBoosting[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelBoosting))

# Make a lift curve
BoostingOutput <- makeLiftPlot(x.evaluate$predictionBoosting,x.evaluate,"Boosting")

TimeAux <- proc.time() - ptm 
#BoostingOutput$summary <- varImp(x.modelBoosting)
BoostingOutput$TimeElapsed <- TimeAux[3]
BoostingOutput$PercCorrect <- mean(x.evaluate$correctBoosting)*100
Boostingconfmatrix <- table(x.evaluate$predictionBoostingClass,x.evaluate$ch_in_string)
Boostingconfmatrix
rm(TimeAux)

stopCluster(cl)


#### 7. RANDOM FOREST ####
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# Create a model using "random forest and bagging ensemble algorithms
# a fast trainer using parallel computation
x.modelRF <- train(BaseFormula_dum, data=x.trainnorm, method="parRF") 

# Use the model to predict the evaluation.
x.evaluate$predictionRF <- predict(x.modelRF, newdata=x.evaluatenorm, type = "prob")

x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionRFClass <- factor(x.evaluate$predictionRFClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctRF <- x.evaluate$predictionRFClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctRF)))

# Extract the class probabilities.
x.evaluate$predictionRF <- x.evaluate$predictionRF[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelRF))

RFOutput <- makeLiftPlot(x.evaluate$predictionRF,x.evaluate,"Random Forest")

TimeAux <- proc.time() - ptm 
#RFOutput$summary <- varImp(x.modelRF)
RFOutput$TimeElapsed <- TimeAux[3]
RFOutput$PercCorrect <- mean(x.evaluate$correctRF)*100
RFconfmatrix <- table(x.evaluate$predictionRFClass,x.evaluate$ch_in_string)
RFconfmatrix
rm(TimeAux)
stopCluster(cl)


#### SOME Summarizing plots ####

OverallTDL <- c(LogitOutput$TDL,SVMOutput$TDL,TreeOutput$TDL,BaggingOutput$TDL,BoostingOutput$TDL,RFOutput$TDL,NNetOutput$TDL)
OverallGINI <- c(LogitOutput$GINI,SVMOutput$GINI,TreeOutput$GINI,BaggingOutput$GINI,BoostingOutput$GINI,RFOutput$GINI,NNetOutput$GINI)

ForGraph <- data.frame(OverallTDL,OverallGINI)

myLeftAxisLabs <- pretty(seq(0, max(ForGraph$OverallTDL), length.out = 10))
myRightAxisLabs <- pretty(seq(0, max(ForGraph$OverallGINI), length.out = 10))

myLeftAxisAt <- myLeftAxisLabs/max(ForGraph$OverallTDL)
myRightAxisAt <- myRightAxisLabs/max(ForGraph$OverallGINI)

ForGraph$OverallTDL1 <- ForGraph$OverallTDL/max(ForGraph$OverallTDL)
ForGraph$OverallGINI1 <- ForGraph$OverallGINI/max(ForGraph$OverallGINI)

op <- par(mar = c(5,3,3,3) + 0.1)

barplot(t(as.matrix(ForGraph[, c("OverallTDL1", "OverallGINI1")])), 
        beside = TRUE, yaxt = "n", 
        names.arg = c("Logit","SVM","Tree","Bagging","Boosting","Random Forest","Neural Network"), 
        ylim=c(0, max(c(myLeftAxisAt, myRightAxisAt))), ylab =	"Top Decile Lift", 
        main="Performance of the Machine Learning Algorithms")

#legend = c("TDL","GINI"), 
axis(2, at = myLeftAxisAt, labels = myLeftAxisLabs)

axis(4, at = myRightAxisAt, labels = myRightAxisLabs)

mtext("GINI Coefficient", side = 4, line = 3, cex = par("cex.lab"))

mtext(c(paste(round(LogitOutput$TimeElapsed,digits=2),"sec"),
        paste(round(SVMOutput$TimeElapsed,digits=2),"sec"),
        paste(round(TreeOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BaggingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BoostingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(RFOutput$TimeElapsed,digits=2),"sec"),
        paste(round(NNetOutput$TimeElapsed,digits=2),"sec")), side = 1, line = 3, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20))
mtext(c(paste(round(LogitOutput$PercCorrect,digits=0),"%"),
        paste(round(SVMOutput$PercCorrect,digits=0),"%"),
        paste(round(TreeOutput$PercCorrect,digits=0),"%"),
        paste(round(BaggingOutput$PercCorrect,digits=0),"%"),
        paste(round(BoostingOutput$PercCorrect,digits=0),"%"),
        paste(round(RFOutput$PercCorrect,digits=0),"%"),
        paste(round(NNetOutput$PercCorrect,digits=0),"%")), side = 1, line = 4, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20))

mtext("Calc. time", side = 1, line = 3, cex = par("cex.lab"), at = -.8)
mtext("% correct", side = 1, line = 4, cex = par("cex.lab"), at = -.8)

x.evaluate$ch_in_string<-as.factor(x.evaluate$ch_in_string)

lift_obj=lift(ch_in_string~predictionBagging+predictionBoosting+predictionTree+predictionNNet+predictionSVM+predictionlogit+predictionRF,data=x.evaluate,class="ch_in")

ggplot(lift_obj)



#### precision Recall F1 ####
tp <- RFconfmatrix[2,1]
fp <- RFconfmatrix[2,2]
tn <- RFconfmatrix[1,2]
fn <- RFconfmatrix[1,1]
# Calculate precision
precision <- tp / (tp + fp)
# Calculate recall
recall <- tp / (tp + fn)
# Calculate the F1 score
f1 <- 2 * (precision * recall) / (precision + recall)
precision
recall
f1
Treeconfmatrix
tp <- Treeconfmatrix[2,1]
fp <- Treeconfmatrix[2,2]
tn <- Treeconfmatrix[1,2]
fn <- Treeconfmatrix[1,1]
# Calculate precision
precision <- tp / (tp + fp)
# Calculate recall
recall <- tp / (tp + fn)
# Calculate the F1 score
f1 <- 2 * (precision * recall) / (precision + recall)
precision
recall
f1
Boostingconfmatrix
tp <- Boostingconfmatrix[2,1]
fp <- Boostingconfmatrix[2,2]
tn <- Boostingconfmatrix[1,2]
fn <- Boostingconfmatrix[1,1]
# Calculate precision
precision <- tp / (tp + fp)
# Calculate recall
recall <- tp / (tp + fn)
# Calculate the F1 score
f1 <- 2 * (precision * recall) / (precision + recall)
precision
recall
f1
Baggingconfmatrix
tp <- Baggingconfmatrix[2,1]
fp <- Baggingconfmatrix[2,2]
tn <- Baggingconfmatrix[1,2]
fn <- Baggingconfmatrix[1,1]
# Calculate precision
precision <- tp / (tp + fp)
# Calculate recall
recall <- tp / (tp + fn)
# Calculate the F1 score
f1 <- 2 * (precision * recall) / (precision + recall)
precision
recall
f1
NNetconfmatrix
tp <- NNetconfmatrix[1,1]
fp <- NNetconfmatrix[1,2]
tn <- NNetconfmatrix[2,2]
fn <- NNetconfmatrix[2,1]
# Calculate precision
precision <- tp / (tp + fp)
# Calculate recall
recall <- tp / (tp + fn)
# Calculate the F1 score
f1 <- 2 * (precision * recall) / (precision + recall)
precision
recall
f1

#### bivariate analysis ####
install.packages("ggplot2")
library(ggplot2)

#yelp_agg <- aggregate(ch_in ~ parking, data=yelp_data, FUN="mean")
#qplot(x=yelp_data$parking,y=yelp_data$ch_in,main="check-in",
   #xlab="parking", ylab="check-in",  xlim=c(0,1), ylim=c(0,1),color=I("blue"))  + theme_gray(base_size = 18)

ggplot(yelp_data, aes(x=yelp_data$n_photo,      
                 y = yelp_data$ch_in)) +
  geom_point()

ggplot(yelp_data, aes(x=yelp_data$cum_max_review_count,      
                      y = yelp_data$ch_in)) +
  geom_point()
