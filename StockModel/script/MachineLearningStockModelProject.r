# TEAM:    The aRmy
# MEMBERS: Lilia, Nick, Tayt


# LIBRARIES ----
library(tidyverse)
library(lubridate)


# FUNCTIONS ----

API_CallMapper <- function(
    SymbolID = "FB", 
    StartDate = lubridate::today() - lubridate::ddays(x = 90), 
    EndDate = lubridate::today() - lubridate::ddays(x = 60), 
    Freq = "1min", 
    TiingoToken = "bb4ce17dee09aa67599085bde6f6c0e420aa5d5f", #NCRIVERA
    # TiingoToken = "bff0b0991227257c787e3575f44287a7455d91d2", #MISTERDF
    form = "json"
){
    
    API_Call <- str_glue(
        "https://api.tiingo.com/iex/{SymbolID}", 
        "/prices?startDate={StartDate}", 
        "&endDate={EndDate}", 
        "&resampleFreq={Freq}", 
        "&token={TiingoToken}", 
        "&format={form}", 
        sep = "")
    
    if (form == "csv"){
        dplyr::tibble(read.csv(file = API_Call))
    } else {
        dplyr::tibble(jsonlite::fromJSON(txt = API_Call))
    }
}


# DATA IMPORTATION ----


data1 <- API_CallMapper(StartDate = "2020-11-16", EndDate = "2020-12-18", Freq = "1min")
data2 <- API_CallMapper(StartDate = "2020-12-21", EndDate = "2021-01-22", Freq = "1min")
data3 <- API_CallMapper(StartDate = "2021-01-25", EndDate = "2021-02-26", Freq = "1min")

facebook <- rbind(data1, data2, data3)
write.csv(facebook, file = "C:/Users/lilir/Desktop/STU/6 DATA MINING/Rfiles DataMining/FacebookData.csv", row.names = FALSE)

facebook <- facebook %>% 
    
    mutate(
        DateTime   = date %>% ymd_hms(tz = "America/New_York"),
        Year       = DateTime %>% year(), 
        Month      = DateTime %>% month(label = TRUE), 
        Week       = DateTime %>% wday(label = TRUE), 
        Day        = DateTime %>% day(), 
        Hour       = DateTime %>% hour(),
        
        
        # Dividing the Period of the Day between the Morning (Before 12PM) and Afternoon
        TimePeriod = case_when(
            Hour < 12 ~ "Morning", 
            Hour < 17 ~ "Afternoon"
        ) %>% factor(ordered = TRUE, levels = c("Morning", "Afternoon")), 
        
        Minute     = DateTime %>% minute(), 
        
        # Dividing the Hour between First Half (Before Minute 31) and Second Half
        HourPeriod = case_when(
            Minute <= 30 ~ "FirstHalf", 
            TRUE ~ "SecondHalf"
        ) %>% factor(ordered = TRUE, levels = c("FirstHalf", "SecondHalf"))
    ) %>% 
    
    select(-date) %>% 
    relocate(DateTime:HourPeriod)
    


write.csv(facebook, file = "C:/Users/lilir/Desktop/STU/6 DATA MINING/Rfiles DataMining/FacebookDataModified.csv", row.names = FALSE)


Tibble_1 <- unite(facebook, ConcatCol, Day, Hour, sep="-") %>% filter(Minute == 0) %>% filter(!grepl("-16", ConcatCol))
Col <- unite(facebook, ConcatCol, Day, Hour, sep="-") %>% filter(Minute == 30) %>% filter(!grepl("-9", ConcatCol)) %>% select(close)

Tibble_2 <- bind_cols(Tibble_1, Col)

Tibble_2 <-  Tibble_2 %>% 
    mutate(Calc = ((close...13 - open) / open)) %>% 
    mutate(
        Classification = case_when(
            Calc > 0.00 ~ "Increase",
            Calc < 0.00 ~ "Decrease", 
            TRUE ~ "Undefined"
            
        )
    ) 

# Time Series 

## Install packages
install.packages("forecast")

## Load the library

library(forecast)


## View the data

TSdata <-  Tibble_2 %>%
    select(DateTime, Calc )
    
    


## Timeseries Decomposition


## Create time-series object

facebookTS <- ts(TSdata, frequency = 150)
facebookTS

## Decompose the time series

facebookDecomposition <- decompose(facebookTS)
facebookDecomposition


## Plot the decomposition
plot(facebookDecomposition)


## Build ARIMA Model


## Build the ARIMA model
arimaModel <- auto.arima(TSdata$Calc)
arimaModel



## Predict 12 months into the future
arimaForecast <- forecast(arimaModel, h = 12)
arimaForecast


## Visualize the forecast


plot(arimaForecast)




# MARS

install.packages("earth")
library(earth)

MARSdata <- Tibble_2 %>%
    select(Calc, 'close...9', high, low, open, 'close...13' )

marsModel <- earth(Calc ~ ., data = MARSdata)
marsPrediction <-  predict(marsModel, MARSdata)
mse(MARSdata$Calc, marsPrediction)


## Random Forest 
install.packages("caret")
install.packages("randomForest")
library(caret)
library(randomForest)
heart = read.csv("https://s3-us-west-2.amazonaws.com/static-resources.zybooks.com/heart.csv")

## convert the target attribute to a factor
Tibble_2$Classification  <- as.factor(Tibble_2$Classification )

## partition the dataset into training and test data
attach(Tibble_2)
samp.size = floor(0.6*nrow(Tibble_2))
set.seed(123)
train_ind = sample(seq_len(nrow(Tibble_2)), size = samp.size)
train.data = Tibble_2[train_ind,]
test.data = Tibble_2[-train_ind,]

## grow a random forest with 500 trees and 3 randomly chosen attributes 
model.rf <- randomForest(Classification~., data=train.data, mtry=3, ntrees=500)
pred <- predict(model.rf, test.data, type="class")
confusionMatrix(pred, test.data$Classification)


# SVM

install.packages("caTools")
library(caTools)

SVMdata <- Tibble_2 %>%
    select(Calc, 'close...9', high, low, open, 'close...13' )

##Set Random Seed
set.seed(123)

## Split the data into 3:1 ratio 
SVMdata$Sample <-  sample.split(SVMdata$Calc, SplitRatio = .75)
SVMdata

## 75% will our train and the rest the test

trainSVM <-  filter(SVMdata, Sample == TRUE)
testSVM <- filter(SVMdata, Sample == FALSE)


install.packages("kernlab")
library(kernlab)


modelSVM <- ksvm(Calc ~ . ,
                 data = trainSVM,
                 kernel = "vanilladot")

## look at basic information about the model 
modelSVM
pred <-  predict(modelSVM, testSVM)

## Generate confusion matrix
confusionMatrix <-  table(pred,
                          testSVM$Calc,
                          dnn = c("Prediction", "Actual"))

## Calculate Accuracy
accuracy <-  sum(diag(confusionMatrix))/ sum(confusionMatrix)
cat("Vanilla Kernel Accuracy:", accuracy)


modelSVMK <- ksvm(Calc ~ . ,
                  data = trainSVM,
                  kernel = "rbfdot")

predk <-  predict(modelSVMK, testSVM)

## Generate confusion matrix
confusionMatrixk <-  table(predk,
                           testSVM$Calc,
                           dnn = c("Prediction", "Actual"))

## Calculate Accuracy
accuracyk <-  sum(diag(confusionMatrixk))/ sum(confusionMatrixk)
cat("RBFdot Accuracy:", accuracyk)


# Deep Learning

install.packages("neuralnet")
library(neuralnet)

DLdata <- Tibble_2 %>%
    select(Calc, 'close...9', high, low, open, 'close...13' )

hist(DLdata$Calc)

## shapiro test 
shapiro.test(DLdata$Calc)

## no normally distributed Pvalues is not greater than alpha 
## Min Max Scalin g 
normalize <-  function(x) {
    return((x-min(x))/(max(x)-min(x)))
}

## Apply normalization to entire dataframe 
concrete_norm <- as.data.frame(lapply(DLdata, normalize))

## Confirm that the range is now between zero and one
summary(concrete_norm$Calc)

##Set Random Seed
set.seed(123)

## Split the data into 3:1 ratio 
DLdata$Sample <-  sample.split(DLdata$Calc, SplitRatio = .75)
DLdata

## 75% will our train and the rest the test

trainDL <-  filter(DLdata, Sample == TRUE)
testDL <- filter(DLdata, Sample == FALSE)

DL_model <-  neuralnet(formula = Calc ~ 'close...9' +  high  + low + open+ 'close...13' + Sample,
                             data = trainDL)
##Vizualize the network topology 

plot(DL_model)

##Predict strength (index 9) when given [1:8]
model_results <-  compute(DL_model, testDL[2:7])

## obtain predicted strength values 
predicted_Calc <- model_results$net.result

**** ## examnine the correlation between predicted and actual values 
cor(predicted_Calc, testDL$Calc)

# AutoML 

install.packages("h2o")
library(h2o)

AMLdata <- Tibble_2 %>%
    select(Classification , Calc, 'close...9', high, low, open, 'close...13' )

localH2O <-  h2o.init()

## Convert the data frame to an H2O Data Frame
localH2O= h2o.init()



## Sample Data
autoSplit <- h2o.splitFrame(data = AMLdataH2O, ratios = c(.75))
train <- autoSplit[[1]]
testValidation <- autoSplit[[2]]



testValidationSplit <- h2o.splitFrame(data = testValidation, ratios = c(.75))
test <- testValidationSplit[[1]]
validation <- testValidationSplit[[2]]

## GLM to Predict MPG
glmModel <- h2o.glm(y = "Calc",
                    x = c("close...9", "high", "low", "open", "close...13"),
                    training_frame = train)

## Predict using the GLM model and the testing dataset
pred = h2o.predict(object = glmModel, newdata = test)

## gather Performance
print(h2o.performance(glmModel, test))

## AutoML to predict MPG Class
autoMLModel <- h2o.automl(y = "Classification",
                          x = c("close...9", "high", "low", "open", "close...13"),
                          training_frame = train,
                          validation_frame = validation,
                          balance_classes = TRUE,
                          max_runtime_secs = 60,
                          seed = 1234)

## Predict using the GLM model and the testing dataset
pred = h2o.predict(object = autoMLModel, newdata = test)

print(h2o.get_leaderboard(object = autoMLModel, extra_columns = 'ALL'))
## Performance
print(h2o.performance(autoMLModel@leader, test))


