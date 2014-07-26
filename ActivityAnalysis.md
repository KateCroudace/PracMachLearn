Analysis of Activity Data
=========================

The training data was loaded into R and some data-cleansing was performed. Firstly all records containing 'N/A' were removed (by identifying those in the first row). The test data set was opened in Excel and columns of missing data were identified. The training data was further reduced down to those predictors which were present in both data sets:

```r
data<-read.csv("pml-training.csv")
dataclean<-data[,!is.na(data[1,])]
dataclean<-(dataclean[,c(2,8:11,21:42,49:51,61:73,83:93)])
```


The data set was then split into two: a training set composing of approximately 60% of the total 19622 records; the remaining 7622 records into a set to be used for cross-validation.


```r
train = sample(1:dim(dataclean)[1],size=12000,replace=F)
trainData = dataclean[train,]
validData = dataclean[-train,]
```

These both still contained a possible 53 predictor variables. In order, to identify those variables which might have most influence, an analysis of near zero variable predictors was performed.


```r
library("caret", lib.loc="C:/Users/crokm/Documents/R/win-library/3.1")
```

```
## Warning: package 'caret' was built under R version 3.1.1
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.1.1
```

```r
var<-nearZeroVar(trainData[,2:53], saveMetrics=TRUE)
var
```

```
##                      freqRatio percentUnique zeroVar   nzv
## roll_belt                1.108        8.6083   FALSE FALSE
## pitch_belt               1.055       13.4000   FALSE FALSE
## yaw_belt                 1.041       14.4750   FALSE FALSE
## total_accel_belt         1.044        0.2417   FALSE FALSE
## gyros_belt_x             1.048        1.0583   FALSE FALSE
## gyros_belt_y             1.093        0.5583   FALSE FALSE
## gyros_belt_z             1.011        1.3250   FALSE FALSE
## accel_belt_x             1.037        1.3167   FALSE FALSE
## accel_belt_y             1.137        1.1333   FALSE FALSE
## accel_belt_z             1.065        2.3250   FALSE FALSE
## magnet_belt_x            1.050        2.4917   FALSE FALSE
## magnet_belt_y            1.069        2.3417   FALSE FALSE
## magnet_belt_z            1.031        3.5417   FALSE FALSE
## roll_arm                51.550       19.3750   FALSE FALSE
## pitch_arm               89.652       22.1250   FALSE FALSE
## yaw_arm                 29.457       21.0667   FALSE FALSE
## total_accel_arm          1.037        0.5333   FALSE FALSE
## gyros_arm_x              1.096        5.2083   FALSE FALSE
## gyros_arm_y              1.506        2.9833   FALSE FALSE
## gyros_arm_z              1.136        1.9917   FALSE FALSE
## accel_arm_x              1.069        6.3417   FALSE FALSE
## accel_arm_y              1.283        4.3417   FALSE FALSE
## accel_arm_z              1.139        6.3917   FALSE FALSE
## magnet_arm_x             1.017       10.9000   FALSE FALSE
## magnet_arm_y             1.000        7.1000   FALSE FALSE
## magnet_arm_z             1.121       10.3917   FALSE FALSE
## roll_dumbbell            1.156       87.2583   FALSE FALSE
## pitch_dumbbell           2.714       84.9250   FALSE FALSE
## yaw_dumbbell             1.091       86.8583   FALSE FALSE
## total_accel_dumbbell     1.059        0.3583   FALSE FALSE
## gyros_dumbbell_x         1.018        1.9167   FALSE FALSE
## gyros_dumbbell_y         1.218        2.1833   FALSE FALSE
## gyros_dumbbell_z         1.068        1.5667   FALSE FALSE
## accel_dumbbell_x         1.020        3.3250   FALSE FALSE
## accel_dumbbell_y         1.007        3.7417   FALSE FALSE
## accel_dumbbell_z         1.097        3.3167   FALSE FALSE
## magnet_dumbbell_x        1.085        8.6250   FALSE FALSE
## magnet_dumbbell_y        1.138        6.7583   FALSE FALSE
## magnet_dumbbell_z        1.065        5.4417   FALSE FALSE
## roll_forearm            11.150       14.7833   FALSE FALSE
## pitch_forearm           60.872       20.8583   FALSE FALSE
## yaw_forearm             14.931       13.9167   FALSE FALSE
## total_accel_forearm      1.154        0.5750   FALSE FALSE
## gyros_forearm_x          1.156        2.3500   FALSE FALSE
## gyros_forearm_y          1.025        5.9417   FALSE FALSE
## gyros_forearm_z          1.133        2.3750   FALSE FALSE
## accel_forearm_x          1.017        6.4583   FALSE FALSE
## accel_forearm_y          1.000        7.9583   FALSE FALSE
## accel_forearm_z          1.101        4.5583   FALSE FALSE
## magnet_forearm_x         1.118       11.8083   FALSE FALSE
## magnet_forearm_y         1.163       15.0083   FALSE FALSE
## magnet_forearm_z         1.056       13.1417   FALSE FALSE
```

The results suggest that there may be some benefit from including roll, pitch and yaw variables for the four accelerometers. We performed a random forest analysis using them via:


```r
model<-train(classe~roll_belt+pitch_belt+yaw_belt+roll_arm+pitch_arm+yaw_arm+roll_dumbbell+pitch_dumbbell+yaw_dumbbell+roll_forearm+pitch_forearm+yaw_forearm, method="rf", data=trainData)
```

```
## Loading required package: randomForest
```

```
## Warning: package 'randomForest' was built under R version 3.1.1
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```
## Warning: package 'e1071' was built under R version 3.1.1
```
and used the model generated to predict the classe to compare with actual values. For the training dataset, the following results were obtained:


```r
predTrain=predict(model,trainData)
confusionMatrix(predTrain,trainData$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 3419    0    0    0    0
##          B    0 2359    0    0    0
##          C    0    0 2100    0    0
##          D    0    0    0 1936    0
##          E    0    0    0    0 2186
## 
## Overall Statistics
##                                 
##                Accuracy : 1     
##                  95% CI : (1, 1)
##     No Information Rate : 0.285 
##     P-Value [Acc > NIR] : <2e-16
##                                 
##                   Kappa : 1     
##  Mcnemar's Test P-Value : NA    
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             1.000    1.000    1.000    1.000    1.000
## Specificity             1.000    1.000    1.000    1.000    1.000
## Pos Pred Value          1.000    1.000    1.000    1.000    1.000
## Neg Pred Value          1.000    1.000    1.000    1.000    1.000
## Prevalence              0.285    0.197    0.175    0.161    0.182
## Detection Rate          0.285    0.197    0.175    0.161    0.182
## Detection Prevalence    0.285    0.197    0.175    0.161    0.182
## Balanced Accuracy       1.000    1.000    1.000    1.000    1.000
```

This shows 100% accuracy on the training data. To check against over-fitting, we perform the same check on the cross-validation dataset:


```r
predValid=predict(model,validData)
confusionMatrix(predValid,validData$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2152   17    0    0    0
##          B    5 1395   19    5    6
##          C    1   20 1288    8    3
##          D    2    5   14 1267    9
##          E    1    1    1    0 1403
## 
## Overall Statistics
##                                         
##                Accuracy : 0.985         
##                  95% CI : (0.982, 0.987)
##     No Information Rate : 0.284         
##     P-Value [Acc > NIR] : < 2e-16       
##                                         
##                   Kappa : 0.981         
##  Mcnemar's Test P-Value : 0.00405       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.996    0.970    0.974    0.990    0.987
## Specificity             0.997    0.994    0.995    0.995    1.000
## Pos Pred Value          0.992    0.976    0.976    0.977    0.998
## Neg Pred Value          0.998    0.993    0.995    0.998    0.997
## Prevalence              0.284    0.189    0.173    0.168    0.186
## Detection Rate          0.282    0.183    0.169    0.166    0.184
## Detection Prevalence    0.285    0.188    0.173    0.170    0.184
## Balanced Accuracy       0.996    0.982    0.985    0.993    0.993
```

As expected, the model does not perform as well as on the training data set, but still achieves a 98% accuracy which we judge to be sufficient to proceed to analyse the test data set:


```r
datatest<-read.csv("pml-testing.csv")
predTest = predict(model,datatest)
predTest
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

These have been submitted separately and have proved to be correct.



