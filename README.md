# Adult-Census-Income
Data Loading, since some of the data values are marked with "?", so we are choosing na.strings = "?"
```{r}

setwd("C:/Users/rinavi/Desktop/Vidhu Work Docs/Capstone")
getwd()
adult1 = read.table("adult1.txt", sep = ",",header = FALSE, strip.white = TRUE, na.strings = "?", stringsAsFactors = TRUE)
adult2 = read.table("adult2.txt", sep = ",",header = FALSE, strip.white = TRUE, na.strings = "?", stringsAsFactors = TRUE)
adult = rbind (adult1,adult2)

colnames(adult)=c("age", "workclass", "fnlwgt","education","educationnum", "maritalstatus","occupation", "relationship", "race", "sex","capitalgain", "capitalloss","hoursperweek","nativecountry", "income")

head(adult)
str(adult)
```
Check the class balance of the data
```{r}

unique(adult$income)
# lets fix the levels <=50K. and >50K. to <=50K and >50K
table(adult$income)
adult$income = as.character(adult$income)
adult$income = with(adult, replace(income, income==">50K.", ">50K"))
adult$income = with(adult, replace(income, income=="<=50K.", "<=50K"))
adult$income = as.factor(adult$income)
table(adult$income)

```

Its an imbalance data. I will handle this imbalanced data with two techniques oversampling and undersampling and then compare the result of the classifier


Lets check the missing values.
```{r}
for (i in 1:15) {
  s=  sum(is.na(adult[,i]))
print(paste (colnames(adult)[i],s))

}

```


```{r}
# lets find the number of rows containing the missing values. To do that I will use the function complete.cases.
table (complete.cases (adult))
incompletecases= adult[which(complete.cases(adult)==FALSE),]
table(incompletecases$income)
```
It means that most of the data points having missing values belong to the higher class.


--- Explore Age
 
```{r}
library(psych)

unique(adult$age)
summary(adult$age)
describe(adult$age)
hist(adult$age)
boxplot.stats(adult$age)$out
```

age is skewed to right because of the presence of outliers, but this is completely natural.

```{r}
library(ggplot2)
ggplot(aes(x = income, y = age), data = adult) + 
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = "point", 
               col = "blue") +
  coord_cartesian(ylim = c(10, 100)) +
  ylab("Age") +
  xlab("Income") +  
  ggtitle("Box Plot of Age by Income") 

```

```{r}

par(mfrow=c(1,2))
hist(adult$age[adult$income=="<=50K"], xlim = c(17,90), ylim = c(0,1600), main = "<=50K", xlab = "age", col = "blue", breaks = seq(10,90,2))
hist(adult$age[adult$income==">50K"], xlim = c(17,90),ylim = c(0,1600), main = ">50K", xlab = "age", col = "red", breaks = seq(10,90,2))
```

The Age variable has high variability. The distribution and mean are quite different for income classes.older people tend to earn more. So age will be a good predictor of income. 

--- Explore fnlwgt

#The 'fnlwgt' final weight estimate refers to population totals derived from CPS by creating "weighted tallies" of any specified socio-economic characteristics of the population. 

 
```{r}
summary(adult$fnlwgt)
describe(adult$fnlwgt)
hist(adult$fnlwgt)
```
it is highly skewed. 
```{r}
ggplot(aes(x = income, y = fnlwgt), data = adult) + geom_boxplot() +
  stat_summary(fun.y = mean, geom = "point", col = "blue") +
    ylab("fnlwgt") +  xlab("Income") +  
  ggtitle("Box Plot of fnlwgt by Income") 

```
It seems that both distributons are similar.
lets run chi square test to make sure.
```{r}
chisq.test(table(adult$sex, adult$income))
```
This variable is removed from the data set due to no impact on income variable.

--Explore Capital gain and Capital loss
```{r}
library(caret)
summary(adult$capitalgain)

hist(adult$capitalgain)
#boxplot.stats(Adult$capitalgain)$out

boxplot(capitalgain~income, adult)
sum(adult$capitalgain==0)
#since 91% are zeroes letus check variance
nearZeroVar (adult$capitalgain, saveMetrics = TRUE)
#looking at the statistics of this variable, I descide to bin this variable.
```

```{r}
library(dplyr)
A =adult[adult$capitalgain !=0,]
summary(A$capitalgain)

adult <- mutate(adult,
        capitalgain_cat = ifelse(adult$capitalgain ==0, "nogain",                           ifelse(adult$capitalgain < 3411 & adult$capitalgain >0, "lowgain",
      ifelse(adult$capitalgain >= 3411 & adult$capitalgain <= 13550, "mediumgain", "highgain"))))
table(adult$capitalgain_cat)
```




```{r}
summary(adult$capitalloss)

hist(adult$capitalloss)
#boxplot.stats(Adult$capitalgain)$out

boxplot(capitalloss~income, adult)
sum(adult$capitalloss==0)
#since 95% are zeroes letus check variance
nearZeroVar (adult$capitalloss, saveMetrics = TRUE)
#looking at the statistics of this variable, I descide to bin this variable.
```

```{r}
B =adult[adult$capitalloss !=0,]
summary(B$capitalloss)

adult <- mutate(adult,
        capitalloss_cat = ifelse(adult$capitalloss ==0, "noloss",                           ifelse(adult$capitalloss < 1672 & adult$capitalloss >0, "lowloss",
      ifelse(adult$capitalloss >= 1672 & adult$capitalloss <= 1977, "mediumloss", "highloss"))))
table(adult$capitalloss_cat)
adult$capitalgain_cat = as.factor(adult$capitalgain_cat)
adult$capitalloss_cat = as.factor(adult$capitalloss_cat)
```
