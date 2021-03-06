# Adult-Census-Income
Data Loading, since some of the data values are marked with "?", so we are choosing na.strings = "?"
```{r}
adult1 = read.table("adult1.txt", sep = ",",header = FALSE, strip.white = TRUE, na.strings = "?", stringsAsFactors = TRUE)
adult2 = read.table("adult2.txt", sep = ",",header = FALSE, strip.white = TRUE, na.strings = "?", stringsAsFactors = TRUE)
adult = rbind (adult1,adult2)

colnames(adult)=c("age", "workclass", "fnlwgt","education","educationnum", "maritalstatus","occupation", "relationship", "race", "sex","capitalgain", "capitalloss","hoursperweek","nativecountry", "income")


head(adult)
str(adult)

unique(adult$income)
# lets fix the levels <=50K. and >50K. to <=50K and >50K
table(adult$income)
adult$income = as.character(adult$income)
adult$income = with(adult, replace(income, income==">50K.", ">50K"))
adult$income = with(adult, replace(income, income=="<=50K.", "<=50K"))
adult$income = as.factor(adult$income)

# deleting 52 duplicate rows.
adult = adult[!duplicated(adult),]
```
Check the class balance of the data and  clean the attribute income.
```{r}

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
incompletecases= adult[which(complete.cases(adult)==FALSE),]
table(incompletecases$income)
nrow(incompletecases[is.na(incompletecases$workclass) & is.na(incompletecases$occupation),])
incompletecases[!is.na(incompletecases$workclass) & is.na(incompletecases$occupation),]
```
It means that most of the data points having missing values belong to the higher class. we see that missing values in workclass has missing in occupation too. After carefully observing I find that, the values which are missing in occupation but not in workclass, are the ones which are never-worked. It totally make sense. But these values have non zero working hours, which seems ambiguity, but lets leave as it is for now.


--- Explore Age
 
```{r}
library(psych)

unique(adult$age)
summary(adult$age)
describe(adult$age)
hist(adult$age)
boxplot.stats(adult$age)$out
```

age is skewed to right because of the presence of outliers, but the outliers are completely natural. These outliers are mild that is they lie below the upper outer fence.

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
ggplot(adult, aes(fnlwgt,color= income)) + geom_density()
```
It seems that both distributons are similar.
lets run chi square test to make sure.
This variable may be removed from the data set due to no impact on income variable.
```{r}
#Wilcox test
wilcox.test(fnlwgt~income, adult)

# p value is greater than 0.05, therefore we cannot reject the null hypothesis. We will accept the null hypothesis. So there is no difference between the means of two distributins. There is no much information in this set.
```

--Explore Capital gain and Capital loss
```{r}
library(caret)
summary(adult$capitalgain)

hist(adult$capitalgain)
boxplot.stats(adult$capitalgain)$out
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

--Explore hoursperweek
```{r}
summary(adult$hoursperweek)
describe(adult$hoursperweek)
boxplot(adult$hoursperweek)
boxplot.stats(adult$hoursperweek)$out

```

```{r}
ggplot(aes(x = income, y = hoursperweek), data = adult) + 
  geom_boxplot() +
  stat_summary(fun.y = mean, 
               geom = "point", 
               col = "blue") +
  coord_cartesian(ylim = c(0, 100)) +
  ylab("hoursperweek") +
  xlab("Income") +  
  ggtitle("Box Plot of hoursperweek by Income") 

par(mfrow=c(1,2))
hist(adult$hoursperweek[adult$income=="<=50K"], xlim = c(0,100), xlab = "hoursperweek", main = "income <=50K")
hist(adult$hoursperweek[adult$income==">50K"], xlm = c(0,100), xlab = "hoursperweek", main =  "income >50K")

```


The variable has many outliers.(Almost 25% of data). Do to deal with it we need to discritize this variable.
```{r}
adult <- mutate(adult,
        hoursperweek_cat = ifelse(adult$hoursperweek < 20, "verylow",                     ifelse(adult$hoursperweek >=20 & adult$hoursperweek <40, "low",
        ifelse(adult$hoursperweek >= 40 & adult$hoursperweek < 60, "average", ifelse(adult$hoursperweek >= 60 & adult$hoursperweek < 80, "high", "veryhigh" )))))
adult$hoursperweek_cat = as.factor(adult$hoursperweek_cat)

```


---Explore sex

From graph, I observe that the percentage of women earning more than 50K a year is very less compared to that of men.
```{r}

ggplot(adult, aes(x = adult$sex, fill = adult$income)) +
  geom_bar(position = "stack",na.rm = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Income", 
       y = "Number of people",
       fill = "Income") +
  ggtitle("Income grouped by sex") +   
  scale_y_continuous(breaks = seq(0,40000,1000))

# Percentage of both the sex with income <50K and >=50K:
prop.table(table(adult$sex, adult$income), margin = 1)*100
```

---Explore workclass
```{r}

summary(adult$workclass)

ggplot(adult, aes(x = adult$workclass, fill = adult$income)) +
  geom_bar(position = "stack",na.rm = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Income", 
       y = "Number of people",
       fill = "Income") +
  ggtitle("Income grouped by workclass") +   
  scale_y_continuous(breaks = seq(0,35000,1000))

# Percentage of workclasses with income <50K and >=50K:
prop.table(table(adult$workclass, adult$income), margin = 1)*100
```
 here a few questions arise.
 1) people in neverworked category should have 0 workhours and that may have <50K income unless they high capital gain whereas people working without pay should have nonzero working hours but also they  may have <50K income unless they high capital gain. lets explore.
```{r}
summary(adult[which(adult$workclass == "Never-worked"),])
summary(adult[which(adult$workclass == "Without-pay"),])
```
From summary, its clear that there is some imbiguity in never-worked class (contains 10 rows) as they don't have zero working hours also there occupation is also unknown. So these 10 rows are conflicting. There are two options for that either we can delete these rows or assign 0 to hourperweek feature. I will go with first one.
Also there are two cases from adult2 dataset where these are working without pay with 0 capital gain and still earning more than 50,000.

```{r}
adult[which(adult$workclass == "Without-pay" & adult$income == ">50K"),]
```
---Explore marital status

```{r}

summary(adult$maritalstatus)

ggplot(adult, aes(x = adult$maritalstatus, fill = adult$income)) +
  geom_bar(position = "stack",na.rm = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Income", 
       y = "Number of people",
       fill = "Income") +
  ggtitle("Income grouped by maritalstatus") +   
  scale_y_continuous(breaks = seq(33,23000,1000))

# Percentage of workclasses with income <50K and >=50K:
prop.table(table(adult$maritalstatus, adult$income), margin = 1)*100
```

--- Explore occupation
```{r}

summary(adult$occupation)

ggplot(adult, aes(x = adult$occupation, fill = adult$income)) +
  geom_bar(position = "stack",na.rm = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Income", 
       y = "Number of people",
       fill = "Income") +
  ggtitle("Income grouped by workclass") +   
  scale_y_continuous(breaks = seq(0,7000,500))

# Percentage of occupation categories with income <50K and >=50K:
prop.table(table(adult$occupation, adult$income), margin = 1)*100
```



--- Explore relationship
```{r}

summary(adult$relationship)

ggplot(adult, aes(x = adult$relationship, fill = adult$income)) +
  geom_bar(position = "stack",na.rm = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Income", 
       y = "Number of people",
       fill = "Income") +
  ggtitle("Income grouped by relationship") +   
  scale_y_continuous(breaks = seq(0,20000,1000))

# Percentage of occupation categories with income <50K and >=50K:
prop.table(table(adult$relationship, adult$income), margin = 1)*100
```


Lets check whether a relation is assigned according to marital status.

```{r}
table(adult$relationship, adult$maritalstatus)

```
Here the data contains 23 rows with relationship "Not in family" but marital status as "married civ spouse".

---Explore race
```{r}
summary(adult$race)

ggplot(adult, aes(x = adult$race, fill = adult$income)) +
  geom_bar(position = "stack",na.rm = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Income", 
       y = "Number of people",
       fill = "Income") +
  ggtitle("Income grouped by race") +   
  scale_y_continuous(breaks = seq(100,42000,1000))

# Percentage of occupation categories with income <50K and >=50K:
prop.table(table(adult$race, adult$income), margin = 1)*100

```

---Explore native region
```{r}
summary(adult$nativecountry)

ggplot(adult, aes(x = adult$nativecountry, fill = adult$income)) +
  geom_bar(position = "stack",na.rm = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Income", 
       y = "Number of people",
       fill = "Income") +
  ggtitle("Income grouped by relationship") +   
  scale_y_continuous(breaks = seq(0,20000,1000))

# Percentage of occupation categories with income <50K and >=50K:
prop.table(table(adult$nativecountry, adult$income), margin = 1)*100
```

--- explore education and education num

```{r}
# first see relationship between them
table(adult$education, adult$educationnum)
```

This table justifies the fact that education num is just the number assigned to the levels of education.
We can delete one of these variable.
```{r}
table(adult$education)


```



