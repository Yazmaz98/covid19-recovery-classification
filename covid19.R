library(dplyr)
library(tidyr)
library("GDAtools")
library(naniar)
library(ggplot2)
library(corrplot)
library(caTools)
library(randomForest)
library(caret)
library(e1071)
library(factoextra)
library(caTools)
library(randomForest)
library(boot)
library(ggpubr)
library(fitdistrplus)
library(mice)
library(ROCR)
library(randomForest)
#install.packages("dendextend")
#library(dendextend)

covidData = read.csv("COVID19_line_list_data.csv",na.strings=c("","NA"," "))
timeSeries= read.csv("time_series_covid_19_recovered.csv",na.strings=c("","NA"," "))

df_tseries = timeSeries

#what countries are present in both datasets?
countries_covid = levels(as.factor(covidData$country))
countries_time = levels(as.factor(timeSeries$Country.Region))
intersect(countries_covid,countries_time)


#-----taking care of the timeseries dataset.
# The dataset provides us with the number of cases in each country/region every day from the 22nd of January to the 16th of April.In order to reduce the number of variables that we're going to work with, we will sum these number to get a number of cases per month.
timeSeries$JanuaryCases = rowSums(timeSeries[,5:14])
timeSeries$FebruaryCases = rowSums(timeSeries[,15:43])
timeSeries$MarchCases = rowSums(timeSeries[,44:74])
timeSeries$AprilCases = rowSums(timeSeries[,75:90])
# We can use this data later to cluster countries based on the number of cases per month.

#Some countries are divided into provinces/district , we'll have to treat each district individually to join the two datasets.
table(as.factor(covidData$country))
table(as.factor(timeSeries$Country.Region))

australiaProvincesTimeseries = unique(as.character(timeSeries$Province.State[timeSeries$Country.Region=="Australia"])) # 8 provinces
australiaProvincesCovid = unique(as.character(covidData$location[covidData$country=="Australia"])) #4

franceProvinceTimeseries = unique(as.character(timeSeries$Province.State[timeSeries$Country.Region=="France"])) #11
franceProvinceCovid = unique(as.character(covidData$location[covidData$country=="France"]))#15

denmarkProvinceTimeseries = unique(as.character(timeSeries$Province.State[timeSeries$Country.Region=="Denmark"])) #3
#denmarkProvinceCovid = unique(as.character(covidData$location[covidData$country=="Denmark"]))#0

netherProvinceTimeseries = unique(as.character(timeSeries$Province.State[timeSeries$Country.Region=="Netherlands"])) #5
netherProvinceCovid = unique(as.character(covidData$location[covidData$country=="Netherlands"]))#0

UKProvinceTimeseries = unique(as.character(timeSeries$Province.State[timeSeries$Country.Region=="United Kingdom"])) #11
UKProvinceCovid = unique(as.character(covidData$location[covidData$country=="UK"]))#5

#However, the datasets do not contain the same number of district

  
#---------------------JOINING TABLES
casesByCountry = dplyr::select(timeSeries,"Province.State","Country.Region","JanuaryCases","FebruaryCases","MarchCases"
                        ,"AprilCases" )
#adding up countries totals for countries with many provinces
indexChina = which(casesByCountry$Country.Region=="China")
chinaTotal = c("", "China",sum(casesByCountry$JanuaryCases[indexChina]),
                   sum(casesByCountry$FebruaryCases[indexChina]),
                   sum(casesByCountry$MarchCases[indexChina]),sum(casesByCountry$AprilCases[indexChina]))

indexAustralia = which(casesByCountry$Country.Region=="Australia")
australiaTotal = c("", "Australia",sum(casesByCountry$JanuaryCases[indexAustralia]),
                   sum(casesByCountry$FebruaryCases[indexAustralia]),
                   sum(casesByCountry$MarchCases[indexAustralia]),sum(casesByCountry$AprilCases[indexAustralia]))

indexUK = which(casesByCountry$Country.Region=="United Kingdom")
ukTotal = c("", "United Kingdom",sum(casesByCountry$JanuaryCases[indexUK]),
            sum(casesByCountry$FebruaryCases[indexUK]),
            sum(casesByCountry$MarchCases[indexUK]),sum(casesByCountry$AprilCases[indexUK]))

indexNTL = which(casesByCountry$Country.Region=="Netherlands")
ntlTotal = c("", "Netherlands",sum(casesByCountry$JanuaryCases[indexNTL]),
             sum(casesByCountry$FebruaryCases[indexNTL]),
             sum(casesByCountry$MarchCases[indexNTL]),sum(casesByCountry$AprilCases[indexNTL]))

indexDenmark = which(casesByCountry$Country.Region=="Denmark")
denmarkTotal = c("", "Denmark",sum(casesByCountry$JanuaryCases[indexDenmark]),
                 sum(casesByCountry$FebruaryCases[indexDenmark]),
                 sum(casesByCountry$MarchCases[indexDenmark]),sum(casesByCountry$AprilCases[indexDenmark]))

indexFrance = which(casesByCountry$Country.Region=="France")
franxeTotal = c("", "France",sum(casesByCountry$JanuaryCases[indexFrance]),
                sum(casesByCountry$FebruaryCases[indexFrance]),
                sum(casesByCountry$MarchCases[indexFrance]),sum(casesByCountry$AprilCases[indexFrance]))


toDelete = c(indexAustralia,indexDenmark,indexFrance,indexNTL,indexUK,indexChina)
casesByCountry=casesByCountry[-toDelete,]
casesByCountry = rbind(casesByCountry,australiaTotal,ukTotal,ntlTotal,denmarkTotal,franxeTotal,chinaTotal)


#------------ covidData NAs treatment
df_covidData = covidData
covidData = covidData[,1:19]
covidData$X = NULL
summary(covidData)
colSums(is.na(covidData))
#We can see from the code above that some variables contain a big portion missing values, let's investigate this further 
gg_miss_var(covidData)
#the dataset contains 1085 observations, we will get rid of all variables that contain more than 250 NAs (a little less than the 1/4th)
# but first let's see how these Nas intersect between variables
gg_miss_upset(covidData)

covidData = dplyr::select(df_covidData, "age","case_in_country","gender","from.Wuhan","visiting.Wuhan",
                   "death","recovered","country" )
casesByCountry$Country.Region = as.character(casesByCountry$Country.Region)
casesByCountry$Country.Region[casesByCountry$Country.Region=="Taiwan*"] = "Taiwan"
casesByCountry$Country.Region[casesByCountry$Country.Region=="US"] = "USA"
casesByCountry$Country.Region[casesByCountry$Country.Region=="United Arab Emirates"] = "UAE"
casesByCountry$Country.Region[casesByCountry$Country.Region=="United Kingdom"] = "UK"
casesByCountry$Country.Region[casesByCountry$Country.Region=="Philippines"] = "Phillipines"

for (i in 1:1085){
  for (j in 1:185) {
    if (covidData$country[i] == casesByCountry$Country.Region[j]){
      covidData$JanCases[i] = casesByCountry$JanuaryCases[j]
      covidData$FebCases[i] = casesByCountry$FebruaryCases[j]
      covidData$MarCases[i] = casesByCountry$MarchCases[j]
      covidData$AprCases[i] = casesByCountry$AprilCases[j]
      break
    } else {
      covidData$JanCases[i] = -99
      covidData$FebCases[i] = -99
      covidData$MarCases[i] = -99
      covidData$AprCases[i] = -99
    }
  }
}

#covidData$reporting.date = as.POSIXct(covidData$reporting.date ,format="%m/%d/%Y")

#filling NAs
length(which(covidData$gender!="male" & covidData$gender!="female")) #177

covidData$age[is.na(covidData$age)] = -99
covidData$case_in_country[is.na(covidData$case_in_country)] = -99
covidData$gender = as.character(covidData$gender)
covidData$gender[is.na(covidData$gender)] = "unknown"
covidData$gender = as.factor(covidData$gender)
covidData$from.Wuhan[is.na(covidData$from.Wuhan)] = "unknown"

#fixing values 
covidData$recovered = as.character(covidData$recovered)
length(which(covidData$recovered!=0 & covidData$recovered!=1)) #152
toChange = which(covidData$recovered!=0 & covidData$recovered!=1)
covidData$recovered[toChange] = 1
covidData$death = as.character(covidData$death)
length(which(covidData$death!=0 & covidData$death!=1)) #21
unknown_deat = which(covidData$death!=0 & covidData$death!=1)
covidData$death[unknown_deat] = "unknown"


#fixing types
covidData$recovered = as.factor(covidData$recovered)
covidData$death = as.factor(covidData$death)
covidData$from.Wuhan = as.factor(covidData$from.Wuhan)
covidData$visiting.Wuhan = as.factor(covidData$visiting.Wuhan)
covidData$JanCases = as.numeric(covidData$JanCases)
covidData$FebCases = as.numeric(covidData$FebCases)
covidData$MarCases = as.numeric(covidData$MarCases)
covidData$AprCases = as.numeric(covidData$AprCases)



#----------2)DATA EXPLORATION
#---visualizing timeseries
Tdf_tseries = t(df_tseries)
Tdf_tseries = as.data.frame(Tdf_tseries)
Tdf_tseries = Tdf_tseries[-c(1,3,4),]
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
Tdf_tseries = header.true(Tdf_tseries)
Time_Series = rownames(Tdf_tseries)
Tdf_tseries <- data.frame(lapply(Tdf_tseries, function(x) as.numeric(as.character(x))))

Tdf_tseries$Time = Time_Series
Tdf_tseries$Time = sub('.', '',Tdf_tseries$Time)
Tdf_tseries$Time = as.POSIXct(Tdf_tseries$Time ,format="%m.%d.%Y")

# total provinces
Tdf_tseries=Tdf_tseries[ , order(names(Tdf_tseries))]

autralia = rowSums(Tdf_tseries[9:16])
china = rowSums(Tdf_tseries[44:76])
denmark = rowSums(Tdf_tseries[86:88])
france = rowSums(Tdf_tseries[103:113])
netherlands = rowSums(Tdf_tseries[172:176])
uk = rowSums(Tdf_tseries[231:241])

toDelete = c(seq(9,16), seq(44,76),seq(86,88),seq(103,113),seq(172,176),seq(231,241))
Tdf_tseries = Tdf_tseries[,-toDelete]

Tdf_tseries$Australia = autralia
Tdf_tseries$China = china
Tdf_tseries$Denmark = denmark
Tdf_tseries$France = france
Tdf_tseries$Netherlands = netherlands
Tdf_tseries$UK = uk

# famous countries during the pandemic
sorted_long <- Tdf_tseries %>% 
  gather(c("China","France","UK","Spain","Italy"),key="Countries",value="Value")
sorted_long %>% 
  ggplot(aes(x= Time,y=Value,col=Countries,group=Countries)) + 
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Evolution of covid-19 in famous countries during the pandemic") 

#eastern europe
sorted_long <- 
  Tdf_tseries %>% 
  gather(c("Belarus","Bulgaria","Czechia","Hungary","Moldova","Poland","Romania","Russia","Slovakia","Ukraine"),
         key="Countries",value="Value")
sorted_long %>% 
  ggplot(aes(x= Time,y=Value,col=Countries,group=Countries)) + 
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Evolution of covid-19 in Eastern Europe") 

#western europe
sorted_long <- 
  Tdf_tseries %>% 
  gather(c("UK","Luxembourg","Ireland","Netherlands","Monaco","Belgium","France","Portugal","Switzerland","Liechtenstein",
           "Czechia"),
         key="Countries",value="Value")
sorted_long %>% 
  ggplot(aes(x= Time,y=Value,col=Countries,group=Countries)) + 
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Evolution of covid-19 in Western Europe") 

#eastern asia
sorted_long <- 
  Tdf_tseries %>% 
  gather(c("China","Japan","Mongolia","India", "Pakistan"),key="Countries",value="Value")
sorted_long %>% 
  ggplot(aes(x= Time,y=Value,col=Countries,group=Countries)) + 
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Evolution of covid-19 in Eastern Asia") 

#western asia
sorted_long <- 
  Tdf_tseries %>% 
  gather(c("Armenia","Azerbaijan","Bahrain","Cyprus","Georgia","Iraq","Israel","Jordan","Kuwait","Lebanon",
           "Oman","West.Bank.and.Gaza","Qatar","Saudi.Arabia","Syria","Turkey","United.Arab.Emirates","Yemen"), 
         key="Countries",value="Value")
sorted_long %>% 
  ggplot(aes(x= Time,y=Value,col=Countries,group=Countries)) + 
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Evolution of covid-19 in Western Asia") 

#north africa
sorted_long <- 
  Tdf_tseries %>% 
  gather(c("Algeria", "Egypt", "Libya", "Morocco", "Sudan", "Tunisia","Western.Sahara"),key="Countries",value="Value")
sorted_long %>% 
  ggplot(aes(x= Time,y=Value,col=Countries,group=Countries)) + 
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Evolution of covid-19 in North Africa") 

#central africa
sorted_long <- 
  Tdf_tseries %>% 
  gather(c("Cameroon", "Central.African.Republic", "Chad", "Congo..Brazzaville.","Equatorial.Guinea", "Gabon",
           "Sao.Tome.and.Principe"),key="Countries",value="Value")
sorted_long %>% 
  ggplot(aes(x= Time,y=Value,col=Countries,group=Countries)) + 
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Evolution of covid-19 in Central Africa") 

#southern africa
sorted_long <- 
  Tdf_tseries %>% 
  gather(c("Angola", "Botswana", "Mozambique", "Namibia", "South.Africa", "Zambia",
           "Zimbabwe"),key="Countries",value="Value")
sorted_long %>% 
  ggplot(aes(x= Time,y=Value,col=Countries,group=Countries)) + 
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Evolution of covid-19 in Southern Africa") 

#north america
sorted_long <- 
  Tdf_tseries %>% 
  gather(c("Antigua.and.Barbuda","Bahamas","Barbados","Belize","Canada","Costa.Rica","Cuba","Dominica",
           "Dominican.Republic","El.Salvador","Grenada","Guatemala","Haiti","Honduras","Jamaica","Mexico","Nicaragua","Panama",
           "Saint.Kitts.and.Nevis","Saint.Lucia","Saint.Vincent.and.the.Grenadines","Trinidad.and.Tobago","US"),key="Countries",
         value="Value")
sorted_long %>% 
  ggplot(aes(x= Time,y=Value,col=Countries,group=Countries)) + 
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Evolution of covid-19 in North america") 

#south america
sorted_long <- 
  Tdf_tseries %>% 
  gather(c("Argentina","Bolivia","Brazil","Chile","Colombia","Ecuador","Guyana","Paraguay","Peru","Suriname",
           "Uruguay","Venezuela"),key="Countries",value="Value")
sorted_long %>% 
  ggplot(aes(x= Time,y=Value,col=Countries,group=Countries)) + 
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Evolution of covid-19 in South america") 

#oceania
sorted_long <- 
  Tdf_tseries %>% 
  gather(c("Australia","Fiji","New.Zealand",
           "Papua.New.Guinea"),key="Countries",value="Value")
sorted_long %>% 
  ggplot(aes(x= Time,y=Value,col=Countries,group=Countries)) + 
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Evolution of covid-19 in Oceania") 

#survival ~ country
covidData %>%
  group_by(country,recovered) %>%
  ggplot(aes(x=country,fill = recovered))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Recovery by country") 
  
#survival ~ gender
covidData %>%
  group_by(gender,recovered) %>%
  ggplot(aes(x=gender,fill = recovered))+
  geom_bar()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Recovery by gender") 
#survival ~ from wuhan
covidData %>%
  group_by(gender,recovered) %>%
  ggplot(aes(x=from.Wuhan,fill = recovered))+
  geom_bar()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Recovery by whether the person travelled from Wuhan or not") 
#survival ~ visiting wuhan
covidData %>%
  group_by(gender,recovered) %>%
  ggplot(aes(x=visiting.Wuhan,fill = recovered))+
  geom_bar()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Recovery by whether the person has visited Wuhan or not") 
#survival ~ age
covidData %>%
  group_by(age,recovered) %>%
  ggplot(aes(x=age,color = recovered))+
  geom_bar()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlim(0, 100)+
  ggtitle("Recovery by age") 

dummy_covid = covidData
dummy_covid$age_category[between(dummy_covid$age,0,10)] = "0-10"
dummy_covid$age_category[between(dummy_covid$age,11,20)] = "11-20"
dummy_covid$age_category[between(dummy_covid$age,21,30)] = "21-30"
dummy_covid$age_category[between(dummy_covid$age,31,40)] = "31-40"
dummy_covid$age_category[between(dummy_covid$age,41,50)] = "41-50"
dummy_covid$age_category[dummy_covid$age>=51] = "50+"
dummy_covid$age_category = as.factor(dummy_covid$age_category)

dummy_covid %>%
  group_by(age_category,recovered) %>%
  ggplot(aes(x=age_category,fill = recovered))+
  geom_bar()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #xlim(0, 100)+
  ggtitle("Recovery by age") 


#----------------3)STATISTICAL TESTS
#is recovery dependent on the gender of the person?
chi_table = table(covidData$gender,covidData$recovered)
#calculating the X2 statistic
p = rowSums(chi_table)
q = colSums(chi_table)
expectedRowProportion=p/sum(chi_table)
expectedRowProportion
expectedColProportion=q/sum(chi_table)
expectedColProportion
E=expectedRowProportion %o% expectedColProportion *sum(chi_table)
X2=sum((chi_table-E)^2/E)
X2
# calculating the p-value
X2dist <- replicate(1000, {
  d1 = sample(covidData$recovered)
  d2 = sample(covidData$gender)
  X = table(d1,d2)
  pi = rowSums(X)
  qi = colSums(X)
  expecRowProportion=pi/sum(X)
  expecColProportion=qi/sum(X)
  E=expecRowProportion %o% expecColProportion *sum(X)
  X2i=sum((X-E)^2/E)
  X2i
})
hist(X2dist)
#The null hypothesis for this test is that there is no relationship between the gender of the patient and his/her recovery
p_value = mean(X2dist>X2)
p_value #
chisq.test(chi_table)

#is recovery dependent on the age of the person?
chi_table = table(dummy_covid$age_category,covidData$recovered)
#calculating the X2 statistic
p = rowSums(chi_table)
q = colSums(chi_table)
expectedRowProportion=p/sum(chi_table)
expectedRowProportion
expectedColProportion=q/sum(chi_table)
expectedColProportion
E=expectedRowProportion %o% expectedColProportion *sum(chi_table)
X2=sum((chi_table-E)^2/E)
X2
# calculating the p-value
X2dist <- replicate(1000, {
  d1 = sample(dummy_covid$recovered)
  d2 = sample(dummy_covid$age_category)
  X = table(d1,d2)
  pi = rowSums(X)
  qi = colSums(X)
  expecRowProportion=pi/sum(X)
  expecColProportion=qi/sum(X)
  E=expecRowProportion %o% expecColProportion *sum(X)
  X2i=sum((X-E)^2/E)
  X2i
})
hist(X2dist)
#The null hypothesis for this test is that there is no relationship between the age of the patient and his/her recovery
p_value = mean(X2dist>X2)
p_value #pvalue > 0.5 we reject H0, age is very relevant to recovery



#is recovery dependent on whether the person comes from Wuhan?
chi_table = table(covidData$from.Wuhan,covidData$recovered)
  #calculating the X2 statistic
p = rowSums(chi_table)
q = colSums(chi_table)
expectedRowProportion=p/sum(chi_table)
expectedRowProportion
expectedColProportion=q/sum(chi_table)
expectedColProportion
E=expectedRowProportion %o% expectedColProportion *sum(chi_table)
X2=sum((chi_table-E)^2/E)
X2
  # calculating the p-value
X2dist <- replicate(1000, {
  d1 = sample(covidData$recovered)
  d2 = sample(covidData$from.Wuhan)
  X = table(d1,d2)
  pi = rowSums(X)
  qi = colSums(X)
  expecRowProportion=pi/sum(X)
  expecColProportion=qi/sum(X)
  E=expecRowProportion %o% expecColProportion *sum(X)
  X2i=sum((X-E)^2/E)
  X2i
})
hist(X2dist)
#The null hypothesis for this test is that there is no relationship between the patient's recovery and whether s/he came from wuhan 
p_value = mean(X2dist>X2)
p_value
# p < 0.05 -> we reject H0 therefore the two variables are dependent
chisq.test(chi_table) #verification ``



#is recovery dependent on whether the person visited Wuhan?
chi_table = table(covidData$visiting.Wuhan,covidData$recovered)
#calculating the X2 statistic
p = rowSums(chi_table)
q = colSums(chi_table)
expectedRowProportion=p/sum(chi_table)
expectedRowProportion
expectedColProportion=q/sum(chi_table)
expectedColProportion
E=expectedRowProportion %o% expectedColProportion *sum(chi_table)
X2=sum((chi_table-E)^2/E)
X2
# calculating the p-value
X2dist <- replicate(1000, {
  d1 = sample(covidData$recovered)
  d2 = sample(covidData$visiting.Wuhan)
  X = table(d1,d2)
  pi = rowSums(X)
  qi = colSums(X)
  expecRowProportion=pi/sum(X)
  expecColProportion=qi/sum(X)
  E=expecRowProportion %o% expecColProportion *sum(X)
  X2i=sum((X-E)^2/E)
  X2i
})
hist(X2dist)
#The null hypothesis for this test is that there is no relationship between whether the patient has visited wuhan and his/her recovery
p_value = mean(X2dist>X2)
p_value #pvalue > 0.05 we reject H0, recovery does depend on whether the person has visited Wuhan or not



#is recovery dependent on the patient's country?
chi_table = table(covidData$country,covidData$recovered)
#calculating the X2 statistic
p = rowSums(chi_table)
q = colSums(chi_table)
expectedRowProportion=p/sum(chi_table)
expectedRowProportion
expectedColProportion=q/sum(chi_table)
expectedColProportion
E=expectedRowProportion %o% expectedColProportion *sum(chi_table)
X2=sum((chi_table-E)^2/E)
X2
# calculating the p-value
X2dist <- replicate(1000, {
  d1 = sample(covidData$recovered)
  d2 = sample(covidData$country)
  X = table(d1,d2)
  pi = rowSums(X)
  qi = colSums(X)
  expecRowProportion=pi/sum(X)
  expecColProportion=qi/sum(X)
  E=expecRowProportion %o% expecColProportion *sum(X)
  X2i=sum((X-E)^2/E)
  X2i
})
hist(X2dist)
#The null hypothesis for this test is that there is no relationship between the gender of the patient and his/her recovery
p_value = mean(X2dist>X2)
p_value 
chisq.test(chi_table,simulate.p.value=TRUE)

#fisher.test(chi_table) 
covidData$country = NULL

#------------------4) Classification
set.seed(123)
split = sample.split(covidData$recovered, SplitRatio = 0.7)
train = subset(covidData, split == TRUE)
test = subset(covidData, split == FALSE)
#-------4)1) logistic regression
model = glm(recovered ~ ., family = 'binomial', data = train)
predictions = predict(model, test, type = 'response')
ROCR_pred = prediction(predictions, test$recovered)

confusion_matrix = table(test$recovered, predictions > 0.5)
accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix) #0.8466
ROCcurve = performance(ROCR_pred, "tpr", "fpr")
plot(ROCcurve)
as.numeric(performance(ROCR_pred, "auc")@y.values)

#-----4)2) random forest
#MANUAL GRID SEARCH
for (sizeNodes in c(10,25,25,50)){
   for (nbTrees in c(50,100,120,150)) {
     RFb = randomForest(recovered~.,data =train , nodesize = sizeNodes, ntree = nbTrees)
     predictForest = predict(RFb, newdata = test)
     forest_confusion_matrix = table(test$recovered, predictForest)
     forest_accuracy = sum(diag(forest_confusion_matrix))/sum(forest_confusion_matrix) 
     cat("nb of trees:",nbTrees,"size of a node",sizeNodes,"accuracy",forest_accuracy, "\n")
   }
 }


RFb = randomForest(recovered ~ ., data=train, nodesize = 25, ntree = 100)
predictForest = predict(RFb, newdata = test)
forest_confusion_matrix = table(test$recovered, predictForest)
forest_accuracy = sum(diag(forest_confusion_matrix))/sum(forest_confusion_matrix) #0.9079


#-----------------5)Clustering
df_clustering = test

df_clustering$gender = as.character(df_clustering$gender)
df_clustering$from.Wuhan = as.character(df_clustering$from.Wuhan)
df_clustering$death = as.character(df_clustering$death)
df_clustering$visiting.Wuhan = as.character(df_clustering$visiting.Wuhan)

df_clustering$gender[df_clustering$gender=="male"] = 0
df_clustering$gender[df_clustering$gender=="female"] = 1
df_clustering$gender[df_clustering$gender=="unknown"] = -1
df_clustering$from.Wuhan[df_clustering$from.Wuhan=="unknown"] = -1
df_clustering$death[df_clustering$death =="unknown"] = -1

df_clustering$gender = as.numeric(df_clustering$gender)
df_clustering$from.Wuhan = as.numeric(df_clustering$from.Wuhan)
df_clustering$death = as.numeric(df_clustering$death)
df_clustering$visiting.Wuhan = as.numeric(df_clustering$visiting.Wuhan)

df_clustering$probabilities = round(predictions*100,2)

#-----5)1) k-means
#FINDING THE NUMBER OF CLUSTERS THAT FITS OUR DATA

preproc <- preProcess(df_clustering)
normData <- predict(preproc, df_clustering)

 wss <- sapply(1:40, function(k){kmeans(df_clustering, k, nstart=25,iter.max = 40 )$tot.withinss})
 plot(1:40, wss,
      type="b", pch = 19, frame = FALSE, main = "Plot of # Clusters vs. tot.withinss", 
      xlab="Number of clusters K",
      ylab="Total within-clusters sum of squares")
 
 bss <- sapply(1:40, function(k){kmeans(df_clustering, k, nstart=25,iter.max = 40 )$betweenss})
 plot(1:40, bss,
      type="b", pch = 19, frame = FALSE, main = "Plot of # Clusters vs. betweenss", 
      xlab="Number of clusters K",
      ylab="Total within-clusters sum of squares")


set.seed(123)
km.res <- kmeans(df_clustering, 3, nstart = 25)
km.res$size
df_clustering$assigned_cluster = km.res$cluster

cluster1 = subset(df_clustering, assigned_cluster ==1 )
cluster2 = subset(df_clustering, assigned_cluster ==2 )
cluster3 = subset(df_clustering, assigned_cluster ==3 )

summary(round(predictions*100,2))
summary(cluster1$probabilities)
summary(cluster2$probabilities)
summary(cluster3$probabilities)

df_clustering$prob_cat[between(df_clustering$probabilities,0,2.5)] = "0-3.52%"
df_clustering$prob_cat[between(df_clustering$probabilities,2.51,28)] = "3.53-28%"
df_clustering$prob_cat[between(df_clustering$probabilities,28.1,59)] = "28-58%"

df_clustering$prob_cat = as.factor(as.character(df_clustering$prob_cat))
df_clustering$assigned_cluster = as.factor(df_clustering$assigned_cluster)

df_clustering %>%
  group_by(assigned_cluster,prob_cat) %>%
  ggplot(aes(x=assigned_cluster,fill = prob_cat))+
  geom_bar()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Dominating probabilty groups by cluster") 

df_clustering$age_category[between(df_clustering$age,0,10)] = "0-10"
df_clustering$age_category[between(df_clustering$age,11,20)] = "11-20"
df_clustering$age_category[between(df_clustering$age,21,30)] = "21-30"
df_clustering$age_category[between(df_clustering$age,31,40)] = "31-40"
df_clustering$age_category[between(df_clustering$age,41,50)] = "41-50"
df_clustering$age_category[df_clustering$age>=51] = "50+"
df_clustering$age_category = as.factor(df_clustering$age_category)

df_clustering %>%
  group_by(age_category,assigned_cluster) %>%
  ggplot(aes(x=assigned_cluster,fill = age_category))+
  geom_bar()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #xlim(0, 100)+
  ggtitle("Dominating age group by cluster") 




#------5)2)hierarchical clustering
distance = dist(df_clustering, method = 'euclidean')
Hclust = hclust(distance, method = "ward.D") 
plot(Hclust)

cut_avg <- cutree(Hclust, k = 3)

plot(Hclust)
rect.hclust(Hclust , k = 3, border = 2:6)
abline(h = 3, col = 'red')

seeds_df_cl <- mutate(df_clustering, cluster = cut_avg)
count(seeds_df_cl,cluster)

df_clustering$hierc_cluster = cut_avg
hierc_cluster1 = subset(df_clustering, hierc_cluster ==1 )
hierc_cluster2 = subset(df_clustering, hierc_cluster ==2 )
hierc_cluster3 = subset(df_clustering, hierc_cluster ==3 )

summary(round(predictions*100,2))
summary(hierc_cluster1$probabilities)
summary(hierc_cluster2$probabilities)
summary(hierc_cluster3$probabilities)


df_clustering$hierc_cluster = as.factor(df_clustering$hierc_cluster)

df_clustering %>%
  group_by(hierc_cluster,prob_cat) %>%
  ggplot(aes(x=hierc_cluster,fill = prob_cat))+
  geom_bar()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Dominating probabilty groups by cluster") 


df_clustering %>%
  group_by(age_category,hierc_cluster) %>%
  ggplot(aes(x=hierc_cluster,fill = age_category))+
  geom_bar()+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  #xlim(0, 100)+
  ggtitle("Recovery by age") 
