homecredit <- read.csv("C:/Users/Martin Kubát/Downloads/homecredit/application_train.csv", stringsAsFactors=TRUE)
#homecredit<- read.csv("~/home-credit-default-risk/application_train.csv", stringsAsFactors=TRUE)
#library(dplyr)
homecredit = homecredit[!grepl("Revolving loans", homecredit$NAME_CONTRACT_TYPE),]   #only cash loans
data = select(homecredit, c(TARGET,CODE_GENDER,FLAG_OWN_REALTY,
                            AMT_INCOME_TOTAL,AMT_CREDIT,AMT_ANNUITY,NAME_FAMILY_STATUS,
                            DAYS_BIRTH,DAYS_EMPLOYED,NAME_EDUCATION_TYPE,NAME_HOUSING_TYPE)) 
# filter only loans
data = na.omit(data)               # omit NA rows
head(data)
table(data$TARGET)

data["YEARS"]=-data["DAYS_BIRTH"]/365
data["YEARS_EMPLOYED"]=-data["DAYS_EMPLOYED"]/365


#data[which.max(data$DAYS_EMPLOYED),]                         
data=data[!grepl(365243, data$DAYS_EMPLOYED),]               # removed about 50K rows, mistake in days employed
summary(data)
boxplot(data$YEARS_EMPLOYED)
hist(data$YEARS_EMPLOYED)


data$CAT_YEARS <- cut(data$YEARS,
                       breaks=c(min(data$YEARS),28,40,52,max(data$YEARS)),
                       labels=c('Young Adults', 'Adults', 'Old Adults', 'Boomers'))
plot(factor(data$TARGET) ~ factor(data$CAT_YEARS), data =data,  col = c("forestgreen","lightsalmon"))
plot(factor(data$TARGET) ~ factor(data$CODE_GENDER), data =data,  col = c("forestgreen","lightsalmon"))

data$CAT_YEARS_1 <- cut(data$YEARS, breaks=seq(min(data$YEARS),max(data$YEARS),by=2))
males=data[data$CODE_GENDER=="M",]
#males=data[data$CODE_GENDER=="M" & data$FLAG_OWN_REALTY=="N",]
plot(factor(TARGET) ~ factor(CAT_YEARS_1), data = males,  col = c("forestgreen","lightsalmon"))
plot(factor(TARGET) ~ factor(CAT_YEARS_1), data = data,  col = c("forestgreen","lightsalmon"))           #stejný mezi muži i ženami
  
     
plot(factor(TARGET) ~ AMT_CREDIT,data=males)   
plot(factor(TARGET) ~ NAME_EDUCATION_TYPE, data = data)
plot(factor(TARGET) ~ NAME_FAMILY_STATUS, data = data)

data$Pocet_splatek=data$AMT_CREDIT/data$AMT_ANNUITY                                                      #počet ročních splátek 
data$Pocet_splatek <- cut(data$Pocet_splatek, breaks=seq(min(data$Pocet_splatek),max(data$Pocet_splatek),by=2))
plot(factor(TARGET)~Pocet_splatek, data = data )                                                         #nelineární vůči targetu


#Tabulka B.1
 #In addition, since, for example, the HCDR data set comprises many variables, some of which have a significantly low fill rate, the omission of missing values while retaining all the features would result in a low number of total observations. Consequently, for all data sets, only variables with at least 80% of non-missing observations are preserved for the analysis.
#only categorical variables with at most two categories will be considered for the analysis.
# The point biserial correlation is numerically equivalent to the Pearson correlation. For two highly correlated variables, the one with the higher correlation with the dependent variable will be retained. 

###### Prvni pokusy SMBINNING
#install.packages("smbinning")
library(smbinning)

?smbinning
(result=smbinning(data, "TARGET", "AMT_INCOME_TOTAL"))

par ( mfrow= c ( 2 , 2 ) )  
boxplot ( data$ AMT_INCOME_TOTAL ~ data $ TARGET ,  
          horizontal=T ,  frame =F ,  col = "lightgrey" , main= "Distribution " )  
mtext ( "A" , 3 )  
smbinning.plot ( result , option= "dist" )  
smbinning.plot ( result , option= "badrate")  
smbinning.plot ( result , option= "WoE")

##WoE
#install.packages("woe")
library(woe)


#Logistická regrese 
M_1 = glm (TARGET ~ CODE_GENDER+FLAG_OWN_REALTY+
           AMT_INCOME_TOTAL+AMT_CREDIT+AMT_ANNUITY+NAME_FAMILY_STATUS+
           DAYS_BIRTH+DAYS_EMPLOYED+NAME_EDUCATION_TYPE+NAME_HOUSING_TYPE, data = data, family = binomial)
summary(M_1)
drop1(M_1, test="Chi")

##### creates binned variables from numeric #####
varibles = c("AMT_INCOME_TOTAL","AMT_CREDIT","AMT_ANNUITY")
for (i in varibles) { 

  result=smbinning(data, "TARGET", i)
  data[paste("binned", i)] <- as.numeric(as.character(cut(data[,i], breaks = result$bands, labels = result$ivtable$IV[1:(length(result$ivtable$IV)-2)])))

}
