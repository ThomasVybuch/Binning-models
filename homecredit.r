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
#Weight of Evidence je v podstatě logaritmus poměru pravděpodobností dvou událostí: pravděpodobnosti, že klient splní podmínku (například koupí produkt), oproti pravděpodobnosti, že klient tuto podmínku nesplní. Výsledek je vážen podle významu jednotlivých kategorií. Vyšší hodnota WOE naznačuje silnější vliv proměnné na danou událost.
#Hodnota GINI se pohybuje od 0 do 1, kde 0 znamená náhodné klasifikování a 1 znamená dokonalé klasifikování. GINI měří měří "nekorektnost" predikcí modelu, a tedy, čím vyšší hodnota GINI, tím lepší je model.
# Čím vyšší je hodnota AUC (blíže k 1), tím lepší je výkonnost modelu.

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

##############pokus o automatizaci################
homecredit<- read.csv("~/home-credit-default-risk/application_train.csv", stringsAsFactors=TRUE)
library(dplyr)
library(smbinning)
library(pROC)
prepare_df <- function(data) {
    data = data[!grepl("Revolving loans", homecredit$NAME_CONTRACT_TYPE),]   #only cash loans
    data = select(data, c(TARGET,CODE_GENDER,FLAG_OWN_REALTY,
                                  AMT_INCOME_TOTAL,AMT_CREDIT,AMT_ANNUITY,NAME_FAMILY_STATUS,
                                  DAYS_BIRTH,DAYS_EMPLOYED,NAME_EDUCATION_TYPE,NAME_HOUSING_TYPE)) 
    # filter only loans
    data = na.omit(data)               # omit NA rows
    data=data[!grepl(365243, data$DAYS_EMPLOYED),]
    data$NUM_ANNUITY=data$AMT_CREDIT/data$AMT_ANNUITY             # CREDIT/ANUITY
    data$ANNUITY_RATIO=data$AMT_INCOME_TOTAL/data$AMT_ANNUITY     # INCOME/ANUITY
    data = data[!duplicated(data), ]
  return (data)
}

data = prepare_df(homecredit)
train  <- data[100001:(dim(data)[1]), ]
test   <- data[1:100000, ]

train <- prepare_df(train)
test <- prepare_df(test)

variables = c("NUM_ANNUITY","ANNUITY_RATIO","DAYS_BIRTH","DAYS_EMPLOYED")

#result=smbinning(data, "TARGET", "ANNUITY_RATIO")
sm_binning <- function(data, variable, test) {
    for (i in variables) { 
        
      result=smbinning(data, "TARGET", i)
      data[paste("binned",i,sep = "")] <- as.numeric(as.character(cut(data[,i], breaks = result$bands, 
                                                              labels = result$ivtable$IV[1:(length(result$ivtable$IV)-2)])))
      print(result)
      smbinning.plot ( result , option= "dist" ) 
      smbinning.plot ( result , option= "WoE")
      
      breaks = c(-Inf,result$bands[2:(length(result$bands)-1)],Inf)                       #breaks=result$bands
      lables = result$ivtable$IV[1:(length(result$ivtable$IV)-2)]
      print(breaks)                         
      #binned_test(test,breaks,lables,i)
      test[paste("binned",i,sep = "")] = as.numeric(as.character(cut(test[,i], breaks = breaks, 
                                                                      labels = lables)))
    }
    data=list(data,test)
  return (data)
}


SM = sm_binning(train,variables,test)

train = SM[[1]]
test = SM[[2]]

M_1 = glm (TARGET ~ CODE_GENDER+FLAG_OWN_REALTY+
             AMT_INCOME_TOTAL+AMT_CREDIT+AMT_ANNUITY+NAME_FAMILY_STATUS+
             DAYS_BIRTH+DAYS_EMPLOYED+NAME_EDUCATION_TYPE+NAME_HOUSING_TYPE, data = train, family = binomial)

M_2 = glm (TARGET ~ CODE_GENDER+FLAG_OWN_REALTY+
             binnedNUM_ANNUITY+binnedANNUITY_RATIO+NAME_FAMILY_STATUS+
             binnedDAYS_BIRTH+binnedDAYS_EMPLOYED+NAME_EDUCATION_TYPE+NAME_HOUSING_TYPE, data = train, family = binomial)


predicted <- predict(M_1, test, type="response")
auc(test$TARGET, predicted)
predicted <- predict(M_2, test, type="response")
auc(test$TARGET, predicted)






# 
#################################################################
# TREES 
#################################################################
# Libraries
library(party)
library(rpart.plot)

# zohledneni train dat
PARTY_TREE2 <- function(Y, X, table) {
  model <- ctree(formula = as.formula(paste(Y, "~", X)),
                 data = table,
                 controls = ctree_control(maxdepth = 3) )
  return(model)
}

for (i in variables) {
  model = PARTY_TREE2(Y = "TARGET", X = i, table = train)
  # insert transformed variables in train
  cats_train <-  predict(model,type="response")
  transformed_values <- as.numeric(factor(cats_train))
  train[paste("TREE",i,sep = "")] <- transformed_values
  
  # Insert transformed variables in test
  cats_test <-  predict(model,type="response", newdata=test)
  transformed_values <- as.numeric(factor(cats_test))
  test[paste("TREE",i,sep = "")] <- transformed_values
}


# Model with variables tranSformed by PARTY_TREE2
M_3 = glm (TARGET ~ CODE_GENDER+FLAG_OWN_REALTY+
             TREENUM_ANNUITY+TREEANNUITY_RATIO+NAME_FAMILY_STATUS+
             TREEDAYS_BIRTH+TREEDAYS_EMPLOYED+NAME_EDUCATION_TYPE+NAME_HOUSING_TYPE, data = train, family = binomial)

predicted <- predict(M_3, test, type="response")
auc(test$TARGET, predicted)



#############################################
# SECOND TREES
#############################################
STROM2 <- function(Y, X, cp, method, table) {
  # Declare control parameters
  controls <- rpart.control(maxcompete = 2, cp = cp, maxdepth = 3)
  # Fit a decision tree using rpart with the specified control parametres
  model <- rpart(formula = as.formula(paste(Y, "~", X)),
                 data = table,
                 control = controls,
                 method = method )
  return(model)
}

for (i in variables) {
  # Create model using rpart
  model = STROM2(Y = "TARGET", X = i, cp= -0.01, method = "class", table = train)
  
  # insert transformed variables in train
  predicted_categories <-  predict(model,type="prob")
  transformed_values <- as.numeric(as.numeric(factor(predicted_categories[,2])))
  train[paste("STROM",i,sep = "")] <- transformed_values

  # Insert transformed variables in test
  cats_test <-  predict(model,type="prob", newdata=test)
  transformed_values <- as.numeric(as.numeric(factor(cats_test[,2])))
  test[paste("STROM",i,sep = "")] <- transformed_values
}

# Model with variables tranSformed by STROM2
M_4 = glm (TARGET ~ CODE_GENDER+FLAG_OWN_REALTY+
             STROMNUM_ANNUITY+STROMANNUITY_RATIO+NAME_FAMILY_STATUS+
             STROMDAYS_BIRTH+STROMDAYS_EMPLOYED+NAME_EDUCATION_TYPE+NAME_HOUSING_TYPE, data = train, family = binomial)

predicted <- predict(M_4, test, type="response")
auc(test$TARGET, predicted)
