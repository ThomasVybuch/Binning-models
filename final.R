# LIBRARIES
library(dplyr)
library(smbinning)
library(pROC)
library(party)
library(rpart)


# Import dat
homecredit<- read.csv("~/home-credit-default-risk/application_train.csv", stringsAsFactors=TRUE)

# Transformace
homecredit$NUM_ANNUITY=homecredit$AMT_CREDIT / homecredit$AMT_ANNUITY             # CREDIT/ANUITY
homecredit$ANNUITY_RATIO=homecredit$AMT_INCOME_TOTAL / homecredit$AMT_ANNUITY     # INCOME/ANUITY
homecredit <- homecredit[homecredit$CODE_GENDER == "M" | homecredit$CODE_GENDER == "F", ] # uprav pohlavi
#summary(homecredit$CODE_GENDER)

homecredit$DAYS_EMPl_NA <- ifelse(homecredit$DAYS_EMPLOYED == 365243, 1, 0)
homecredit$DAYS_EMPLOYED[homecredit$DAYS_EMPLOYED == 365243] <- 0

homecredit=homecredit[!grepl("Unknown", homecredit$NAME_FAMILY_STATUS),]
homecredit["NAME_FAMILY_STATUS"]<-droplevels(homecredit["NAME_FAMILY_STATUS"], "Unknown")

variables_ALL = c("TARGET","CODE_GENDER",
              "NUM_ANNUITY","ANNUITY_RATIO","NAME_FAMILY_STATUS",
              "DAYS_BIRTH","DAYS_EMPLOYED","NAME_EDUCATION_TYPE","NAME_HOUSING_TYPE",
              "DAYS_LAST_PHONE_CHANGE",
              "NAME_CONTRACT_TYPE",
              "DAYS_EMPl_NA"
)

# Funkce vyberu promennych z datasetu
prepare_df <- function(data, variables) {
  data = select(data, variables) 
  
  # transformations
  data = na.omit(data)               # omit NA rows
  data = data[!duplicated(data), ] # more applications from the same person
  return (data)
}

# Set seed
set.seed(12345)

# Final datasets
data = prepare_df(homecredit, variables_ALL)
train  <- data[100001:(dim(data)[1]), ]
test   <- data[1:100000, ]

# vEKTOR SPOJITYCH DAT
variables = c("NUM_ANNUITY","ANNUITY_RATIO","DAYS_BIRTH","DAYS_EMPLOYED", "DAYS_LAST_PHONE_CHANGE")

# VEKTOR KATEGORICKYCH DAT
variables_CAT = c("CODE_GENDER",
                  "NAME_FAMILY_STATUS",
                  "NAME_EDUCATION_TYPE","NAME_HOUSING_TYPE",
                  "NAME_CONTRACT_TYPE",
                  "DAYS_EMPl_NA")



##############################################################
### GAM
##############################################################













##############################################################
### SMBINNING
##############################################################

sm_binning <- function(data, variable, test) {
  for (i in variables) { 
    
    result=smbinning(data, "TARGET", i)
    data[paste("binned",i,sep = "")] <- as.numeric(as.character(cut(data[,i], breaks = result$bands, 
                                                                    labels = result$ivtable$IV[1:(length(result$ivtable$IV)-2)])))
    print(result)
    par(mfrow = c(1, 2))
    smbinning.plot ( result , option= "dist" ) 
    smbinning.plot ( result , option= "WoE")
    par(mfrow = c(1, 1))
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

train_sm = SM[[1]]
test_sm = SM[[2]]

M_2 = glm (TARGET ~ CODE_GENDER+NAME_CONTRACT_TYPE+DAYS_EMPl_NA+
             binnedNUM_ANNUITY+binnedANNUITY_RATIO+NAME_FAMILY_STATUS+
             binnedDAYS_BIRTH+binnedDAYS_EMPLOYED+NAME_EDUCATION_TYPE+NAME_HOUSING_TYPE+
             binnedDAYS_LAST_PHONE_CHANGE, data = train_sm, family = binomial)

predicted2 <- predict(M_2, test_sm, type="response")
auc(test_sm$TARGET, predicted2)







##############################################################
### TREES
##############################################################

# !!!DO NOT LAUNCH BEFORE GAM MODELS!!!

# Set seed
set.seed(12345)

#############################################
# PARTY 
#############################################

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
  cats_test <-  predict(model,type="response", newdata=test)
  all <- rbind(cats_train, cats_test)
  # transform all
  transformed_values <- as.numeric(factor(all))
  # Insert transformed variables
  train[paste("TREE",i,sep = "")] <- transformed_values[1:nrow(cats_train)]
  test[paste("TREE",i,sep = "")] <- transformed_values[(nrow(cats_train)+1):nrow(all)]
}


# Model with variables tranSformed by PARTY_TREE2
M_3 = glm (TARGET ~ CODE_GENDER+NAME_CONTRACT_TYPE+DAYS_EMPl_NA+
             TREENUM_ANNUITY+TREEANNUITY_RATIO+NAME_FAMILY_STATUS+
             TREEDAYS_BIRTH+TREEDAYS_EMPLOYED+NAME_EDUCATION_TYPE+NAME_HOUSING_TYPE+
             TREEDAYS_LAST_PHONE_CHANGE, data = train, family = binomial)

predicted3 <- predict(M_3, test, type="response")
auc(test$TARGET, predicted3)



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
  cats_test <-  predict(model,type="prob", newdata=test)
  all <- rbind(predicted_categories, cats_test)
  
  # transform all
  transformed_values <- as.numeric(factor(all[,2]))
  
  # Insert transformed variables
  train[paste("STROM",i,sep = "")] <- transformed_values[1:nrow(predicted_categories)]
  test[paste("STROM",i,sep = "")] <- transformed_values[(nrow(predicted_categories)+1):nrow(all)]
}

# Model with variables tranSformed by STROM2
M_4 = glm (TARGET ~ CODE_GENDER+NAME_CONTRACT_TYPE+DAYS_EMPl_NA+
             STROMNUM_ANNUITY+STROMANNUITY_RATIO+NAME_FAMILY_STATUS+
             STROMDAYS_BIRTH+STROMDAYS_EMPLOYED+NAME_EDUCATION_TYPE+NAME_HOUSING_TYPE+
             STROMDAYS_LAST_PHONE_CHANGE, data = train, family = binomial)

predicted4 <- predict(M_4, test, type="response")
auc(test$TARGET, predicted4)


