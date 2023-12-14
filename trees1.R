homecredit<- read.csv("application_train.csv", stringsAsFactors=TRUE, sep = ',')
library(dplyr)
#homecredit = homecredit[!grepl("Revolving loans", homecredit$NAME_CONTRACT_TYPE),]   #only cash loans
data = select(homecredit, c(TARGET,CODE_GENDER,FLAG_OWN_REALTY,
                            AMT_INCOME_TOTAL,AMT_CREDIT,AMT_ANNUITY,NAME_FAMILY_STATUS,
                            DAYS_BIRTH,DAYS_EMPLOYED,NAME_EDUCATION_TYPE,NAME_HOUSING_TYPE)) 
# filter only loans
data = na.omit(data)               # omit NA rows
head(data)
table(data$TARGET)
attach(data)





########################################
# ctree
########################################
library(party)

PARTY_TREE <- function(Y, X, table) {
  # Fit a decision tree using CTREE with the specified control parameters
  model <- ctree(formula = as.formula(paste(Y, "~", X)),
                 data = table,
                 controls = ctree_control(maxdepth = 3)
  )
  cats <- predict(model,type="response")
  return(as.numeric(factor(cats)))
}



# Names 
variables = c("DAYS_BIRTH","DAYS_EMPLOYED")

# Iterate through Names in data table
for (i in variables) {
  # Ue PARTY_TREE 
  transformed_values = PARTY_TREE(Y = "TARGET", X = i, table = data)
  
  # Add new colums
  data[paste("TREE",i,sep = "")] <- transformed_values
}


# Resulting variables readz for models x compatibility issues with main Rscript






#########################################
### rpart
#########################################
library(rpart.plot)

# Another variable
controls <- rpart.control(maxcompete = 2, cp = -0.01, maxdepth = 3)
# Fit a decision tree using rpart with the specified control parameters
model4 <- rpart(TARGET ~ AMT_CREDIT, data = data, control = controls, method="class", parms = list(split = 'gini'))
# Display the resulting tree
print(model4)
rpart.plot(model4, box.palette = "auto", shadow.col = "gray")
str(model4$frame)
rpart.rules(model4)
prp(model4, extra = 1)

predicted_categories <- predict(model4, newdata = data, type = "prob")
as.numeric(factor(predicted_categories[,2]))
unique(predicted_categories[,2])




# trees in a function
STROM <- function(Y, X, cp, method, table) {
  # control parameters
  controls <- rpart.control(maxcompete = 2, cp = cp, maxdepth = 3)
  
  # Fit a decision tree using rpart with the specified control parametres
  model <- rpart(formula = as.formula(paste(Y, "~", X)),
                 data = table,
                 control = controls,
                 method = method
                )
  predicted_categories <- predict(model, newdata = table, type = "prob")
  return(as.numeric(factor(predicted_categories[,2])))
}





# Names 
variables = c("DAYS_BIRTH","DAYS_EMPLOYED")
cp_value <- -0.01 #usualy positive X NO POSITIVE WORKS???
method_value <- "class"


# Iterate through Names in data table
for (i in variables) {
  # Ue PARTY_TREE 
  transformed_values = STROM(Y = "TARGET", X = i, cp = cp_value, method = method_value, table = data)
  
  # Add new colums
  data[paste("STROM",i,sep = "")] <- transformed_values
  #TREE_DATA[[i]] <- transformed_values
}

head(data)



