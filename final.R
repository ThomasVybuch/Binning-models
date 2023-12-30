# Import dat
homecredit<- read.csv("~/home-credit-default-risk/application_train.csv", stringsAsFactors=TRUE)

# Transformace
homecredit$NUM_ANNUITY=homecredit$AMT_CREDIT / homecredit$AMT_ANNUITY             # CREDIT/ANUITY
homecredit$ANNUITY_RATIO=homecredit$AMT_INCOME_TOTAL / homecredit$AMT_ANNUITY     # INCOME/ANUITY
homecredit <- homecredit[homecredit$CODE_GENDER == "M" | homecredit$CODE_GENDER == "F", ] # uprav pohlavi
#summary(homecredit$CODE_GENDER)

homecredit$DAYS_EMPl_NA <- ifelse(homecredit$DAYS_EMPLOYED == 365243, 1, 0)
homecredit$DAYS_EMPLOYED[homecredit$DAYS_EMPLOYED == 365243] <- 0

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









##############################################################
### TREES
##############################################################


