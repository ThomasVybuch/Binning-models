homecredit<- read.csv("~/home-credit-default-risk/application_train.csv", stringsAsFactors=TRUE)

library(dplyr)
library(pROC)
library(ggplot2)

summary(homecredit)

homecredit %>%
  select(TARGET, CODE_GENDER,FLAG_OWN_REALTY,
         AMT_INCOME_TOTAL,AMT_CREDIT,AMT_ANNUITY,NAME_FAMILY_STATUS,
         DAYS_BIRTH,DAYS_EMPLOYED,NAME_EDUCATION_TYPE,NAME_HOUSING_TYPE,
         DAYS_LAST_PHONE_CHANGE, DAYS_ID_PUBLISH, HOUR_APPR_PROCESS_START ) %>%
  summary()

df  <- select(homecredit, c(TARGET,CODE_GENDER,FLAG_OWN_REALTY,
                            AMT_INCOME_TOTAL,AMT_CREDIT,AMT_ANNUITY,NAME_FAMILY_STATUS,
                            DAYS_BIRTH,DAYS_EMPLOYED,NAME_EDUCATION_TYPE,NAME_HOUSING_TYPE
                            , DAYS_LAST_PHONE_CHANGE, DAYS_ID_PUBLISH, HOUR_APPR_PROCESS_START
))
summary(df)
df = na.omit(df)               # omit NA rows
df$NUM_ANNUITY=df$AMT_CREDIT/df$AMT_ANNUITY             # CREDIT/ANUITY
df$ANNUITY_RATIO=df$AMT_INCOME_TOTAL/df$AMT_ANNUITY 
summary(df)

#df = df[df$AMT_INCOME_TOTAL < 1000000,]
ggplot(data=df, aes(x=df$AMT_INCOME_TOTAL, group=df$TARGET, fill=factor(df$TARGET))) +
  geom_density(alpha=.5) +
  labs(x = "AMT_INCOME_TOTAL",
       y = "Hustota",
       fill = "TARGET") + 
  scale_fill_manual(values = c("0" = "darkblue", "1" = "gold")) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_x_continuous(limits = c(0, 500000))


# PODEZØELÉ SPOJITÉ
ggplot(data=df, aes(x=df$DAYS_LAST_PHONE_CHANGE, group=df$TARGET, fill=factor(df$TARGET))) +
  geom_density(alpha=.5) +
  labs(x = "DAYS_LAST_PHONE_CHANGE",
       y = "Hustota",
       fill = "TARGET") + 
  scale_fill_manual(values = c("0" = "darkblue", "1" = "gold")) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))


ggplot(data=df, aes(x=df$DAYS_ID_PUBLISH, group=df$TARGET, fill=factor(df$TARGET))) +
  geom_density(alpha=.5) +
  labs(x = "DAYS_ID_PUBLISH",
       y = "Hustota",
       fill = "TARGET") + 
  scale_fill_manual(values = c("0" = "darkblue", "1" = "gold")) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))






# TRANSFORMED VARIABLES
ggplot(data=df, aes(x=df$NUM_ANNUITY, group=df$TARGET, fill=factor(df$TARGET))) +
  geom_density(alpha=.5) +
  labs(x = "NUM_ANNUITY",
       y = "Hustota",
       fill = "TARGET") + 
  scale_fill_manual(values = c("0" = "darkblue", "1" = "gold")) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))


ggplot(data=df, aes(x=df$ANNUITY_RATIO, group=df$TARGET, fill=factor(df$TARGET))) +
  geom_density(alpha=.5) +
  labs(x = "ANNUITY_RATIO",
       y = "Hustota",
       fill = "TARGET") + 
  scale_fill_manual(values = c("0" = "darkblue", "1" = "gold")) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))+
  scale_x_continuous(limits = c(0, 40))




# ZBYLÉ SPOJITÉ
# DAYS_BIRTH,DAYS_EMPLOYED

ggplot(data=df, aes(x=df$DAYS_BIRTH, group=df$TARGET, fill=factor(df$TARGET))) +
  geom_density(alpha=.5) +
  labs(x = "DAYS_BIRTH",
       y = "Hustota",
       fill = "TARGET") + 
  scale_fill_manual(values = c("0" = "darkblue", "1" = "gold")) + 
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))


#df = df[df$DAYS_EMPLOYED < 365243,]
ggplot(data=df, aes(x=df$DAYS_EMPLOYED, group=df$TARGET, fill=factor(df$TARGET))) +
  geom_density(alpha=.5) +
  labs(x = "DAYS_EMPLOYED",
       y = "Hustota",
       fill = "TARGET") + 
  scale_fill_manual(values = c("0" = "darkblue", "1" = "gold")) 
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))
  

  
# DISKRETNI PROMENNE
# CODE_GENDER,FLAG_OWN_REALTY, NAME_FAMILY_STATUS, NAME_EDUCATION_TYPE,NAME_HOUSING_TYPE
  ggplot(data=df, aes(x=df$CODE_GENDER, group=df$TARGET, fill=factor(df$TARGET))) +
    geom_density(alpha=.5) +
    labs(x = "CODE_GENDER", y = "Hustota", fill = "TARGET") + 
    scale_fill_manual(values = c("0" = "darkblue", "1" = "gold")) + scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))

  ggplot(data=df, aes(x=df$FLAG_OWN_REALTY, group=df$TARGET, fill=factor(df$TARGET))) + 
    geom_density(alpha=.5) +
    labs(x = "FLAG_OWN_REALTY", y = "Hustota", fill = "TARGET") + 
    scale_fill_manual(values = c("0" = "darkblue", "1" = "gold")) + scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))
  
  ggplot(data=df, aes(x=df$NAME_FAMILY_STATUS, group=df$TARGET, fill=factor(df$TARGET))) +
    geom_density(alpha=.5) +
    labs(x = "NAME_FAMILY_STATUS", y = "Hustota", fill = "TARGET") + 
    scale_fill_manual(values = c("0" = "darkblue", "1" = "gold")) + scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))
  
  ggplot(df, aes(x = factor(NAME_FAMILY_STATUS), fill = factor(df$TARGET))) +
    geom_bar() +
    labs(title = "Rozlozeni hodnot promenne TARGET",
         x = "NAME_FAMILY_STATUS",
         y = "Pocet pozorovani",
         fill = "TARGET") +  # Legenda
    scale_fill_manual(values = c("0" = "darkblue", "1" = "gold"))+
    scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))
                       
  ggplot(data=df, aes(x=df$NAME_HOUSING_TYPE, group=df$TARGET, fill=factor(df$TARGET))) +
    geom_density(alpha=.5) +
    labs(x = "NAME_HOUSING_TYPE", y = "Hustota", fill = "TARGET") + 
    scale_fill_manual(values = c("0" = "darkblue", "1" = "gold")) + scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))
  
  ggplot(data=df, aes(x=df$NAME_EDUCATION_TYPE, group=df$TARGET, fill=factor(df$TARGET))) +
    geom_density(alpha=.5) +
    labs(x = "NAME_EDUCATION_TYPE", y = "Hustota", fill = "TARGET") + 
    scale_fill_manual(values = c("0" = "darkblue", "1" = "gold")) + scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE))
  
  
  
  
  
  
  
##########################

ggplot(data=homecredit, aes(x=homecredit$CODE_GENDER, group=homecredit$TARGET, fill=factor(homecredit$TARGET))) +
  geom_density(alpha=.5) +
  labs(x = "DAYS_BIRTH",
       y = "Hustota",
       fill = "TARGET") + 
  scale_fill_manual(values = c("0" = "darkblue", "1" = "gold")) + 
  scale_y_continuous(labels = scales::percent)













#############################################
# Resulting function for main code

homecredit$NUM_ANNUITY=homecredit$AMT_CREDIT / homecredit$AMT_ANNUITY             # CREDIT/ANUITY
homecredit$ANNUITY_RATIO=homecredit$AMT_INCOME_TOTAL / homecredit$AMT_ANNUITY     # INCOME/ANUITY
homecredit <- homecredit[homecredit$CODE_GENDER == "M" | homecredit$CODE_GENDER == "F", ] # uprav pohlavi
summary(homecredit$CODE_GENDER)

# asi neimputovat
#median_employment <- median(homecredit$DAYS_EMPLOYED)
#homecredit$DAYS_EMPLOYED[homecredit$DAYS_EMPLOYED == 365243] <- median_employment
homecredit$DAYS_EMPl_NA <- ifelse(homecredit$DAYS_EMPLOYED == 365243, 1, 0)
homecredit$DAYS_EMPLOYED[homecredit$DAYS_EMPLOYED == 365243] <- 0
  
variables = c("TARGET","CODE_GENDER",
              "NUM_ANNUITY","ANNUITY_RATIO","NAME_FAMILY_STATUS",
              "DAYS_BIRTH","DAYS_EMPLOYED","NAME_EDUCATION_TYPE","NAME_HOUSING_TYPE",
              "DAYS_LAST_PHONE_CHANGE",
              "NAME_CONTRACT_TYPE",
              "DAYS_EMPl_NA"
              )

prepare_df <- function(data, variables) {
  data = select(data, variables) 
  
  # transformations
  data = na.omit(data)               # omit NA rows
  data = data[!duplicated(data), ] # more applications from the same person
  return (data)
}

data = prepare_df(homecredit, variables)

set.seed(12345)
chosen_rows <- sample(1:nrow(data), 100000)
test <- data[chosen_rows, ]
train <- data[-chosen_rows, ]

