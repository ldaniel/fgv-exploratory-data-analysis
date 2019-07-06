
# loading required libraries --------------------------------------------------
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(magrittr)
library(feather)

# loading other scripts do be used here ---------------------------------------
source("./scripts/step_00_config_environment.R")
source("./scripts/step_01_create_functions.R")
source("./scripts/step_02_data_ingestion.R")
source("./scripts/step_03_data_cleaning.R")
source("./scripts/step_04_label_translation.R")
source("./scripts/step_05_data_enhancement.R")

# data prep -------------------------------------------------------------------

temp <- left_join(loan, disposition, by = c('account_id', 'type')) %>% 
  left_join(client, by = 'client_id') %>%
  left_join(district, by = 'district_id') %>% 
  left_join(creditcard, by = 'disp_id') %>% 
  left_join(account_balance, by = 'account_id') %>% 
  left_join(account_transaction_pattern, by = 'account_id') %>% 
  mutate(card_age_month = (issued %--% 
                             make_date(1998, 12, 31)) / months(1),
         last_transaction_age_days = ((last_transaction_date.y %--% 
                                         make_date(1998, 12, 31)) / days(1))) %>% 
  dplyr::select(c("amount.x", "duration", "payments", "status", "defaulter", "contract_status",
           "gender", "age", "district_name", "region", 
           "no_of_inhabitants", "no_of_municip_inhabitants_less_499", 
           "no_of_municip_500_to_1999", "no_of_municip_2000_to_9999", 
           "no_of_municip_greater_10000", "no_of_cities", "ratio_of_urban_inhabitants", 
           "average_salary", "unemploymant_rate_1995", "unemploymant_rate_1996", 
           "no_of_enterpreneurs_per_1000_inhabitants", "no_of_commited_crimes_1995", 
           "no_of_commited_crimes_1996", "type.y", "card_age_month","account_balance", 
           "avg_balance","transaction_count", "amount.y", 
           "last_transaction_age_days", 
           "prop_old_age_pension", "prop_insurance_payment", 
           "prop_sanction_interest","prop_household", 
           "prop_statement", "prop_interest_credited", 
           "prop_loan_payment", "prop_other"))

colnames(temp) <- c("x_loan_amount", "x_loan_duration", "x_loan_payments", "x_loan_status", 
                    "y_loan_defaulter", "x_loan_contract_status",
                    "x_client_gender", "x_client_age", "x_district_name", "x_region", 
                    "x_no_of_inhabitants", "x_no_of_municip_inhabitants_less_499", 
                    "x_no_of_municip_500_to_1999", "x_no_of_municip_2000_to_9999", 
                    "x_no_of_municip_greater_10000", "x_no_of_cities", "x_ratio_of_urban_inhabitants", 
                    "x_average_salary", "x_unemploymant_rate_1995", "x_unemploymant_rate_1996", 
                    "x_no_of_enterpreneurs_per_1000_inhabitants", "x_no_of_commited_crimes_1995", 
                    "x_no_of_commited_crimes_1996", "x_card_type", "x_card_age_month","x_account_balance", 
                    "x_avg_account_balance","x_transaction_count", "x_transaction_amount", 
                    "x_last_transaction_age_days","x_transaction_amount_date_filter", 
                    "x_prop_old_age_pension", "x_prop_insurance_payment", 
                    "x_prop_sanction_interest","x_prop_household", 
                    "x_prop_statement", "x_prop_interest_credited", 
                    "x_prop_loan_payment", "x_prop_other")

temp <- dplyr::select(temp, y_loan_defaulter, everything())

temp$x_card_type = ifelse(is.na(temp$x_card_type), 'no card', as.character(temp$x_card_type))
temp$x_card_age_month = ifelse(is.na(temp$x_card_age_month), 0, temp$x_card_age_month)
temp$y_loan_defaulter = as.numeric(temp$y_loan_defaulter)


# taking multicolinear variables from the dataset.
temp <- dplyr::select(temp, -c(x_no_of_inhabitants, x_no_of_cities,
                               x_average_salary, x_unemploymant_rate_1995,
                               x_unemploymant_rate_1996, x_no_of_commited_crimes_1996 ,
                               x_prop_old_age_pension, x_transaction_count, 
                               x_transaction_amount_date_filter, x_prop_insurance_payment, 
                               x_prop_household, x_prop_statement, 
                               x_prop_loan_payment, x_prop_other, 
                               x_district_name, x_region, x_loan_status, 
                               x_loan_contract_status, x_prop_sanction_interest))

loan_reg_dataset <- temp

rm(temp)
gc()

# evaluating multicolinearity of remaining variables.
library(mctest)

vars.quant <- select_if(loan_reg_dataset, is.numeric)

omcdiag(vars.quant, loan_reg_dataset$y_loan_defaulter)
imcdiag(vars.quant, loan_reg_dataset$y_loan_defaulter)
mctest::mc.plot(vars.quant, loan_reg_dataset$y_loan_defaulter)

library(corrplot)
corrplot.mixed(cor(vars.quant))

# sampling ----------------------------------------------------------------------------
library(caret)

set.seed(12345)
index <- createDataPartition(loan_reg_dataset$y_loan_defaulter, p= 0.7,list = FALSE)

data.train <- loan_reg_dataset[index, ] # base de desenvolvimento: 70%
data.test  <- loan_reg_dataset[-index,] # base de teste: 30%

# Checando se as propor??es das amostras s?o pr?ximas ? base original
prop.table(table(loan_reg_dataset$y_loan_defaulter))
prop.table(table(data.train$y_loan_defaulter))
prop.table(table(data.test$y_loan_defaulter))


# fit the logistic model -------------------------------------------------------------

model <- glm(data = data.train, formula = y_loan_defaulter ~ ., 
             family= binomial(link='logit'))

anova(model)
summary(model)


# model evaluation -------------------------------------------------------------------

library(hmeasure) 

glm.prob.train <- predict(model, type = "response")
glm.prob.test <- predict(model, newdata = data.test, type= "response")


glm.train <- HMeasure(data.train$y_loan_defaulter, glm.prob.train)
glm.test  <- HMeasure(data.test$y_loan_defaulter, glm.prob.test)

glm.train$metrics
glm.test$metrics

library(rms)
val.prob(glm.prob.train, data.train$y_loan_defaulter, smooth = F)
hist(glm.prob.test, breaks = 25, col = "lightblue",xlab= "Probabilidades",
     ylab= "Frequ?ncia",main= "Regress?o Log?stica")

boxplot(glm.prob.test ~ data.test$y_loan_defaulter, col= c("red", "green"), 
        horizontal= T)

library(pROC)
roc1 <- roc(data.test$y_loan_defaulter, glm.prob.test)
y1 <- roc1$sensitivities
x1 <- 1-roc1$specificities

plot(x1, y1,  type="n",
     xlab = "1 - Especificidade", 
     ylab= "Sensitividade")
lines(x1, y1,lwd=3,lty=1, col="purple") 

# accuracy metrics -----------------------------------

fitted.results <- ifelse(glm.prob.test > 0.5,1,0)

misClasificError <- mean(fitted.results != data.test$y_loan_defaulter)
print(paste('Accuracy', 1 - misClasificError))
