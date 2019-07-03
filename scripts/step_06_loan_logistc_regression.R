
# loading required libraries --------------------------------------------------
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)

# loading other scripts do be used here ---------------------------------------
source("./scripts/step_00_config_environment.R")
source("./scripts/step_01_create_functions.R")
source("./scripts/step_02_data_ingestion.R")
source("./scripts/step_03_data_cleaning.R")
source("./scripts/step_04_label_translation.R")
source("./scripts/step_05_data_enhancement.R")

# data prep -------------------------------------------------------------------

transaction.date.ini = make_date(1993, 01, 01)
transaction.date.end = make_date(1998, 12, 31)

account_transaction_pattern <- group_by(transaction, account_id) %>% 
  summarise(transaction_count = n(),
            transaction_amount = sum(amount),
            last_transaction_date = max(date),
            transaction_amount_date_filter = crossprod(amount,  
                                                       date >= transaction.date.ini &
                                                         date <= transaction.date.end),
            
            percent_amount_old_age_pension = crossprod(amount, 
                                                       date >= transaction.date.ini &
                                                         date <= transaction.date.end &
                                                         k_symbol == 'old age pension') / 
              transaction_amount_date_filter,
            percent_amount_insurance_payment = crossprod(amount, 
                                                         date >= transaction.date.ini &
                                                           date <= transaction.date.end &
                                                           k_symbol == 'insurrance payment') / 
              transaction_amount_date_filter,
            percent_amount_sanction_interest = crossprod(amount, 
                                                         date >= transaction.date.ini &
                                                           date <= transaction.date.end &
                                                           k_symbol == 'sanction interest') /
              transaction_amount_date_filter,
            percent_amount_household = crossprod(amount, 
                                                 date >= transaction.date.ini &
                                                   date <= transaction.date.end &
                                                   k_symbol == 'household') / 
              transaction_amount_date_filter,
            percent_amount_statement = crossprod(amount, 
                                                 date >= transaction.date.ini &
                                                   date <= transaction.date.end &
                                                   k_symbol == 'statement') / 
              transaction_amount_date_filter,
            percent_amount_interest_credited = crossprod(amount, 
                                                         date >= transaction.date.ini &
                                                           date <= transaction.date.end &
                                                           k_symbol == 'interest credited') / 
              transaction_amount_date_filter,
            percent_amount_loan_payment = crossprod(amount, 
                                                    date >= transaction.date.ini &
                                                      date <= transaction.date.end &
                                                      k_symbol == 'loan payment') / 
              transaction_amount_date_filter,
            percent_amount_other = crossprod(amount, 
                                             date >= transaction.date.ini &
                                               date <= transaction.date.end &
                                               k_symbol == '') /
              transaction_amount_date_filter
  )

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
  select(c("amount", "duration", "payments", "status", "defaulter", "contract_status",
           "gender", "age", "district_name", "region", 
           "no_of_inhabitants", "no_of_municip_inhabitants_less_499", 
           "no_of_municip_500_to_1999", "no_of_municip_2000_to_9999", 
           "no_of_municip_greater_10000", "no_of_cities", "ratio_of_urban_inhabitants", 
           "average_salary", "unemploymant_rate_1995", "unemploymant_rate_1996", 
           "no_of_enterpreneurs_per_1000_inhabitants", "no_of_commited_crimes_1995", 
           "no_of_commited_crimes_1996", "type.y", "card_age_month","account_balance", 
           "avg_balance","transaction_count", "transaction_amount", 
           "last_transaction_age_days","transaction_amount_date_filter", 
           "percent_amount_old_age_pension", "percent_amount_insurance_payment", 
           "percent_amount_sanction_interest","percent_amount_household", 
           "percent_amount_statement", "percent_amount_interest_credited", 
           "percent_amount_loan_payment", "percent_amount_other"))

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
                    "x_percent_amount_old_age_pension", "x_percent_amount_insurance_payment", 
                    "x_percent_amount_sanction_interest","x_percent_amount_household", 
                    "x_percent_amount_statement", "x_percent_amount_interest_credited", 
                    "x_percent_amount_loan_payment", "x_percent_amount_other")

temp <- select(temp, y_loan_defaulter, everything())

temp$x_card_type = ifelse(is.na(temp$x_card_type), 'no card', as.character(temp$x_card_type))
temp$x_card_age_month = ifelse(is.na(temp$x_card_age_month), 0, temp$x_card_age_month)
temp$y_loan_defaulter = as.numeric(temp$y_loan_defaulter)

temp <- select(temp, -c(x_percent_amount_old_age_pension, x_loan_contract_status))


loan_reg_dataset <- temp

loan_reg_dataset <- select(loan_reg_dataset, -c(x_no_of_inhabitants, x_no_of_cities, 
                                                x_average_salary, x_unemploymant_rate_1995, 
                                                x_unemploymant_rate_1996, x_no_of_commited_crimes_1996 ,
                                                x_transaction_count, x_transaction_amount_date_filter, 
                                                x_percent_amount_insurance_payment, x_percent_amount_household, 
                                                x_percent_amount_statement, x_percent_amount_loan_payment, 
                                                x_percent_amount_other))

rm(temp, transaction.date.ini, transaction.date.end)
gc()

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


# Avaliando multicolinearidade - vars quantitativas
library(mctest)

vars.quant <- select_if(loan_reg_dataset, is.numeric)

omcdiag(vars.quant, loan_reg_dataset$y_loan_defaulter)
imcdiag(vars.quant, loan_reg_dataset$y_loan_defaulter)
mctest::mc.plot(vars.quant, loan_reg_dataset$y_loan_defaulter)

loan_reg_dataset <- select(loan_reg_dataset, -c(x_no_of_inhabitants, x_no_of_cities, 
                                                x_average_salary, x_unemploymant_rate_1995, 
                                                x_unemploymant_rate_1996, x_no_of_commited_crimes_1996 ,
                                                x_transaction_count, x_transaction_amount_date_filter, 
                                                x_percent_amount_insurance_payment, x_percent_amount_household, 
                                                x_percent_amount_statement, x_percent_amount_loan_payment, 
                                                x_percent_amount_other))

# calculando matriz de correlacao
library(ppcor)
library(corrplot)

cor_matrix <- pcor(vars.quant, method = "pearson")
corrplot.mixed(cor_matrix$estimate)

cor_matrix$estimate

glm_temp <- select(temp, -c(x_percent_amount_old_age_pension, x_loan_status,
                            x_district_name, x_region))

model <- glm(data = glm_temp, formula = y_loan_defaulter ~ ., 
             family= binomial(link='logit'))

summary(model)



# AMOSTRAGEM DO DADOS
library(caret)

set.seed(12345)
index <- createDataPartition(base$y_subscribe, p= 0.7,list = F)

data.train <- base[index, ] # base de desenvolvimento: 70%
data.test  <- base[-index,] # base de teste: 30%

# Checando se as propor??es das amostras s?o pr?ximas ? base original
prop.table(table(base$y_subscribe))
prop.table(table(data.train$y_subscribe))
prop.table(table(data.test$y_subscribe))