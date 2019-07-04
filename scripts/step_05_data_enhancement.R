# this step aims to improve the analysis by adding auxiliary information ------

# get gender and birthday from birth_number column in client table
client <- client %>% 
  mutate(gender = GetGenderFromBirthnumber(birth_number)) %>% 
  mutate(birth_date = GetBirthdateFromBirthnumber(birth_number, gender)) %>% 
  mutate(age = GetAgeFromBirthnumber(birth_number))

# improving loan data by having a classification regarding its payment status
loan <- mutate(loan, defaulter = as.logical( plyr::mapvalues(status, c ('A','B','C','D'), c(FALSE,TRUE,FALSE,TRUE))),
               contract_status = plyr::mapvalues(status, c ('A','B','C','D'), c('finished','finished','running','running')),
               type = 'Owner')

# improving client data by having its age group
client <- mutate(client, age_bin = paste(findInterval(age, c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) * 10,'+'))

# create account balance table
account_balance <- arrange(transaction, desc(date), account_id) %>%
  group_by(account_id) %>%
  mutate(avg_balance = mean(balance)) %>%
  filter(row_number() == 1) %>%
  dplyr::select(account_id, date, balance, avg_balance)

colnames(account_balance) <- c("account_id", "last_transaction_date", 'account_balance', 'avg_balance')

# calculate some derived variables from transaction table to feed our logist regression model.

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