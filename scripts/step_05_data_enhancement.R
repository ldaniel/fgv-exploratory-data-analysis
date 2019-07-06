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

if (!file.exists('data/account_transaction_pattern.feather')) {
  account_transaction_pattern <- select(transaction, c(trans_id, account_id, date, amount, k_symbol)) %>% 
    mutate(k_symbol = ifelse(k_symbol == '' | is.na(k_symbol), 'other', k_symbol)) %>% 
    spread(key = k_symbol, value = amount) %>%
    replace(is.na(.), 0) %>% 
    mutate(amount = rowSums(.[4:11])) %>%
    group_by(account_id) %>%
    summarise(transaction_count = n(),
              last_transaction_date = max(date),
              amount = sum(amount),
              prop_household = sum(household) / amount,
              prop_insurance_payment = sum(`insurance payment`) / amount,
              prop_interest_credited = sum(`interest credited`) / amount,
              prop_loan_payment = sum(`loan payment`) / amount,
              prop_old_age_pension = sum(`old age pension`) / amount,
              prop_other = sum(`other`) / amount,
              prop_sanction_interest = sum(`sanction interest`) / amount,
              prop_statement = sum(`statement`) / amount)
  write_feather(account_transaction_pattern, 'data/account_transaction_pattern.feather')
} else {
  account_transaction_pattern <- read_feather('data/account_transaction_pattern.feather')
}
