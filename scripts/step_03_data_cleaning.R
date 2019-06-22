# analysing missing values and other strange conditions -----------------------
temp <- filter(transaction, is.na(account))
temp <- filter(transaction, operation == "", bank == "", !is.na(type), is.na(account))
temp <- filter(transaction, operation == "")
temp <- filter(transaction, bank == "")
temp <- filter(transaction, k_symbol == "")
temp <- filter(transaction, is.na(account))

View(temp)
summary(transaction)
str(transaction)

matrixplot(transaction)
