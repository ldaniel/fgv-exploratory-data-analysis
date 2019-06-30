# this step aims to improve the analysis by adding auxiliary information ------

# improving loan data by having a classification regarding its payment status
loan <- mutate(loan, defaulter = as.logical( plyr::mapvalues(status, c ('A','B','C','D'), c(FALSE,TRUE,FALSE,TRUE))),
               contract_status = plyr::mapvalues(status, c ('A','B','C','D'), c('finished','finished','running','running')),
               type = 'Owner')

# improving client data by havivng its age group
client <- mutate(client, age_bin = paste(findInterval(age, c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) * 10,'+'))