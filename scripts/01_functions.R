#------------------------------------------------------------------------------
# The birth_number column is given in the form of YYMMDD for men,
# and YYMM+50DD for women. The objective of this function is to return
# the gender of the client via the birth_number.
GetGenderFromBirthnumber <- function(var_birth_number) {
  
  month <- substr(var_birth_number, 3, 4)
  result <- ifelse(as.integer(month) > 50, "w", "m")
  
  return(result)
}

#------------------------------------------------------------------------------
# The birth_number column is given in the form of YYMMDD for men,
# and YYMM+50DD for women. The objective of this function is to return
# the final birthday as Date.
GetBirthdateFromBirthnumber <- function(var_birth_number, var_gender) {
 
  year <- paste("19", substr(var_birth_number, 1, 2), sep="")
  month <- ifelse(var_gender == "m", substr(var_birth_number, 3, 4), as.integer(substr(var_birth_number, 3, 4)) - 50)
  day <- substr(var_birth_number, 5, 6)
  result <- as.Date(paste(year, "-", month, "-", day, sep=""), format = "%Y-%m-%d")
  
  return(result)
}

#------------------------------------------------------------------------------
# The objective of this function is to convert the strange bank date style 
# to the regular R Date datatype.
ConvertToDate <- function(var_date) {
  
  year <- paste("19", substr(var_date, 1, 2), sep="")
  month <- substr(var_date, 3, 4)
  day <- substr(var_date, 5, 6)
  result <- as.Date(paste(year, "-", month, "-", day, sep=""), format = "%Y-%m-%d")
  
  return(result)
}