# Functions for extracting climate data before and after specified events
# Used to graph building and cooling of temperatures surrounding a heatevent
#
# Heatbuild Function ----

heatbuild.fun <- function(df, temp_col, date_col, t = 39, days_before=5, days_after=5){
  temp_col <- deparse(substitute(temp_col)) #grab the non-string column names from function and parse them into strings
  date_col_string <- deparse(substitute(date_col))
  criteria <- df[which(df[[temp_col]]>=t & df[["tod"]]=="day"),]
  templist <- list()
  for(i in 1:nrow(criteria)){
    temp.df <- data.frame()
    temp.station <- as.numeric(criteria[i,"station"])
    temp.date <- criteria[i,date_col_string]
    temp.df <- dplyr::filter(df, station == temp.station & tod == "day")
    temp.df <- dplyr::filter(temp.df, dplyr::between(!!enquo(date_col), temp.date - lubridate::days(days_before), temp.date + lubridate::days(days_after)))
    #!!enquo is used to delay the interpretation of the code and quote-and-unquote pattern the date_col. Newer versions of Rlang use curly curly {{date_col}}
    temp.df$iteration <- i
    rows <- nrow(temp.df)
    temp.df$seqdate <- seq(1,rows,1) #writes a column on each iteration that is a sequence from the start date through to end. heatevent should be 5-6 most of the time
    temp.df$it_date <- criteria[i,date_col_string]
    templist[[i]] <- as.data.frame(temp.df)
  }
  heatbuild <- do.call(rbind, templist)
  return(heatbuild)
}

# criteria <- nsw_sites[which(nsw_sites[["temperature"]]>=39 & nsw_sites[["tod"]]=="day"),]
# templist <- list()
# for(i in 1:nrow(criteria)){
#   temp.df <- data.frame()
#   temp.station <- as.numeric(criteria[i,"station"])
#   temp.date <- criteria[i,"date"]
#   temp.df <- filter(nsw_sites, station == temp.station & tod == "day")
#   temp.df <- filter(temp.df, between(date, temp.date - days(5), temp.date + days(5)))
#   temp.df$iteration <- i
#   rows <- nrow(temp.df)
#   temp.df$seqdate <- seq(1,rows,1) #writes a column on each iteration that is a sequence from the start date through to end. heatevent should be 5-6 most of the time
#   temp.df$it_date <- criteria[i,"date"]
#   templist[[i]] <- as.data.frame(temp.df)
# }
# heatbuild <- do.call(rbind, templist)
