# Functions for calculating drought frequency
#
# droughtwave() returns each criterion row of data (i.e. heatevent) with an additonal
# column counting the number of days below precipitation (p) threshold

# Droughtwave ----
droughtwave <- function(df, temp_col, rain_col, t = 35, p = 1, d=15) {
  temp_col <- deparse(substitute(temp_col)) #grab the non-string column names from function and parse them into strings
  rain_col <- deparse(substitute(rain_col))
  criteria <- df[which(df[[temp_col]]>=t & df[[rain_col]]<p),] #create a dataframe of rows that meet the droughtwave criteria
  output <- data.frame() #create an empty dataframe before the loop starts
  #This loop creates an empty vector and dataframe, loops through the criteria dataset from above and extracts the 15 rows before the criteria date
  templist = list()
  for(i in 1:nrow(criteria)){
    temp.row <- vector(mode="numeric", length=1) #create new blank vector
    temp.df <- data.frame() #create blank dataframe
    temp.row <- as.numeric(row.names(criteria[i,])) #extract the row name of each iteration that meets the above criteria
    temp.row.minus <- as.numeric(temp.row)-d #create a vector which is criteria row name/number - the number of days before we want
    temp.df <- df[temp.row.minus:temp.row,] #extract the 15 day period before the heatday
    #Elimination criteria here
    elim.row <- suppressWarnings(as.numeric(row.names(temp.df[max(which(temp.df[[rain_col]]>=1)),]))) #extract the maximum (highest number, closest to our heat day) row number where rain >=1
    elim.row[is.na(elim.row)] <- as.numeric(row.names(temp.df[1,])) #if its an NA (i.e. none of the days are >=1) then set the value to the first (15th day before) row
    drought.days <- temp.row - elim.row #calculate how many days without rain. If the value was an NA this will =15
    #Need to bind the output drought.days to the original heatwave day
    temp.df <- cbind(criteria[i,], drought.days)
    #output <- rbind(output, temp.df)
    templist[[i]] <- as.data.frame(temp.df) #appending the criteria + drought.day row to the end of a list instead of Rbind. Rbind takes alot of computational power as it copies the dataframe each iteration
  }
  output <- do.call(rbind, templist) #using do.call to run rbind after the loop. This means 1 rbind not hundreds.
  return(output)
}

# Droughtwave_date ----

droughtwave_date <- function(df, temp_col, rain_col, date_col, t = 35, p = 1, d=15) {
  temp_col <- deparse(substitute(temp_col)) #grab the non-string column names from function and parse them into strings
  rain_col <- deparse(substitute(rain_col))
  date_col <- deparse(substitute(date_col))
  criteria <- df[which(df[[temp_col]]>=t & df[[rain_col]]<p),] #create a dataframe of rows that meet the droughtwave criteria
  output <- data.frame()
  templist <- list()
  #false.list <- list()
  for(i in 1:nrow(criteria)){
    temp.row <- vector(mode="numeric", length=1) #create new blank vector
    temp.df <- data.frame() #create blank dataframe
    temp.row <- as.numeric(row.names(criteria[i,])) #extract the row name of each iteration that meets the above criteria
    temp.row.minus <- as.numeric(temp.row)-d #create a vector which is criteria row name/number - the number of days before we want
    temp.df <- df[temp.row.minus:temp.row,] #extract the 15 day period before the heatday
    #elimination criteria
    date.correct <- ifelse(temp.df[1,date_col] == temp.df[d,date_col]-lubridate::days(d), TRUE, FALSE)
    if(date.correct == TRUE){
      elim.row <- suppressWarnings(as.numeric(row.names(temp.df[max(which(temp.df[[rain_col]]>=p)),]))) #extract the maximum (highest number, closest to our heat day) row number where rain >=1
      elim.row[is.na(elim.row)] <- as.numeric(row.names(temp.df[1,])) #if its an NA (i.e. none of the days are >=1) then set the value to the first (15th day before) row
      drought.days <- temp.row - elim.row #calculate how many days without rain. If the value was an NA this will =15
      #Need to bind the output drought.days to the original heatwave day
      temp.df <- cbind(criteria[i,], drought.days)
      #output <- rbind(output, temp.df)
      templist[[i]] <- as.data.frame(temp.df)
    } else{
      elim.row <- suppressWarnings(as.numeric(row.names(temp.df[max(which(temp.df[[rain_col]]>=p)),])))
      elim.row[is.na(elim.row)] <- as.numeric(row.names(temp.df[1,]))
      drought.days <- as.numeric(df[temp.row,date_col] - df[elim.row,date_col])
      temp.df <- cbind(criteria[i,], drought.days)
      templist[[i]] <- as.data.frame(temp.df)
      #false.list[[i]] <- as.numeric(row.names(criteria[i,]))
    }
  }
  output <- do.call(rbind, templist) #using do.call to run rbind after the loop. This means 1 rbind not hundreds.
  #falses <- do.call(rbind, false.list)
  #outputlist <- list("data" = output, "falses" = falses)
  return(output)
}

