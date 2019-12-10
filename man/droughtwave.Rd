\name{droughtwave}
\alias{droughtwave}
\title{Calculate frequency of rows meeting selected criteria, before a selected event}
\usage{
droughtwave(df, temp_col, rain_col, t = 35, p = 1, d=15)
}
\arguments{

}
\description{
The purpose of this function is to iterate/loop through a dataframe, extracting rows that meet the criteria set by the arguments t and p. From there, it will count the number of rows (equal to d) before the extracted row where p <= the input.

This function requires an input of a dataframe and named arguments of two columns. For the purposes of this package, these arguments have been named temp_col and rain_col for temperature and precipitation, respectively.

Following this, the function takes in three numerical inputs. t and p are numerical breakpoints set as the lower limit (in the case of t) and upper limit (in the case of p) for their respectively columns, temp_col and rain_col. d is the number of days/rows before the criteria row
}
\examples{
drought_days <- droughtwave(weather,
                            temp_col = temperature,
                            rain_col = precipitation)
View(drought_days)
}
