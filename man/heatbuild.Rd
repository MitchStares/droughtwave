\name{heatbuild}
\alias{heatbuild}
\title{Extract rows before and after selected criteria}
\usage{
heatbuild(df, temp_col, date_col, t = 39, days_before=5, days_after=5)
}
\arguments{

}
\description{
This function loops through a data frame, looking for heatevents (specified by the limit t) and extracting the previous and following days, denoted by the arguments days_before and days_after.

This function relies on the lubridate package for date delination, rather than row numbers. This requires a date column - date_col - which needs to be class(date).

The intention of this function is to visualise the building of temperatures and the cooling down of temperatures surrounding the peak day or "heatevent".
}
\examples{
build_summary <- heatbuild(weather,
                            temp_col = temperature,
                            date_col = date)
View(build_summary)
}
