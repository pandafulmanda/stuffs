library(rvest)
library(purrr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(plotly)

#' Base url for data
base_url <- "http://www.espn.com/nba/attendance/_/year/"
#' Range of available years
years <- seq(2001, 2017)

#' `as_num` for converting number strings with commas to numbers.
as_num <- function(string){
  num <- string %>%
    gsub(',', '', .) %>%
    as.numeric
  num  
}

#' `make_header` from the first two rows of data.
make_header <- function(index, headers){
  if(index > 2) {
    header <- index %>%
      c %>%
      select(headers, .) %>%
      unlist %>%
      paste(., collapse = ' ')
  } else {
    header <- as.vector(unlist(headers[index])[2])
  }
  header
}

get_data <- function(year){
  url <- paste(base_url, year, sep='')
  year_attendance <- url %>%
    read_html %>%
    html_nodes('table') %>%
    html_table()

#' Make better headers and remove the 2 column name rows.
  header_rows <- year_attendance[[1]][1:2,]
  names(year_attendance[[1]]) <- seq(1, length(header_rows)) %>%
    map(~ make_header(., header_rows)) %>%
    unlist
  year_attendance <- year_attendance[[1]][-1:-2,]

#' Convert `--` to NAs and clean up data types
  year_attendance[year_attendance == '--'] <- NA
  year_attendance <- transform(year_attendance,
                               `Home PCT` = as.numeric(`Home PCT`),
                               `Road PCT` = as.numeric(`Road PCT`),
                               `Overall PCT` = as.numeric(`Overall PCT`),
                               `Home GMS` = as.numeric(`Home GMS`),
                               `Road GMS` = as.numeric(`Road GMS`),
                               `Overall GMS` = as.numeric(`Overall GMS`),
                               `Home TOTAL` = as_num(`Home TOTAL`),
                               `Home AVG` = as_num(`Home AVG`),
                               `Road AVG` = as_num(`Road AVG`),
                               `Overall AVG` = as_num(`Overall AVG`),
                               `RK` = as.numeric(`RK`)
                               )
#' Add `Year` column.
  year_attendance['Year'] <- year
  year_attendance
}

#' `get_data` for each year and combine the data by binding the rows of data.
all_years_attendance <- years %>%
  map(get_data) %>%
  bind_rows

# NA for outlier.
all_years_attendance[157,]$Home.PCT <- NA

base_plot <- ggplot(all_years_attendance, aes(
  x = Year,
  y = Home.PCT,
  color = TEAM
))
line_plot <- base_plot + geom_line()

ggplotly(line_plot)
