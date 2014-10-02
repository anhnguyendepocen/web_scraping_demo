# Clay Ford
# R Users Group October 2014 meetup
# a very basic web scraping example

# IMDB Top 250 movies
# URL: http://www.imdb.com/chart/top?ref_=nv_ch_250_4

library(XML)
library(stringr)
library(dplyr)

movies <- readLines("http://www.imdb.com/chart/top?ref_=nv_ch_250_4")

# web scraping complete!
# let the clean up begin...

# find the index numbers that mark the begin and end of table
start <- which(grepl("table class=\"chart\"", movies))
end <- which(grepl("The formula for calculating the", movies))
movies250 <- movies[start:(end-3)]

# start and end with table tags
movies250[1]
movies250[length(movies250)]

# use readHTMLTable() to convert HTML table to data frame
imdb250 <- readHTMLTable(movies250)

# extract the data frame from the first list element
imdb250 <- imdb250[[1]]

# drop the garbage rows
imdb250 <- imdb250[,c(2,3)]

# fix the names so they don't have spaces
names(imdb250) <- c("Title", "Rating")

# make rating numeric
imdb250$Rating <- as.numeric(as.character(imdb250$Rating))

# add a column for rank
imdb250$Rank <- as.numeric(row.names(imdb250))

# Title column needs work
imdb250$Title

# split the Title string at "\n" (creates list)
tmp <- strsplit(as.character(imdb250$Title), split = "\n")
tmp[[1]]
tmp[[1]][2] # title
tmp[[1]][3] # year


# extract title and year from tmp list

# get title
lapply(tmp, function(x)x[2])
# turn into a vector
unlist(lapply(tmp, function(x)x[2]))
# trim leading spaces
str_trim(unlist(lapply(tmp, function(x)x[2])))

# combine the above into one line to get Titles
imdb250$Title <- str_trim(unlist(lapply(tmp, function(x)x[2])))


# get year
lapply(tmp, function(x)x[3])
# turn into vector
unlist(lapply(tmp, function(x)x[3]))
# extract the year; [0-9]{4} = regular expression to match 4-digit numbers
str_extract(unlist(lapply(tmp, function(x)x[3])), "[0-9]{4}")

# combine the above into one line to get Year
imdb250$Year <- str_extract(unlist(lapply(tmp, function(x)x[3])), "[0-9]{4}")
imdb250$Year <- as.numeric(imdb250$Year)

# tidy up
rm(movies, movies250, tmp, start, end)

# see which years have the most movies on the list
imdb250 %>% 
  group_by(Year) %>%
  summarise(Total=n()) %>%
  arrange(desc(Total)) %>%
  head(10)

# see the 1995 movies
imdb250 %>% 
  filter(Year==1995)
  