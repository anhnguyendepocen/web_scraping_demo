# Clay Ford
# R Users Group October 2014 meetup
# some very basic web scraping examples


# EXAMPLE 1 ---------------------------------------------------------------

# IMDB Top 250 movies
# URL: http://www.imdb.com/chart/top?ref_=nv_ch_250_4

library(XML)
library(stringr)
library(dplyr)

# read the web page source code
movies <- readLines("http://www.imdb.com/chart/top?ref_=nv_ch_250_4", warn=FALSE)

# web scraping complete!
# let the clean up begin...

# find the index numbers that mark the begin and end of table
start <- which(grepl("table class=\"chart\"", movies))
end <- which(grepl("The formula for calculating the", movies))
movies250 <- movies[start:(end-3)]

# start and end with table tags
movies250[1]
movies250[length(movies250)]

# use readHTMLTable() from XML package to convert HTML table to data frame
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

# extract title 
title <- str_extract(string = imdb250$Title, pattern = "\n.*\n")
title <- gsub("\n","",title)
title <- str_trim(title)

# extract year
year <- str_extract(string = imdb250$Title, pattern = "\\([0-9]{4}\\)")
year <- gsub("\\(|\\)","",year)
imdb250$Year <- as.numeric(year)

imdb250$Title <- title

# see which years have the most movies on the list
imdb250 %>% 
  group_by(Year) %>%
  summarise(Total=n()) %>%
  arrange(desc(Total)) %>%
  head(10)

# see the 1995 movies
imdb250 %>% 
  filter(Year==1995)
  
rm(movies, movies250, end, start, title, year)

# EXAMPLE 2 ---------------------------------------------------------------

# multiple pages
# Craigslist listings
# https://charlottesville.craigslist.org/search/cba

dat <- readLines("https://charlottesville.craigslist.org/search/cba?s=0&", warn=FALSE)

# function to prep craigslist data
# dat is data vector obtained from readLines()
CLPrep <- function(dat){
  # generate an R structure representing the HTML structure
  raw2 <- htmlTreeParse(dat, useInternalNodes = TRUE)
  
  # the items are in nodes called "a" with a class attribute equal to "hdrlnk"
  item <- xpathApply(raw2,"//a[@class='hdrlnk']", xmlValue)
  item <- unlist(item)
  
  # date listed in nodes called "span" with a class attribute equal to "date"
  date <- xpathApply(raw2,"//span[@class='date']", xmlValue)
  date <- unlist(date)
  date <- as.Date(date,format="%b %d") # convert Oct 2 to 2014-10-02
  
  # item has the price listed twice or not all
  # need to get row number of which items have prices
  i <- which(grepl(pattern = "class=\"price\"", x = dat))
  tmp <- dat[i]
  # Split text at "</p>" since every item ends with that tag
  items <- strsplit(x = tmp, split = "</p>")
  items <- unlist(items)
  
  # which items have prices?
  ind <- grep(pattern = "class=\"price\"", x=items)
  
  # price listed in nodes called "span" with a class attribute equal to "price"
  prices <- xpathApply(raw2,"//span[@class='price']", xmlValue)
  prices <- unlist(prices)
  
  # keep every other price (remove dupes)
  prices <- prices[seq_along(prices) %% 2 == 0] 
  prices <- as.numeric(gsub("\\$","",prices))
  
  # fill in prices per ind (ie, row numbers which had prices)
  price <- rep(NA,length(item))
  price[ind] <- prices
  
  # create data frame
  data.frame(item, date, price, stringsAsFactors = F)

}

# Note URL increments by 100:
# https://charlottesville.craigslist.org/search/cba?s=0&
# https://charlottesville.craigslist.org/search/cba?s=100&
# https://charlottesville.craigslist.org/search/cba?s=200&
# increment URLs by 100; stop if code contains "no results"

cl.out <- c() # create an empty object to store results
j <- 0 # for URL
repeat{
  raw <- readLines(paste0("https://charlottesville.craigslist.org/search/cba?s=",j,"&"), warn=F)
  if (any(grepl(pattern = "no results", x = raw))) break else {
    cl.out <- rbind(cl.out,CLPrep(raw))
    j <- j + 100
  }
}

summary(cl.out$price)

