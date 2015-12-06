#Collecting Data of Linkedin Profiles
require(plyr)
require(jsonlite)

#Fetching and Binding Data

source("Console.R") #For Keywords input by user

#if HTTP 400, please check Keyword

#Function to clean fetched data

clean <- function(result) {
  
  #Deleting irrelevant variables
  
  result$GsearchResultClass = NULL
  
  result$content = NULL
  
  result$title = NULL
  
  result$clicktrackUrl = NULL
  
  result$formattedUrl = NULL
  
  result$unescapedUrl = NULL
  
  result$richSnippet$metatags = NULL
  
  result$richSnippet$hcard$photo = NULL
  
  result$visibleUrl = NULL
  
  result$titleNoFormatting = NULL
  
  result$Organisation$hcard$title = NULL
 
  #Arranging irrelevant Variables
  
  result$Name <- result$richSnippet$hcard$fn
  result$Organisation <- result$richSnippet$person$org
  result$Role <- result$richSnippet$person$role 
  result$Location <- result$richSnippet$person$location
  result$Content <- result$contentNoFormatting
  result$img <- result$richSnippet$cseImage$src
  result$thumbnail <- result$richSnippet$cseThumbnail$src
  result$height <- result$richSnippet$cseThumbnail$height
  result$width <- result$richSnippet$cseThumbnail$width
  
  #Deleting redundant after arrangement

  result$richSnippet = NULL
  result$contentNoFormatting = NULL

  return(result) 
}

#Data is fecthed from custom google search in json and saved as data frame

#Fetching data from all pages and merging into dataframe

result <- clean(result)

for(i in 0:(pnum - 1))
{
 urlc <- paste(url, start, 20*i, sep = "") #No. of Pages to Fetch
  
 catch <- jsonlite::fromJSON(urlc)$results #Result array
 
 catch = clean(catch) 
 
 result <- rbind.fill(result, catch) #Merge data frames
 
 rm(catch)
}

#View Collected Data
View(result)
