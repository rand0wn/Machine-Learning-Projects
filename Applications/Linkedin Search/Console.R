#Overview of Project


#Input Variables:

Name <- readline(prompt = "Enter name : ")
Location <- readline(prompt = "Enter Location : ")
Organisation <- readline(prompt = "Enter Organisation : ")
Role <- readline(prompt = "Enter Role : ")
Info <- readline(prompt = "Enter any other relevant information(skills, projects) : ")

#Keyword to pass to custom search
Keywords <- paste(Name, Location, Organisation, Role, Info, sep = "+")

#URL to Fetch from
ur <- "https://www.googleapis.com/customsearch/v1element?key=AIzaSyCVAXiUzRYsML1Pv6RwSG1gunmMikTzQqY&num=20&cx=009462381166450434430:ecyvn9zudgu&q="
url <- paste(ur, Keywords, sep = "")
start <- "&start="

#Maximum Page Count
p <- jsonlite::fromJSON(url)
result <- p$results #Intialise empty result array for Fetch data
result <-  result[0,] 
page <- p$cursor$pages
pnum <- as.integer(dim(page["start"])[1]) #Max Page Count without next, max 100 instances
rm(p, page)
