#Analysing & Returning Meaningful Results

#Procedure:

#Check Order:

#1.Name 2.Location 3.Organisation 4.Role 5.Info


search <- function(result)
{

  Show <- data.frame()
  
if(Name != "") #Name is not empty
{
  Show <- subset(result, mapply(grepl, Name, result$Name))
}  

if(Location != "") #Location is not empty
{  
  Show <- subset(Show, mapply(grepl, Location, result$Location))
}

if(Organisation != "") #Organisation is not empty
{
  Show <- subset(Show, mapply(grepl, Organisation, result$Organisation)) 
}

if(Role != "") #Role is not empty
{
  Show <- subset(Show, mapply(grepl, Role, result$Role))
}

if(Info != "") #Info is not empty
{
  Show <- subset(Show, mapply(grepl, Info, result$Info)) 
}

  return(Show)
}    

Show <- search(result)

View(Show)