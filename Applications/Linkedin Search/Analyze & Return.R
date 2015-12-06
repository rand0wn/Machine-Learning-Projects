#Analysing & Returning Meaningful Results

#Procedure:

#Check Order:

#1.Name 2.Location 3.Organisation 4.Role 5.Info

Show <- data.frame()

if(Name != "") #Name is not empty
{
  Show <- subset(result, grepl(Name, result$Name, fixed = TRUE, ignore.case = TRUE ))
}  

if(Location != "") #Location is not empty
{  
  Show <- subset(Show, grepl(Location, result$Location ))
}

if(Organisation != "") #Organisation is not empty
{
  Show <- subset(Show, grepl(Organisation, result$Organisation )) 
}

if(Role != "") #Role is not empty
{
  Show <- subset(Show, grepl(Role, result$Role ))
}

if(Info != "") #Info is not empty
{
  Show <- subset(Show, grepl(Info, result$Info )) 
}

