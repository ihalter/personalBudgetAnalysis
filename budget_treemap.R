#call this to start
intake_data <- function() {
  
  #------
  ##Require installation of RColorBrewer and treemap packages
  if (!require("RColorBrewer")) {
    install.packages("RColorBrewer")
    library(RColorBrewer)
  }
  
  if (!require("treemap")) {
    install.packages("treemap")
    library(RColorBrewer)
  }
  
  #------
  ##read in total budgeting file
  budget <- read.csv("C:\\Users\\halte\\Desktop\\R from home\\2015_budget_flat_file.csv", header = T,
                     col.names = c("Key","Year","Month","Day","transactionID","Date","Company","Amount",
                                   "Description","Class","Method of Payment"))
  
  #------
  ##remove the description column, or any other columns on intake as you see fit
  budget <- budget[names(budget) != "Description"]
  
  #------
  ##change the Amount to a numeric, instead of a text field
  budget$Amount <- as.numeric(budget$Amount)
  
  #------
  ##Change the Date field to an actual date format, not a string, just uncomment
  #budget$Date <- as.Date(budget$Date, "%m/%d/%y")
  
  #return the budget matrix
  return(budget)
  
  }

#still a work in progress
prepare_tree <- function(dataset) {
  
  #a treemap class requires 
  dataset = as.data.frame(dataset)
  
  #--------
  ##Set parameters to treemap by user input
  #Index
  ind = readline(prompt = "Pick an Index Column: ")
  if (!(ind %in% colnames(dataset))) {
    print("This Index column is not in the dataset")
    break
  }
  
  #Title
  titl = readline("Pick a title: ")
  
  #Algorithm
  alg = readline(prompt = "Pick an algorithm, squarified or pivotSize: ")
  
  #Pick a variable which determines vertical size
  #break out of loop, rather then throw an error, have them call the function again
  vSiz = readline("Pick Sizing Dimension Variable: ")
  if (!(vSiz %in% colnames(dataset))) {
    print("This Sizing Dimension column is not in the dataset")
    break
  }
  
  #Pick a variable that determines colors
  vCol = readline("Pick Color Dimension Variable: ")
  if (!(vSiz %in% colnames(dataset))) {
    print("This Color Dimension column is not in the dataset")
    break
  }
  
  #--------
  #Choose a color palette
  
  
  #Contains list of Colorspace palettes and COlorBrewer set options
  #--------
  ## Create the treemap
  tr <- treemap(dtf = dataset,index = ind, title = titl, algorithm = alg,
                vSize = vSiz, vColor = vCol, type = "index", palette = "Blues")  
  
  return(tr)  
}





plot(tr) #pulls the treemap up in the viewer

tr <- treemap(dtf = df_budg,index = "Class", title = "Budget Analysis", algorithm = "pivotSize",
              vSize = "Amount", vColor = "Month", type = "index", palette = "Blues", position.legend = "bottom")

#problems, need to set up multiple indexes to correct sorting algorithm
#need to make sense on how the pallette works

#publish this as markdown

#create a Shiny webapp so I can play around with it