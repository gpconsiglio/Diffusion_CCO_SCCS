library(sqldf)
library(igraph)
library(reshape2)
library(plyr)

######################################### old stuff #############################################################

# Reads csv as data frame. Contains publications with authors, year, and method
RawPubs <- read.csv("AllPubRaw.csv", header=T, sep=",")

# Generates preliminary edgelist from first author to other authors, with year and method data
PubMeltRaw <- melt(RawPubs, id.vars = c("Author1", "Year", "Method"))

# Many papers do not have 36 authors. Remove rows with no author data
# Fill blank spaces with NA, then use na.omit
PubMeltRaw[PubMeltRaw == ""] <- NA
PubMelt <- na.omit(PubMeltRaw)



# Divide data into two tables: first authors (a1) and 'other' authors (aO)----------------------------------------

nodes_a1_raw <- sqldf("SELECT Author1 AS author, 'a1' AS type, Method AS method, Year AS year FROM PubMelt")
nodes_aO_raw <- sqldf("SELECT value AS author, 'aO' AS type, Method AS method, Year AS year FROM PubMelt")

# Authors who published in both methods are now either CCO,SSCO or SSCO,CCO
nodes_a1_cm <- sqldf("SELECT author, type, GROUP_CONCAT(DISTINCT method) AS method, year AS Year
                     FROM nodes_a1_raw
                     GROUP BY author")

nodes_aO_cm <- sqldf("SELECT author, type, GROUP_CONCAT(DISTINCT method) AS method, year AS Year
                     FROM nodes_aO_raw
                     GROUP BY author")

# Create nodes list
nodesall <- sqldf("SELECT author, type, method, year FROM nodes_a1_cm 
                  UNION SELECT author, type, method, year FROM nodes_aO_cm")

# concatenate authors who've published as both a1 & aO 
nodes <- sqldf("SELECT author, GROUP_CONCAT(type,'') AS type, method, min(year) AS year
               FROM nodesall
               GROUP BY author")

# Remove nodes beyond their initial entry (ie. first publication)
nodes <- sqldf("SELECT author, type, method, min(year) AS year FROM nodes GROUP BY author")


# In the final table of nodes, all authors are listed with the year of their first publication in
# the field, along with method(s) and type(s) of author position




# Replace old data frame with new data frame that adds a column of numbered IDs
nodes <- data.frame(i = 1:length(nodes$author), author = nodes$author, 
                    type = nodes$type, method = nodes$method, year = nodes$year)

# Reseparate nodes (with numbered ID data) into two dataframes for a1 IDs and aO IDs
a1_id <- subset(nodes, type != 'aO', select = c(i, author, type, year))
aO_id <- subset(nodes, type != 'a1', select = c(i, author, type, year))

# Rename columns (this will come in handy when we join the tables)
a1_id <- rename(a1_id, c("i"="a1_id"))
aO_id <- rename(aO_id, c("i"="aO_id"))

# Let's join the tables!
edges <- sqldf("SELECT PubMelt.Author1, a1_id.a1_id, PubMelt.value, PubMelt.year  
               FROM PubMelt INNER JOIN a1_id
               ON PubMelt.Author1 = a1_id.author")

edges <- sqldf("SELECT edges.Author1, edges.a1_id, edges.value, edges.year, aO_id.aO_id  
               FROM edges INNER JOIN aO_id
               ON edges.value = aO_id.author")

# final edges dataframe with only IDs and year
edges <- data.frame(a1 = edges$a1_id, aO = edges$aO_id, year = edges$Year)

# add edges ID to table
edges$id <- 1:length(edges$a1)

################################### 2016-07-11 ADDING GIULIA'S QUALITY DATA ##########################################
################################################# NEW SCRIPT #########################################################


# Create a function for concatenating -----------------------------------------------------------------------

clean_data <- function(df) {
  
  # the variable of the function is "df"
  # when we run this function, df will equal each slices (i.e. df for each year) we've made before
  # the actual SQL command follows very similar logic as previous concatenations
  
  sqldf("SELECT author, GROUP_CONCAT(DISTINCT method) AS method,
    avg(reportScore) AS reportScore, avg(methodsScore) AS methodsScore
    FROM df
    GROUP BY author") 
  
  
  
  # ____PLACEHOLDER FOR MORE DATA CLEANING TO COME ______
  
}



# Let's run the function! ----------------------------------------------------
# Make a list to store the output of the loops into
node_list <- list()

# Make a vector of names we will assign to our data frames
df_names <- c()
for (i in c(1:22)) {
  df_names[i] <- sprintf("nodes%d", 1991+i)
}

# load the dataset
qualityraw2013 <- read.csv("quality2013.csv", header=T, sep=",")

# Make a temp df of data up to a certain year, then clean this data using function
for (i in c(1992:2013)) {
  dfs <- qualityraw2013[qualityraw2013$year <= i, ]
  dfs <- clean_data(dfs)
  node_list[[i]] <- dfs # stores all dfs in a list
  assign(df_names[i-1991], node_list[[i]]) # attaches a name to these dfs
}


  



################################# ADDING REPORT/METHOD GRADES TO TABLE #############################################
########### TRIAL WITH JUST 2013, CHANGE LATER FOR THE LOOP ######################

sqldf("SELECT author, GROUP_CONCAT(DISTINCT method) AS method,
    avg(reportScore) AS reportScore, avg(methodsScore) AS methodsScore
    FROM df
    GROUP BY author")  


# attempt 1: doesn't work???
#if (node2013$reportScore >= 80) {
#  node2013$reportGrade <- "a"
#} else {
#  node2013$reportGrade <- 'trial'
#}

# attempt2
node2013$reportGrade <- ifelse(node2013$reportScore >= 80, 'a', NA)
node2013$reportGrade <- ifelse( ((node2013$reportScore < 80) & (node2013$reportScore >=60)) , 'b', NA)

# Check Git history for more scraped code



############# OTHER OPTIONS/SCRAPPED CODE ###############################################


# OPTION 0: POOR ------------------------------------------------------------------------

# Import quality data from csv into R
quality2013 <- read.csv("quality2013.csv", header=T, sep=",")

# extract pieces of quality2013 such that quality[y] is a slice of quality2013 up to year y

# make a list
quality <- list();

# create a loop where we make a data frame for years up to 2013-i
# then store this to the 2013-i position in the prev defined list
for (i in c(1:21)) {
  dfs <- quality2013[quality2013$year <= (2013-i), ]
  quality[[ (2013-i) ]] <- dfs
}

# add quality2013 to the list too
quality[[2013]] <- quality2013

# OPTIONAL: check if we've done it right
quality[2012]  # lots of NA values, but shouldn't affect us

# TEMPORARY SOLUTION: JUST TAKE EVERYTHING OUT OF THE LIST LOL
quality2012 <- quality[[2012]]
quality2011 <- quality[[2011]]
quality2010 <- quality[[2010]]

# OPTION 1: make a slice, temporarily store the output, then run clean_data on it ----------------- 

concatenate <- function(df) {
  sqldf("SELECT author, GROUP_CONCAT(DISTINCT method) AS method,
        avg(reportScore) AS reportScore, avg(methodsScore) AS methodsScore
        FROM df
        GROUP BY author")
}

# load the dataset
quality2013 <- read.csv("quality2013.csv", header=T, sep=",")

# make a list
quality <- list();

# create a loop where we make a data frame for years up to 2013-i
# then store this to the 2013-i position in the prev. definied list
for (i in c(1:21)) {
  dfs <- quality2013[quality2013$year <= (2013-i), ]
  quality[[ (2013-i) ]] <- concatenate(dfs)
  print(quality[[(2013-i)]])
  message("-------------")
}

# add quality2013 to the list too
quality[[2013]] <- concatenate(quality2013)

# OPTION 2: make a slice, clean the data, then loop this for all the slices ----------------------


clean_data <- function(df) {
  sqldf("SELECT author, GROUP_CONCAT(DISTINCT method) AS method,
    avg(reportScore) AS reportScore, avg(methodsScore) AS methodsScore
    FROM df
    GROUP BY author")
}

# load the dataset
quality2013 <- read.csv("quality2013.csv", header=T, sep=",")


for (i in c(1:21)) {
  dfs <- quality2013[quality2013$year <= (2013-i), ]
  dfs <- concatenate(dfs)
  print(dfs)
  message("-------------")
}

