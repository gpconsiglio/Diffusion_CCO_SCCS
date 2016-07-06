# Detailed comments (and faulty scripts) for replacing names with unique ID

library(sqldf)
library(igraph)
library(reshape2)

######################################### SET-UP (network1.R) #####################################################

# Reads csv as data frame. Contains publications with authors, year, and method
RawPubs <- read.csv("AllPubRaw.csv", header=T, sep=",")

# Edgelist from first author to other authors with year and method data
# See 'eGO//melt_cast.R' for more documentation, or read up on the melt() function.
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

######################################### NEW SCRIPT #####################################################

# Replace old data frame with new data frame that adds a column of numbered IDs
# Structure: create a data frame where column i is a vector of numbers, and the other columns of 'nodes'
nodes <- data.frame(i = 1:length(nodes$author), author = nodes$author, 
                    type = nodes$type, method = nodes$method, year = nodes$year)


# Note: carpool code uses SQL. I decided to rewrite it in R, because the code kept breaking in SQL (see
# the bottom of this script. I suspect that SQL could do an even neater job, but oh well...)

# Create df of first authors IDs

# This is done by taking a subset of the original dataframe to exclude aOs using !=
# This leaves only 1st auth or those who've published as both
# FAULTY CODE FOR LEARNING:
a1_id <- subset(nodes, type != 'aO', select = (i, author, type, year)) 

# It's important to use the c( ) notation to store things to a vector. Thus:
a1_id <- subset(nodes, type != 'aO', select = c(i, author, type, year))
aO_id <- subset(nodes, type != 'a1', select = c(i, author, type, year))

# Rename columns (this will come in handy when we join the tables)
a1_id <- rename(a1_id, c("i"="a1_id"))
aO_id <- rename(aO_id, c("i"="aO_id"))

# Let's join the tables!

# A join allows us to literally join two tables when they both match on a certain attribute.
# For example, if we have Table1 with columns "Name" and "TestScores1", and
# Table2 with columns "Name" and "TestScores2", we can get a complete table with "Name", "TestScores1", "TestScores2"

# Translation: SELECT the column 'author1' from the table 'PubMelt', column 'a1_id' from table 'a1_id', etc.
# INNER JOIN PubMelt (our edgelist of a1 and aO relationships) and a1_id (author 1 IDs)
# The matching value is the name of the 1st authors. In PubMelt, this column is named 'Author1', and in a1_id, this
# column is named "author"
edges <- sqldf("SELECT PubMelt.Author1, a1_id.a1_id, PubMelt.value, PubMelt.year  
                 FROM PubMelt INNER JOIN a1_id
                 ON PubMelt.Author1 = a1_id.author")

# SQL does not allow us to join three tables at the same time. We need to join the first two (edges and a1 IDs),
# store this temporarily, then join this with the other table (aO IDs)
edges <- sqldf("SELECT edges.Author1, edges.a1_id, edges.value, edges.year, aO_id.aO_id  
                 FROM edges INNER JOIN aO_id
                 ON edges.value = aO_id.author")

# New dataframe with only IDs
edges <- data.frame(a1 = edges$a1_id, aO = edges$aO_id, year = edges$Year)

# add edges ID to table
edges$id <- 1:length('a1')

########### CODES NOT USED, BUT DOCUMENTED FOR LEARNING PURPOSES ####################################################

# Attempt, based on carpool sample code, but doesn't work for some reason :(
linkt1 <- sqldf("SELECT PubMelt.Author1, PubMelt.value, PubMelt.year, nodes.i
                FROM PubMelt INNER JOIN nodes 
                ON PubMelt.Author1 = nodes.author
                FROM PubMelt INNER JOIN nodes 
                ON PubMelt.values = nodes.author") # syntax error: joining on two things


# more code which don't work LOL --------------------------------------------------------------------

# only keep author 1? this doesnt work and idk why
# Which returns a TRUE or FALSE! This is why it is often used with logical operators, but
# clearly I'm still not understanding part of the story...
# maybe i'm not selecting the right column/row structure? i. don't. know.
a1_id <- data.frame(-nodes[which(nodes$authors == "aO"), ]) 

# this should simply rename the nodes with the IDs?? Which is not what we need. Either way,
# it doesn't work, soooooooo.....
PubMelt$Author1 <- nodes_a1_gp[match(PubMelt$Author1, nodes$author, 'i')]


