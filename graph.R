# Generating network in igraph

######################################### SET-UP ##################################################################

library(sqldf)
library(igraph)
library(reshape2)
library(plyr)

######################################### melt_cast.R #############################################################

# Reads csv as data frame. Contains publications with authors, year, and method
RawPubs <- read.csv("AllPubRaw.csv", header=T, sep=",")

# Edgelist from first author to other authors with year and method data
# See 'eGO//melt_cast.R' for more documentation, or read up on the melt() function.
PubMeltRaw <- melt(RawPubs, id.vars = c("Author1", "Year", "Method"))

# Many papers do not have 36 authors. Remove rows with no author data
# Fill blank spaces with NA, then use na.omit
PubMeltRaw[PubMeltRaw == ""] <- NA
PubMelt <- na.omit(PubMeltRaw)

######################################### concatenate.R ###########################################################

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


######################################## Name_to_ID.R #########################################################

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

############################################ NEW SCRIPT ##############################################################

# generate an igraph object, using our edge list and vertice data
g <- graph.data.frame(edges, directed = T, vertices = nodes)

# Apply the Fruchterman-Reingold layout
layout_g <- layout.fruchterman.reingold(g)

create_graph_png <- function(max_date,i) {
  
  # Translation: make a subgraph of 'g'. Edgelist to use is entries in 'edges' which happened
  # before the input date. 
  # A subgraph gives part of a graph, but will only contain the specified vertices & edges.

  # the subgraph.edges argument will only take edges in the form of numerical ids.
  # We thus choose from the 'edges' dataframe, where we choose all rows where the year is
  # less than/equal to (<=) to the max_date (i.e. our input date)
  # (Note the role of the which() function, which selects everything that returns T for the logical
  # expression inside the bracket.)
  # The column to choose is the 'id' column with the edge IDs
  
  gx <- subgraph.edges(g,edges[which(edges$year <= max_date), 'id'], 
                       delete.vertices=F)

  # Specify both the vertex fill and vertex frame colour. "Black" is a preset colour in igraph
  # V(gx) selects all nodes in the subgraph
  # Probably not very necessary? (this is taken from the carpool code)
  V(gx)$color <- "black"
  V(gx)$frame.color <- "black"
  
  # RGB is one of the ways to specify colour in programming. 
  # Alpha value denotes transparency (This is consistent across many programming contexts)
  # A simple search on Google will return the RGB values for other colours you may wish to use
  col_a1 <- rgb( .9, .0, .4, alpha=.4) 
  
  # Select entries in 'nodes' which returns TRUE if their type is a1
  # i.e. select 1st authors, and set their fill/frame colours to the colour defined in above col_a1
  V(gx)[which(nodes$type == "a1")]$color <- col_a1
  V(gx)[which(nodes$type == "a1")]$frame.color <- col_a1
 
  # Repeat for aO
  col_aO <- rgb( .1, .6, 1 , alpha=.4)
  V(gx)[which(nodes$type == "aO")]$color <- col_aO
  V(gx)[which(nodes$type == "aO")]$frame.color <- col_aO

  # Repeat for those published as both author positions. Note that `|` means "or"
  col_hybrid <- rgb( .4, .3, 1, alpha=.4)
  V(gx)[which(nodes$type == "a1aO" | nodes$type == "aOa1")]$color <- col_hybrid
  V(gx)[which(nodes$type == "a1aO" | nodes$type == "aOa1")]$frame.color <- col_hybrid
  
  # the size of a node grows with the number of attached edges
  # This is done logarithmically, so that the nodes with larger degrees will not fill the entire page.
  
  V(gx)$size <- log(3*degree(gx))
  
  ##### FEATURE CURRENTLY VOIDED #################################################################
  #
  # if node is 3 years or less, colour glows green.
  # logic statement: year of network (max_date) - year of the node is less/equal to 3 
  #newones <- hsv(.36,1,1,alpha=.5)
  #V(gx)[which((max_date - nodes$year) <= 3)]$color <- newones
  #V(gx)[which((max_date - nodes$year) <= 3)]$frame.color <- newones
  # 
  # New nodes come in very big, then decreases, then "settles" at a "native size"
  #V(gx)[which((max_date - nodes$year) <= 1)]$size <- 7
  #V(gx)[which( ((max_date - nodes$year)>1) & ((max_date - nodes$year)<=3) )]$size <- 1.6
  #
  ###############################################################################################
  
  
  # Get's rid of the not yet to be displayed nodes
  # alpha = 0 means completely transparent
  
  # (So technically, if gx is being made corretly, this should be needed.
  # For some reason, nodes from beyond 1992 is showing up in the 1992 graph, etc.
  # This fixes the bug, but I'm still trying to figure out exactly why...)
  
  notyet <- hsv(1,1,1,alpha=0)
  
  V(gx)[which(degree(gx) < 1)]$frame.color=notyet 
  V(gx)[which(degree(gx) < 1)]$color=notyet 
  V(gx)[which(degree(gx) < 1)]$size=0
  
  # Sets background colour
  bgcolor <- hsv(0.66,0.05,1)
  
  # Textual annotations!
  
  # For years before 2013, annotate with the plot title and the year
  plot_title <- paste("Co-authorship Network of CCO and SSCO Methods in Pharmacoepidemiology | ")
  if(max_date < 2013) {
    plot_title <- paste(plot_title, max_date)
  }
  
  # For years after 2013, label the graph as 2013. When we run the function, I will be instructing the
  # program to run the function more than necessary, so that the viewer will have more time to inspect
  # the final graph in 2013. This line ensures the correct label is placed on these plots
  if(max_date >= 2013) {
    plot_title <- paste(plot_title, "2013")
  }
  
  # Legend
  sub_title <- "PINK = First authors || BLUE = Other authors || PURPLE: Publishd as both"
  
  par(bg=bgcolor)
  # create a PNG and prints it to designated location: type `?png()` into console for more info
  # `sprintf()` runs the commands inside of it, but returns it in a string format
  # (%03d, i): if i=1, you will see 001. if i=2, you will see 002. etc.
  # NOTE TO SELF: CHANGE THIS LOCATION EACH TIME YOU PRINT
  png(sprintf("C:\\Users\\Amy\\Documents\\R_Git\\R_outputs\\testI\\testI%02d.png", i),
  width=5000, height=5000, bg=bgcolor, res=372)
  
  # Some other miscellaneous specifications
    plot(gx, margin=0, frame=F, main=plot_title, sub=sub_title, 
       vertex.label=NA, edge.color=rgb(.2,.2,.2), edge.arrow.mode=0, 
       layout=layout_g, edge.width=.8)
  text(10,10, label=max_date)
  dev.off() # shuts off current graphic printing device
}


# Let's print! Could take a while... =============================================================================

# First pub was in 1992, therefore start_date is 1991 because of the start_date+i argument
# Translation: loop the function we just wrote 25 times (starting in 1992), to generate a new subgraph each year 
# Note: althogh 2013 - 1991 = 22, I'm looping for 25 times, to have the animation stay longer at 2013.
# The terminal message, therefore, will tell you that you're printing year 2015 when the data is actually for 2013
# The plot titles in the results will accurately reflect the year.
one_year_prev <- 1991
for (i in c(1:25)) {
  message(sprintf("printing year %d", one_year_prev + i))
  create_graph_png(one_year_prev + i, i)
}

######################## test colours here:
plot(x=1:10, y=rep(7,10), pch=19, cex=3, col=rgb( .9, .0, .4, alpha=.4))
points(x=1:10, y=rep(6,10), pch=19, cex=3, col=rgb( .8, .4, .8, alpha=.4))
points(x=1:10, y=rep(5,10), pch=19, cex=3, col=rgb( .4, .3, 1, alpha=.4))
points(x=1:10, y=rep(4,10), pch=19, cex=3, col=rgb( .1, .6, 1 , alpha=.4))
