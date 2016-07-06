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
edges$id <- 1:length('a1')

############################################ NEW SCRIPT ##############################################################

# generate an igraph object, using our edge list and vertice data
g <- graph.data.frame(edges, directed = T, vertices = nodes)

# Apply the Fruchterman-Reingold layout
layout_g <- layout.fruchterman.reingold(g)

######################################################################################################################
#                                   START RUNNING FUNCTION FROM HERE TO SAVE TIME                                    #
######################################################################################################################

create_graph_png <- function(max_date,i) {
  
  # Structure of function: subgraph.edges(graph, eids, delete.vertices = TRUE)
  # Translation: make a subgraph of 'g'. Edgelist to use is entries in 'edges' which happened
  # before the input date. (<= less than/equal to; which() returns T/F based on a logical condition)
  # A subgraph gives part of a graph, but will only contain the specified vertices & edges.
  gx <- subgraph.edges(g,
                       edges[which(edges$year <= max_date),'i'], 
                       delete.vertices=F)

  # Specify both the vertex fill and vertex frame colour. "Black" is a preset colour in igraph
  # V(gx) selects all nodes in the subgraph
  # lol why is this even necessary (this is taken from the carpool code)
  V(gx)$color <- "black"
  V(gx)$frame.color <- "black"
  
  # HSV is one of the ways to specify colour in programming. Often used for ease of controlling colour intensity
  # Alpha value denotes transparency (This is consistent across many programming contexts)
  # designates all a1 colours
  col_a1 <- hsv(0,1,1,alpha=.5)
  
  # Select entries in 'nodes' which returns TRUE if their type is a1
  # i.e. select 1st authors, and set their fill/frame colours to the colour defined in above col_a1
  V(gx)[which(nodes$type == "a1")]$color <- col_a1
  #V(gx)[which(nodes$type == "a1")]$frame.color <- col_a1

  # specifies size
  V(gx)[which(nodes$type == "a1")]$size <- 1
 
  # Repeat for aO
  col_aO <- hsv(0.66,1,1,alpha=.5)
  V(gx)[which(nodes$type == "aO")]$color <- col_aO
  V(gx)[which(nodes$type == "aO")]$frame.color <- col_aO
  #V(gx)[which(nodes$type == "aO")]$size <- 1

  # Repeat for those published as both author positions. | means OR
  col_hybrid <- hsv(.7,1,1,alpha=.5)
  V(gx)[which(nodes$type == "a1aO" | nodes$type == "aOa1")]$color <- col_hybrid
  V(gx)[which(nodes$type == "a1aO" | nodes$type == "aOa1")]$frame.color <- col_hybrid
  #V(gx)[which(nodes$type == "a1aO" | nodes$type == "aOa1")]$size <- 1

  # the size of a node grows with the number of attached edges
  # [BUG BUG BUG] doesnt work lol
  V(gx)$size <- degree(gx)^(2)
  
  # new nodes emphasized by bright color and a changing size
  newones <- hsv(.36,1,1,alpha=.5)
  
  # if node is 3 years or less, colour glows green.
  # logic statement: year of network (max_date) - year of the node is less/equal to 3 
  #V(gx)[which((max_date - nodes$year) <= 3)]$color <- newones
  #V(gx)[which((max_date - nodes$year) <= 3)]$frame.color <- newones
  
  # Similar logic. Make size 2.5 when node appears, after a while make it a bit smaller
  V(gx)[which( ((max_date - nodes$year)>3) & ((max_date - nodes$year)<=5) )]$size <- 1.6
  V(gx)[which((max_date - nodes$year) <= 3)]$size <- 2.5
  
  # [BUG BUG BUG BUG] get's rid of the not yet to be displayed nodes
  # [AL] this line is a little odd. technically, these nodes shouldn't have been drawn in anyway
  # alpha = 0 means completely transparent
  notyet <- hsv(1,1,1,alpha=0)
  
  # [BUG BUG BUG BUG] get's rid of the not yet to be displayed nodes
  # ok but technically NOTHING should have a degree less than 1...if it does, clearly gx wasnt
  # drawn correctly???? wtf
  # but if these lines are commented out, everything works fine. w t f
  # V(gx)[which(degree(gx) < 1)]$frame.color=notyet 
  # V(gx)[which(degree(gx) < 1)]$color=notyet 
  # V(gx)[which(degree(gx) < 1)]$size=0
  
  bgcolor <- hsv(0.66,0.05,1)
  
  # the textual annotations
  plot_title <- paste("Co-authorship Network of CCO and SSCO Methods in Pharmacoepidemiology | ",max_date)
  if(max_date <= 1995) {
    plot_title <- paste(plot_title," - We can add a titles which change...")
  }
  
  if(max_date <= 2000) {
    plot_title <- paste(plot_title," - ...overtime, to mark major events...")
  }
  
  if(max_date <= 2005) {
    plot_title <- paste(plot_title," - ...or characterize certain time periods.")
  }
  
  if(max_date <= 2010) {
    plot_title <- paste(plot_title," - cats")
  }
  
    # advertisement
  sub_title <- "First authors: red; Other authors: blue"
  
  par(bg=bgcolor)
  # let's create a PNG and plot the graph onto it
  # 'sprintf()' runs the commands inside of it, but returns it in a string format
  # (%03d, i): if i=1, you will see 001. if i=2, you will see 002. etc.
  
  ############### NTS: REMEMBER TO CHANGE THIS LOCATION EACH TIME U PRINT ##################
  png(sprintf("C:\\Users\\Amy\\Documents\\R_Git\\R_outputs\\testD\\testD%03d.png", i),
  width=5000, height=5000, bg=bgcolor, res=372)
  ###########################################################################################
  
    plot(gx, margin=0, frame=F, main=plot_title, sub=sub_title, 
       vertex.label=NA, edge.color=rgb(.2,.2,.2), edge.arrow.mode=0, 
       layout=layout_g, edge.width=.8)
  text(10,10, label=max_date)
  dev.off() # shuts off current graphic printing device
}


# Let's print! Could take a while... =============================================================================

# let's get it started!
# First pub was in 1992, therefore start_date is 1991 because of the start_date+i argument
# 2013 - 1991 = 22
# Translation: loop the function we just wrote 22 times (start a 1995), to generate a new subgraph each year 
one_year_prev <- 1991
for (i in c(1:22)) {
  message(sprintf("printing year %d", one_year_prev + i))
  create_graph_png(one_year_prev + i, i)
}


# problems: 


######################## DEBUGGING TRIAL SCRAP #############################

# test colours here:
plot(x=1:10, y=rep(5,10), pch=19, cex=3, col=hsv(.7,1,1,alpha=.5))

# notyet bug
V(g)[which(degree(g) > 10)] # degrees work fine
V(g)[which(degree(g) < 1)] # none
