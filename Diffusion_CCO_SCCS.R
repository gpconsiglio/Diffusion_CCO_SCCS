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

############################################ graph.R ##############################################################

# generate an igraph object, using our edge list and vertice data
g <- graph.data.frame(edges, directed = T, vertices = nodes)

# Apply the Fruchterman-Reingold layout
layout_g <- layout.fruchterman.reingold(g)

create_graph_png <- function(max_date,i) {
  
  # Define the subgraphs (i.e. new subgraph for each year)
  gx <- subgraph.edges(g,edges[which(edges$year <= max_date), 'id'], 
                       delete.vertices=F)

  # Specify default vertex colour
  V(gx)$color <- "black"
  V(gx)$frame.color <- "black"
  
  # Colour first authors pink
  col_a1 <- rgb( .9, .0, .4, alpha=.4) 
  V(gx)[which(nodes$type == "a1")]$color <- col_a1
  V(gx)[which(nodes$type == "a1")]$frame.color <- col_a1
 
  # Colour other authors blue
  col_aO <- rgb( .1, .6, 1 , alpha=.4)
  V(gx)[which(nodes$type == "aO")]$color <- col_aO
  V(gx)[which(nodes$type == "aO")]$frame.color <- col_aO

  # Authors who've published in both positions are purple
  col_hybrid <- rgb( .4, .3, 1, alpha=.4)
  V(gx)[which(nodes$type == "a1aO" | nodes$type == "aOa1")]$color <- col_hybrid
  V(gx)[which(nodes$type == "a1aO" | nodes$type == "aOa1")]$frame.color <- col_hybrid
  
  # Node grows with the number of attached edges logarithmically
  V(gx)$size <- log(3*degree(gx))
  
  # Author nodes which haven't published yet in a given year are made transparent
  notyet <- hsv(1,1,1,alpha=0)
  V(gx)[which(degree(gx) < 1)]$frame.color=notyet 
  V(gx)[which(degree(gx) < 1)]$color=notyet 
  V(gx)[which(degree(gx) < 1)]$size=0
  
  # Plot background colour
  bgcolor <- hsv(0.66,0.05,1)
  
  # the textual annotations
  plot_title <- paste("Co-authorship Network of CCO and SSCO Methods in Pharmacoepidemiology | ")
  if(max_date < 2013) {
    plot_title <- paste(plot_title, max_date)
  }
  
  if(max_date >= 2013) {
    plot_title <- paste(plot_title, "2013")
  }
  
  # Legend
  sub_title <- "PINK = First authors || BLUE = Other authors || PURPLE: Publishd as both"
  
  par(bg=bgcolor)
  
  # Specifies where to print to! CHOOSE A LOCAL DIRECTORY ON YOUR COMPUTER :-)
  png(sprintf("C:\\Users\\Amy\\Documents\\R_Git\\R_outputs\\testI\\testI%02d.png", i),
  width=5000, height=5000, bg=bgcolor, res=372)
 
  # Some other miscellaneous plot specifications
    plot(gx, margin=0, frame=F, main=plot_title, sub=sub_title, 
       vertex.label=NA, edge.color=rgb(.2,.2,.2), edge.arrow.mode=0, 
       layout=layout_g, edge.width=.8)
  text(10,10, label=max_date)
  
  # Shuts off the graphic printing device
  dev.off()
}


# Let's run it!
# Note: even though 2013 - 1991 = 22, I'm running the loop 25 times to the animation will "extend"
# how long we can view the final 2013 graph for.
one_year_prev <- 1991
for (i in c(1:25)) {
  message(sprintf("printing year %d", one_year_prev + i))
  create_graph_png(one_year_prev + i, i)
}
