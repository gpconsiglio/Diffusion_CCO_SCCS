#######################################################################################
# [AL] SAMPLE CODE FROM http://www.joyofdata.de/blog/r-code-for-igraph-animation/     #
#                                                                                     #
# [AL] Be careful: the majority of this code is essentially redundant data-cleaning,  #
# [AL] specific for the author's situation. Good reading for demonstrating 'locking'  #
# [AL] nodes in place. This is done by calculating layout for graphy as a whole, then #
# [AL] calculating subgraphs by date, and stitching into video using ffmpeg.          #
#                                                                                     #
# NB: Comments NOT marked by [AL] are by original author. Also compare with online    # 
# to see which comments are by the author himself.                                    #
#######################################################################################

# [AL] This code uses the sqldf library, which performs SQL selections on R dataframes
library('sqldf')

library('igraph')

# [AL] The original csv file not available. 
# [AL] Dummy table was generated for demo purposes.
# [AL] 3 columns in CSV: Driver's ID (drv), passenger's ID (pas), date of ride (d)
links <- read.table("carpool_dummydata2.csv", 
                    header=T, sep=",")

################# [AL] DATA CLEANING (applicable but not vital for CG projects) ################

# 1. ID's for the drivers (drv) & date (d) of his earlieast offered ride
# 2. Passengers ID (pas) & date (d) of earliest participation
# this list defines the possible nodes
# [AL] First two arguments creates two data frames
# [AL] Because igraph requires data frames to generate graph
# [AL] In plain English, this line tells us to create 'links' as a data frame
# [AL] with drv = the drv column in 'links', pas = the 'pas' column in 'links',
# [AL] and d as the 'd' column. Note that the strings in original table would've been imported
# [AL] as factors, so we need to change its type to a date.
links <- data.frame(drv = links$drv, pas = links$pas, d = as.Date(links$d))

# extract the drvs/pass and the first date of participation

# [AL] Quote SQL as string using sqldf. Translation: select the 'pas' column & rename it 'user_id'
# [AL] Note that 'p' was never in original data. This SQL line adds a new column called 'type'
# [AL] and fills it with 'p' (to further understand, run and view the table in the Environment tab)
# [AL] The 'group by' part is SQL, and allows us to collapse multiple passengers into one entry.
# [AL] We need to thus specify the min(d) (i.e. use the earliest date).
# [AL] If min(d) was not specified, the resulting value would actually be different!

nodes_pas <- sqldf("select pas as user_id, 'p' as type, min(d) as date 
                   from links group by user_id")

# [AL] For learning purposes, compare 'links' with 'nodes_pas'. You can do this from the Environment
# [AL] tab in RStudio on the right. Note that we've gone from data containing rides and dates
# [AL] to a list of passengers with an assigned ID, associated type 'p', and date of ride.

# [AL] Now for the drivers! Same structure as before.
# [AL] Since pas and drv have same ID in my dummy data, I modified original code by adding 'drv*1000'
# [AL] to differentiate driver 1 from passenger 1.

nodes_drv <- sqldf("select drv as user_id, 'd' as type, min(d) as date 
                   from links group by user_id")

# cast the date into a date relative to Jan 1 1970
# [AL] This is good coding practice (if interested, Google "Unix Epoch")
# [AL] If uninterested/low on time, just skip these lines. :-)
nodes_pas$date <- as.Date(nodes_pas$date, origin="1970-01-01")
nodes_pas$date <- as.Date(nodes_pas$date, origin="1970-01-01")

# union of passenger- and driver-nodes
# [AL] "Union select" is SQL for "the union of set A and set B" (recall probability notations).
# [AL] Translation: Need to define and store all nodes - thus, select all drivers and passengers.
nodes <- sqldf("select user_id, type, date from nodes_pas 
               union select user_id, type, date from nodes_drv")

# group by ID because a person can be pas and a drv!
# then the type is "pd" or "dp"
# [AL] CG PARALLEL: author is both first author in one and coauthor in other
nodes <- sqldf("select user_id, group_concat(type,'') as type, 
               min(date) as date from nodes group by user_id")

# adding a gaplessly incrementing ID and again cast to date
# (either I was drunk or it makes sense and I forgot why ...)
# [AL] As the code author admits: these lines are redundant & don't really make sense.
# [AL] Translation: create column, create a column named 'i' & number all rows, starting at 1 
# [AL] The data frame now has a few 'sub-categories' and incorporates previous cleaning.
# [AL] For the overall picture, this is a flawed line of code - just ignore it...
nodes <- data.frame(i = 1:length(nodes$user_id), user_id = nodes$user_id, 
                    type=nodes$type, date=as.Date(nodes$date, origin="1970-01-01"))

# compute the edges using the new neater IDs
# [AL] This is a SQL join function. (Read up on this; this is a concept-heavy yet useful function)
link_list <- sqldf("SELECT n_drv.i AS drv_i, n_pas.i AS pas_i, l.d AS date
                    FROM links l JOIN nodes n_drv
                    ON l.drv = n_drv.user_id
                    JOIN nodes n_pas ON l.pas = n_pas.user_id")

###### END OF DATA CLEANING #################################################################################
###### NOTE: For the dummy data, link_list is same as original csv, since no data cleaning was necessary ####

# and now we create the (i)graph - undirected and using Fruchterman-
# Reingold for positioning. Looks like no big deal but took about 8 hours
# on my notebook for the big animation.
# [AL] Translation: create an igraph object named 'g' where the edge list is 'link_list'
# [AL] Edges not direct, vertices are the cleaned-out 'nodes' df from above.
g <- graph.data.frame(link_list, directed=F, vertices=nodes)

# [AL] IMPORTANT: notice that the layout algorithm is calculated for g (i.e. the overall picture)
layout_g <- layout.fruchterman.reingold(g)

# [AL] I added the following lines to actually plot g and see if everything works.
# [AL] If you open link_list from the Environment tab, you'll see 10 rows,
# [AL] but the graph has 12 nodes. This is why nodes 10, 11, 12 are isolated, since they don't actually
# [AL] appear on the edge list.
plot(g)

# IDs for the edges
# [AL] The author chose to generate numbers as IDs, by numbering all drivers starting at 1
link_list$i <- 1:length(link_list$drv_i)
# [AL] Open 'link_list' again. There is a new column 'i' added.

# this function creates a PNG with the graph for every day
# [AL] This entire chunk between the curly brackets serves to define a new function.
# [AL] Read up more on this R feature.

# [AL] This R function has two arguments: max_date, and i.
# [AL] TIP: as you're combing through all the logic, don't forget that max_date and i are both variables
# [AL] to be manipulated. Think of it as functions in calculus, but with more useful/complex manipulations.

create_graph_png <- function(max_date,i) {
  
  # gives the for until 'max_date' relevant edges
  # [AL] The author probably missed a word with his above comment...
  # [AL] Structure of function: subgraph.edges(graph, eids, delete.vertices = TRUE)
  # [AL] Translation: make a subgraph of 'g', where the edge list is entries in 'link_list' which happened
  # [AL] before the maximum date. (<= is less than/equal to; which() is an R structure)
  # [AL] Definitions: subgraph gives part of a graph, but will only contain the specified vertices & edges.
  # [AL] Why add the 'i' argument? 'link_list' is a data frame. To subscript it (as we do here), the column
  # [AL] must also be specified. We want the column to be user defineable.
  gx <- subgraph.edges(g,
                       link_list[which(link_list$date <= max_date),'i'], 
                       delete.vertices=F)
  
  # [AL] in R, you need to specify both the vertex fill and vertex frame colour:
  V(gx)$color="black"
  V(gx)$frame.color="black"
  
  # [AL] HSV (hue saturation value): easy to vary intensity of a set colour
  # [AL] in programming, alpha is usually transparency
  col_driver <- hsv(0,1,1,alpha=.5)
  
  # [AL] Subscript (i.e. select) from 'gx'. We only want certain values, therefore use 'which()' function;
  # [AL] So which nodes do we want? We want nodes where type is 'd' (aka driver)
  # [AL] Also, use == since it's a test. Use = to set a variable to a value
  # [AL] We set colour equal to col=driver (defined above)
  V(gx)[which(nodes$type == "d")]$color=col_driver
  # [AL] And repeat for frame colour. 
  V(gx)[which(nodes$type == "d")]$frame.color=col_driver
  
  # [AL] Repeat above for passenger node colours.
  col_passenger <- hsv(0.66,1,1,alpha=.5)
  V(gx)[which(nodes$type == "p")]$color=col_passenger
  V(gx)[which(nodes$type == "p")]$frame.color=col_passenger
  
  # [AL] And repeat again for hybrids
  col_hybrid <- hsv(.7,1,1,alpha=.5)
  V(gx)[which(nodes$type == "pd" | nodes$type == "dp")]$color=col_hybrid
  V(gx)[which(nodes$type == "pd" | nodes$type == "dp")]$frame.color=col_hybrid
  
  # the size of a node grows with the number of attached edges
  V(gx)$size <- degree(gx)^(1/3)
  
  # takes care of the new ones being emphasized by bright color
  # and a changing size
  newones <- hsv(.36,1,1,alpha=.5)
  
  # [AL] Goal: We want to make nodes glow in the colour as defined by 'new ones'
  # [AL] if node date - max_date (i.e. the input variable of the function) is younger than 5. 
  # [AL] The result is of the type "date". To make sure R understands our imputs, turn this into a number.
  # [AL] We select those where the absolute value of this date (as a number) is less than 5
  # [AL] Then repeat for the frame colour
  V(gx)[which(abs(as.numeric(nodes$date - max_date)) < 5)]$color=newones
  V(gx)[which(abs(as.numeric(nodes$date - max_date)) < 5)]$frame.color=newones
  
  # [AL] Similar logic. Make size 2.5 when node appears, after an age of 1 turn it to size 1.6
  V(gx)[which(abs(as.numeric(nodes$date - max_date)) < 3)]$size=1.6
  V(gx)[which(abs(as.numeric(nodes$date - max_date)) < 1)]$size=2.5
  
  #V(gx)[which(abs(as.numeric(nodes$date - max_date)) < 3)]$size=3
  #V(gx)[which(abs(as.numeric(nodes$date - max_date)) < 2)]$size=5
  #V(gx)[which(abs(as.numeric(nodes$date - max_date)) < 1)]$size=3
  # [AL] I have no idea why he marked that code as a comment....I suppose he changed his mind about the
  # [AL] code and forgot to edit it out before publishing on his blog
  
  # get's rid of the not yet to be displayed nodes
  # [AL] this line is a little odd. technically, these nodes shouldn't have been drawn in anyway
  # [AL] try taking these lines out and plotting again
  notyet <- hsv(1,1,1,alpha=0)
  
  V(gx)[which(degree(gx) < 1)]$frame.color=notyet 
  V(gx)[which(degree(gx) < 1)]$color=notyet 
  
  V(gx)[which(degree(gx) < 1)]$size=0
  
  bgcolor <- hsv(0.66,0.05,1)
  
  # the textual annotations
  plot_title <- paste("Please tell me this script works./ ",max_date)
  if(max_date >= as.Date("2012-03-06")) {
    plot_title <- paste(plot_title," - Add a new title!")
  }
  if(max_date >= as.Date("2012-08-09")) {
    plot_title <- paste(plot_title," - Start of a new era.")
  }
  if(max_date >= as.Date("2012-11-05")) {
    plot_title <- paste(plot_title," - Another major change!")
  }
  if(max_date >= as.Date("2013-01-24")) {
    plot_title <- paste(plot_title," - Another major change! Cats.")
  }
  
  # advertisement
  sub_title <- "Hello world! This is a test."
  
  par(bg=bgcolor)
  # let's create a PNG and plot the graph onto it
  # [AL] 'sprintf()' is a wrapper for the C function sprintf, which returns a character vector containing
  # [AL] a formatted combination of text and variable values
  png(sprintf("C:\\Users\\Amy\\Documents\\R\\carpool\\TestD\\TestD%03d.png", i)
      ,width=5000, height=5000, bg=bgcolor, res=372)
  plot(gx, margin=0, frame=F, main=plot_title, sub=sub_title, 
       vertex.label=NA, edge.color=rgb(.2,.2,.2), edge.arrow.mode=0, 
       layout=layout_g, edge.width=.8)
  text(10,10, label=max_date)
  dev.off() # shuts off current graphic printing device
}

# let's get it started!
# [AL] Run the function that we wrote above, starting at 2011-03-31
# [AL] Run loop 64 times (I chose 64 since it is slightly more than 3 months)
# [AL] + i to the input date: i.e. loop the function we just wrote 64 times, to generate a
# [AL] new subgraph for each new day. 
start_date <- as.Date("2011-01-01")
for (i in c(1:64)) {
  create_graph_png(start_date + i, i)
}