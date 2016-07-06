# this script documents the steps/logic reasoning behind generating the edgelist for network1.R

############## SET UP (ie. first part of network1.R script, to generate objects we need) ##########################

library(sqldf) # to quote SQL
library(igraph) # to generate network objects
library(reshape2) # to melt
library(plyr) # to rename

# Reads csv as data frame. Contains publications with authors, year, and method
RawPubs <- read.csv("AllPubRaw.csv", header=T, sep=",")

# Edgelist from first author to other authors with year and method data
# See 'eGO//melt_cast.R' for more documentation, or read up on the melt() function.
PubMelt <- melt(RawPubs, id.vars = c("Author1", "Year", "Method")) # Ignore warning message

# Many papers do not have 36 authors. Remove rows with no author data
# Fill blank spaces with NA, then use na.omit
PubMelt[PubMelt == ""] <- NA
PubMelt <- na.omit(PubMelt)

############################################ NEW SCRIPT ##########################################################

# Divide data into two tables: first authors (a1) and 'other' authors (aO)----------------------------------------

# I used SQL because the carpool sample code used SQL. While it's difficult and confusing learn two languages
# simulatenously, SQL produces more elegant and clean results with less code.
# More documentation on SQL synthax in eGO
# SELECT [name of original column] AS [name of new column]
# add a new column named 'type' and fill it with a1

nodes_a1_raw <- sqldf("SELECT Author1 AS author, 'a1' AS type, Method AS method, Year AS year FROM PubMelt")
nodes_aO_raw <- sqldf("SELECT value AS author, 'aO' AS type, Method AS method, Year AS year FROM PubMelt")

# What if an author used both CCO and SSCO? Later on, we'll be deleting all entries beyond the first
# year. Hence, we need to determine which authors have used both method early on. 
# To do this, we'll concatenate values in the 'method' column
# (We don't need to rename 'author' or 'type' columns. In SQL, we don't need to add AS after the expression)
# group_concat() collapses data for the same individual repeated in different rows into the same row.
# THE CODE BELOW IS FAULTY, but run it for learning purposes:
nodes_a1_cm_error <- sqldf("SELECT author, type, GROUP_CONCAT(method) AS method, year AS Year 
                          FROM nodes_a1_raw
                          GROUP BY author")

# However, notice that authors with >1 publications end up with a huge messy chain of methods
# Hence, add the DISTINCT argument to only concatendate different values (i.e. only collapse if both
# CCO and SSCO, not if only one) 
nodes_a1_cm <- sqldf("SELECT author, type, GROUP_CONCAT(DISTINCT method) AS method, year AS Year
                      FROM nodes_a1_raw
                      GROUP BY author")
# Authors who published in both methods are now either CCO,SSCO or SSCO,CCO

# Repeat for aO:
nodes_aO_cm <- sqldf("SELECT author, type, GROUP_CONCAT(DISTINCT method) AS method, year AS Year
                      FROM nodes_aO_raw
                      GROUP BY author")
# at this point, authors who are both aO and a1 should be in both tables

# Create nodes list (i.e. all authors) -----------------------------------------------------------------

# "Union select" is SQL for "the union of set A and set B" (think about probability and set theory).
# We want a table of all nodes, and their type as an attribute. Thus, union select both tables.
# Currently, the author is re-listed every time they publish again. (e.g. see Andrews N)
nodesall <- sqldf("SELECT author, type, method, year FROM nodes_a1_cm 
                  UNION SELECT author, type, method, year FROM nodes_aO_cm")

# Authors who've published as both a1 & aO ------------------------------------------------------------------

# Group because a person can be first author and a collab author.
# THIS CODE IS WRONG. Notice what happens if we don't specify min(year):
nodes <- sqldf("SELECT author, GROUP_CONCAT(type,'') AS type, method, year
                FROM nodes
                GROUP BY author")

# Alright, let's fix it up:
nodes <- sqldf("SELECT author, GROUP_CONCAT(type,'') AS type, method, min(year) AS year
                FROM nodesall
               GROUP BY author")

# Now that we've characterized all authors by type(s) and method(s), we can
# remove nodes beyond their initial entry (ie. first publication)
# GROUP BY allows us to collapse multiple entries. min(year) specifies which which year value to keep.
# for example, you can also do avg(year), etc.
# BELOW CODE IS NOT WHAT WE WHAT. Run it to learn about SQL synthax...
nodes <- sqldf("SELECT author, type, method, min(year) FROM nodes GROUP BY author")

# We want a clean column name. New version of line above, to rename in one step:
nodes <- sqldf("SELECT author, type, method, min(year) AS year FROM nodes GROUP BY author")


# In the final table of nodes, all authors are listed with the year of their first publication in
# the field, along with method and type of author (first author or other author)
# NB: those who've published as both author positions has type "a1aO" or "aOa1"

############## CODE WHICH DOESN'T WORK? ################################

# for learning purposes, let's try a few things with R syntax:

nodes['CCO'] # error: no column name specified
nodes$method['apples'] # N/A (b/c there are none with this method)
nodes$method['CCO'] # still doesn't work????

