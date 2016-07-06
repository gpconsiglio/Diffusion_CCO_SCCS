library(sqldf)
library(igraph)
library(reshape2)

# Reads csv as data frame. Contains publications with authors, year, and method
# Table was saved as a comma-delimited .csv file, hence the sep="," argument.
RawPubs <- read.csv("AllPubRaw.csv", header=T, sep=",")

# What is the melt() function? DEMO ==================================================================================

# Melt df, with variable ID as authors 1-9
# we need to choose an identification variable, which must be a string
  # sprint() prints a string, which id.vars needs
  # %d, [number] tells the program to replace %d with the number after the comma
    # This is why we need the au_n vector
      # allows us to select authors from 1-9 without typing repeatedly
au_n = c(1:9)
PubMeltEx <- melt(RawPubs, id.vars = c(sprintf("Author%d", au_n)))

#################### CG data test ##############################

# Edgelist from first author to other authors with year and method data
PubMelt <- melt(RawPubs, id.vars = c("Author1", "Year", "Method"))

# Many papers do not have 36 authors. Remove rows with no author data

# Change all blanks to NA, a unique type recognized by R
PubMelt[PubMelt == ""] <- NA
# We can now use the na.omit function to omit all NA
PubMelt <- na.omit(PubMelt)

# StackOverflow solutions which should work but didn't...
# PubMelt <- PubMelt[-which(PubMeltRaw$variable == "")]  #error
# PubMelt <- PubMelt[,complete.cases(PubMeltBlanks)]  #error
# PubMelt <- PubMelt[!(PubMeltBlanks$variable == "")]  #error
