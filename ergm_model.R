# Exponential Random Graph Models (ERGMs)

# install.packages("statnet") # For fitting ERGMs
# install.packages("igraph")  # For network plotting
# install.packages("texreg")  # For printing "nicer" model output

rm(list = ls())

library(statnet)
library(ergm)
options(ergm.loglik.warn_dyads=FALSE) 


messageEdgelist <- read.csv("data/slack_edgelist_adjusted_id.csv")
female_nodes <- read.csv("data/female_nodes.csv")
message <- as.network.matrix(messageEdgelist, 
                             matrix.type = "edgelist", 
                             ignore.eval=FALSE,
                             names.eval='Weight',
                             directed = TRUE)
message

set.vertex.attribute(message, "female",read.csv("data/female_nodes.csv")$female)
message

# Double-check the values for all of the node variables
get.vertex.attribute(advice,"female")

help("ergm-terms",package = "ergm")

summary(message ~ edges)                     # Number of edges (ties)
summary(message ~ mutual)                    # Number of pairs of reciprocated ties
#edges = 965306
#mutual = 482653

# Outdegree distribution
summary(message ~ odegree(0:983))    
# Indegree distribution.
summary(message ~ idegree(0:983))

#TODO: How can these two be the same? 
# One parameter summarizing outdegree distribution - tendency against outdegree hubs
summary(message ~ gwodegree(log(2),fixed=T)) 
#gwodeg.fixed.0.693147180559945 1966

# One parameters summarizing indegree distribution - tendency against indegree hubs
summary(message ~ gwidegree(log(2),fixed=T)) 
#gwideg.fixed.0.693147180559945  1966

## Pairs of nodes with one shared partner, two shared partners, etc.
summary(message ~ desp(1:983))                 
summary(message ~ dgwesp(log(2),fixed = T))  

# Look at Exogenous statistics: terms based on advice ties AND other ties / node attributes
summary(message ~ nodematch("female"))            # Number of ties between people of the same sex
# summary(message ~ nodematch("department"))        # Number of ties between people working in the same department


# The following commands do model estimation for ERGMs.
# This may take a second. Text will print in-console to update you on progress in model estimation.
model1 <- ergm(advice ~ edges                 # This is  a tendency towards a greater number of advice ties existing. Based on a statistic counting the number of ties.
               # Structural patterns
               + mutual                      # This is a tendency towards reciprocity for the advice ties. Based on a statistic counting the number of reciprocated ties.
               + edgecov(hundreds_messages)  # This is the effect of every 100 messages sent from i->j on likelihood of an advice tie. Based on a weighted sum of advice ties x 100s of messages sent
               + nodemix("leader",base = 3)
               # Model constraints
               , constraints =~ bd(maxout=5) # This constraint enforces the maximum outdegree is 5
) 
summary(model1) 
