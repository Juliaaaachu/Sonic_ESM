# Exponential Random Graph Models (ERGMs)

# install.packages("statnet") # For fitting ERGMs
# install.packages("igraph")  # For network plotting
# install.packages("texreg")  # For printing "nicer" model output

library(statnet)
options(ergm.loglik.warn_dyads=FALSE) 

messageEdgelist <- read.csv("slack_message_edgelist.csv")
message <- as.network.matrix(messageEdgelist, matrix.type = "edgelist")
message
