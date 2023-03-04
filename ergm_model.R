# Exponential Random Graph Models (ERGMs)

# install.packages("statnet") # For fitting ERGMs
# install.packages("igraph")  # For network plotting
# install.packages("texreg")  # For printing "nicer" model output

rm(list = ls())
list.files()

library(statnet)
library(ergm)
library(sna)
library(network)


options(ergm.loglik.warn_dyads=FALSE) 


messageEdgelist <- read.csv("data/slack_edgelist_adjusted_id.csv")
female_nodes <- read.csv("data/female_nodes.csv")
message <- as.network.matrix(messageEdgelist, 
                             matrix.type = "edgelist", 
                             ignore.eval=TRUE,
                             directed = TRUE)
message

set.vertex.attribute(message, "female",read.csv("data/female_nodes.csv")$female)
# message%v%"female" <- female_nodes
message

# Double-check the values for all of the node/edge variables
# get.edge.value(message, "Weight")
get.vertex.attribute(message,"female")

help("ergm-terms",package = "ergm")

# ------------------------------------------------------------------------------
summary(message ~ edges)
summary(message ~ mutual)         # Number of pairs of reciprocated ties
#edges = 17377
#mutual = 8371

network.density(message) 
#0.01800155

sum(has.edges(message))
#603 True

#Extract nonisolated Nodes 
non_isolated_nodes <- which(has.edges(message))

#Extract subgraph from nonisolated nodes
non_isolated_messages <- get.inducedSubgraph(message, v = non_isolated_nodes)
non_isolated_messages

summary(non_isolated_messages ~ edges)

# ------------------------------------------------------------------------------
# Outdegree distribution
summary(message ~ odegree(0:983))    
# Indegree distribution.
summary(message ~ idegree(0:983))

# -------------------------------------------------------------------------------

# One parameter summarizing outdegree distribution - tendency against outdegree hubs
summary(message ~ gwodegree(log(2),fixed=T)) 
# gwodeg.fixed.0.693147180559945 
# 1104.868

# One parameters summarizing indegree distribution - tendency against indegree hubs
summary(message ~ gwidegree(log(2),fixed=T)) 
# gwideg.fixed.0.693147180559945 
# 1148.351 

# -------------------------------------------------------------------------------
## Pairs of nodes with one shared partner, two shared partners, etc.
summary(message ~ desp(1:983))
# > summary(message ~ desp(1:983))                 
# esp.OTP1   esp.OTP2   esp.OTP3   esp.OTP4   esp.OTP5   esp.OTP6   esp.OTP7   esp.OTP8   esp.OTP9 
# 413        464        590        630        696        714        744        765        752 
# esp.OTP10  esp.OTP11  esp.OTP12  esp.OTP13  esp.OTP14  esp.OTP15  esp.OTP16  esp.OTP17  esp.OTP18 
# 853        793        774        772        716        666        642        546        612 
# esp.OTP19  esp.OTP20  esp.OTP21  esp.OTP22  esp.OTP23  esp.OTP24  esp.OTP25  esp.OTP26  esp.OTP27 
# 537        440        419        406        362        338        293        282        229 
# esp.OTP28  esp.OTP29  esp.OTP30  esp.OTP31  esp.OTP32  esp.OTP33  esp.OTP34  esp.OTP35  esp.OTP36 
# 229        212        187        148        161        117        105         81         60 
# ...

#gwesp.OTP.fixed.0.693147180559945 
#31191.23
summary(message ~ dgwesp(log(2),fixed = T))  
# ------------------------------------------------------------------------------

# Look at Exogenous statistics: terms based on advice ties AND other ties / node attributes
# Number of ties between people of the same sex
summary(message ~ nodematch("female"))            
# 8636

#Number of isolated networks (node not connecting to any other node)
isolates(message, diag=FALSE)


component_dist(message, connected="strong")
component_dist(message, connected="weak")
# Number of ties between people working in the same department
# summary(message ~ nodematch("department"))  

# ------------------------------------------------------------------------------
# The following commands do model estimation for ERGMs.
# This may take a second. Text will print in-console to update you on progress in model estimation.
# model1 <- ergm(message ~ edges                 # This is  a tendency towards a greater number of advice ties existing. Based on a statistic counting the number of ties.
#                # Structural patterns
#                + mutual                      # This is a tendency towards reciprocity for the advice ties. Based on a statistic counting the number of reciprocated ties.
#                # + edgecov(hundreds_messages)  # This is the effect of every 100 messages sent from i->j on likelihood of an advice tie. Based on a weighted sum of advice ties x 100s of messages sent
#                # + nodemix("leader",base = 3)
#                # # Model constraints
#                # , constraints =~ bd(maxout=5) # This constraint enforces the maximum outdegree is 5
# ) 
# summary(model1) 

# Iteration 8 of at most 60:
#ergm(formula = message ~ edges + mutual)

#Monte Carlo Maximum Likelihood Results:
  
# Estimate Std. Error MCMC % z value Pr(>|z|)    
# edges  -7.30721    0.03916      0  -186.6   <1e-04 ***
#   mutual 10.57737    0.07927      0   133.4   <1e-04 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Null Deviance: 1338198  on 965306  degrees of freedom
# Residual Deviance:   95062  on 965304  degrees of freedom
# 
# AIC: 95066  BIC: 95090  (Smaller is better. MC Std. Err. = 19.46)

model2 <- ergm(message ~ edges
               + mutual
               # + gwidegree(log(2), fixed = T)                 # Inverted preferential attachment (indegree)
               # + gwodegree(2, fixed = T, cutoff = 5)              # Inverted preferential attachment (outdegree)
               # + dgwesp(log(2), type = "OTP", fixed = T, cutoff =5)    # A modified version of Outgoing Two Path(i->j + i->k->j) structures. Geometrically weighted version of transitivity
               # Node attribute effects
               + nodematch("female")) 
               # Control settings for MCMC-MLE algorithm
               # , control = control.ergm(MCMC.effectiveSize = 50))
summary(model2) 
# Warning message:
#   In ergm_MCMC_sample(s, control, theta = mcmc.init, verbose = max(verbose -  :
# Unable to reach target effective size in iterations alotted.
# # 
# Monte Carlo Maximum Likelihood Results:
#   
#   Estimate Std. Error MCMC %  z value Pr(>|z|)    
# edges            -7.301332   0.040858      0 -178.698   <1e-04 ***
#   mutual           10.576813   0.080814      0  130.879   <1e-04 ***
#   nodematch.female -0.009155   0.011545      0   -0.793    0.428    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Null Deviance: 1338198  on 965306  degrees of freedom
# Residual Deviance:   94964  on 965303  degrees of freedom
# 
# AIC: 94970  BIC: 95005  (Smaller is better. MC Std. Err. = 20.53)
library(texreg)
screenreg(list("model2"=model2))

pdf('model2diagnostics.pdf')              # Open a pdf file to save to
mcmc.diagnostics(model2) # Run the markov chain monte carlo diagnostics
dev.off()                # Closes and saves the pdf



model3 <- ergm(message ~ edges
               + mutual
               + gwidegree(2, fixed = T))

summary(model3) 
screenreg(list("model3"=model3))

pdf('model3diagnostics.pdf')              # Open a pdf file to save to
mcmc.diagnostics(model2) # Run the markov chain monte carlo diagnostics
dev.off()                # Closes and saves the pdf

# -------------------------------------------------------------------------------------------------
# Test the goodness of fit of the model
# Compiles statistics for these simulations as well as the observed network, and calculates p-values 
# -------------------------------------------------------------------------------------------------
# Model 2:
# It may take a second for this command to run.
gof2 <- gof(model2, verbose=T, burnin=1e+5, interval=1e+5, control = control.gof.ergm(nsim = 200))
dev.off()
plot(gof2)
gof2

