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

# Double-check the values for all of the node/edge variables
get.edge.value(message, "Weight")
get.vertex.attribute(message,"female")

help("ergm-terms",package = "ergm")

# ------------------------------------------------------------------------------
summary(message ~ edges)
summary(message ~ mutual)         # Number of pairs of reciprocated ties
#edges = 17377
#mutual = 8371

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
# 8635

# Number of ties between people working in the same department
# summary(message ~ nodematch("department"))  

# ------------------------------------------------------------------------------
# The following commands do model estimation for ERGMs.
# This may take a second. Text will print in-console to update you on progress in model estimation.
model1 <- ergm(message ~ edges                 # This is  a tendency towards a greater number of advice ties existing. Based on a statistic counting the number of ties.
               # Structural patterns
               + mutual                      # This is a tendency towards reciprocity for the advice ties. Based on a statistic counting the number of reciprocated ties.
               # + edgecov(hundreds_messages)  # This is the effect of every 100 messages sent from i->j on likelihood of an advice tie. Based on a weighted sum of advice ties x 100s of messages sent
               # + nodemix("leader",base = 3)
               # # Model constraints
               # , constraints =~ bd(maxout=5) # This constraint enforces the maximum outdegree is 5
) 
summary(model1) 

# Iteration 10 of at most 60:
#   Optimizing with step length 1.0000.
# The log-likelihood improved by 0.0036.
# Convergence test p-value: 0.0022. Converged with 99% confidence.
# Finished MCMLE.
# Evaluating log-likelihood at the estimate. Fitting the dyad-independent submodel...
# Bridging between the dyad-independent submodel and the full model...
# Setting up bridge sampling...
# Using 16 bridges: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 .
# Bridging finished.
# This model was fit using MCMC.  To examine model diagnostics and check for degeneracy, use the mcmc.diagnostics()
# function.
# Warning message:
#   In ergm_MCMC_sample(s, control, theta = mcmc.init, verbose = max(verbose -  :
#    Unable to reach target effective size in iterations alotted.
#    > summary(model1) 
#    Call:
#     ergm(formula = message ~ edges + mutual)
#     Monte Carlo Maximum Likelihood Results:
#                                                                      
#       Estimate Std. Error MCMC % z value Pr(>|z|)    
#edges  -7.30907    0.04163      0  -175.6   <1e-04 ***
#mutual 10.58330    0.08416      0   125.7   <1e-04 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#                                                                    
# Null Deviance: 1338198  on 965306  degrees of freedom
# Residual Deviance:   95047  on 965304  degrees of freedom

#  AIC: 95051  BIC: 95075  (Smaller is better. MC Std. Err. = 17.15)


model2 <- ergm(message ~ 
                 mutual
               + gwidegree(log(2), fixed = T)                 # Inverted preferential attachment (indegree)
               + gwodegree(2, fixed = T, cutoff = 5)              # Inverted preferential attachment (outdegree)
               + dgwesp(log(2), type = "OTP", fixed = T, cutoff =5)    # A modified version of Outgoing Two Path(i->j + i->k->j) structures. Geometrically weighted version of transitivity
               # Node attribute effects
               + nodematch("female") 
               # Control settings for MCMC-MLE algorithm
               , control = control.ergm(MCMC.effectiveSize = 50))
summary(model2) 
