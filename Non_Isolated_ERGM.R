# Exponential Random Graph Models on Nonisolated Network (ERGMs)

# install.packages("statnet") # For fitting ERGMs
# install.packages("igraph")  # For network plotting
# install.packages("texreg")  # For printing "nicer" model output

rm(list = ls())
list.files()

library(statnet)
library(ergm)
library(texreg)
# library(network)
# library(sna)

options(ergm.loglik.warn_dyads=FALSE) 


messageEdgelist <- read.csv("data/slack_edgelist_adjusted_id.csv")
female_nodes <- read.csv("data/female_nodes.csv")
message <- as.network.matrix(messageEdgelist, 
                             matrix.type = "edgelist", 
                             ignore.eval=TRUE,
                             directed = TRUE)
message

set.vertex.attribute(message, "female",read.csv("data/female_nodes.csv")$female)
message

# Double-check the values for all of the node/edge variables
# get.edge.value(message, "Weight")
get.vertex.attribute(message,"female")

help("ergm-terms",package = "ergm")

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

network.density(non_isolated_messages)
#0.04786973

# Outdegree distribution
summary(non_isolated_messages ~ odegree(0:604))   
# Indegree distribution
summary(non_isolated_messages ~ idegree(0:983))

# Look at Exogenous statistics: terms based on advice ties AND other ties / node attributes
# Number of ties between people of the same sex
summary(non_isolated_messages ~ nodematch("female"))            
# 8636 (should be the same as full network with isolated nodes, and is the same)


# ------------------------------------------------------------------------------
model1 <- ergm(non_isolated_messages ~ edges
               + mutual
)
summary(model1)

screenreg(list("model1"=model1))

pdf('non_iso_model1diagnostics.pdf')              # Open a pdf file to save to
mcmc.diagnostics(model1) # Run the markov chain monte carlo diagnostics
dev.off()                # Closes and saves the pdf

# ergm(formula = non_isolated_messages ~ edges + mutual)
# 
# Monte Carlo Maximum Likelihood Results:
#   
#   Estimate Std. Error MCMC % z value Pr(>|z|)    
# edges  -6.29585    0.03957      0  -159.1   <1e-04 ***
#   mutual  9.56674    0.07969      0   120.1   <1e-04 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Null Deviance: 503233  on 363006  degrees of freedom
# Residual Deviance:  77125  on 363004  degrees of freedom
# 
# AIC: 77129  BIC: 77151  (Smaller is better. MC Std. Err. = 21.01)


# ============================
#   model1       
# -----------------------------
#   edges               -6.30 ***
#   (0.04)   
# mutual               9.57 ***
#   (0.08)   
# -----------------------------
#   AIC              77129.50    
# BIC              77151.10    
# Log Likelihood  -38562.75    
# =============================
#   *** p < 0.001; ** p < 0.01; * p < 0.05
# ------------------------------------------------------------------------------

model2 <- ergm(non_isolated_messages ~ edges
               + mutual
               + nodematch("female")) 
summary(model2)


screenreg(list("model2"=model2))

pdf('non_iso_model2diagnostics.pdf')              # Open a pdf file to save to
mcmc.diagnostics(model2) # Run the markov chain monte carlo diagnostics
dev.off()                # Closes and saves the pdf

# all:
#   ergm(formula = non_isolated_messages ~ edges + mutual + nodematch("female"))
# 
# Monte Carlo Maximum Likelihood Results:
#   
#   Estimate Std. Error MCMC %  z value Pr(>|z|)    
# edges            -6.293426   0.038994      0 -161.397   <1e-04 ***
#   mutual            9.564151   0.077325      0  123.688   <1e-04 ***
#   nodematch.female -0.004674   0.011840      0   -0.395    0.693    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Null Deviance: 503233  on 363006  degrees of freedom
# Residual Deviance:  77150  on 363003  degrees of freedom
# 
# AIC: 77156  BIC: 77188  (Smaller is better. MC Std. Err. = 23.97)
# 
# ===============================
#   model2       
# -------------------------------
#   edges                 -6.29 ***
#   (0.04)   
# mutual                 9.56 ***
#   (0.08)   
# nodematch.female      -0.00    
# (0.01)   
# -------------------------------
#   AIC                77155.78    
# BIC                77188.19    
# Log Likelihood    -38574.89    
# ===============================
#   *** p < 0.001; ** p < 0.01; * p < 0.05


# ------------------------------------------------------------------------------
model3 <- ergm(non_isolated_messages ~ edges
               + mutual
               + gwidegree(log(2), fixed = T))

summary(model3) 
screenreg(list("model3"=model3))

pdf('non_iso_model3diagnostics.pdf')              # Open a pdf file to save to
mcmc.diagnostics(model3) # Run the markov chain monte carlo diagnostics
dev.off()                # Closes and saves the pdf