library(statnet)
library(igraph)
library(sna)
library(numDeriv)
library(ergm)

#Import Data sets#
Communication = read.csv("Communication.csv", header = F)
Influence = read.csv("Influence.csv", header = F)
Issue = read.csv("Issue.csv", header = F)

#Convert to Matrix#
Communication_matrix = as.matrix.data.frame(Communication)
Influence_matrix = as.matrix.data.frame(Influence)
Issue_matrix = as.matrix.data.frame(Issue)

#Convert to Network#
communication_network = graph_from_adjacency_matrix(Communication_matrix)
Influence_network = graph_from_adjacency_matrix(Influence_matrix)
Issue_network = graph_from_adjacency_matrix(Issue_matrix)


#Diameter#
diameter(communication_network, directed = F, weights = NA)
diameter(Influence_network, directed = F, weights = NA)
diameter(Issue_network, directed = F, weights = NA)

#Density#
edge_density(communication_network, loops = F)
edge_density(Influence_network, loops = F)
edge_density(Issue_network, loops = F)

#Reciprocity#
reciprocity(communication_network)
reciprocity(Influence_network)
reciprocity(Issue_network)

#Transitivity#
transitivity(communication_network)
transitivity(Influence_network)
transitivity(Issue_network)

#Gould Fernandez Code#
Node_Attributes = read.csv("Node_Attributes.csv", stringsAsFactors = FALSE, header = T)
PAC = Node_Attributes$PAC 
brokerage(Communication_matrix,PAC)

degree(communication_network)
betweenness(communication_network)

# Stuff #
communication_result = data.frame(Degrees = degree(communication_network), Between = betweenness(communication_network), Eigenvec = evcent(communication_network))
communication_result

influence_result = data.frame(Degrees = degree(Influence_network), Between = betweenness(Influence_network), Eigenvec = evcent(Influence_network))
influence_result

issue_result = data.frame(Degrees = degree(Issue_network), Between = betweenness(Issue_network), Eigenvec = evcent(Issue_network))
issue_result

## Question 4 - Node Analysis ##
Age = Node_Attributes$Age
Coalitions_Count = Node_Attributes$Coalitions_Count

Influence_Network_indegree = degree(Influence_network, cmode='idegree')
cor.test(Influence_Network_indegree, Age)
cor.test(Influence_Network_indegree, Coalitions_Count)
cor.test(Influence_Network_indegree, PAC)

# Per.Cor.test #
network_correlation_test = qaptest((list(Influence_network, communication_network,gcor, g1=1, g2=2, reps=1000)))
summary(network_correlation_test)
plot(network_correlation_test)



## Question 5 / ERGM ##
Infl_small_Network = as.network(Influence_network[1:50,1:50])
Com_small_Network = as.network(communication_network[1:50,1:50])
Iss_small_Network = as.network(Issue_network[1:50,1:50])
PAC_small = PAC[1:50]
Age_small = Age[1:50]

## Question 6 - Do they represent the larger networks ##
set.seed(11111)
plot.network(Infl_small_Network)
BinNomNet50  = as.network(matrix(rbinom(2500,1,gden(Infl_small_Network)), nr=50, nc=50))
plot.network(BinNomNet50)

# ERGM Small ##
m1 = ergm(Com_small_Network ~ edges)
m2 = ergm(Com_small_Network ~ edges+mutual) 
m3 = ergm(Com_small_Network ~ edges+asymmetric)
m4 = ergm(Com_small_Network ~ edges+mutual+asymmetric)
m5 = ergm(Com_small_Network ~ edges+mutual+twopath)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)

### GOF Tests ###
gof_m1 = gof(m1)
gof_m1
plot(gof_m1)

gof_m2 = gof(m2)
plot(gof_m2)
gof_m2

gof_m3 = gof(m3)
gof_m3
plot(gof_m3)

gof_m4 = gof(m4)
gof_m4
plot(gof_m4)

gof_m1 = gof(m1)
plot(gof_m1)


## 13 - ERGM Smaller Communications Network ##
m6 = ergm(Com_small_Network ~ edges+mutual+edgecov(Iss_small_Network))
m6
summary(m6)


## 14 ##
Issue_Valued = read.csv("IssueValued.csv", header = F)
Issue_Valued_Matrix = as.matrix(Issue_Valued)
Iss_Valued_small = Issue_Valued_Matrix[1:50,1:50]
Iss_small_Network %e% "Iss_Valued_small" = Iss_Valued_small

m7 = ergm(Com_small_Network ~ edges+mutual+edgecov(Iss_small_Network, attrname = "Iss_Valued_small"))
m7
summary(m7)

## 15 and 16 ##
Com_small_Network%v%"Age" = Age
Com_small_Network%v%"PAC" = PAC

m8 = ergm(Com_small_Network ~ edges+mutual+edgecov(Iss_small_Network, attrname = "Iss_Valued_small")+nodecov("Age")+nodecov("PAC"))
m8
summary(m8)

