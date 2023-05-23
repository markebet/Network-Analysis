library(dplyr)
library(binda)
library(entropy)
library(igraph)
library(network)
library(sna)

## Advice ##
advice_matrix = matrix(c(
  0,	1,	1,	1,	1,	0,	0,	1,	0,	1,	1,	1,	1,	0,	0,	0,	1,	0,	0,	1,
  1,	0,	0,	1,	0,	0,	0,	1,	0,	1,	0,	1,	1,	0,	0,	0,	0,	0,	0,	1,
  1,	0,	0,	0,	1,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,
  1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
  1,	0,	1,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
  1,	1,	0,	0,	0,	0,	0,	0,	0,	1,	0,	1,	1,	0,	0,	0,	0,	0,	0,	1,
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
  0,	1,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,
  1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
  1,	1,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	1,	0,	1,	0,	0,	0,	0,	1,
  1,	1,	0,	0,	0,	0,	0,	1,	0,	0,	0,	1,	0,	0,	1,	0,	0,	0,	0,	1,
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	0,
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
  1,	0,	1,	0,	1,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
  1,	1,	0,	0,	0,  0,	0,	1,	0,	1,	0,	1,	1,	0,	0,	0,	0,	0,	0,	0)
  ,nrow = 20
  ,ncol = 20
  ,byrow = TRUE)

dimnames(advice_matrix) = list(
c("Zafar", "Julianna", "Jackie", "Matthew", "Adam", "Mohamed", "Greg", "Jonathan", 
  "Marko", "Melinda", "Gary", "Tom", "Bob", "John", "Richard", "Alicia", "Zohar",
  "Basil", "Debra", "Tony"),
c("Zafar", "Julianna", "Jackie", "Matthew", "Adam", "Mohamed", "Greg", "Jonathan", 
  "Marko", "Melinda", "Gary", "Tom", "Bob", "John", "Richard", "Alicia", "Zohar",
  "Basil", "Debra", "Tony"))

Advice_two_mode_network = graph.incidence(advice_matrix)
Advice_two_mode_network
plot(Advice_two_mode_network)

Advice_one_mode_network = bipartite.projection(Advice_two_mode_network)
Advice_one_mode_network
get.adjacency(Advice_one_mode_networ$proj1,sparse=FALSE,attr = "weight")
get.adjacency(Advice_one_mode_networ$proj2,sparse=FALSE,attr = "weight")

plot(Advice_one_mode_network$proj1,
     vertex.color = "green",
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertex.label.cex= 0.8,
     layout = layout.kamada.kawai) 

advice_nn = as.network(advice_matrix)
prom = data.frame(deg = degree(advice_nn), btw = betweenness(advice_nn), evc = evcent(advice_nn))
prom
cor(prom)

## Friendship ##
friend_matrix = matrix(c(
  0,	0,	0,	1,	0,	1,	1,	0,	1,	1,	0,	0,	0,	0,	0,	0,	0,	1,	0,	1,
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,
  1,	0,	1,	0,	1,	1,	0,	0,	1,	1,	1,	1,	1,	0,	0,	0,	1,	1,	0,	1,
  0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	1,	1,	1,	0,	0,	0,	1,	0,	0,	0,
  1,	0,	0,	1,	0,	0,	1,	1,	0,	1,	0,	0,	0,	1,	1,	1,	0,	1,	1,	1,
  1,	0,	0,	1,	0,	1,	0,	0,	1,	1,	0,	0,	0,	0,  1,	0,	0,	1,	0,	1,
  0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	0,	0,	0,	1,	0,	1,	0,	1,	1,	0,
  1,	0,	0,	1,	0,	1,	1,	0,	0,	1,	0,	0,	0,	0,	1,	0,	0,	1,	0,	1,
  1,	0,	0,	1,	0,	1,	1,	0,	1,	0,	0,	0,	0,	0,	1,	0,	0,	1,	0,	1,
  0,	0,	1,	1,	1,	0,	0,	0,	0,	0,	0,	1,	1,	0,	0,	0,	1,	0,	0,	0,
  0,	0,	1,	1,	1,	0,	0,	0,	0,	0,	1,	0,	1,	0,	0,	0,	1,	0,	0,	0,
  0,	0,	1,	1,	1,	0,	0,	0,	0,	0,	1,	1,	0,	0,	0,	0,	1,	0,	0,	0,
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	1,	0,
  1,	0,	0,	1,	0,	1,	0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	0,	1,	0,	1,
  0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0,	1,	0,	0,	0,	0,	1,	0,
  0,	0,	1,	1,	0,	0,	0,	0,	0,	0,	1,	0,	1,	0,	0,	0,	0,	0,	0,	0,
  1,	0,	0,	1,	0,	1,	1,	1,	1,	1,	0,	0,	0,	1,	1,	1,	0,	0,	1,	1,
  0,	0,	0,	0,	0,	1,	0,	1,	0,	0,	0,	0,	0,	1,	0,	1,	0,	1,	0,	0,
  1,	0,	0,	1,	0,	1,	1,	0,	1,	1,	0,	0,	0,	0,	1,	0,	0,	1,	0,	0)
  ,nrow = 20
  ,ncol = 20
  ,byrow = TRUE)

dimnames(friend_matrix) = list(
  c("Zafar", "Julianna", "Jackie", "Matthew", "Adam", "Mohamed", "Greg", "Jonathan", 
    "Marko", "Melinda", "Gary", "Tom", "Bob", "John", "Richard", "Alicia", "Zohar",
    "Basil", "Debra", "Tony"),
  c("Zafar", "Julianna", "Jackie", "Matthew", "Adam", "Mohamed", "Greg", "Jonathan", 
    "Marko", "Melinda", "Gary", "Tom", "Bob", "John", "Richard", "Alicia", "Zohar",
    "Basil", "Debra", "Tony"))

Friend_two_mode_network = graph.incidence(friend_matrix)
Friend_two_mode_network
plot(Friend_two_mode_network)

Friend_one_mode_network = bipartite.projection(Friend_two_mode_network)


get.adjacency(Friend_one_mode_network$proj1,sparse=FALSE,attr = "weight")
get.adjacency(Friend_one_mode_network$proj2,sparse=FALSE,attr = "weight")

#Friend Network#
plot(Friend_one_mode_network$proj1,
     vertex.color = "green",
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertex.label.cex= 0.8,
     layout = layout.kamada.kawai)

friend_nn = as.network(friend_matrix)
friend_cen = data.frame(deg = degree(friend_matrix), btw = betweenness(friend_matrix), evc = evcent(friend_matrix))
friend_cen
cor(friend_cen)

## Work ###
work_matrix = matrix(c(
  1,	0,	0,	0,	0,
  0,	1,	0,	0,	0,
  0,	0,	1,	0,	0,
  0,	0,	0,	1,	0,
  0,	0,	1,	0,	0,
  0,	0,	0,	1,	0,
  0,	0,	0,	1,	0,
  0,	1,	0,	0,	0,
  1,	0,	0,	0,	0,
  0,	0,	1,	0,	0,
  0,	0,	0,	0,	1,
  0,	1,	0,	0,	0,
  0,	1,	0,	0,	0,
  0,	0,	0,	0,	1,
  0,	0,	0,	1,	0,
  0,	0,	0,	0,	1,
  0,	0,	1,	0,	0,
  1,	0,	0,	0,	0,
  0,	0,	0,	0,	1,
  1,	0,	0,	0,	0)
  ,nrow = 20
  ,ncol = 5
  ,byrow = TRUE)

dimnames(work_matrix) = list(
  c("Zafar", "Julianna", "Jackie", "Matthew", "Adam", "Mohamed", "Greg", "Jonathan", 
    "Marko", "Melinda", "Gary", "Tom", "Bob", "John", "Richard", "Alicia", "Zohar",
    "Basil", "Debra", "Tony"),
  c("Executive", "Sales", "Analytics", "HR", "Accounting"))

work_matrix

# Work Network #
Work_two_mode_network = graph.incidence(work_matrix)
Work_two_mode_network
plot(Work_two_mode_network)

work_one_mode_network = bipartite.projection(Work_two_mode_network)


get.adjacency(work_one_mode_network$proj1,sparse=FALSE,attr = "weight")
get.adjacency(work_one_mode_network$proj2,sparse=FALSE,attr = "weight")

#Friend Network#
plot(work_one_mode_network$proj1,
     vertex.color = "green",
     vertext.size = 2,
     edge.arrow.size = 0.1,
     vertex.label.cex= 0.8,
     layout = layout.kamada.kawai)
