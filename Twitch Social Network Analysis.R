install.packages("igraph")

library(igraph)
library(dplyr)

edges <- read.csv("C://Users//lenovo//OneDrive//Desktop//Project intro to social//large_twitch_edges.csv")
features <- read.csv("C://Users//lenovo//OneDrive//Desktop//Project intro to social//large_twitch_features.csv")

sum(is.na(edges))  # Total missing values
colSums(is.na(edges))  # Missing values per column

edges <- edges[!duplicated(edges), ]  # Remove exact duplicates

sum(is.na(features))  # Total missing values
colSums(is.na(features))  # Missing values per column

table(features$language)  # Inspect language distribution
table(features$mature)  # Check values in 'mature' (e.g., should be binary or NA)

# Create a subset of edges for visualization
subset_edges <- edges[1:100, ]
graph_subset <- graph_from_data_frame(subset_edges, directed = FALSE)

# Plot the subset graph
plot(graph_subset, vertex.size = 5, vertex.label = NA, main = "Sample Subgraph")

# Select a few features to summarize
features_summary <- features %>% 
  select(views, language) %>% 
  head(10)

print(features_summary)

hist(features$views, breaks = 50, main = "Distribution of Views", xlab = "Views")

barplot(sort(table(features$language), decreasing = TRUE), las = 2, main = "Language Distribution")

graph <- graph_from_data_frame(edges, directed = FALSE)

# Sample a subset of nodes from the graph
sampled_nodes <- sample(V(graph), 10000)  # Adjust the number of nodes as needed

# Create a subgraph with the sampled nodes
subgraph <- induced_subgraph(graph, sampled_nodes)

# Plot the subgraph
plot(subgraph, 
     vertex.size = log(V(subgraph) + 1),  # Scale size by centrality
     vertex.label = NA, 
     edge.arrow.size = 0.5, 
     main = "Sampled Subgraph of 10000 Nodes"
)


degree_values <- degree(graph)

head(degree_values)  # View the degree values of the first few nodes
summary(degree_values)  # Summary statistics of the degree values

max_degree_node <- which.max(degree_values)
print(V(graph)[max_degree_node]$name)  # Name of the node with the highest degree

min_degree_node <- which.min(degree_values)
print(V(graph)[min_degree_node]$name)  # Name of the node with the lowest degree

hist(degree_values, breaks = 50, main = "Degree Distribution", xlab = "Degree", col = "skyblue", border = "white")

degree_freq <- table(degree_values)
barplot(degree_freq, main = "Degree Frequency Distribution", xlab = "Degree", ylab = "Frequency", col = "lightgreen")


components <- components(graph)
print(components$csize) # Sizes of connected components

avg_degree <- mean(degree_values)
print(avg_degree)

subgraph <- induced_subgraph(graph, sample(V(graph), 1000))
betweenness <- betweenness(subgraph)
closeness <- closeness(subgraph)

head(betweenness)  # View top betweenness centrality values
head(closeness)    # View top closeness centrality values

max_betweenness_node <- which.max(betweenness)
max_closeness_node <- which.max(closeness)
print(V(subgraph)[max_betweenness_node]$name)  # Node with max betweenness
print(V(subgraph)[max_closeness_node]$name)   # Node with max closeness

plot(
  subgraph,
  vertex.size = log(betweenness + 1) * 5,  # Scale size by centrality
  vertex.color = ifelse(betweenness > quantile(betweenness, 0.75), "red", "blue"),
  vertex.label = NA, main = "Betweenness Centrality"
)

# Check for any non-finite values in closeness
sum(!is.finite(closeness))  # Count non-finite values (e.g., Inf, NaN)

closeness[!is.finite(closeness)] <- 0  # Replace non-finite values (Inf, NaN) with 0

quantile_closeness <- quantile(closeness, 0.75, na.rm = TRUE)

plot(
  subgraph,
  vertex.size = log(closeness + 1) * 5,  # Scale size by centrality
  vertex.color = ifelse(closeness > quantile_closeness, "green", "blue"),
  vertex.label = NA, main = "Closeness Centrality"
)

betweenness_rank <- sort(betweenness, decreasing = TRUE)
closeness_rank <- sort(closeness, decreasing = TRUE)
print(head(betweenness_rank))  # Top nodes by betweenness
print(head(closeness_rank))    # Top nodes by closeness


# Map features to nodes
V(graph)$views <- features$views[match(V(graph)$name, features$numeric_id)]
V(graph)$language <- features$language[match(V(graph)$name, features$numeric_id)]
print(vertex_attr(graph))

summary(V(graph)$views)
table(V(graph)$language)


components <- components(graph)
print(components$csize) # Sizes of connected components


# Sample a smaller subgraph for testing
subgraph <- induced_subgraph(graph, sample(V(graph), 1000))  # Adjust the size to your needs

# Run Louvain on the smaller subgraph
communities_subgraph <- cluster_louvain(subgraph)
plot(communities_subgraph, subgraph)

# Function to generate a Barabási-Albert (scale-free) network
generate_scale_free_graph <- function(n, m) {
  # n = number of nodes, m = number of edges to attach from a new node to existing nodes
  graph <- barabasi.game(n, m = m, directed = FALSE)
  plot(graph, main = paste("Barabási-Albert Model (m =", m, ")"))
  return(graph)
}

# Example usage
scale_free_graph <- generate_scale_free_graph(100, 3)

# Function to generate a bipartite graph
generate_bipartite_graph <- function(m, n, p) {
  # m = number of nodes in set 1, n = number of nodes in set 2, p = probability of edge creation
  graph <- sample_bipartite(m, n, p = p, directed = FALSE)
  plot(graph, main = "Bipartite Graph")
  return(graph)
}

# Example: Split the graph's nodes into two sets based on degree
set1_nodes <- V(graph)[degree(graph) < median(degree(graph))]
set2_nodes <- V(graph)[degree(graph) >= median(degree(graph))]

# Select a subset of nodes from each set (e.g., 10 nodes from each set)
set1_subset <- sample(set1_nodes, 10)  # Select 10 nodes from set1
set2_subset <- sample(set2_nodes, 10)  # Select 10 nodes from set2

# Create a subgraph induced by the selected subset of nodes
bipartite_subgraph_subset <- induced_subgraph(graph, c(set1_subset, set2_subset))

# Plot the subset of the bipartite graph
plot(bipartite_subgraph_subset, main = "Subset of Bipartite Graph")

# Convert graph to adjacency matrix
adj_matrix <- as_adjacency_matrix(graph)
print(adj_matrix)

# Convert graph to adjacency list (without 'mode' argument)
adj_list <- as_adj(graph)
print(adj_list)


