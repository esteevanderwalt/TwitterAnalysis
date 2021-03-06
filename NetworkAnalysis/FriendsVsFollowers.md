# Network Analysis - Friends vs Followers







In mathematics, and more specifically in graph theory, a graph is a structure amounting to a set of objects in which some pairs of the objects are in some sense "related." The objects correspond to mathematical abstractions called vertices (also called nodes or points) and each of the related pairs of vertices is called an edge (also called an arc or line).Typically, a graph is depicted in diagrammatic form as a set of dots for the vertices, joined by lines or curves for the edges. Graphs are one of the objects of study in discrete mathematics.

The edges may be directed or undirected. For example, if the vertices represent people at a party, and there is an edge between two people if they shake hands, then this graph is undirected because any person A can shake hands with a person B only if B also shakes hands with A. In contrast, if any edge from a person A to a person B corresponds to A's admiring B, then this graph is directed, because admiration is not necessarily reciprocated. The former type of graph is called an undirected graph and the edges are called undirected edges while the latter type of graph is called a directed graph and the edges are called directed edges.

Most networks are defined as one-mode networks with one set of nodes that are similar to each other. However, several networks are in fact two-mode networks (also known as affiliation or bipartite networks; Borgatti and Everett, 1997; Latapy et al., 2008). These networks are a particular kind, with two different sets of nodes, and ties existing only between nodes belonging to different sets. A distinction is often made between the two node sets based on which set is considered more responsible for tie creation (primary or top node set) than the other (secondary or bottom node set).

We will use Gephi as main tool for this part of the analysis.

## First a plain hashtag graph from Gephi that was run for about 2 minutes on 'school', 'homework' tweets

![Hashtag Graph] (network_twitter_hashtags_20160919.png)

## Friends graph
Import the following into Gephi
- list of nodes (users)
- list of edges (who their friends are)

Weight the nodes by amount of friends

Here is the results from Gephi:

Here is a similar result but now on a world map:

## Followers graph
Import the following into Gephi
- list of nodes (users)
- list of edges (who they follow)

Weight the nodes by amount of followers

Here is the results from Gephi:

Here is a similar result but now on a world map:

## Investigate only tweets who actually replied to another (ie try to eliminate bots)
Import the following into Gephi
- list of nodes (users)
- list of edges (who they responded to)

Weight the nodes by amount of responses

Here is the results from Gephi:

Here is a similar result but now on a world map:
