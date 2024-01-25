import networkx as nx
import time

# Create our graph.
G = nx.Graph()

# Add all the nodes.
for line in open("input").readlines():
    k, vv = line.split(": ")
    for v in vv.split():
        # Add edge in both directions per the problem. Capacity is
        # used for minimum cut.
        G.add_edge(k, v, capacity=1.0)
        G.add_edge(v, k, capacity=1.0)


# Use Stoer-Wagner to find the minimum cut.
# https://en.wikipedia.org/wiki/Stoer%E2%80%93Wagner_algorithm
now = time.perf_counter()
(_, partitions) = nx.algorithms.connectivity.stoerwagner.stoer_wagner(G)
print(
    "p1: ",
    len(partitions[0]) * len(partitions[1]),
    f"({time.perf_counter() - now:0.4f}s)",
)

# Alternatively, we can use the minimum cut algorithm and just go
# through all the nodes until we find the one that cuts the graph into
# 3 pieces.
now = time.perf_counter()
for a in G.nodes():
    for b in G.nodes():
        if a == b:
            continue
        cuts, partition = nx.minimum_cut(G, a, b)
        if cuts == 3:
            print(
                "p1-min-cut: ",
                len(partition[0]) * len(partition[1]),
                f"({time.perf_counter() - now:0.4f}s)",
            )
            exit()
