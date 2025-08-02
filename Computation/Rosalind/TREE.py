import networkx as nx
import matplotlib.pyplot as plt

with open("./data/rosalind_tree.txt", "r") as f:
    lines = f.readlines()
    total_nodes = int(lines[0].strip())

    edge_list = []
    for edge in lines[1:]:
        edge_list.append(list(map(int, edge.split())))

G = nx.Graph()

for n in range(1, total_nodes + 1):
    G.add_node(n)

print(edge_list)
G.add_edges_from(edge_list)

print(nx.number_connected_components(G) - 1)

fig, ax = plt.subplots()
fig.set_size_inches(50, 50)
nx.draw(G, ax=ax)
fig.savefig("TREE.pdf")
