import os
import networkx as nx
import matplotlib.pyplot as plt
import random
from prettytable import PrettyTable


# Function to read TSP file with coordinates
def read_tsp_file(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()

    coord_section_index = lines.index("NODE_COORD_SECTION\n") + 1

    coordinates = {}
    for line in lines[coord_section_index:]:
        if line.strip() == "EOF":
            break
        parts = line.split()
        node = int(parts[0])
        x, y = map(int, parts[1:])
        coordinates[node] = (x, y)

    return coordinates


# Kruskal's MST algorithm
def kruskal_mst(coordinates):
    vertices = list(coordinates.keys())
    edges = []
    total_weight = 0.0

    # Creating a complete graph with weights as natural numbers
    for u in vertices:
        for v in vertices:
            if u != v:
                euclidean_distance = round(((coordinates[u][0] - coordinates[v][0]) ** 2 +
                                            (coordinates[u][1] - coordinates[v][1]) ** 2) ** 0.5)
                edges.append((u, v, euclidean_distance))

    edges = sorted(edges, key=lambda item: item[2])

    # Performing makeSet(v) procedure
    # For each vertex:
    # parent(v) = v
    # rank(v) = 0
    parent = {vertex: vertex for vertex in vertices}
    rank = {vertex: 0 for vertex in vertices}

    def find(i):
        while parent[i] != i:
            i = parent[i]
        return i

    def union(x, y):
        xroot = find(x)
        yroot = find(y)

        if rank[xroot] < rank[yroot]:
            parent[xroot] = yroot
        elif rank[xroot] > rank[yroot]:
            parent[yroot] = xroot
        else:
            parent[yroot] = xroot
            rank[xroot] += 1

    mst_edges = []
    e = 0
    i = 0

    # We want a MST and we work on an undirected, connected graph
    # That is why |E| = |V| - 1
    while e < len(vertices) - 1:
        u, v, w = edges[i]
        i += 1
        x = find(u)
        y = find(v)

        if x != y:
            e += 1
            mst_edges.append((u, v))
            total_weight += w
            union(x, y)

    return mst_edges, total_weight


# Function to create TSP cycle randomly
def create_random_tsp_cycle(coordinates):
    vertices = coordinates.copy()
    # Get a list of all vertex numbers
    tsp_cycle = list(vertices.keys())
    # Shuffle the list to get a random order
    random.shuffle(tsp_cycle)

    return tsp_cycle


# Function to create TSP cycle using MST edges
def create_tsp_cycle_dfs(mst_edges):
    G = nx.Graph()
    G.add_edges_from(mst_edges)

    visited = set()
    tsp_cycle = []

    def dfs(node):
        visited.add(node)
        for neighbor in G.neighbors(node):
            if neighbor not in visited:
                tsp_cycle.append(neighbor)
                dfs(neighbor)

    # Choose a starting node
    nodes = list(G.nodes())
    start_node = nodes[0]

    # Perform DFS starting from the random node
    dfs(start_node)

    # Add the starting node at the end to complete the cycle
    tsp_cycle.append(start_node)

    return tsp_cycle


# Function to calculate the weight of a TSP cycle
def calculate_tsp_cycle_weight(coordinates, tsp_cycle):
    total_weight = 0.0
    for i in range(len(tsp_cycle) - 1):
        u = tsp_cycle[i]
        v = tsp_cycle[i + 1]
        euclidean_distance = round(((coordinates[u][0] - coordinates[v][0]) ** 2 +
                             (coordinates[u][1] - coordinates[v][1]) ** 2) ** 0.5)
        total_weight += euclidean_distance

    return total_weight


# Function to visualize TSP cycle
def visualize_tsp_cycle(coordinates, tsp_cycle_nodes, path=None):
    G = nx.Graph()

    for node, (x, y) in coordinates.items():
        G.add_node(node, pos=(x, y))

    pos = nx.get_node_attributes(G, 'pos')

    plt.figure(figsize=(16, 8))
    nx.draw(G, pos, with_labels=False, node_size=50,
            font_size=6, font_color='black', font_weight='bold',
            node_color='black', edge_color='red', width=2)

    # Draw TSP cycle edges
    tsp_cycle_edges = [(tsp_cycle_nodes[i], tsp_cycle_nodes[i + 1]) for i in range(len(tsp_cycle_nodes) - 1)]
    tsp_cycle_edges.append((tsp_cycle_nodes[-1], tsp_cycle_nodes[0]))  # Add the last edge to complete the cycle
    nx.draw_networkx_edges(G, pos, edgelist=tsp_cycle_edges, edge_color='green', width=2)

    plt.savefig(path)
    plt.show()


# Function to visualize MSTs
def visualize_mst(coordinates, mst_edges=None, path=None):
    G = nx.Graph()

    for node, (x, y) in coordinates.items():
        G.add_node(node, pos=(x, y))

    for u, v in mst_edges:
        G.add_edge(u, v)

    pos = nx.get_node_attributes(G, 'pos')
    plt.figure(figsize=(12, 8))
    nx.draw(G, pos, with_labels=False, node_size=50,
            font_size=6, font_color='black', font_weight='bold',
            node_color='black', edge_color='red', width=2)

    plt.savefig(path)
    plt.show()


# Function to print a table
def print_table(headers, data):
    table = PrettyTable(headers)
    table.add_rows(data)
    print(table)

# Directories
tsp_directory = '/home/monek/PycharmProjects/AM/L1/data'
mst_directory = '/home/monek/PycharmProjects/AM/L1/plots/MST'
cycles_directory = '/home/monek/PycharmProjects/AM/L1/plots/Cycles'

# Table headers
table_headers = ["Name", "MST Weight", "Cycle Weight"]
table_data = []

# Table headers for ex. 3
headers_ex_3 = ["Name", "Minimum Weight", "Average 50", "Average 10"]
ex_3_data = []

file_names = []
mst_weights = []
tsp_weights = []
min_weights = []
avg_50_weights = []
avg_10_weights = []

for filename in os.listdir(tsp_directory):
    tsp_file_path = os.path.join(tsp_directory, filename)
    coordinates = read_tsp_file(tsp_file_path)

    mst_edges, mst_weight = kruskal_mst(coordinates)

    # Create TSP cycle using MST edges
    tsp_cycle_nodes = create_tsp_cycle_dfs(mst_edges)
    # Calculate the weight of the TSP cycle
    tsp_cycle_weight = calculate_tsp_cycle_weight(coordinates, tsp_cycle_nodes)

    filename_without_extension = os.path.splitext(filename)[0]
    # Add data to the table
    table_data.append([filename_without_extension, mst_weight, tsp_cycle_weight])

    path_mst = os.path.join(mst_directory, f'{filename_without_extension}.jpg')
    visualize_mst(coordinates, mst_edges, path_mst)

    path_cycle = os.path.join(cycles_directory, f'{filename_without_extension}.jpg')
    visualize_tsp_cycle(coordinates, tsp_cycle_nodes, path_cycle)

    minimum_weight = float('inf')
    minimum_weight_10 = float('inf')
    minimum_weight_50 = float('inf')
    avg_10 = 0
    avg_50 = 0
    for i in range(1, 1001):
        inner_tsp_cycle = create_random_tsp_cycle(coordinates)
        inner_tsp_cycle_weight = calculate_tsp_cycle_weight(coordinates, inner_tsp_cycle)
        if inner_tsp_cycle_weight < minimum_weight_10:
            minimum_weight_10 = inner_tsp_cycle_weight
        if inner_tsp_cycle_weight < minimum_weight_50:
            minimum_weight_50 = inner_tsp_cycle_weight
        if inner_tsp_cycle_weight < minimum_weight:
            minimum_weight = inner_tsp_cycle_weight
        if i % 10 == 0:
            avg_10 += minimum_weight_10
            minimum_weight_10 = float('inf')
        if i % 50 == 0:
            avg_50 += minimum_weight_50
            minimum_weight_50 = float('inf')

    avg_10 /= 100
    avg_50 /= 20
    ex_3_data.append([filename_without_extension, minimum_weight, avg_50, avg_10])
    # Preparing a comparison plot
    file_names.append(filename_without_extension)
    mst_weights.append(mst_weight)
    tsp_weights.append(tsp_cycle_weight)
    min_weights.append(minimum_weight)
    avg_50_weights.append(avg_50)
    avg_10_weights.append(avg_10)


print_table(table_headers, table_data)
print_table(headers_ex_3, ex_3_data)

# Plotting
fig, axs = plt.subplots(figsize=(12, 8))
categories = ["MST", "TSP", "MIN", "AVG50", "AVG10"]
# Create line plots for each file
for i, filename in enumerate(file_names):
    axs.plot(categories, [mst_weights[i], tsp_weights[i], min_weights[i], avg_50_weights[i], avg_10_weights[i]], label=filename)

axs.set_xlabel('Categories')
axs.set_ylabel('Weights')

axs.legend()
axs.grid(True)

plt.tight_layout()
comparison_plot_path = os.path.join('/home/monek/PycharmProjects/AM/L1/plots', "comparison_plot.jpg")
plt.savefig(comparison_plot_path)
plt.show()
