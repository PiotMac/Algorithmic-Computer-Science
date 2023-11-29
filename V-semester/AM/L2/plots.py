import csv
import os
import networkx as nx
import matplotlib.pyplot as plt


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


# Function to visualize MSTs
def visualize_cycle(coordinates, edges=None, path=None):
    G = nx.Graph()

    for node, (x, y) in coordinates.items():
        G.add_node(node, pos=(x, y))

    for u, v in edges:
        G.add_edge(u, v)

    pos = nx.get_node_attributes(G, 'pos')
    plt.figure(figsize=(12, 8))
    nx.draw(G, pos, with_labels=False, node_size=50,
            font_size=6, font_color='black', font_weight='bold',
            node_color='black', edge_color='red', width=2)

    plt.savefig(path)
    plt.show()


# Directories
tsp_directory = '/home/monek/PycharmProjects/AM/L2/data'
mst_directory = '/home/monek/PycharmProjects/AM/L2/plots/MST'
cycles_data_directory = '/home/monek/PycharmProjects/AM/L2/csv/cycles'
cycles_directory = '/home/monek/PycharmProjects/AM/L2/plots/Cycles'

for filename in os.listdir(tsp_directory):
    tsp_file_path = os.path.join(tsp_directory, filename)
    coordinates = read_tsp_file(tsp_file_path)
    no_of_points = len(coordinates)

    base_name, current_extension = os.path.splitext(filename)
    filename_csv = base_name + ".csv"
    cycle_csv_edges = os.path.join(cycles_data_directory, filename_csv)
    # Read edges from CSV file
    edges = []
    with open(cycle_csv_edges, 'r') as csvfile:
        csvreader = csv.reader(csvfile, delimiter=';')
        next(csvreader)  # Skip header
        for row in csvreader:
            start_vertex, end_vertex = map(int, row)
            edges.append((start_vertex, end_vertex))

    filename_png = base_name + ".png"
    visualize_cycle(coordinates, edges, filename_png)
