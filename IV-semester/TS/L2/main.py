from random import randint
import networkx as nx
import matplotlib.pyplot as plt
from functools import reduce
import dijkstar as ds
from copy import deepcopy
from random import random

"""
G = [[0] * 20 for _ in range(20)]

# creating nodes with at least one edge
for i in range(0, 20):
    j = i
    while j == i:
        j = randint(0, 19)
    G[i][j] = 1

# creating the rest of the edges

for _ in range(9):
    i = randint(0, 19)
    j = randint(0, 19)
    G[i][j] = 1

# making the matrix symmetric
for i in range(0, 20):
    for j in range(0, 20):
        if G[i][j] == 1:
            G[j][i] = 1
            
# emptying lower triangle of the matrix
for i in range(0, 20):
    j = 0
    while j < i:
        G[i][j] = 0
        j += 1

# creating a graph and adding 20 nodes
G_nx = nx.Graph()
G_nx.add_nodes_from(list(range(0,20)))

# adding edges to the graph
for i in range(0, 20):
    row = G[i]
    for j in range(0, 20):
        if row[j] == 1:
            G_nx.add_edge(i, j)

# writing the created matrix into a file
with open("test-matrix.txt", "w+") as f:
    for i in range(0, 20):
        row = G[i]
        for j in range(0,20):
            f.write(str(row[j]))
        f.write('\n')

# drawing the graph
# nx.draw(G_nx)

# reading the graph from the file

NODES_COUNT = 20
G = []
with open("test-matrix.txt", "r") as f:
    line = f.readline()
    while line:
        G.append(
            list(map(
                lambda x: int(x),
                list(line)[:NODES_COUNT]
                )
            ))
        line = f.readline()

G_nx = nx.Graph()
G_nx.add_nodes_from(list(range(0, NODES_COUNT)))

# creating an image of the graph
for i in range(0, NODES_COUNT):
    row = G[i]
    for j in range(0, NODES_COUNT):
        if row[j] == 1:
            G_nx.add_edge(i, j)

pos = nx.spring_layout(G_nx)
nx.draw_networkx_nodes(G_nx, pos, node_color="white")
nx.draw_networkx_labels(G_nx, pos)
nx.draw_networkx_edges(G_nx, pos)
plt.show()
"""
# efficiency in Gbps of one edge
EFFICIENCY_DEFAULT = 2 * (10 ** 9)
# default network packet size
PACKET_SIZE = 4096
# maximum and minimum capacity of packets traversing one edge
MAX_CAPACITY = 1000
MIN_CAPACITY = 100

NODES_COUNT = 20

# read the graph from file
G = []
with open("topology-matrix.txt", "r") as f:
    line = f.readline()
    while line:
        G.append(
            list(map(
                lambda x: int(x),
                list(line)[:NODES_COUNT]
                )
            ))
        line = f.readline()


# creating the intensity matrix
def create_intensity_matrix():
    N = [[0] * NODES_COUNT for _ in range(NODES_COUNT)]
    for i in range(0, NODES_COUNT):
        j = i+1
        while j < NODES_COUNT:
            # generate intensity
            N[i][j] = randint(MIN_CAPACITY, MAX_CAPACITY)
            j += 1
    return N


# generate the base N
N = create_intensity_matrix()


class NetworkOverload(Exception):
    pass


# measuring the reliability of a network
class Reliability(object):
    # initializing the topology and its copy for tests
    def __init__(self, G):
        self._network_original = G
        self._G = deepcopy(G)

    def _reset(self):
        self._G = deepcopy(self._network_original)

    # using dijkstar to find the shortest path in the graph
    def _prepare_for_testing(self):
        self._G_ds = ds.Graph(undirected=True)
        for i in range(0, NODES_COUNT):
            row = self._G[i]
            for j in range(0, NODES_COUNT):
                if row[j] == 1:
                    self._G_ds.add_edge(i, j, 1)
        # creating the "c" matrix where every number is the same
        self._efficiency = [[0] * NODES_COUNT for _ in range(NODES_COUNT)]
        for i in range(0, NODES_COUNT):
            for j in range(0, NODES_COUNT):
                if self._G[i][j] == 1:
                    self._efficiency[i][j] = EFFICIENCY_DEFAULT
                    self._efficiency[j][i] = EFFICIENCY_DEFAULT

    # creating the "a" function based on intensity matrix
    def _create_flow_function(self, N):
        self._flow = [[0] * NODES_COUNT for _ in range(NODES_COUNT)]
        for i in range(0, NODES_COUNT):
            j = i + 1
            while j < NODES_COUNT:
                intensity = N[i][j]
                # creating list of paths between all nodes
                nodes = ds.find_path(self._G_ds, i, j).nodes
                for k in range(0, len(nodes) - 1):
                    current = nodes[k]
                    next = nodes[k + 1]
                    self._flow[current][next] += intensity
                    self._flow[next][current] += intensity
                    # making sure a(e) < c(e) for each edge
                    if self._efficiency[current][next] < self._flow[current][next] * PACKET_SIZE:
                        raise NetworkOverload()
                j += 1

    # calculating T
    def avg_delay(self, N):
        T = 0
        for i in range(0, NODES_COUNT):
            j = i + 1
            while j < NODES_COUNT:
                if self._G[i][j] == 1:
                    # using the given equation: T = T + (a(e) / (c(e) / m - a(e)))
                    T += self._flow[i][j] / ((self._efficiency[i][j] / PACKET_SIZE) - self._flow[i][j])
                j += 1
        N_sum = reduce(lambda so_far, curr: so_far + sum(curr), N, 0)
        # finally dividing it by G
        T /= N_sum
        return T

    # function responsible for measuring the reliability of a given network
    def measure(self, intensity, edge_probability, T_max):
        successes = 0
        network_overloads = 0
        nopath_errors = 0
        timeouts = 0
        total_count = 500
        # repeating the test a lot
        for _ in range(0, total_count):
            # perhaps damaging some edges
            for i in range(0, NODES_COUNT):
                for j in range(0, NODES_COUNT):
                    if self._G[i][j] == 1 and random() >= edge_probability:
                        self._G[i][j] = 0

            # creating the shortest paths and adding "c" function
            self._prepare_for_testing()
            # creating the "a" function
            try:
                self._create_flow_function(intensity)
            except ds.NoPathError:
                # an important edge has been destroyed, hence resetting the topology
                self._reset()
                nopath_errors += 1
                continue
            except NetworkOverload:
                # the connection has been overloaded and is unable to sustain traffic
                network_overloads += 1
                self._reset()
                continue

            # comparing T to T_max
            T = self.avg_delay(N)
            if T < T_max:
                successes += 1
            else:
                timeouts += 1
            self._reset()

        return successes / total_count, network_overloads, nopath_errors, timeouts


rel = Reliability(G)

reliability, network_overloads, no_path_errors, timeouts = rel.measure(N, 0.99, 0.001)

print("reliability      :", reliability)
print("network overloads:", network_overloads)
print("nopath errors    :", no_path_errors)
print("timeouts         :", timeouts)
