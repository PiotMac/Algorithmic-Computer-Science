#include "graph.hpp"
using namespace std;
// Helper function to calculate Euclidean distance between two points
int calculate_distance(const std::pair<int, int>& point1, const std::pair<int, int>& point2) {
    return round(sqrt(pow(point1.first - point2.first, 2) + pow(point1.second - point2.second, 2)));
}

// Helper function to calculate the weight of a TSP cycle
int calculate_tsp_cycle_weight(std::map<int, std::pair<int, int>>& coordinates, std::vector<int> tsp_cycle, int** costs) {
    int total_weight = 0;
    
    for (int i = 0; i < tsp_cycle.size() - 1; i++) {
        int u = tsp_cycle[i];
        int v = tsp_cycle[i + 1];
        total_weight += costs[u - 1][v - 1];
    }

    return total_weight + costs[0][tsp_cycle.size() - 1];
}

// Function to create TSP cycle using MST edges and randomly chosen first node
vector<int> create_tsp_cycle_dfs(const vector<Edge>& mst_edges, int random_vertex_number) {

    set<int> visited;
    vector<int> tsp_cycle;

    // Choose a starting node
    int start_node = random_vertex_number;

    tsp_cycle.push_back(start_node);

    function<void(int)> dfs = [&](int node_id) {
        visited.insert(node_id);

        set<int> neighbours;
        for (const auto& edge : mst_edges) {
            if (edge.edge_coordinates.first == node_id) {
                neighbours.insert(edge.edge_coordinates.second);
            } else if (edge.edge_coordinates.second == node_id) {
                neighbours.insert(edge.edge_coordinates.first);
            }
        }

        for (int neighbour : neighbours) {
            if (visited.find(neighbour) == visited.end()) {
                tsp_cycle.push_back(neighbour);
                dfs(neighbour);
            }
        }
    };

    // Perform DFS starting from the random node
    dfs(start_node);

    return tsp_cycle;
}

int find(std::map<int, int>& parent, int i) {
    while (parent[i] != i) {
        i = parent[i];
    }
    return i;
}

void union_set(std::map<int, int>& parent, std::map<int, int>& rank, int x, int y) {
    int xroot = find(parent, x);
    int yroot = find(parent, y);

    if (rank[xroot] < rank[yroot]) {
        parent[xroot] = yroot;
    } else if (rank[xroot] > rank[yroot]) {
        parent[yroot] = xroot;
    } else {
        parent[yroot] = xroot;
        rank[xroot] += 1;
    }
}

// Kruskal's MST algorithm
pair<vector<Edge>, int> kruskal_mst(map<int, pair<int, int>>& coordinates) {
    vector<Vertex> vertices;
    vector<Edge> edges;
    int total_weight = 0;

    for (const auto& kvp : coordinates) {
        int vertex_id = kvp.first;
        std::pair<int, int> coords = kvp.second;
        Vertex new_vertex{coords, vertex_id};
        vertices.push_back(new_vertex);
    }

    for (const Vertex& u : vertices) {
        for (const Vertex& v : vertices) {
            if (u.id != v.id) {
                int euclidean_distance = calculate_distance(u.vertex_coordinates, v.vertex_coordinates);
                Edge newly_created_edge{{u.id, v.id}, euclidean_distance};
                edges.push_back(newly_created_edge);
            }
        }
    }

    sort(edges.begin(), edges.end(), [&](const auto& a, const auto& b) {
        return a.edge_weight < b.edge_weight;
    });

    std::map<int, int> parents;
    std::map<int, int> ranks;

    for (const Vertex& vertex : vertices) {
        parents[vertex.id] = vertex.id;
        ranks[vertex.id] = 0;
    }

    vector<Edge> mst_edges;
    int e = 0;
    int i = 0;

    while (e < vertices.size() - 1) {
        int u, v, weight;
        tie(u, v, weight) = make_tuple(edges[i].edge_coordinates.first, edges[i].edge_coordinates.second, edges[i].edge_weight);

        i++;

        int x = find(parents, u);
        int y = find(parents, v);

        if (x != y) {
            e++;
            Edge new_edge{{u, v}, weight};
            mst_edges.push_back(new_edge);
            total_weight += weight;
            union_set(parents, ranks, x, y);
        }
    }

    return {mst_edges, total_weight};
}

// Local Search Algorithm
pair<int, int> local_search_random(map<int, pair<int, int>>& coordinates, vector<int>& current_cycle, int** costs, std::mt19937 gen) {
    int current_value = calculate_tsp_cycle_weight(coordinates, current_cycle, costs);

    int no_of_steps = 0;
    bool wasBetterCycleFound;
    const int size_of_current_cycle = static_cast<int>(current_cycle.size());

    std::uniform_int_distribution<size_t> dist(0, size_of_current_cycle - 2);

     // Generate neighbours (invert moves)
    while (true) {
        int smallestFoundWeightChange = 0;
        int best_i_index, best_j_index = 0;
        wasBetterCycleFound = false;

        for (int k = 0; k < size_of_current_cycle; k++) {
            int i = dist(gen);
            int j = dist(gen);
            if(i > j) {
                int temp;
                temp = i;
                i = j;
                j = temp;
            }
            
            int currentlyFoundWeightChange = 0;

            currentlyFoundWeightChange = - costs[current_cycle[i] - 1][current_cycle[i + 1] - 1]
                                         - costs[current_cycle[j] - 1][current_cycle[j + 1] - 1]
                                         + costs[current_cycle[i] - 1][current_cycle[j] - 1]
                                         + costs[current_cycle[i + 1] - 1][current_cycle[j + 1] - 1];

            if(currentlyFoundWeightChange < smallestFoundWeightChange) {
                smallestFoundWeightChange = currentlyFoundWeightChange;
                wasBetterCycleFound = true;
                best_i_index = i;
                best_j_index = j;
            }
        }

        if (smallestFoundWeightChange == 0) {
            return {current_value, no_of_steps};
        }

        no_of_steps++;
        reverse(current_cycle.begin() + best_i_index + 1, current_cycle.begin() + best_j_index + 1);
        current_value = calculate_tsp_cycle_weight(coordinates, current_cycle, costs);
    }

    // Return the final solution value and the number of steps
    return {current_value, no_of_steps};
}

#include <iostream>
#include "graph.hpp"

using namespace std;

// Function to print vertices and edges to a CSV file
void print_cycle_to_csv(const std::map<int, std::pair<int, int>>& coordinates, const std::vector<int>& cycle, const std::string& filename) {
    std::ofstream file(filename);
    file << "Vertex ID, X Coordinate, Y Coordinate\n";
    for (const auto& kvp : coordinates) {
        int vertex_id = kvp.first;
        std::pair<int, int> coords = kvp.second;
        file << vertex_id << ";" << coords.first << ";" << coords.second << "\n";
    }
    file << "Start Vertex; End Vertex\n";
    for (int i = 0; i < cycle.size() - 1; i++) {
        file << cycle[i] << ";" << cycle[i + 1] << endl;
    }
    file << cycle[cycle.size() - 1] << ";" << cycle[0] << endl;
    file.close();
}

// Function to print vertices and edges to a CSV file
void print_mst_to_csv(const std::map<int, std::pair<int, int>>& coordinates, const std::vector<Edge>& edges, const std::string& filename) {
    std::ofstream file(filename);
    file << "Vertex ID, X Coordinate, Y Coordinate\n";
    for (const auto& kvp : coordinates) {
        int vertex_id = kvp.first;
        std::pair<int, int> coords = kvp.second;
        file << vertex_id << ";" << coords.first << ";" << coords.second << "\n";
    }
    file << "Start Vertex; End Vertex\n";
    for (const Edge& edge : edges) {
        std::pair<int, int> coords = edge.edge_coordinates;
        file << coords.first << ";" << coords.second  << "\n";
    }
    file.close();
}

// Function to read TSP file with coordinates
map<int, pair<int, int>> read_tsp_file(const string& file_path) {
    ifstream file(file_path);
    map<int, pair<int, int>> coordinates;

    string line;
    bool coord_section = false;

    while (getline(file, line)) {
        if (line == "NODE_COORD_SECTION\r") {
            coord_section = true;
            continue;
        } else if (coord_section && line == "EOF\r") {
            break;
        }

        if (coord_section) {
            int node;
            int x, y;
            istringstream iss(line);
            iss >> node >> x >> y;

            coordinates[node] = make_pair(x, y);
        }
    }

    return coordinates;
}