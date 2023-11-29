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