#pragma once

struct Vertex {
    std::pair<int, int> vertex_coordinates;
    int id;
};

struct Edge {
    // Pair of vertices' ids
    std::pair<int, int> edge_coordinates;
    int edge_weight;
};