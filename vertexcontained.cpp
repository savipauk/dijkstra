#include <climits>
#include <cmath>
#include <iostream>
#include <vector>

typedef unsigned long long ullong;

using namespace std;

struct edge {
    int connectsTo;
    ullong distance;
};

struct vertex {
    int id;
    vector<edge> edges;
};

struct vertexMap {
    vertex vert;
    ullong distance;
    bool visited;
};

int main() {
    int n, a, b, c, k, l;
    cout << "Unesite prirodan broj n: ";
    cin >> n;
    cout << "Unesite prirodan broj a: ";
    cin >> a;
    cout << "Unesite prirodan broj b: ";
    cin >> b;
    cout << "Unesite prirodan broj c: ";
    cin >> c;

    do {
        cout << "Unesite vrh k: ";
        cin >> k;
    } while (k > n);

    do {
        cout << "Unesite vrh l: ";
        cin >> l;
    } while (l > n || l == k);

    vector<vertex> vertices = {};
    for (int i = 1; i <= n; i++) {
        vector<edge> connectsTo = {};
        for (int j = i + 1; j <= n; j++) {
            ullong distance = pow(a, abs(i - j) % c) + b * pow(i - j, 2) - 1;
            connectsTo.push_back({j, distance});
        }

        vertices.push_back({i, connectsTo});
    }

    vector<vertexMap> finalMap = {};
    for (vertex i : vertices) {
        ullong distance = ULLONG_MAX;
        if (i.id == k) {
            distance = 0;
        }

        finalMap.push_back({i, distance, false});
    }

    while (1) {
        vertex current = vertices.at(0);
        int position = 0;
        ullong minDist = ULLONG_MAX;
        for (int i = 0; i < (int)finalMap.size(); i++) {
            vertexMap vm = finalMap.at(i);

            if (vm.visited) {
                continue;
            }

            if (vm.distance < minDist) {
                minDist = vm.distance;
                current = vm.vert;
                position = i;
            }
        }

        if (current.id == l) {
            break;
        }

        finalMap.at(position).visited = true;

        for (edge e : current.edges) {
            // distanca do finalMap.at(v.edge.connectsTo) = v.edge.distance
            int other = e.connectsTo;

            ullong newDistance = minDist + e.distance;
            if (newDistance < finalMap.at(other - 1).distance) {
                finalMap.at(other - 1).distance = newDistance;
            }
        }
    }

    cout << "Udaljenost vrhova k i l je " << finalMap.at(l - 1).distance
         << endl;

    return 0;
}
