#include <algorithm>
#include <climits>
#include <cmath>
#include <iostream>
#include <vector>

typedef unsigned long long ullong;

using namespace std;

struct edge {
    int vertex1;
    int vertex2;
    ullong weight;

    bool operator==(const edge &other) const {
        return (weight == other.weight) &&
               ((vertex1 == other.vertex1 && vertex2 == other.vertex2) ||
                (vertex1 == other.vertex2 && vertex2 == other.vertex1));
    }
};

struct vertexMap {
    int vert;
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

    vector<edge> edges = {};
    for (int i = 1; i <= n; i++) {
        for (int j = i + 1; j <= n; j++) {
            ullong weight = pow(a, abs(i - j) % c) + b * pow(i - j, 2) - 1;

            edge e = {i, j, weight};
            edges.push_back(e);
        }
    }

    vector<vertexMap> finalMap = {};
    for (int i = 1; i <= n; i++) {
        ullong distance = ULLONG_MAX;
        if (i == k) {
            distance = 0;
        }

        finalMap.push_back({i, distance, false});
    }

    while (1) {
        // find vertex with the lowest distance which is still uvisited
        int current = k;
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

        finalMap.at(position).visited = true;

        if (current == l) {
            break;
        }

        // note its neighbours (all others)
        for (vector<edge>::iterator e = edges.end() - 1; e != edges.begin() - 1;
             e--) {

            int other = -1;
            if (e->vertex1 == current) {
                other = e->vertex2;
            }

            if (e->vertex2 == current) {
                other = e->vertex1;
            }

            if (other == -1) {
                continue;
            }

            ullong newDistance = minDist + e->weight;
            if (newDistance < finalMap.at(other - 1).distance) {
                finalMap.at(other - 1).distance = newDistance;
            }

            // edges.erase(e);
        }

    }

    cout << "Udaljenost vrhova k i l je " << finalMap.at(l - 1).distance
         << endl;

    return 0;
}
