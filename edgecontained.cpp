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

    vector<vertexMap> unvisited = {};
    vector<vertexMap> finalMap = {};
    for (int i = 1; i <= n; i++) {
        ullong distance = ULLONG_MAX;
        if (i == k) {
            distance = 0;
        }

        unvisited.push_back({i, distance});
        finalMap.push_back({i, distance});
    }

    while (1) {
        // find vertex with the lowest distance which is still uvisited
        int current = k;
        int position = 0;
        ullong minDist = ULLONG_MAX;
        for (int i = 0; i < (int)unvisited.size(); i++) {
            vertexMap vm = unvisited.at(i);
            if (vm.distance < minDist) {
                minDist = vm.distance;
                current = vm.vert;
                position = i;
            }
        }

        if (current == l) {
            break;
        }

        // remove it from unvisited
        unvisited.erase(unvisited.begin() + position);

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
                for (int i = 0; i < (int)unvisited.size(); i++) {
                    if (other == unvisited.at(i).vert) {
                        unvisited.at(i).distance = newDistance;
                        break;
                    }
                }

                finalMap.at(other - 1).distance = newDistance;
            }

            // edges.erase(e);
        }

        if ((int)unvisited.size() == 0) {
            break;
        }
    }

    cout << "Udaljenost vrhova k i l je " << finalMap.at(l - 1).distance
         << endl;

    return 0;
}
