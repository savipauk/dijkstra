#include <climits>
#include <cmath>
#include <iostream>
#include <vector>

using namespace std;

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

    vector<vector<uint64_t>> adjacency (n, vector<uint64_t>(n, 0));
    for (int i = 1; i <= n; i++) {
        vector<uint64_t> row (n, 0);
        for (int j = 1; j <= n; j++) {
            uint64_t distance = pow(a, abs(i - j) % c) + b * pow(i - j, 2) - 1;
            row.at(j - 1) = distance;
        }
        adjacency.at(i - 1) = row;
    }

    vector<uint64_t> distances (n, ULLONG_MAX);
    distances.at(k - 1) = 0;
    vector<bool> visits (n, false);

    while (1) {
        int current = 0;
        uint64_t minDist = ULLONG_MAX;
        for (int i = 0; i < n; i++) {
            if (visits.at(i)) {
                continue;
            }

            if (distances.at(i) < minDist) {
                minDist = distances.at(i);
                current = i + 1;
            }
        }
    
        if (current == l) {
            break;
        }

        visits.at(current - 1) = true;

        vector<uint64_t> row = adjacency.at(current - 1);
        for (int i = 0; i < n; i++) {
            if (i == current - 1) {
                continue;
            }
            
            uint64_t newDistance = minDist + row.at(i);
            if (newDistance < distances.at(i)) {
                distances.at(i) = newDistance;
            }
        }
    }

    cout << "Udaljenost vrhova k i l je " << distances.at(l - 1) << endl;

    return 0;
}
