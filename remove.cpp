#include <iostream>
#include <vector>

using namespace std;

int main() {
    vector<int> vct = {-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11};

    for (vector<int>::iterator i = vct.begin(); i != vct.end(); i++) {
        cout << *i << " ";
    }

    cout << endl;

    // cout << *(vct.end() - 1);

    for (vector<int>::iterator i = vct.end() - 1; i != vct.begin() - 1; i--) {
        cout << *i << " ";
        if (*i == 5)
            vct.erase(i);
    }

    cout << endl;

    for (int i : vct) {
        cout << i << " ";
    }

    return 0;
}
