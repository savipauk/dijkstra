### Dijkstras algorithm implemented in various ways using C++ and Haskell

Edge weight is calculated as $f(i, j) = a^{|i - j| \mod c} + b(i - j)^2 + 1$.

`adjacencymatrix.cpp` is the fastest algorithm and it's nearly instant for a high number of vertices.
