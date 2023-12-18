### Dijkstras algorithm implemented in various ways using C++ and Haskell

Edge weight is calculated as $f(i, j) = a^{|i - j| \mod c} + b(i - j)^2 + 1$.

`adjacencymatrix.cpp` is the fastest algorithm and it's nearly instant for a high number of vertices.



This was made for my Discrete Mathematics lab and I planned on optimizing the Haskell variant as it's very slow, however I never got around to do it.
