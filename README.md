<h1>Shared-Nearest-Neighbours-Based-DBSCAN</h1>

<h3>Problem Statement:</h3>

Implement given algorithm in Scheme programming language for clustering data points given in a dataset D. Assume an ordering of D where ith data point has index i.

<h3>Parameters:</h3>

* K
* ε (≤ K)
* MinPts (≤ K)

<h3>Algorithm:</h3>

1. Sparsify Similarity Matrix
2. Construct the shared neighbor graph G
3. Identify core points
4. Form clusters using core points
5. Identify noise points
6. Assign border points to clusters
