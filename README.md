# store-graph

**disclaimer: the code is not very clean but usuable. This was a quick
experiment.**

to evaluate nixpkgs in the repository root directory run (usually takes <5min):

```
NIX_STORE_DIR=$(pwd)/store NIX_STATE_DIR=$(pwd)/tmp nix-instantiate default.nix -A filtered
```

to produce a graph, stored in `out.dot`, and the adjacency matrix of the graph,
stored in `out.map`, run:

```
stack build
stack exec -- store-graph-exe ./store/
```

The blog post data is included in the [data](./data) folder. Unfortunately I
didn't save the exact nixpkgs revision that I used to generate it. If you rerun
the program, you might get slightly different results because it will be
generated for the nixpkgs revision that is saved in
[nixpkgs.nix](./nixpkgs.nix).
