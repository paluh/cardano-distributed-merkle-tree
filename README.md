# plutus-merkleized-bst

WIP: Part of my ongoing weekend project 'Unbound Accounting for Marlowe'.

## Why BST?
* Because `insert` for Merkle tree with sorted keys seems to overcomplicated. I'm following advice found here [^1]
* And yes I considered static tree prefilled with "empty values" but it would clearly add too much complexity to the preliminary state validation.
* Because we lack bitwise operators in Plutus to even try Patricia Merkle Trie.
* Because we lack crypto primitives to even think about Verkle Tree.

## "Unbound Accounting" on the chain?
Yes, simple "membership" or "insertion" proof checking on Cardano can be easily splitted between multiple transactions.

* We can use "proof token" which known verification policy will mint.
* The token will encode the query in the token name (we will hash the query).
* Please note that verification policy can use recursievely use itself meaning if you provide a token of a subproof it could continue checking from that point on. 

## Naming convention
I retained 'BST' in the repository name for broader recognition and understanding. In the literature, a similar data structure has been referred to as an "authenticated search tree"[^2].

[^1]: ["Why you should probably never sort your Merkle tree's leaves"](https://alinush.github.io/2023/02/05/Why-you-should-probably-never-sort-your-Merkle-trees-leaves.html) by Alin Tomescu
[^2]: ["Accountable certificate management using undeniable attestations"](https://doi.org/10.1145%2F352600.352604) by Ahto Buldas and Peeter Laud and Helger Lipmaa
