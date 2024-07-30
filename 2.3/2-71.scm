; See the related image for what the tree looks like when n = 5.
; The n = 10 tree is similar, but "taller".
; The tree structure is "linear", i.e., at least one subtree
; of each non-leaf node is a node because the frequencies are
; powers of 2 and 1 + 2 + ... + 2^(n-1) = 2^n - 1 < 2^n.
; The most frequent symbol is attached to the root node of
; the tree and, thus, only a single bit is required to encode it.
; The least frequent symbol is at the end of the chain and, thus,
; n - 1 bits are required to encode it.
