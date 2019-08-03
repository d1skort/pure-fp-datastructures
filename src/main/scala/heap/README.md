| Heap                     | insert         | merge         | findMin | deleteMin |
| :---                     |     :---:      |         :---: | :---:   | :---:     |
| Leftist Heap             | O(logn)        | O(logn)       | O(1)    | O(logn)   |
| WeightBiasedLeftist Heap | O(logn)        | O(logn)       | O(1)    | O(logn)   |
| ExplicitMin Heap         | inner heap     | inner heap    | O(1)    | innerHeap |
| Binomial Heap            | O(1) *         | O(logn)       | O(logn) | O(logn)   |
| Splay Heap               | O(logn) *      | O(n)          | O(n)    | O(logn)   |
| Pairing Heap             | O(1)           | O(1)          | O(1)    | O(n)      |

note: * means amortized

note about Pairing Heap:

We can prove that `insert`, `merge` and `deleteMin` takes `O(logn)` amortized time. Also, nobody succeeded with prove or disprove that this operations takes `O(1)` amortized time.