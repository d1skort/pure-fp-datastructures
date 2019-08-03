| Heap                     | insert         | merge         | findMin | deleteMin |
| :---                     |     :---:      |         :---: | :---:   | :---:     |
| Leftist Heap             | O(logn)        | O(logn)       | O(1)    | O(logn)   |
| WeightBiasedLeftist Heap | O(logn)        | O(logn)       | O(1)    | O(logn)   |
| ExplicitMin Heap         | inner heap     | inner heap    | O(1)    | innerHeap |
| Binomial Heap            | O(1) *         | O(logn)       | O(logn) | O(logn)   |
| Splay Heap               | O(logn) *      | O(n)          | O(n)    | O(logn)   |

note: * means amortized