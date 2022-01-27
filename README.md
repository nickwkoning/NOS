# NOS
This package contains an implementation of the greedy algorithm described in Koning & Hemerik (2022) to construct near-oracle subgroups of the sign-flipping group. Its key feature is the function `construct_near_oracle_subgroup`, which asks a user to specify the number of signs (`n`), the maximum order of the subgroup (`max_order`), a function that measures the quality of a subgroup (`leak_fun`), the number of candidate subgroups considered in each step of the search (`num_candidates`), and the option for a group to use as seed value (`group`).

Besides this, the package offers a more efficient implementation to find a range of subgroups with `n` signs in the function `construct_near_oracle_subgroup_path`, which additionally makes use of batching to reduce memory usage. 

Another function of potential interest is `construct_oracle_subgroup`, which constructs an oracle subgroup of `n` signs with the largest order of at most `max_order`.
