# Implementation 

We store state information in a Item construction defined as 
$$
I = (p,\delta,s,j,\sigma,\sigma',\nable,C,\Delta)
$$
where 
- $p$ is the **production**
- $\delta$ is the **dot** meaning parsing fontier inside the produciton
- $s$ is the starting position (?)
- $j$ the input end position
- $\sigma$ the entry typing state
- $\sigma'$ the current typing state (?)
- $\nabla$ the path to the node (?)
- $C$ the children refernces
- $\Delta$ the children typing states

We also define a completion as 
$$
\mathcal{C} = (n,s,e,\gamma)
$$
 - $n$ the nonterminal 
 - $s$ the start index
 - $e$ the end index
 - $\gamma$ the node

The waiter as 
$$
\Omega = (i,\sigma,\nabla)
$$
- $i$ an item
- $\sigma$ a typing state
- $\nabla$ a path

