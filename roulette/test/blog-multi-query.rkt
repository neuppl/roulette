#lang roulette/example/blog

fixed RealMatrix A = [1, 2; 3, 4];
fixed RealMatrix B = [10, 20; 30, 40];
query A + B;  // [11, 22; 33, 44]
 

random Integer i ~ UniformInt(0, 1);
random RealMatrix a = [i, 10; 20, i];
query toInt(a[i][i]) == i;

random RealMatrix V ~ Categorical({[1, 2] -> 0.4, [3, 4] -> 0.6});
query V;


random Integer X ~ UniformInt(0, 1);
query X;          // {0: 1/2, 1: 1/2}
query X, X + X;   // {(0, 0): 1/2, (1, 2): 1/2}