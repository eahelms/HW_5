n = 15
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)
llr(x,y,z,1)

library(microbenchmark)
microbenchmark(llr)
