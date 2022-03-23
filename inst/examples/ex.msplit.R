library(roahd)

N = 10
P= 5
grid = seq( 0, 1, length.out = P )
C = exp_cov_function( grid, alpha = 0.3, beta = 0.4 )
values = generate_gauss_fdata( N,
                                      centerline = sin( 2 * pi * grid ),
                                      Cov = C )
fD = fData( grid, values )
x0=list(as.list(grid))
fun=mean_lists()
rrr<-conformal.fun.msplit(x=NULL,t_x=NULL, y=fD,t_y=NULL, x0=list(x0[[1]]),
                          fun$train.fun, fun$predict.fun,alpha=0.2,
                          split=NULL, seed=FALSE, randomized=FALSE,seed.rand=FALSE,
                          verbose=FALSE, rho=NULL,B=2,lambda=0)


