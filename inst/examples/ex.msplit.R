# Here we will just test with y with fData type. A list of points or a mfData,
# or an fda type are still valid options as input.
# Please refer to the example for conformal.fun.split


## fData #############################

N = 30
P = 10
grid = seq( 0, 1, length.out = P )
C = roahd::exp_cov_function( grid, alpha = 0.3, beta = 0.4 )
values = roahd::generate_gauss_fdata( N,
                                      centerline = sin( 2 * pi * grid ),
                                      Cov = C )
fD = roahd::fData( grid, values )
x0=list(as.list(grid))
fun=mean_lists()
B=2

final.multi=conformal.fun.msplit(NULL,NULL, fD, x0, fun$train.fun, fun$predict.fun,
                                 alpha=0.1,
                                 split=NULL, seed=FALSE, randomized=FALSE,seed_beta=FALSE,
                                 verbose=FALSE, training_size=NULL,s_type="st-dev",B,lambda=0,
                                 tau = 0)

