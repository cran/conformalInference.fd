### SIMULATED DATA

sample_size=50

xout <- seq(from=0,to=1,length.out=301)

nb <- 6
w=c(1,1,1)

set.seed(-1)
coeff <- matrix(rnorm(nb*length(w)),nb,length(w))
bsbasis_obj <- fda::create.bspline.basis(rangeval=c(0,1),
                                    nbasis=nb, norder=4)
fd_obj <- fda::fd(coeff, bsbasis_obj)
beta <- t(matrix(w,length(w),1)%*%rep(1,length(xout)))*fda::eval.fd(xout, fd_obj)
beta_dim1=beta[,1:2]
beta_dim2=beta[,c(1,3)]

rm(coeff,bsbasis_obj,fd_obj,w,beta)

cov_dim1=array(NA,c(sample_size+1,length(xout),2))
cov_dim2=array(NA,c(sample_size+1,length(xout),2))

cov_dim1[,,1]=matrix(1,sample_size+1,length(xout)) #x_0: intercetta
cov_dim1[,,2]=matrix(rep(1:(sample_size+1)/(sample_size+1),length(xout)),
                     sample_size+1,length(xout)) #x_{i2}=i/(n+1) per ogni i=1,...,n+1

cov_dim2[,,1]=matrix(1,sample_size+1,length(xout)) #x_0: intercetta
cov_dim2[,,2]=matrix(rep((1:(sample_size+1)/(sample_size+1))^2,
                         length(xout)),sample_size+1,length(xout))

deterministic_data_dim1=vapply(1:(sample_size+1),function(j)
  rowSums(beta_dim1*cov_dim1[j,,]),numeric(length(xout)))
deterministic_data_dim2=vapply(1:(sample_size+1),function(j)
  rowSums(beta_dim2*cov_dim2[j,,]),numeric(length(xout)))

w_error=rep(1,(sample_size+1)*2)
set.seed(1234)
coeff_error <- matrix(rnorm(nb*(sample_size+1)*2),nb,(sample_size+1)*2)
bsbasis_obj_error <-fda::create.bspline.basis(rangeval=c(0,1),
                                          nbasis=nb, norder=4)
fd_obj_error <- fda::fd(coeff_error, bsbasis_obj_error)
error_data <- t(matrix(w_error,length(w_error),1)
                %*%rep(1,length(xout)))*fda::eval.fd(xout, fd_obj_error)

rm(coeff_error,bsbasis_obj_error,fd_obj_error,w_error)


data_dim1=deterministic_data_dim1+error_data[,1:(sample_size+1)]
data_dim2=deterministic_data_dim2+error_data[,(sample_size+2):(2*sample_size+2)]

rm(beta_dim1,beta_dim2,deterministic_data_dim1,deterministic_data_dim2,error_data,nb)

set.seed(1234)
obs_tbp=sample(1:(sample_size+1),1)

data_y=lapply(1:(sample_size+1),function(x) NULL)

for(i in 1:(sample_size+1)){
  data_y[[i]]=list(data_dim1[,i],data_dim2[,i])
}

new_data_y=list(data_y[[obs_tbp]])
data_y=data_y[-obs_tbp]

rm(data_dim1,data_dim2)

data_x=lapply(1:(sample_size+1),function(x) NULL)

for(i in 1:(sample_size+1)){
  data_x[[i]]=list(t(cov_dim1[i,,]),t(cov_dim2[i,,]))
}

new_x=list(data_x[[obs_tbp]])
data_x=data_x[-obs_tbp]

rm(obs_tbp,i,cov_dim1,cov_dim2)


n0=1
set.seed(1234)
len=length(data_x)
id=sample(1:len,n0)
n=len-n0
t=list(xout)
fun=mean_lists()
y0=data_y[id]
p<-length(y0[[1]])
grid_len<-length(y0[[1]][[1]])


### run once

true.jack = conformal.fun.jackplus (x=data_x[-id],t_x=NULL, y=data_y[-id],t_y=t,
                                    x0=data_x[id], fun$train.fun, fun$predict.fun,alpha=0.1)



