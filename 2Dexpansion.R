#Set the number of points
number_of_points=1000

#Generate the raw points
x<-rnorm(number_of_points)
y<-rnorm(number_of_points)
#mean subtract the points
x<-x-mean(x)
y<-y-mean(x)
mat<-matrix(
     c(x,y),
     nrow=number_of_points,
     ncol=2)
#stretch one axis
mat[,1]=mat[,1]*4
#rotate the points 
theta=3.14/10
rot<-matrix(
     c(cos(theta),-sin(theta),sin(theta),cos(theta)),
     nrow=2,
     ncol=2,
     byrow=TRUE)
data=mat %*% rot

#Generate the ensamble average matrix
EA<-matrix(c(0,0,0,0),nrow=2,ncol=2)
for(p in 1:number_of_points){
     EA=EA + (data[p,] %o% data[p,])
}
EAM<-EA/number_of_points

#get the eigenvalues and vectors 
eig<-eigen(EAM,TRUE)


#display data with arrows
plot(
     data,
     xlim=c(-10,10),
     ylim=c(-10,10),
     main="Kl expansion",
     sub="with vectors",
     xlab="X dim",
     ylab="Y dim"
)
arrows(
     c(0,0),c(0,0),
     eig$vectors[1,]*eig$values*4,
     eig$vectors[2,]*eig$values*4,
     code=2,
)

#whitened data
data_white<-data %*% eig$vectors
plot(
     data_white,
     xlim=c(-10,10),
     ylim=c(-10,10),
     main="Kl expansion",
     sub="Whitening",
     xlab="X dim",
     ylab="Y dim"
)
