generate.data <- function(n1,n2){
	theta <- runif(n1+n2,0,2*pi)
	r <- c(rnorm(n1,0,0.2),rnorm(n2,1,0.1))
	X <- cbind(r*cos(theta),r*sin(theta))
	y <- c(rep(0,n1),rep(1,n2))
	return(list(X=X,y=y))
}

library(keras)
dat <- generate.data(100,100)
X <- dat$X
y <- to_categorical(dat$y,2)
model <- keras_model_sequential()
layer_dense(model,units=10,activation='relu',input_shape=c(2))
layer_dense(model,units=2,activation='softmax')
compile(model,loss="categorical_crossentropy",optimizer="nadam")
history <- fit(model,X,y,epochs=500,verbose=0)

grid <- data.matrix(expand.grid(x=seq(-1.5,1.5,length.out=31), y=seq(-1.5,1.5,length.out=31)))
prob.grid <- predict_proba(model,grid)
contour(x=seq(-1.5,1.5,length.out=31),y=seq(-1.5,1.5,length.out=31),z=matrix(prob.grid[,1],31,31),levels=0.5,drawlabels=F)
points(X[,1],X[,2],col=dat$y+1)