
library(plot3D)
x = seq(-2,2,by=0.1)
y = seq(-2,2,by=0.1)
#f = function(x,y){ 3*(1-x)^2*exp(-(x^2) - (y+1)^2) - 10*(x/5 - x^3 - y^5)*exp(-x^2-y^2) - 1/3*exp(-(x+1)^2 - y^2) }
f = function(x,y){ y*exp(-x^2-y^2) }
#f = function(x,y){ (x-2)^2+(y+1)^2 }
z = outer(x, y, f)

color = jet.col(100)
zfacet = z[-1,-1] + z[-1,-ncol(z)] + z[-nrow(z),-1] + z[-nrow(z),-ncol(z)]
facetcol = cut(zfacet,100) 
surf = persp(z, theta = 45, phi = 45, col = color[facetcol], shade = 0.001,xlab = "X", ylab = "Y", zlab = "Z") 

X= cbind(x1=x,x2=y)

grad = function(x,y,theta,lambda) {
  dfx = -2*x*y*exp(-x^2-y^2) - lambda*theta[1]
  dfy = (1-2*y^2)*exp(-x^2-y^2) - lambda*theta[2]
  out = c(dfx, dfy)
  return(out) 
}

#grad = function(x,y,theta,lambda) {
#  dfx = 2*(x-2)- lambda*theta[1]
#  dfy = 2*(y+1) - lambda*theta[2]
#  out = c(dfx, dfy)
#  return(out) 
#}

sgd = function(X,b0,n,alpha, lambda) {
  cost = matrix( f(b0[1],b0[2]),nrow=n,ncol=1)
  m = nrow(X)
  beta = matrix(b0,nrow=n,ncol=length(b0))
  for (i in 2:n) {
    alpha = alpha - alpha/n
    sample = sample.int(m,1)
    Xi = X[sample,]
    beta[i,] = beta[i-1,]-alpha*grad(Xi[1],Xi[2],beta[i-1,],lambda)
    cost[i] = f(beta[i,1],beta[i,2])
  }
  print(tail(beta))
  print(tail(cost))
  return(list(beta, cost))
}

b0 = c(0.5,0.5)
out = sgd(X,b0,30,0.1, 0.000)
beta = out[[1]]
cost = out[[2]]
plot(cost, type = "l", xlab = "Iter")

p1=beta[,1]
p2=beta[,2]
surf = persp(z, theta = 45, phi = 45, col = color[facetcol], shade = 0.05,xlab = "X", ylab = "Y", zlab = "Z",scale=F) 

points(trans3d(p1,p2,f(p1,p2),surf), pch=4,col="red")
points(trans3d(p1,p2,f(p1,p2),surf), type="l",col="red")

