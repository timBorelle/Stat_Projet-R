# Projet stat
# Timothée BORELLE
# Etienne LOPVET

#
## Ex 1

# a)
x = 1
p = 2

dpareto <- function(x, p){
  p/(x^(p+1))
}

reponse = dpareto(x,p)

# b)
dpareto <- function(x, p){
  p/(x^(p+1))
}

x = seq(1,3, by=0.01)

y1 = dpareto(x,2)
y2 = dpareto(x,4)
y3 = dpareto(x,6)
y4 = dpareto(x,8)

ye = dpareto(x,exp(1))

plot(x, y1, type="n", xlab = 'x', ylab = 'f(x)', xlim=c(1,3), ylim=c(0, 9), main ="Densité dpareto(x,p)")
lines(x,y1, col="green") 
lines(x,ye, col="red") 
lines(x,y2, col="blue") 
lines(x,y3, col="orange") 
lines(x,y4, col="black")

legend("topright", 
       legend = c("p = 2", "p = e", "p = 4", "p = 6", "p = 8"),
       col = c("green", "red", "blue", "orange", "black"), lty = 1)


# c)
ppareto <- function(x,p){
  ifelse(x < 1, 0, 1-(1/(x^p)))
}

# d)
x = 0:50
#x = seq(1,3, by=0.1)

xlog = log(x)
z1 = log(1-ppareto(x,2))
z2 = log(1-ppareto(x,4))
z3 = log(1-ppareto(x,6))
z4 = log(1-ppareto(x,8))
ze = log(1-ppareto(x,exp(1)))

plot(xlog, z1, type="n", xlab = 'log(x)', ylab = 'f(x)', xlim=c(0,3), ylim=c(-5, 0), main ="Courbe paramétrée (log x, log(1 − F (x))")
lines(xlog,z1, col="green") 
lines(xlog,ze, col="red") 
lines(xlog,z2, col="blue") 
lines(xlog,z3, col="orange") 
lines(xlog,z4, col="black")


legend("topright", 
       legend = c("p = 2", "p = e", "p = 4", "p = 6", "p = 8"),
       col = c("green", "red", "blue", "orange", "black"), lty = 1)

# e)
qpareto <- function(y, p){
  if((y > 0)&&(y < 1)&&(p > 0)){
    (1/(1-y))^(1/p)
  } else {
    stop("y<=0 ou y>=1 ou p<=0")
  }
}

rpareto <- function(n, p){qpareto(runif(n), p)}

p = 1/2; n = 1000
x = seq(1, 10, length.out = n)
y = dpareto(x, p)
z = rpareto(n,p)

hist(z[z <= 10], freq = F, main=as.character(p))
lines(x, y, lty = 1, col = "red", lwd = 2)



## Ex 2
n = 50000


x = rpareto(50000, 1, 3)



#
## Ex 3

