# Projet stat
# Timothée BORELLE
# Etienne LOPVET

#
## Ex 1

# a)
x = 1
p = 2

dpareto <- function(x, p) {
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
dpareto <- function(x, p){
  p/(x^(p+1))
}

qpareto <- function(y, p) {
  if((y > 0)&&(y < 1)&&(p > 0)) {
    (1/(1-y))^(1/p)
  } else {
    stop("y<=0 ou y>=1 ou p<=0")
  }
}

rpareto <- function(n, p){qpareto(runif(n), p)}

n = 1000
x = seq(1, 10, length.out = n)
p = c(2,exp(1),4,6,8)

par(mfrow= c(2,3))
for (i in 1:5) {
  y = dpareto(x, p[i])
  z = rpareto(n,p[i])
  
  hist(z[z <= 10],
       xlab = 'A trouver', ylab = 'A trouver', xlim=c(1, 15/p[i]), ylim=c(0, p[i]),
       freq = F, main=paste("Titre a trouver, p = ", toString(p[i]), sep = "") )
  
  lines(x, y, lty = 1, col = "red", lwd = 2)
}



## Ex 2

# a)
n = 50000
x = rpareto(n, 3)

# c)
xm = cumsum(x)/(1:n)

# d)
plot(xm,
     type = "l", col = "blue",
     xlab = 'A préciser', ylab = 'A préciser',
     main ="Titre à trouver")
abline(h = 1.5, col = "red")

# e)
n = 50000
x = rpareto(n, 1)

xm = cumsum(x)/(1:n)

plot(xm,
     type = "l", col = "blue",
     xlab = 'A préciser', ylab = 'A préciser',
     main ="Titre à trouver")



#
## Ex 3

# a)
n = 1000
R = 5000
p = 3
x = rpareto(n, p)

# b)
n = 1000
R = 5000
r = 1:R

xnr = cumsum(x)/(1:n)

# c)
p = 3
esperenceX1 = p/(p-1)
varianceX1 = p/(((p-1)^2)*(p-2))
res = sqrt(n)*((xnr-esperenceX1)/(sqrt(varianceX1))) 

hist(res,
     col = "black",
     #ylim=c(0,0.5), xlim=c(-15,15),
     freq = F,
     xlab="x-axis label", ylab="y-axis label",
     main="Exercice 3 c)"
)
# Exercice 3 c)

x1 <- dnorm(r)
#p2 <- 
lines(x1, 
      lty = 1, 
      col = "red",
      #ylim=c(0,0.4), xlim=c(-15,15),
      lwd = 2
)

x2 <-rnorm(r)
lines(x2, 
      lty = 1, 
      col = "orange",
      #ylim=c(0,0.4), xlim=c(-15,15),
      lwd = 2
)

#legend("topleft", 
#        legend = c(
#          "densité N(0,1)",
#          "répartion N(0,1)",
#          "Histogram "), # √n *  ̄ n,r −E[X1] / √Var[X1]
#        col = c("red", "orange", "black"), 
#        lty = 1, cex=0.75, 
#        bty = "n"
#)
