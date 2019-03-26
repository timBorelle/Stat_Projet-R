# Projet stat
# Timothée BORELLE
# Etienne LOPVET

#
## Ex 1
x = 1
p = 2

# a)
dpareto <- function(x, p){
  reponse = p/(x^(p+1))
  return(reponse)
}

reponse = dpareto(x,p)

# b)
#dpareto <- function(x, p){
#  reponse = p/(x^(p+1))
#}

y1 = dpareto(1,2)
y2 = dpareto(1,4)
y3 = dpareto(1,6)

Fn1 = ecdf(y1) 
Fn2 = ecdf(y2) 
Fn3 = ecdf(y3) 

plot(Fn1,verticals=T,do.p=F, col="green", xlab = 'p', ylab = 'f(x)', 
     xlim=c(0, 10), ylim=c(0, 1), main ="Densité dpareto(x,p)") 
lines(Fn2,verticals=T,do.p=F, col="blue") 
lines(Fn3,verticals=T,do.p=F, col="black") 

legend("topleft", 
       legend = c("20", "40", "60"),
       col = c("green", "blue", "black"), lty = 1)


#plot(density(dpareto(x,p)), density(dpareto(x,p)), type = 'l', xlab = 'Densité', ylab = 'p', main ="Densité dpareto(x,p)")

# c)
ppareto <- function(x,p){
  if(x<1)
    return(0)
  else
    return(1-(1/(x^p)))
}

z1 = log(x)
z2 = log(ppareto(1,4))

Gn1 = ecdf(z1) 
Gn2 = ecdf(z2) 

plot(Gn1,verticals=T,do.p=F, col="green", xlab = 'p', ylab = 'f(x)', 
     xlim=c(0, 10), ylim=c(0, 1), main ="Densité ppareto(x,p)") 
lines(Gn2,verticals=T,do.p=F, col="blue") 

legend("topleft", 
       legend = c("log x", "log(1-F(x))"),
       col = c("green", "blue"), lty = 1)


#
## Ex 2



#
## Ex 3

