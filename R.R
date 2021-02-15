#import the data set
data("airquality")
data <- airquality

#We want to build a contingency table 
#with dichotomous variables
#The samples we care about are
#Temperature : temp (v)
#Ozone : ozone (u)
#Month : month (w)

#first step : drop all NA values

data <- data[!is.na(data$Ozone),]
data <- data[!is.na(data$Solar.R),]

#111 row now
rownames(data) <- 1:111
data <- cbind(data$Ozone, data$Temp, data$Month)

#second step : process u et v into dichotomous samples

#u process
#first class [1, 31] #0
#second class (31, 168] #1

u <- rep(0, length(data$Ozone))
for (i in 1:length(data$Ozone)){
  if (data$Ozone[i]>31) 
  {u[i] <- 1
  }
  else u[i] <- u[i]
}

#v process
#first class [57, 79] #0
#second class (79, 97] #1

v <- rep(0, length(data$Temp))
for (i in 1:length(data$Temp)){
  if (data$Temp[i]>79) 
  {v[i] <- 1
  }
  else v[i] <- v[i]
}

#last sample we care about for our contingency table
#month

df <- cbind(u,v, data$Month)

#contingency table 
cont <- table(df[,1], df[,2], df[,3])

#else we just copy the table in instructions

copy <- cbind(c(17,0,6,1),c(4,2,1,2),c(2,3,0,21),c(5,3,3,12),c(18,2,1,8))

#y vector 

y <- c(17, 4, 2, 5, 18, 0, 2, 3, 3, 2, 6, 1, 0, 3, 1, 1, 2, 21, 12, 8)

#design matrix X 

X <- matrix(data = 0, nrow = 20, ncol = 16)
colnames(X) <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8',
                 'X9', 'X10', 'X11', 'X12', 'X13', 'X14', 'X15', 'X16')

X[1,1] <- 1 #y1 est la seule donn?e qui est dans l'intercept
#le reste est ? 0 dans X1
#X2 est l'indicatrice pour la deuxi?me modalit? de ozone
#Les 10 derni?res donn?es y11 ? y20 sont dans ozo2
X[11:20,2] <- c(rep(1, 10))
#X3 est l'indicatrice pour la deuxi?me modalit? de temp
#les 6 ? 10?me et 16 ? 20?me valeurs sont dans temp2
X[6:10, 3] <- c(rep(1, 5))
X[16:20, 3] <- c(rep(1, 5))
#X4 correspond ? la deuxi?me modalit? de month
X[c(2,7,12,17),4] <- c(rep(1,4))
#X5 correspond ? la troisi?me modalit? de month
X[c(3,8,13,18),5] <- c(rep(1,4))
#X6 correspond ? la quatri?me modalit? de month
X[c(4,9,14,19),6] <- c(rep(1,4))
#X7 correspond ? la cinqui?me modalit? de month
X[c(5,10,15,20),7] <- c(rep(1,4))
#X8 interactions entre ozo2 et temp 2
X[16:20, 8] <- c(rep(1,5))
#X9 interactions entre ozo2 et month2
X[c(12,17), 9] <- c(1,1)
#X10 interactions entre ozo2 et month3
X[c(13,18), 10] <- c(1,1)
#X11 ozo2:mon4
X[c(14,19), 11] <- c(1,1)
#X12 ozo2:mon5
X[c(15,20), 12] <- c(1,1)
#X13 temp2:mon2
X[c(7,17), 13] <- c(1,1)
#X14 temp2:mon3
X[c(8,18), 14] <- c(1,1)
#X15 temp2:mon4
X[c(9,19), 15] <- c(1,1)
#X16 temp2:mon5
X[c(10,20), 16] <- c(1,1)




variance <- function(x){
  
  return(n*solve(t(x)%*%x))
}

#la densité de la loi multinormal de paramètre (0, sigma) ou sigma dépend de la matrice X
priori <- function(mu, sigma, y,X){
  
  k <- ncol(sigma)
  y <- t(y)
  dmn <- exp((-1/2)*t(y-mu)%*%solve(sigma)%*%(y- mu))/sqrt(((2*pi)^k)*det(sigma))  
  return(dmn)
}
mcor <- cor(X)
mcor
A=matrix(0, 16, 1)
priori(A, mcor,y, X)



