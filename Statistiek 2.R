#Exercise 1
#First create samples x, y and z
set.seed('1234')
nx <- 5000
ny <- 1800
nz <- 299
mux <- 400
muy <- 550
muz <- 350
sdx <- 130
sdy <- 50
sdz <- 8
x <- rnorm(nx, mux, sdx)
y <- rnorm(ny, muy, sdy)
z <- rnorm(nz, muz, sdz)
#Calculate the standard error SE for sample means
SEx <- sdx / sqrt(nx)
SEy <- sdy / sqrt(ny)
SEz <- sdz / sqrt(nz)
#calculate a 95% confidence interval
OGx <- meanx - 1.96 * SEx
BGx <- meanx + 1.96 * SEx
OGy <- meany - 1.96 * SEy
BGy <- meany + 1.96 * SEy
OGz <- meanz - 1.96 * SEz
BGz <- meanz + 1.96 * SEz
#calculate a 66% confidence interval
crit66 <- qnorm(1 - 0.34/2)
OGx <- meanx - crit66 * SEx
BGx <- meanx + crit66 * SEx
OGy <- meany - crit66 * SEy
BGy <- meany + crit66 * SEy
OGz <- meanz - crit66 * SEz
BGz <- meanz + crit66 * SEz
#calculate
meanx <- mean(x)
meany <- mean(y)
meanz <- mean(z)

zx <- (meanx - mux) / SEx
zy <- (meany - muy) / SEy
zz <- (meanz - muz) / SEz
#calculate critical z-value for a = 0.05
critz <- qnorm(1 - 0.05/2)
abs(zx) > critz
abs(zy) > critz
abs(zx) > critz

#Exercise 2
scores <- c(9.9, 7.9, 5.1, 1.8, 1.6, 6.3, 0.9, 5.1, 
  0.7, 4.1)
meanscores <- mean(scores)
nscores <- length(scores)
sdscores <- sd(scores)
mu <- 5.5
df <- nscores - 1
tvalue <- (meanscores - mu)/(sdscores / sqrt(nscores))
critt <- qt(1 - 0.05/2,df)
# Exercise 3
scores1 <- c(3, 7, 4, 2, 6)
scores2 <- c(6, 8, 7, 5, 5)
difference <- scores2 - scores1
meandif <- mean(difference)
sddif <- sd(difference)
scoresn <- length(difference)
tvaluedif <- meandif / (sddif / sqrt(scoresn))
df2 <- scoresn - 1
critt2 <- qt(1- 0.05/2, df2)
