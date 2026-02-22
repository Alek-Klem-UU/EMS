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


# Calculate the mean values
meanx <- mean(x)
meany <- mean(y)
meanz <- mean(z)


# Calculate a 95% confidence interval
crit95 <- qnorm(1 - 0.05/2)
OGx_95 <- meanx - crit95 * SEx
BGx_95 <- meanx + crit95 * SEx
OGy_95 <- meany - crit95 * SEy
BGy_95 <- meany + crit95 * SEy
OGz_95 <- meanz - crit95 * SEz
BGz_95 <- meanz + crit95 * SEz
# Calculate a 66% confidence interval
crit66 <- qnorm(1 - 0.34/2)
OGx <- meanx - crit66 * SEx
BGx <- meanx + crit66 * SEx
OGy <- meany - crit66 * SEy
BGy <- meany + crit66 * SEy
OGz <- meanz - crit66 * SEz
BGz <- meanz + crit66 * SEz

zx <- (meanx - mux) / SEx
zy <- (meany - muy) / SEy
zz <- (meanz - muz) / SEz
# Calculate critical z-value for a = 0.05
critz <- qnorm(1 - 0.05/2)
abs(zx) > critz
abs(zy) > critz
abs(zz) > critz

cat("Standard error X: ", SEx, "\n")
cat("Standard error Y: ", SEy, "\n")
cat("Standard error Z: ", SEz, "\n\n")

cat("95% Confidence Interval X: [", OGx_95, ",", BGx_95, "]\n")
cat("95% Confidence Interval Y: [", OGy_95, ",", BGy_95, "]\n")
cat("95% Confidence Interval Z: [", OGz_95, ",", BGz_95, "]\n\n")

cat("66% Confidence Interval X: [", OGx, ",", BGx, "]\n")
cat("66% Confidence Interval Y: [", OGy, ",", BGy, "]\n")
cat("66% Confidence Interval Z: [", OGz, ",", BGz, "]\n\n")

cat("Z-value X:", zx, "\n")
cat("Z-value Y:", zy, "\n")
cat("Z-value Z:", zz, "\n\n")

cat("Critical Z-value:", critz, "\n\n")

cat("Significant difference for X:", abs(zx) > critz, "\n")
cat("Significant difference for Y:", abs(zy) > critz, "\n")
cat("Significant difference for Z:", abs(zz) > critz, "\n\n")

#Exercise 2
scores <- c(9.9, 7.9, 5.1, 1.8, 1.6, 6.3, 0.9, 5.1, 0.7, 4.1)

meanscores <- mean(scores)
nscores <- length(scores)
sdscores <- sd(scores)

mu <- 5.5
df <- nscores - 1

tvalue <- (meanscores - mu)/(sdscores / sqrt(nscores))
critt <- qt(1 - 0.05/2,df)

cat("Sample Mean:", meanscores, "\n")
cat("Calculated T-value:", tvalue, "\n")
cat("Critical T-value:", critt, "\n")
cat("Is it significantly different from 5.5?:", abs(tvalue) > critt, "\n\n")

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

cat("Mean Difference:", meandif, "\n")
cat("Paired T-value:", tvaluedif, "\n")
cat("Significant improvement?:", tvaluedif > critt2, "\n\n")

women <- c(2, 3, 5, 2, 6, 7)
men <- c(6, 9, 7, 6, 9, 8, 7, 8)

ftest <- var.test(women, men)

cat("F-test p-value for variances:", ftest$p.value, "\n")

cat("Final T-test results:\n")
t.test(women, men, var.equal = TRUE)
