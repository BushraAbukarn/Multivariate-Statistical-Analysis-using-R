title: "STAT 450"
output: html_document
date: "2024-02-03"

```{r "Lec #1 Matrix and Vectors"}
v <- c(3,2,3)
#Is Something A Vector 
is.vector(v)
is.matrix(v)

#Transpose of a vector 
vp <- t(v)
vp

#Adding Vectors 
w <- c(4,1,2)
s <- v + w
s

#Subtracting Vectors 
d <- v - w
d

#Multiplication by a Scalar 
s <- 3*v
s

#Vector Dot Product
d <- v %*% w
d

#Length of a vector 
v 
sqrt(v %% v)

#We can write a function to get the norm of a vector 
normvec <- function(x) + {sqrt(x%%x)}
normvec(v)

#Unit Length Vector 
l <- c(1/2, 1/2, 1/2, 1/2)
l
sqrt(l%*%l)

#The Unit of Vector 
u <- c(1,1,1)
u

#The matrix of the function 
A <- matrix(c(2,3,-2,1,2,2),3,2)
A

#Multiplication by a Scalar 
c <- 3
c*A

#Matrix Addition & Subtraction 
B <- matrix(c(1,4,-2,1,2,1),3,2)
B
C <- A + B 
C 
D <- A - B 
D

#matrix Multiplication 
D <- matrix(c(2,-2,1,2,3,1),2,3)
D
C <- D %*% A
C

#Transpose of a Matrix 
AT <- t(A)
AT
ATT <- t(AT)
ATT

#Inverse of the Matrix 
A <- matrix(c(4,4,-2,2,6,2,2,8,4),3,3)
A
AI <- solve(A)
AI

#Determinant of a Matrix 
D <- det(A)
D
```

```{r "Lec 2 Multivariate Sample Data"}
#Exclude ID
nutrition <- subset(nutrition_all, select = -(id))
names(nutrition)

#Getting the sample mean vector 
colMeans(nutrition)

#Getting the descriptive statistics 
summary(nutrition)

#Getting the sample covariance matrix 
var(nutrition)

#Getting the sample correlation matrix 
cor(nutrition)
```

```{r "Lec 3 The Multivariate Normal Distribution"}
nutrition <- read.table("C:/Users/bushr/OneDrive/Desktop/nutrition.txt", header = FALSE, 
                        col.names = c("id", "calcium", "iron", "protein", "vitamin_a", "vitamin_c"))
#Exclude ID
nutrition <- subset(nutrition_all, select = -(id))
names(nutrition)

#Univariate plot
hist(nutrition$calcium)

#Bivariate plot
pairs(~nutrition$calcium + nutrition$iron + nutrition$protein + nutrition$vitamin_a +nutrition$vitamin_c)

#3D plots
par(mfrow=c(1,1))
scatterplot3d(x=nutrition$iron, y=nutrition$protein, z=nutrition$calcium)
```

```{r "Lec 4 The Multivariate Normal Distribution"}
Mu = c(1, 1)
Sigma = matrix(c(2, -1, -1, 1), nrow = 2)

x = seq(-4, 6, 0.1) 
y = x
N = length(x)
density = matrix(0, N, N)
for (i in 1:N) {
  for (j in 1:N) {
    density[i, j] = dmvnorm(c(x[i], y[j]), mean = Mu, sigma = Sigma)
  }
}

# 3D plot of the bivariate normal density
persp(x, y, density, theta = 10, phi = 25, shade = 0.5,
      col = "gold", r = 1, ltheta = 25, ticktype = "detailed")
```

```{r "Lec 5 Comparisons of Multivariate Population Data"}
#T-test
attach(HOME_SALES) 
names(HOME_SALES)
X = data.frame(FINISHED_AREA, SALES_PRICE, BEDROOMS)
HotellingsT2(X=X, mu=c(2500, 270000, 3.5))
```

```{r "Lec 6 Two sample Hotelling's T^2"}
attach(HOME_SALES)
names(HOME_SALES)
X = data.frame(FINISHED_AREA, SALES_PRICE, BEDROOMS)
HotellingsT2(X=X, mu=c(2500, 2700, 3.5))
HotellingsT2(X=X[POOL=="YES",], Y=X[POOL=="NO",])
HotellingsT2(X=X[POOL=="YES",], Y=X[POOL=="NO",], mu=c(500,5000,0))
```

```{r "Lec 7 & Compare Several Multivariate Population Means (MANOVA)"}
RESULT = manova(cbind(SALES_PRICE, FINISHED_AREA, BEDROOMS) ~ QUALITY*STYLE, data=HOME_SALES)
summary(RESULT)

#"summary.aov" for the univariate ANOVA results for each response component 
summary.aov(RESULT)
```

```{r "Lec 8 Multivariate regression"}
# This example studies the antidepressant called amitriptyline, its side effects
# and consequences of an overdose. The data contain the following seven variables
# collected from n=17 overdose cases.
# Response variables:
# TOT = total TCAD plasma level
# AMI = amount of amitriptyline present in TCAN plasma level # Predictor variables:
# GEN = gender (1 = female, 0 = male)
# AMT = amount of antidepressants taken at the time of overdose
# PR = PR wave measurement 
# DIAP = diastolic blood pressure
# QRS = QRS wave measurement

#Read the data 
DEPR = read.table("C:/Users/bushr/OneDrive/Desktop/SideEffects.DAT",
                  col.names = c("TOT", "AMI", "GEN", "AMT", "PR", "DIAP", "QRA"))

#Fit the multivariate multiple regression model with 2 responses and 5 predictors 
reg = lm(cbind(TOT, AMI) ~ GEN + AMT + PR + DIAP + QRA, data=DEPR)
reg

#The summary simply returns separate analyses of univariate multiple regression model
summary(reg)

#The matrix of estimated slopes
coef(reg)

#The estimated variance-covariance matrix of sample regression slopes
vcov(reg)

#MANOVA of this multivariate regression 
anova(reg)

#Predict the multivariate response for the given combination of predictor variables 
predict(reg, data.frame(GEN=1, AMT=5000, PR=130, DIAP=75, QRA=100))
```
