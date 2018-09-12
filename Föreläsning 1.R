x <- c(10,8,1)
class(x)

y <- rep(1, times=3)
y1<- seq(0, 100, length.out = 10000)

z=c(1,2,3)

seq(1,4, by=0.5)

## Lists ##
y<- 1:10
x<- c("Hello", "Hi", "yo")
list1 <- list(x,y)
list2 <- list(first=x, second=y)
data.frame(list2)

## Matrices ##
mat1 <- matrix(y, nrow=5, ncol=2, byrow=TRUE)
mat1

y2 <- 2*y

mat2 <- cbind(y,y2)
mat3 <- rbind(y,y2)

## Data frames ##

df1 <- data.frame(age=c(20, 21),
                  citizen=c("swedish", "British"),
                  sex=c("female", "male"),
                  Employed=c(TRUE, FALSE))


## Arrays ##
y=seq(1,8)
array1 <- array(y, dim=c(2,2,2))


v <- c(12, "Hello")

vec2<- c(1.3,2.3,4.5)
vec2[c(1,3)]



mat1[1:2,]


list2

list3<-list2[1]
class(list3)
list4<-list2[[1]]
class(list4)
class(list2$first)

df1[df1$citizen=="swedish", ]

df1[ , c("age", "Employed")]

a <- 31:41
b <- 11:21


ab<-cbind(a,b)
ab
class(ab)
v2<-ab[,1, drop=FALSE]
class(v2)

x<- rnorm(100,0,1)
mean(x)
xs<-(x-mean(x))^2
s<- sum(xs)
avv <- sqrt(s/(length(x)-1))
avv
sd(x)
