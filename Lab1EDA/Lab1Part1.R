# Problem 1
sales1<-c(12,14,16,29,30,45,19,20,16, 19, 34, 20)
sales2<-rpois(12,34) # random numbers, Poisson distribution, mean at 34, 12 numbers
par(bg="cornsilk")
plot(sales1, col="blue", type="o", ylim=c(0,100), xlab="Month", ylab="Sales" )
title(main="Sales by Month")
lines(sales2, type="o", pch=22, lty=2, col="red")
grid(nx=NA, ny=NULL)
legend("topright", inset=.05, c("Sales1","Sales2"), fill=c("blue","red"), horiz=TRUE)

# Problem 2
sales<-read.table("part1/salesdata.txt", header=T)
sales # to verify that data has been read
barplot(as.matrix(sales), main="Sales Data", ylab= "Total",beside=T, col=rainbow(5))

# Problem 3
fn<-boxplot(sales,col=c("orange","green"))$stats
text(1.45, fn[3,2], paste("Median =", fn[3,2]), adj=.4, cex=.7)
text(0.45, fn[3,1],paste("Median =", fn[3,1]), adj=.1, cex=.7)
grid(nx=NA, ny=NULL)

# Problem 4
fb1<-read.csv("part1/FB.csv")
aapl1<-read.csv("part1/AAPL.csv")
names(fb1)
names(aapl1)
plot(aapl1$Adj.Close, col="blue", type="o", ylim=c(0,200),xlim=c(0,60), xlab="Days", ylab="Price" )
lines(fb1$Adj.Close, type="o", pch=22, lty=2, col="red")
legend("bottomright", inset=.05, c("Apple","Facebook"), fill=c("blue","red"), horiz=TRUE)
# Just study the distribution of the adjusted close of the stock price of Apple.
hist(aapl1$Adj.Close, col=rainbow(8))

# Problem 5

library('ggplot2')
data(mpg)
attach(mpg)
head(mpg)
summary(mpg)
#after analysis remove the data from the memory
detach(mpg)

library (help=datasets)
library(datasets)
head(uspop)
plot(uspop)

# Problem 6
library("ggmap")

library("maptools")
library("maps")
register_google(key = "AIzaSyD2kr-Qs_hBVYOY620mjqgRtKJSr92wdRU") 
visited <- c("SFO", "Chennai", "London", "Melbourne", "Lima,Peru", "Johannesbury, SA")
ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-70, 80), mar=c(0,0,0,0))
points(visit.x,visit.y, col="red", pch=36)

# Here is another example using the map of The United States.
visited <- c("SFO", "New York", "Buffalo", "Dallas, TX")
ll.visited <- geocode(visited)
visit.x <- ll.visited$lon
visit.y <- ll.visited$lat
map("state", fill=TRUE, col=rainbow(50), bg="lightblue", mar=c(0,0,7,7))
points(visit.x,visit.y, col="yellow", pch=36)

# Problem 7
data(mtcars)
attach(mtcars)
head(mtcars)
plot(mtcars[c(1,3,4,5,6)], main="MTCARS Data")
plot(mtcars[c(1,3,4,6)], main="MTCARS Data")
plot(mtcars[c(1,3,4,6)], col=rainbow(5),main="MTCARS Data")
detach(mtcars)
# Problem 8

ggplot(mtcars, aes(x=mpg, y=disp)) + geom_point()
