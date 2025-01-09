
library(ggplot2)
#first one
para <- function(x, a=1) a*x^2
x <- -20:20
y <- para(x)
y
df <- data.frame(x,y)
df
ggplot(df, aes(x,y))+geom_line()

ellipse <- function(x, a =7, b=3) b/a*sqrt(a^2-x^2) 
z <- ellipse(x)
ggplot(data.frame(x,z), aes(x,z))+geom_line()

par(mfrow = c(1,2))
line <- function(x, m=2, c =0) m*x+c
x1 <- -5:10
ggplot(data.frame(x1, y1 =line(x1)), aes(x1, y1))+geom_line()

line1 <- function (x, m=2, c=0) m*x+(c+3)
x2 <- -5:10
ggplot(data.frame(x2, y2 = line1(x2)), aes(x2,y2))+geom_line()
