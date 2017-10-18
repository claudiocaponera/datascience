
setwd('C:/Users/Claudio.Caponera/Documents/datascience')
#Crunch conference - R workshop script

x <- 4
# x <- x^2
# sqrt(x)


square_root <- function(x) {
  x ^ 0.5
}

square_root(4)
square_root(x)
square_root(x=4)


f <- function(x) 2 * x + 1

x <- 1:5
f(x)

plot(x, f(x))

## plot continous fn


x <- seq(1:100)
x <- seq(1,5,by=0.1)

plot(x, f(x))

plot(x, f(x), type='l')

plot(x, f(x), type='l', main='test', xlab='hi')

curve(f)


#Plot 1 period of sine wave
x <- seq(0, 2 * pi, by=0.1)
plot(x, sin(x), type='l')
curve(sin, from=0, to=2*pi)


# runif
######
runif(1)
r <- runif(1:100)

round(runif(10)) * 2 - 1
cumsum(round(runif(10)) * 2 - 1)

#set.seed(50)

plot(cumsum(round(runif(10)) * 2 - 1), type='s')

plot(cumsum(sample(c(-1,1), 5, replace=TRUE)), type='s')

#other way


h <- c(174,170,160)
w <- c(90,80,70)
plot(h,w)
cor(h,w)
fit <- lm(w ~ h)
summary(fit)
#does not work well: very few obs that also do not cover range of possible ages/weights
abline(fit) #plot needs to be called before


hh <- round(seq(from=0, to=200,length.out = 100))
ww <- round(seq(from=30, to=100,length.out = 100))
plot(hh,ww)
fit <- lm(ww ~ hh)
summary(fit)

#data frame
df <- data.frame(weight=w, height=h)
df[,2]

cor(df)
plot(df)

df$bmi <- df$weight/(df$height/100)^2

df <- read.csv('http://bit.ly/heightweight')
df$weightKg <- df$weightLb*0.45359237
df$heightCm <- df$heightIn*2.54
df$bmi <- df$weightKg/(df$heightCm/100)^2

summary(df)

range(df$bmi)
diff(range(df$bmi))

plot(df$heightCm,df$weightKg)
lm <- lm(weightKg ~ heightCm, df)
summary(lm)
abline(lm)


hist(df$bmi)
boxplot(df$bmi)

library(beanplot)
beanplot(df$bmi)

# boxplot {
#   rbeta(1000,0.1,0.1),
#   runif(1000) * 2 - 0.5,
#   rnorm(1000, 0.5, 0.75)
# }

hist(rbeta(1000, 0.1, 0.1))

boxplot(bmi ~ sex, df)


table(df$sex)
barplot(table(df$sex))

dotchart(table(df$sex), xlim=c(0,150))

pairs(df)


library(GGally)
ggpairs(df)

library(pairsD3)
pairsD3(df)



##stats

t.test(heightCm ~ sex, data=df)

aov(heightCm ~ sex, data=df)

summary(aov(heightCm ~ sex, data=df))

TukeyHSD(aov(heightCm ~ sex, data=df))



library(ggplot2)

diamonds
str(diamonds)


#data
#mapping
#shapes
#stats
p <- ggplot(diamonds, aes(x= cut)) + geom_bar(fill='orange') + theme_bw()
p <- p + scale_y_log10()
p <- p + coord_flip()
p <- p+ facet_wrap(color ~ clarity)
p


ggplot(diamonds, aes(cut, fill=color)) + geom_bar(position='fill')+
  theme(legend.position = 'none')

ggplot(diamonds, aes(cut, fill=color)) + geom_bar(position='dodge')+
  theme(legend.position = 'none')

ggplot(diamonds, aes(price)) + geom_histogram(binwidth = 1000)
ggplot(diamonds, aes(price, fill=cut)) + geom_density()
ggplot(diamonds, aes(price, fill=cut)) + geom_density(alpha= 0.2)


ggplot(diamonds, aes(carat,price, color=cut)) + geom_point() + geom_smooth()
ggplot(diamonds, aes(carat,price)) + geom_point(aes(color=cut)) + geom_smooth()
ggplot(diamonds, aes(carat,price)) + geom_point(aes(color=cut)) + geom_smooth(method='lm', color='red',se=TRUE)

ggplot(diamonds, aes(carat,price)) + geom_point(aes(color=cut), alpha=0.5) + geom_smooth()

library(hexbin)
ggplot(diamonds, aes(carat,price)) + geom_hex()

library(ggthemes)
p <- ggplot(diamonds, aes(carat,price)) +  geom_point(aes(color=cut), alpha=0.2)
p <- p + theme_economist() + scale_color_economist()
p <- p + theme_stata() + scale_color_stata()
p <- p + theme_excel() + scale_color_excel()
p

ggplot(diamonds, aes(cut, price)) + geom_boxplot()


mtcars

#plot distribution of horspower
ggplot(mtcars,aes(hp))+geom_histogram()

#barplot on the number of carburetors per trasmission
ggplot(mtcars,aes(carb, fill=factor(am)))+geom_bar()

#boxplot of horsepower by the number of carb
ggplot(mtcars,aes(factor(carb),hp))+geom_boxplot()

#horsepower and weight by number of carb
ggplot(mtcars,aes(wt,hp,color=factor(carb)))+geom_point()
ggplot(mtcars,aes(wt,hp))+geom_point()+facet_wrap(~carb)


#horsepower and weight by number of carb with a trendline
ggplot(mtcars,aes(wt,hp))+geom_point()+geom_smooth(method='lm')+facet_wrap(~carb)


library(data.table)
library(hflights)


##########
rm(list = ls())

#PCA
library(jpeg)
#download.file('http://bit.ly/BudapestBI-R-img','nasa.jpg')
img <- readJPEG('C:/Users/Claudio.Caponera/Documents/datascience/nasa.jpg')

str(img)
dim(img)

h <- dim(img)[1]
w <- dim(img)[2]

imgid <- matrix(img,h*w)

pca <- prcomp(imgid) 
summary(pca)
str(pca$x)

image(matrix(pca$x[,1], nrow=h), col=gray.colors(100))


#example2
library(readxl)
cities <- read_excel('C:/Users/Claudio.Caponera/Documents/datascience/psoft-telepules-matrix-30000.xls')
cities <- cities[,-1]
cities <- cities[-nrow(cities),]
str(cities)

mds <- cmdscale(as.dist(cities))
#returns only returns 2 columns (PCA all, you need to choose)
#articial numbers that summarise information 
plot(mds)
text(mds[,-1], mds[,2], rownames(mds))

mds <- mds* -1

#example 3
mds <- cmdscale(dist(mtcars))
plot(mds)
text(mds[,1], mds[,2], rownames(mds))

mds <- as.data.frame(mds)
ggplot(mds, aes(V1, V2, label=rownames(mtcars)))+geom_text()

library(ggrepel)
ggplot(mds, aes(V1, V2, label=rownames(mtcars)))+geom_text_repel()
#if number of observations is low



#ggplotly
##ggiraffe @ David Gohel


#only numerical variables
#transform into dummies before PCA if you have categorical

str(iris)

ggplot(iris, aes(Sepal.Length, Sepal.Width, color=Species))+geom_point()+geom_smooth(method='lm')

summary(lm(Sepal.Width ~ Sepal.Length, iris))

# -> clustering
# -> decision trees

#clustering - hierarchical
dm <- dist(iris[, 1:4])
hc <- hclust(dm)
plot(hc)

#techniques to choose number of clusters
rect.hclust(hc, k=3)
cutree(hc,3)
#including how it looks like by adding other variables. 


for (i in 2:10) {
plot(iris$Sepal.Length,iris$Sepal.Width, col=cutree(hc,i), pch=as.numeric(iris$Species))
}


#decision trees
library(rpart)
fit <- rpart(Species ~ ., iris)
plot(fit)
text(fit)

library(partykit)
plot(as.party(fit))

iris$rnd <- runif(150)
iris <- iris[order(iris$rnd),]
iris$rnd <- NULL
str(iris)


train <- iris[1:100,]
test <- iris[101:150,]

fit <- rpart(Species ~ ., train)
plot(as.party(fit))
prediction <- predict(fit, newdata=test, type='class')

table(test$Species, prediction)


#another option would be multinonial logistic regression

library(class)
?kn

#random forest

#boosting - create decision tree, then create another one to fix previous one.


#H2o
library(h2o)
h2o.init()

#---------------
library(hflights)
str(hflights)
#pre-classification problems
head(hflights.hex)
#predict model is flight is cancelled or not
#we can also predict if flight was 50min delayed or not 

hflights.hex[, 'FlightNum'] <- as.factor()







