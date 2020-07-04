#Read the Mall_ customers.csv file
customer_data=read.csv("C:/Users/LENOVO/Documents/Work/mall-customers/Mall_Customers.csv")
#Display the csv file
customer_data
#Knowing the structure of the Mall_ customers dataset 
str(customer_data)
#getting the names of the columns
names(customer_data)
#Going through first few entries
head(customer_data)
#getting the summary(Basic Statistics) of the dataset
summary(customer_data)
#Standard deviation of Different age of customers
sd(customer_data$Age)
summary(customer_data$Annual.Income..k..)
#Visualization of gender distribution in the dataset
a=table(customer_data$Genre)
a
barplot(a,main="Gender Distribution of Customers",
        ylab="Number of Customers",xlab="Gender",
        col=rainbow(2),legend=rownames(a))
#Pie chart for Gender Distribution
pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
library("plotrix")
pie3D(a,labels=lbs,main="Pie Chart Depicting Ratio of Males and Females")
#Conclusion:-The percentage of females is 56% as comapared to that of males that is 44%

#Age Distribution
summary(customer_data$Age)
#Hostogram for the same
hist(customer_data$Age,col="green",main="Histogram to Frequency count of Age Class",xlab="Age Class",ylab="Frequency",labels=TRUE)
#Conclusion:->The age Frequency of 30-40 age class is the maximun 
#create a boxplot
boxplot(customer_data$Age,col="black",main="Boxplot for Descriptive Analysis of Age")
#Shows min age is apprx 18 and max is apprx 70 but the mean age is between 30 and 40

#Study of the Annual income of the customer 
summary(customer_data$Annual.Income..k..)
hist(customer_data$Annual.Income..k..,col="green",xlab="Annual income Class",ylab="Frequency",labels=TRUE)
#conclusion:Most of the customers Annual income lies between 60-80
plot(density(customer_data$Annual.Income..k..),
     col="green",xlab="Annual Income Class",
     ylab="Density",labels=TRUE)
polygon(density(customer_data$Annual.Income..k..),col="#ccff66")
#conclusion is the min is 15 and the max is 137 and with 70 have highest frequency

#Study the spending score of the customer
summary(customer_data$Spending.Score..1.100.)
boxplot(customer_data$Spending.Score..1.100.,col = "green",main="Boxplot for the spendinf score of various customer")
hist(customer_data$Spending.Score..1.100.,col="green",xlab="Spending score class",ylab="Frequency of customers",labels = TRUE)
#conclusion max customers in 40-50 spending score

#k-Means Algorithm of clustering
library(purrr)
set.seed(123)
# function to calculate total intra-cluster sum of square 
iss <- function(k) {
  kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
}
k.values <- 1:10
iss_values <- map_dbl(k.values, iss)
plot(k.values, iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total intra-clusters sum of squares")

#Now, let us take k = 6 as our optimal cluster 

k6<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
k6

#Visualizing the Clustering Results using the First Two Principle Components

pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)
pcclust$rotation[,1:2]
pcclust=prcomp(customer_data[,3:5],scale=FALSE) #principal component analysis
summary(pcclust)
pcclust$rotation[,1:2]

#Visualizing the Clustering Results

set.seed(1)
ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

