#Install the below packages

install.packages("DataExplorer")
install.packages("psych")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("car")
install.packages("tree")
install.packages("rpart.plot")

#****************************************************************************************
#Section 1: Read data
#****************************************************************************************

setwd("C:/Users/vchan/OneDrive/Desktop/MGSC 661 - Prof Juan Serpa/Final term_AirlinesDelay")
df = read.csv("Life_Expectancy_data_2.csv")
colnames(df)[1]<-"Country"

df_numerical = df[,c(4:21)]

#****************************************************************************************
#Section 2: Data pre-processesing
#****************************************************************************************

library(DataExplorer)
create_report(df_numerical)

library(psych)
pairs.panels(df_numerical[,c(2:18)], pch=".", gap=0) 

#-----Select relevant columns-----
library(dplyr)
df_numerical_3 = df_numerical %>%select(Life.expectancy,	Adult.Mortality,	Alcohol,	BMI,	Polio,	Total.expenditure,	Diphtheria,	HIV.AIDS,	GDP,	Population,	thinness..1.19.years,	Income.composition.of.resources)

#----Exploration - Initial PCA----------------

#Scale the data
df_numerical_3 = scale(df_numerical_3)
df_numerical_3= as.data.frame(df_numerical_3)

library(ggplot2)
library(ggfortify)

#2d PCA plot without clustering
library(car)
pca_2 = prcomp(df_numerical_3, scale=TRUE)

autoplot(pca_2, data=df_numerical_3, loadings=TRUE, loadings.label = TRUE,
         loadings.label.size = 4)
 
#Output of PCA
pca_output = as.data.frame(pca_2$rotation)

#Language group
library(ggplot2)

#--------Generate plots for %variation explained------

#Overall % variation explained plot

pve=(pca_2$sdev^2)/sum(pca_2$sdev^2)
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1), pch=16, col = "red", 
     cex=1.5, main= "Variation explained by PCA", xlab="Principal Component", ylab="Variation")
plot(cumsum(pve), ylim=c(0,1), pch=16, col = "red", 
     cex=1.5, main= "Variation (cumulative) explained by PCA", xlab="Principal Component", ylab="Variation")

par(mfrow=c(1,1))


#PC1 plot
ggplot(data=pca_output, aes(x=rownames(pca_output), y=pca_output$PC1, fill=ifelse(pca_output$PC1< 0, "green","red"))) +
  geom_bar(stat="identity")+
  coord_flip()+
  theme_classic()+
  xlab("Predictor")+
  ylab("PC1")+
  geom_hline(yintercept=0,  color="black", size=1)+
  theme(legend.position = "none")+
  labs(title="% Variation Explained by PC1")+
  theme(plot.title = element_text(hjust = 0.5, size=15))
  
#PC2 plot
ggplot(data=pca_output, aes(x=rownames(pca_output), y=pca_output$PC2, fill=ifelse(pca_output$PC2< 0, "green","red"))) +
  geom_bar(stat="identity")+
  coord_flip()+
  theme_classic()+
  xlab("Predictor")+
  ylab("PC2")+
  geom_hline(yintercept=0,  color="black", size=1)+
  theme(legend.position = "none")+
  labs(title="% Variation Explained by PC2")+
  theme(plot.title = element_text(hjust = 0.5, size=15))

#PC3 plot
ggplot(data=pca_output, aes(x=rownames(pca_output), y=pca_output$PC3, fill=ifelse(pca_output$PC3< 0, "green","red"))) +
  geom_bar(stat="identity")+
  coord_flip()+
  theme_classic()+
  xlab("Predictor")+
  ylab("PC3")+
  geom_hline(yintercept=0,  color="black", size=1)+
  theme(legend.position = "none")+
  labs(title="% Variation Explained by PC3")+
  theme(plot.title = element_text(hjust = 0.5, size=15))

#PC4 plot
ggplot(data=pca_output, aes(x=rownames(pca_output), y=pca_output$PC4, fill=ifelse(pca_output$PC4< 0, "green","red"))) +
  geom_bar(stat="identity")+
  coord_flip()+
  theme_classic()+
  xlab("Predictor")+
  ylab("PC4")+
  geom_hline(yintercept=0,  color="black", size=1)+
  theme(legend.position = "none")+
  labs(title="% Variation Explained by PC4")+
  theme(plot.title = element_text(hjust = 0.5, size=15))

## Correlation Plot

df_correlation=df_numerical

colnames(df_correlation)

library(dplyr)

colnames(df_correlation)[1]<-"Life Expectancy"
colnames(df_correlation)[2]<-"Adult Mortality"
colnames(df_correlation)[3]<-"Infant Deaths"
colnames(df_correlation)[5]<-"Spend on Health as % of GDP"
colnames(df_correlation)[8]<-"Under 5 deaths"
colnames(df_correlation)[10]<-"Health spend as % total govt. spend"
colnames(df_correlation)[12]<-"HIV AIDS"
colnames(df_correlation)[15]<-"Thinness 10-19 years"
colnames(df_correlation)[16]<-"Thinness 5-9 years"
colnames(df_correlation)[17]<-"HDI"

sub=dplyr::select(df_correlation,"Life Expectancy",	"Adult Mortality",	Alcohol,	BMI,	Polio,	Total.expenditure,	Diphtheria,	HIV.AIDS,	GDP,	Population,	thinness..1.19.years,	Income.composition.of.resources)
cormat = round(cor(sub),2)
cormat

library(reshape2)
melted_cormat= melt(cormat)
head(melted_cormat)
library(ggplot2)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
upper_tri


# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)



# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#Descriptive plots

#------Life.expectancy------

#Language group

ggplot(df, aes(x=Life.expectancy)) +
  geom_histogram(color="blue", fill="light blue")+
  geom_vline(data=df, aes(xintercept=median(Life.expectancy)), color="red",size=1.5,
             linetype="dashed")+
  labs(title="Life Expectancy - Frequency distribution",x="Life Expectancy", y = "Frequency")+
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")

#------Adult.Mortality------

ggplot(df, aes(x=Adult.Mortality)) +
  geom_histogram(color="blue", fill="light blue")+
  geom_vline(data=df, aes(xintercept=median(Adult.Mortality)), color="red",size=1.5,
             linetype="dashed")+
  labs(title="Adult Mortality - Frequency distribution",x="Adult Mortality", y = "Frequency")+
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")

#------Alcohol------

ggplot(df, aes(x=Alcohol)) +
  geom_histogram(color="blue", fill="light blue")+
  geom_vline(data=df, aes(xintercept=median(Alcohol)), color="red",size=1.5,
             linetype="dashed")+
  labs(title="Alcohol - Frequency distribution",x="Alcohol", y = "Frequency")+
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")

#------Polio------

ggplot(df, aes(x=Polio)) +
  geom_histogram(color="blue", fill="light blue")+
  geom_vline(data=df, aes(xintercept=median(Polio)), color="red",size=1.5,
             linetype="dashed")+
  labs(title="Polio - Frequency distribution",x="Polio", y = "Frequency")+
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")

#------Total.expenditure------

ggplot(df, aes(x=Total.expenditure)) +
  geom_histogram(color="blue", fill="light blue")+
  geom_vline(data=df, aes(xintercept=median(Total.expenditure)), color="red",size=1.5,
             linetype="dashed")+
  labs(title="Total Expenditure - Frequency distribution",x="Total Expenditure", y = "Frequency")+
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")

#------Diphtheria------

ggplot(df, aes(x=Diphtheria)) +
  geom_histogram(color="blue", fill="light blue")+
  geom_vline(data=df, aes(xintercept=median(Diphtheria)), color="red",size=1.5,
             linetype="dashed")+
  labs(title="Diphtheria - Frequency distribution",x="Diphtheria", y = "Frequency")+
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")

#------HIV.AIDS------

ggplot(df, aes(x=HIV.AIDS)) +
  geom_histogram(color="blue", fill="light blue")+
  geom_vline(data=df, aes(xintercept=median(HIV.AIDS)), color="red",size=1.5,
             linetype="dashed")+
  labs(title="HIV AIDS - Frequency distribution",x="HIV AIDS", y = "Frequency")+
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")


#------GDP------
ggplot(df, aes(x=GDP)) +
  geom_histogram(color="blue", fill="light blue")+
  geom_vline(data=df, aes(xintercept=median(GDP)), color="red",size=1.5,
             linetype="dashed")+
  labs(title="GDP - Frequency distribution",x="GDP", y = "Frequency")+
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")

#------Population------
ggplot(df, aes(x=Population)) +
  geom_histogram(color="blue", fill="light blue")+
  geom_vline(data=df, aes(xintercept=median(Population)), color="red",size=1.5,
             linetype="dashed")+
  labs(title="Population - Frequency distribution",x="Population", y = "Frequency")+
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")


#------thinness..1.19.years------
ggplot(df, aes(x=thinness..1.19.years)) +
  geom_histogram(color="blue", fill="light blue")+
  geom_vline(data=df, aes(xintercept=median(thinness..1.19.years)), color="red",size=1.5,
             linetype="dashed")+
  labs(title="Thinness 10-19 years - Frequency distribution",x="Thinness 10-19 years", y = "Frequency")+
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")


#------Income.composition.of.resources------

ggplot(df, aes(x=Income.composition.of.resources)) +
  geom_histogram(color="blue", fill="light blue")+
  geom_vline(data=df, aes(xintercept=median(Income.composition.of.resources)), color="red",size=1.5,
             linetype="dashed")+
  labs(title="HDI - Frequency distribution",x="HDI", y = "Frequency")+
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 20, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 18),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=14,face="bold"),
         legend.position="bottom")


#*************************************************
#Section 3: Unsupervised Learning---------------
#*************************************************
#Run k means clustering

#Elbow curve
k.max <- 15
data <- df_numerical_3
wss <- sapply(1:k.max,
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Best value of k = 3

#----------Run seed and km.3 lines together--------------

set.seed(500)
km.3 = kmeans(df_numerical_3,3)

#*************************************************
#Section 4: PCA plot with clusters--------------
#*************************************************

#k= 3
df_numerical_3$label= km.3$cluster

table(df_numerical_3$label)

df_numerical_3$label=as.factor(df_numerical_3$label)
df_numerical_3 = df_numerical_3[,c(-13)]

#Visualize clusters in 2d
pca = prcomp(df_numerical_3, scale=TRUE)

#Add cluster label
df_numerical_3$label= km.3$cluster
df_numerical_3$label=as.factor(df_numerical_3$label)

#2d
autoplot(pca, data=df_numerical_3, loadings=TRUE, loadings.label = TRUE,
         loadings.label.size = 4, colour='label')


write.csv(df_numerical_3, file = "clusters3.csv",row.names=FALSE)


#*************************************************
#Section 5: Regression Tree------------------
#*************************************************

library(tree)
library(rpart)
library(rpart.plot)

library(dplyr)
df_numerical_3 = df_numerical %>%select(Life.expectancy,	Adult.Mortality,	Alcohol,	BMI,	Polio,	Total.expenditure,	Diphtheria,	HIV.AIDS,	GDP,	Population,	thinness..1.19.years,	Income.composition.of.resources)

attach(df_numerical_3)

#-------Plot regression tree after finding the best cp value-------
par(mfrow=c(1,1))
mytree=rpart(Life.expectancy~Adult.Mortality + Alcohol + 
               BMI + Polio + Total.expenditure + Diphtheria +
               HIV.AIDS + GDP + Population +
               thinness..1.19.years + Income.composition.of.resources,control=rpart.control(cp=0.004))

rpart.plot(mytree)

yhat = predict(mytree, df_numerical_3[,c(-1)])
mean((yhat-df_numerical_3[,c(1)])^2)

#MSE - 10.63

#*************************************************
#Section 6: Insights---------------
#*************************************************

library(dplyr)
df_numerical_insight = df %>%select(HIV.AIDS, Income.composition.of.resources, thinness..1.19.years, Total.expenditure)

russia = subset(df, df$Country=="Russian Federation")
tnt= subset(df, df$Country=="Trinidad and Tobago")

library(cowplot)

#Russia
a = ggplot(russia, aes(x=Year, y=HIV.AIDS)) +
  geom_line(color="blue", fill="light blue")+
  labs(title="HIV AIDS Trend",x="Year", y = "HIV AIDS")+
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 14, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 14),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=12,face="bold"),
         legend.position="bottom")

b= ggplot(russia, aes(x=Year, y=Income.composition.of.resources)) +
  geom_line(color="blue", fill="light blue")+
  labs(title="HDI",x="Year", y = "HDI")+
  ylim(0.65,0.8) +
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 14, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 14),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=12,face="bold"),
         legend.position="bottom")

c= ggplot(russia, aes(x=Year, y=thinness..1.19.years)) +
  geom_line(color="blue", fill="light blue")+
  labs(title="Thinness 10-19 years",x="Year", y = "Thinness 10-19 years")+
  ylim(2,3) +
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 14, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 14),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=12,face="bold"),
         legend.position="bottom")

d= ggplot(russia, aes(x=Year, y=Total.expenditure)) +
  geom_line(color="blue", fill="light blue")+
  labs(title="Total expenditure",x="Year", y = "Total expenditure")+
  ylim(4,8) +
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 14, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 14),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=12,face="bold"),
         legend.position="bottom")

plot_grid(a, b, c, d ,labels = "AUTO")

#Tnt
a = ggplot(tnt, aes(x=Year, y=HIV.AIDS)) +
  geom_line(color="blue", fill="light blue")+
  labs(title="HIV AIDS Trend",x="Year", y = "HIV AIDS")+
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 14, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 14),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=12,face="bold"),
         legend.position="bottom")

b= ggplot(tnt, aes(x=Year, y=Income.composition.of.resources)) +
  geom_line(color="blue", fill="light blue")+
  labs(title="HDI",x="Year", y = "HDI")+
  ylim(0.7,0.8) +
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 14, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 14),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=12,face="bold"),
         legend.position="bottom")

c= ggplot(tnt, aes(x=Year, y=thinness..1.19.years)) +
  geom_line(color="blue", fill="light blue")+
  labs(title="Thinness 10-19 years",x="Year", y = "Thinness 10-19 years")+
  ylim(5,7) +
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 14, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 14),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=12,face="bold"),
         legend.position="bottom")

d= ggplot(tnt, aes(x=Year, y=Total.expenditure)) +
  geom_line(color="blue", fill="light blue")+
  labs(title="Total expenditure",x="Year", y = "Total expenditure")+
  ylim(4,7) +
  theme_classic() +
  theme( plot.title = element_text(color = "black", size = 14, face = "bold", hjust= 0.5),
         legend.title = element_text(color = "black", size = 14),
         legend.text = element_text(color = "black", size = 14, face="bold"),
         axis.text=element_text(size=12, face="bold"),
         axis.title=element_text(size=12,face="bold"),
         legend.position="bottom")

plot_grid(a, b, c, d ,labels = "AUTO")

