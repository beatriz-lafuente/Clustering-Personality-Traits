###### Pattern Recognition for Personalities ######

### Objective of the project: Identify groups of different personalities. 

# Questions asked in the questionnaire:
# Main body of questions - QX - Answers on a Likert scale from 1 to 5
# Secondary body of questions - TIPIX - Answers on a Likert scale from 1 to 7
# Tertiary set of questions - VCLX - Answers on a binary scale (0 and 1), 1 if the question is correct and 0 otherwise
# A set of additional social and demographic issues with different scales

# Three values are stored for each QX question:
# QXA - Respondent's response
# QXI - The position of the question in the quiz since the questions are asked in a random order
# QXE - The time spent on each question, in milliseconds

# "Q1" : "Never tell anyone the real reason you did something unless it is useful to do so.",
# "Q2" : "The best way to handle people is to tell them what they want to hear.",
# "Q3" : "One should take action only when sure it is morally right.",
# "Q4" : "Most people are basically good and kind.",
# "Q5" : "It is safest to assume that all people have a vicious streak and it will come out when they are given a chance.",
# "Q6" : "Honesty is the best policy in all cases.",
# "Q7" : "There is no excuse for lying to someone else.",
# "Q8" : "Generally speaking, people won't work hard unless they're forced to do so.",
# "Q9" : "All in all, it is better to be humble and honest than to be important and dishonest.",
# "Q10" : "When you ask someone to do something for you, it is best to give the real reasons for wanting it rather than giving reasons which carry more weight.",
# "Q11" : "Most people who get ahead in the world lead clean, moral lives.",
# "Q12" : "Anyone who completely trusts anyone else is asking for trouble.",
# "Q13" : "The biggest difference between most criminals and other people is that the criminals are stupid enough to get caught.",
# "Q14" : "Most people are brave.",
# "Q15" : "It is wise to flatter important people.",
# "Q16" : "It is possible to be good in all respects.",
# "Q17" : "P.T. Barnum was wrong when he said that there's a sucker born every minute.",
# "Q18" : "It is hard to get ahead without cutting corners here and there.",
# "Q19" : "People suffering from incurable diseases should have the choice of being put painlessly to death.",
# "Q20" : "Most people forget more easily the death of their parents than the loss of their property."

# TIPIX - "I see myself as:"
# TIPI1 : Extroverted, enthusiastic.
# TIPI2 : Critical, quarrelsome.
# TIPI3 : Dependable, self-disciplined.
# TIPI4 : Anxious, easily upset.
# TIPI5 : Open to new experiences, complex.
# TIPI6 : Reserved, quiet.
# TIPI7 : Sympathetic, warm.
# TIPI8 : Disorganized, careless.
# TIPI9 : Calm, emotionally stable.
# TIPI10 : Conventional, creative.

# VCL1 : boat
# VCL2 : incoherent
# VCL3 : pallid
# VCL4 : robot
# VCL5 : audible
# VCL6 : cuivocal - This word does not exist, it was just used as a validation technique
# VCL7 : paucity
# VCL8 : epistemology
# VCL9 : florted - This word does not exist, it was just used as a validation technique
# VCL10 : decide
# VCL11 : pastiche
# VCL12 : verdid - This word does not exist, it was just used as a validation technique
# VCL13 : abysmal
# VCL14 : lucid
# VCL15 : betray
# VCL16 : funny

# education : "How much education have you completed?", 1=Less than high school, 2=High school, 3=University degree, 4=Graduate degree
# urban : "What type of area did you live when you were a child?", 1=Rural (country side), 2=Suburban, 3=Urban (town, city)
# gender : "What is your gender?", 1=Male, 2=Female, 3=Other
# engnat : "Is English your native language?", 1=Yes, 2=No
# age : "How many years old are you?"
# hand : "What hand do you use to write with?", 1=Right, 2=Left, 3=Both
# religion : "What is your religion?", 1=Agnostic, 2=Atheist, 3=Buddhist, 4=Christian (Catholic), 5=Christian (Mormon), 6=Christian (Protestant), 7=Christian (Other), 8=Hindu, 9=Jewish, 10=Muslim, 11=Sikh, 12=Other
# orientation : "What is your sexual orientation?", 1=Heterosexual, 2=Bisexual, 3=Homosexual, 4=Asexual, 5=Other
# race : "What is your race?", 10=Asian, 20=Arab, 30=Black, 40=Indigenous Australian, 50=Native American, 60=White, 70=Other
# voted : "Have you voted in a national election in the past year?", 1=Yes, 2=No
# married : "What is your marital status?", 1=Never married, 2=Currently married, 3=Previously married
# familysize : "Including you, how many children did your mother have?"		
# major : "If you attended a university, what was your major (e.g. "psychology", "English", "civil engineering")?"

# Data collected by the server:
# introelapse : time spent on each of the pages
# testelapse : time spent on each of the pages
# surveyelapse : time spent on each of the pages
# screenw :	respondent screen width in pixels
# screenh :	respondent screen height in pixels
# country : the location of the respondent's internet network

###### Libraries ######

# Loading the necessary libraries
library(plyr) 
library(dplyr)
library(corrplot)
library(psych)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(gpairs)
library(cluster)
library(countrycode)
library(mclust) # for GMM estimation
library(lattice)

###### Loading dataset ######

df <- read.csv("C:\\data\\data.csv", sep='\t', stringsAsFactors = F)

###### Data Cleaning ######

# Eliminate null lines
df = df[df$Q1A !='NULL',]

# Eliminate unnecessary variables
drop = c("Q1E","Q2E", "Q3E","Q4E","Q5E","Q6E","Q7E","Q8E","Q9E","Q10E")
df= df[,!colnames(df) %in% drop]
drop = c("Q11E","Q12E", "Q13E","Q14E","Q15E","Q16E","Q17E","Q18E","Q19E","Q20E")
df= df[,!colnames(df) %in% drop]

drop = c("Q1I","Q2I", "Q3I","Q4I","Q5I","Q6I","Q7I","Q8I","Q9I","Q10I")
df= df[,!colnames(df) %in% drop]
drop = c("Q11I","Q12I", "Q13I","Q14I","Q15I","Q16I","Q17I","Q18I","Q19I","Q20I")
df= df[,!colnames(df) %in% drop]

drop = c("VCL1","VCL2","VCL3","VCL4","VCL5","VCL6","VCL7","VCL8","VCL9",
         "VCL10","VCL11","VCL12","VCL13","VCL14","VCL15","VCL16")
df= df[,!colnames(df) %in% drop]

drop = c("introelapse","testelapse","surveyelapse","screenw","screenh")
df= df[,!colnames(df) %in% drop]

# Transforming the first twenty variables Q1A... to Q20A into numeric
i = 1
while(i <= 20){
  df[,i] = as.numeric(df[,i])
  
  i= i+1
}

# Profile variables

# Education
# Clearing values that don't make any sense/impossible - 0
unique(df$education)
df <- df[df$education != 0, ]

# Urban
# Clearing values that don't make any sense/impossible - 0
unique(df$urban)
df <- df[df$urban != 0, ]

# Gender
# Clearing values that don't make any sense/impossible - 0
unique(df$gender)
df <- df[df$gender != 0, ]

# Engnat
# Clearing values that don't make any sense/impossible - 0
unique(df$engnat)
df <- df[df$engnat != 0, ]

# Hand
# Clearing values that don't make any sense/impossible - 0
unique(df$hand)
df <- df[df$hand != 0, ]

# Religion
# Clearing values that don't make any sense/impossible - 0
unique(df$religion)
df <- df[df$religion != 0, ]

# Orientation
# Clearing values that don't make any sense/impossible - 0
unique(df$orientation)
df <- df[df$orientation != 0, ]

# Race
# Clearing values that don't make any sense/impossible - 0
unique(df$race)
df <- df[df$race != 0, ]

# Voted
# Clearing values that don't make any sense/impossible - 0
unique(df$voted)
df <- df[df$voted != 0, ]

# Married
# Clearing values that don't make any sense/impossible - 0
unique(df$married)
df <- df[df$married != 0, ]

# Familysize
# Clearing values that don't make any sense/impossible - 0
unique(df$familysize)

df <- df[df$familysize != 0, ]
df <- df[df$familysize <= 10, ]

# Age
# This variable appears with strange maximum values, so it is relevant to visualize different values of the variable
unique(df$age)

# Clearing values that don't make any sense/impossible
df <- df[df$age <= 60, ]

# Major
# There are many strange values in this variable
table(df$major)
unique(df$major)
df$major = as.factor(df$major)

# Country
# Convert countries to continents
unique(df$country)
df$country[df$country == "NONE"] <- NA

df <- df %>%
  mutate(continent = factor(countrycode(country, origin = 'iso2c', destination = 'continent')) )

df$country = as.factor(df$country)
df$continent = as.factor(df$continent)

df <- df[,!names(df) %in% c(
  "country"
)]

set.seed(123)

# Get data frame only with about 15% of observations
df <- sample_n(df, 10000) # 10,000 obs. of 45 variables

# Sort columns
var = c("Q1A", "Q2A", "Q3A", "Q4A", "Q5A", "Q6A", "Q7A", "Q8A", "Q9A", "Q10A", "Q11A",
        "Q12A", "Q13A", "Q14A", "Q15A", "Q16A", "Q17A", "Q18A", "Q19A", "Q20A", "TIPI1",
        "TIPI2", "TIPI3", "TIPI4", "TIPI5", "TIPI6", "TIPI7", "TIPI8", "TIPI9", "TIPI10",
        "education", "urban", "gender", "engnat", "age",
        "hand", "religion", "orientation", "race", "voted", "married", "familysize",
        "major","continent")

df <- df[, var]

###### Brief introduction of the database ######

dim(df) # 10000 obs. of  44 variables
summary(df[,31:44])
summary(is.na(df[,31:44]))
describe(df[,31:44])

# Initial dataset view
# Continents of province
barplot(table(df$continent),col = "purple", ylim = c(0, 6000), main="Distribution of the variable 'continent' in the data set",
        xlab="Continents",ylab="Number of respondents [in units]")

# Age distribution of respondents
barplot(table(df$age),col = "purple", ylim = c(0, 800), main="Distribution of the variable 'age' in the data set",
        xlab="Age [in years]",ylab="Number of respondents [in units]")

# Continents of province
barplot(table(df$education),col = "purple", ylim = c(0, 5000), main="Distribution of the variable 'education' in the data set",
        xlab="School level",ylab="Number of respondents [in units]")

barplot(table(df$religion),col = "purple", ylim = c(0, 3000), main="Distribution of the variable 'religion' in the data set",
        xlab="Religion",ylab="Number of respondents [in units]")

barplot(table(df$race),col = "purple", ylim = c(0, 7000), main="Distribution of the variable 'race' in the data set",
        xlab="Race",ylab="Number of respondents [in units]")

barplot(table(df$urban),col = "purple", ylim = c(0, 5000), main="Distribution of the variable 'urban' in the data set",
        xlab="Urban level",ylab="Number of respondents [in units]")

# Box plot of respondents ages
boxplot(df$age)

###### PCA ######

# Selection of variables to PCA
# Selection of the questions with type "QA"
data <- df[,1:20]

# Correlation matrix
corr_matrix <- round(cor(data),2)
print(corr_matrix)

# Correlation plot
corrplot(corr_matrix, method="circle", type = "upper", tl.col = "black") # bolas
corrplot(corr_matrix, method="circle", type = "upper", order = "hclust", tl.col = "black") # grouping order
corrplot(corr_matrix, method="color", type = "upper", tl.col = "black") # cores
corrplot(corr_matrix, method="color", type = "upper", order = "hclust", tl.col = "black", tl.cex = 0.5) # grouping order

# Bartlett test & KMO
cortest.bartlett(corr_matrix) # p-value < 0.05, then PCA can be useful.

KMO(corr_matrix) # all variables have MSA > 0.5. No variable need to be deleted.

# Extraction and number of components
# Scale the data
dataZ <- scale(data)

# Assume the number of components. (nfactors) = number of variables
# Always a unrotated extraction
pc20 <- principal(dataZ, nfactors=20, rotate="none", scores=TRUE)

# Kaiser criterion & scree plot - Find the number of components.

# Eigenvalues - Variance of principal components
round(pc20$values, 3) # Kaiser

plot(pc20$values, type = "b", main = "Scree plot para Quest?es A (1-20)", 
     xlab = "Number of PC", ylab = "Eigenvalue") # Scree plot

# Kaiser Criterion suggests 3 components.
# Scree plot suggests 2 components.

# Explained variance
pc20$loadings
pc20$communality
  
# Extraction of 7 components solution
pc7 <- principal(dataZ, nfactors=7, rotate="none")

# Rotate (7 components)
pc7 <- principal(dataZ, nfactors = 7, rotate = "varimax")
pc7$loadings

# Check communalities
round(pc7$communality, 2) # they are ok

# Maybe add more components due to loss of information
pc8 <- principal(dataZ, nfactors = 8, rotate = "varimax")
pc8$loadings

round(pc8$communality, 2)

# PC Scores

# PC Scores with 7 components
pc7sc <- principal(dataZ, nfactors = 7, rotate = "none", scores = TRUE)
round(pc7sc$scores, 3) # compute de scores

mean(pc7sc$scores[,1]) # media = -3.758236e-17
sd(pc7sc$scores[,1]) # standard-deviation = 1

round(cor(pc7sc$scores[, 1:7]),2) # components are independent

# PC Scores with 8 components
pc8sc <- principal(dataZ, nfactors = 8, rotate = "none", scores = TRUE)
round(pc8sc$scores, 3) # compute scores

mean(pc8sc$scores[,1]) # media = -3.758236e-17
sd(pc8sc$scores[,1]) # standard-deviation = 1

round(cor(pc8sc$scores[, 1:8]),2) # components are independent

# It was decided to keep the 7 components.

# Add variables scores to data frame
df$Calculismo <- pc7sc$scores[,1]
df$Honestidade <- pc7sc$scores[,2]
df$Bem_das_Pessoas <- pc7sc$scores[,3]
df$Livre_Arbitrio <- pc7sc$scores[,4]
df$Ingenuidade <- pc7sc$scores[,5]
df$Prioridades <- pc7sc$scores[,6]
df$Virtude <- pc7sc$scores[,7]

# Scatter plot of different components
plot(df$Calculismo, df$Honestidade, pch = 20, xlab="PC1 (Calculismo)", 
     ylab="PC2 (Honestidade)", main = "Scores: PC1 vs PC2")

plot(df$Virtude, df$Honestidade, pch = 20, xlab="PC7 (Virtude)", 
     ylab="PC2 (Honestidade)", main = "Scores: PC7 vs PC2")

plot(df$Livre_Arbitrio, df$Ingenuidade, pch = 20, xlab="PC4 (Livre_Arbitrio)", 
     ylab="PC5 (Ingenuidade)", main = "Scores: PC4 vs PC5")

# Create data frame that contains only principal component scores
df_scores <- df[,45:51]

# Scatter plot of principal components
pairs(df_scores, pch=20, lower.panel = NULL)

####### Hierarchical Clustering #######

# Compute distances
person.dist <- dist(scale(df_scores))

#### Hclust Ward.D2
hclust_ward <- hclust(person.dist, method="ward.D2")
plot(hclust_ward, labels = FALSE, main = "Standardized * Euclidian * Ward.D2")
abline(h = 85, col = "red")

groups.k3.ward <- cutree(hclust_ward, k=3) # cut the tree into 3 clusters
rect.hclust(hclust_ward, k=3, border="red")
aggregate(df_scores, list(groups.k3.ward), mean)


#### Hclust Average-linkage
hclust_average <- hclust(person.dist, method="average")
plot(hclust_average, labels = FALSE, main = "Standardized * Euclidian * average" )
abline(h = 5.8, col = "red")

groups.k2.avg <- cutree(hclust_average, k=2) # cut the tree into 2 clusters
rect.hclust(hclust_average, k=2, border="red")
aggregate(df_scores, list(groups.k2.avg), mean)

# 'Ward' method was the best so far
# Now the method 'Complete' to validate

#### Hclust Complete-linkage
hclust_complete <- hclust(person.dist, method="complete")
plot(hclust_complete, labels = FALSE, main = "Standardized * Euclidian * complete")
abline(h = 10, col = "red")

groups.k2.complete <- cutree(hclust_complete, k=2) # cut the tree into 2 clusters
rect.hclust(hclust_complete, k=2, border="red")
aggregate(df_scores, list(groups.k2.complete), mean)

# Ward method suggests 3 clusters
# Complete-linkage method suggests 2 clusters

# Another validation method: Silhouette
plot(silhouette(groups.k3.ward, person.dist)) # average silhouette width = 0.07
plot(silhouette(groups.k2.complete, person.dist)) # average silhouette width = 0.04

# range < 0.25 - No substantial structure

# Observe cluster 'Ward'
df_scores$cluster_ward <- groups.k3.ward
pairs(df_scores[,1:7], col = df_scores$cluster_ward, lower.panel = NULL)

# Scale the data
std_data <- scale(df_scores[,1:7])

# Elbow plot

wssplot <- function(xx, nc=15, seed=1234){
  wss <- (nrow(std_data)-1)*sum(apply(std_data,2,var)) 
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(xx, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(std_data, nc=50)


###### K-means clustering ######

set.seed(321)

# k = 3 - 3 clusters and 50 initial random solutions
kmeans.k3 <- kmeans(std_data, 3, nstart=10) 

# All elements of the cluster output
attributes(kmeans.k3)

# Observe centroids of clusters
kmeans.k3$centers

# Classification
kmeans.k3$cluster

# Number of observations left in each cluster
kmeans.k3$size

# Comparison of Ward and K-Means clusters
table(groups.k3.ward, kmeans.k3$cluster)

# Validation - Silhouette
plot(silhouette(kmeans.k3$cluster, person.dist)) # average silhouette width = 0.14

# range < 0.25 - No substantial structure

set.seed(321)

# Solutions with 2 clusters: K-Means e Ward.D2

# K-Means
kmeans.k2 <- kmeans(std_data, 2, nstart=5)
kmeans.k2$centers
kmeans.k2$cluster
kmeans.k2$size
plot(silhouette(kmeans.k2$cluster, person.dist)) 
# Average silhouette width = 0.18. Improved a little.

# Hclust Ward.D2
plot(hclust_ward, labels = FALSE, main = "Standardized * Euclidian * Ward.D2")
groups.k2.ward <- cutree(hclust_ward, k=2) # cut the tree into 2 clusters
rect.hclust(hclust_ward, k=2, border="red")
plot(silhouette(groups.k2.ward, person.dist)) 
# Average silhouette width = 0.2. Improved! :-D

# Solutions with 4 clusters: K-Means e Ward.D2

# K-Means
kmeans.k4 <- kmeans(std_data, 4, nstart=10)
kmeans.k4$centers
kmeans.k4$cluster
kmeans.k4$size
plot(silhouette(kmeans.k4$cluster, person.dist)) 
# Average silhouette width = 0.12. Didn't improve :(

# Hclust Ward.D2
plot(hclust_ward, labels = FALSE, main = "Standardized * Euclidian * Ward.D2")
groups.k4.ward <- cutree(hclust_ward, k=4) # cut the tree into 4 clusters
rect.hclust(hclust_ward, k=4, border="red")
plot(silhouette(groups.k4.ward, person.dist)) 
# Average silhouette width = 0.07. Didn't improve.

# Best solution seems to be 2 clusters! Both in K-Means and in Ward.D2

# Comparison of 2 clusters solutions
table(kmeans.k2$cluster, groups.k2.ward)

###### PAM ######

set.seed(321)

pam.k2 <- pam(std_data, 2)

# Comparison of Ward and PAM clusters
table(groups.k2.ward, pam.k2$clustering)
plot(silhouette(pam.k2$clustering, person.dist)) # 0.1


# Comparison of  Ward and K-Means clusters
table(groups.k2.ward, kmeans.k2$cluster)

# Add best clusters to df_scores
df_scores$cluster_pam <- pam.k2$clustering
df_scores$cluster_kmeans <- kmeans.k2$cluster
df_scores$cluster_ward <- groups.k2.ward

# Scatter plot of the clusters with best results
pairs(df_scores[,1:7], pch=19, cex = 0.1, col = df_scores$cluster_ward, lower.panel = NULL)

pairs(df_scores[,1:7], pch=19, cex = 0.1, col = df_scores$cluster_kmeans, lower.panel = NULL)

pairs(df_scores[,1:7], pch=19, cex = 0.1, col = df_scores$cluster_pam, lower.panel = NULL)

    # Best graphic - PAM


###### Clustering of variables ######

var_dist <- as.dist(1-cor(df[,1:20]))
var_hclust <- hclust(var_dist)
var_name <- c("Q1A", "Q2A", "Q3A", "Q4A", "Q5A", "Q6A", "Q7A", "Q8A", "Q9A", "Q10A", "Q11A",
                "Q12A", "Q13A", "Q14A", "Q15A", "Q16A", "Q17A", "Q18A", "Q19A", "Q20A")
plot(var_hclust, label=var_name, hang=-1)


# Correlations and corrplot
correlation <- cor(df[,1:20])
round(correlation,3)

corrplot.mixed(correlation,
               order = "hclust",
               tl.pos = "lt",
               upper = "ellipse")

# PCA and Clustering
clusplot(pam.k2, labels = 2, col.p = pam.k2$clustering)


###### Gaussian Mixture Model ######

# GMM with 2 components
gmm2 <- Mclust(std_data[,1:7], G=2)
summary(gmm2, parameters = TRUE)

# Plot results
plot(gmm2, what = "density", )
plot(gmm2, what = "uncertainty")


###### BIC ######
BIC <- mclustBIC(std_data[,1:7])
plot(BIC)

# Model estimation
BICresults <- Mclust(std_data[,1:7], x = BIC)
summary(BICresults, parameters = TRUE)

# Classification
plot(BICresults, what = "classification")


###### Cluster Profile Classification ######

# View centroid for each cluster and understand what it means
pam.k2$medoids

pc7$loadings

names(df)

# Creation of a dataframe with the profile variables and the cluster to which each observation belongs
columns = c("education","urban","gender", "engnat", "age", "hand", "religion", "orientation", "race", "voted",
            "married", "familysize", "major", "continent", "cluster")
df_final <- data.frame(matrix(nrow = 10000, ncol = length(columns)))
colnames(df_final) = columns

df_final$cluster<- pam.k2$clustering
df_final[, 1:14] <- df[, 31:44]

# Separation of the final data frame into two distinct ones, each relative to its cluster to facilitate analysis
cluster1 <- filter(df_final, cluster == 1)
cluster2 <- filter(df_final, !cluster == 1)

# General statistics of each of the clusters
summary(cluster1[, 1:14])
summary(cluster2[, 1:14])

describe(cluster1[, 1:14])
describe(cluster2[, 1:14])

# Visualization and characterization of the cluster profile

par(mfrow = c(1,2))
barplot(table(cluster1$education)/nrow(cluster1),col = "red", ylim = c(0, 1), main="Distribution of variable 'education' in cluster 1",
        xlab="School level",ylab="Relative frequency")
barplot(table(cluster2$education)/nrow(cluster2),col = "blue", ylim = c(0, 1), main="Distribution of variable 'education' in cluster 2",
        xlab="School level",ylab="Relative frequency")

par(mfrow = c(1,2))
barplot(table(cluster1$urban)/nrow(cluster1),col = "red", ylim = c(0, 1), main="Distribution of variable 'urban' in cluster 1",
        xlab="Urban level",ylab="Relative frequency")
barplot(table(cluster2$urban)/nrow(cluster2),col = "blue", ylim = c(0, 1), main="Distribution of variable 'urban' in cluster 2",
        xlab="Urban level",ylab="Relative frequency")

par(mfrow = c(1,2))
barplot(table(cluster1$gender)/nrow(cluster1),col = "red", ylim = c(0, 1), main="Distribution of variable 'gender' in cluster 1",
        xlab="Gender",ylab="Relative frequency")
barplot(table(cluster2$gender)/nrow(cluster2),col = "blue", ylim = c(0, 1), main="Distribution of variable 'gender' in cluster 2",
        xlab="Gender",ylab="Relative frequency")

par(mfrow = c(1,2))
barplot(table(cluster1$engnat)/nrow(cluster1),col = "red", ylim = c(0, 1), main="Distribution of variable 'engnat' in cluster 1",
        xlab="English as a native language",ylab="Relative frequency")
barplot(table(cluster2$engnat)/nrow(cluster2),col = "blue", ylim = c(0, 1), main="Distribution of variable 'engnat' in cluster 2",
        xlab="English as a native language",ylab="Relative frequency")

par(mfrow = c(1,2))
barplot(table(cluster1$age)/nrow(cluster1),col = "red", ylim = c(0, 1), main="Distribution of variable 'age' in cluster 1",
        xlab="Age [in years]",ylab="Relative frequency")
barplot(table(cluster2$age)/nrow(cluster2),col = "blue", ylim = c(0, 1), main="Distribution of variable 'age' in cluster 2",
        xlab="Age [in years]",ylab="Relative frequency")

par(mfrow = c(1,2))
barplot(table(cluster1$religion)/nrow(cluster1),col = "red", ylim = c(0, 1), main="Distribution of variable 'religion' in cluster 1",
        xlab="Religion",ylab="Relative frequency")
barplot(table(cluster2$religion)/nrow(cluster2),col = "blue", ylim = c(0, 1), main="Distribution of variable 'religion' in cluster 2",
        xlab="Religion",ylab="Relative frequency")

par(mfrow = c(1,2))
barplot(table(cluster1$race)/nrow(cluster1),col = "red", ylim = c(0, 1), main="Distribution of variable 'race' in cluster 1",
        xlab="Race",ylab="Relative frequency")
barplot(table(cluster2$race)/nrow(cluster2),col = "blue", ylim = c(0, 1), main="Distribution of variable 'race' in cluster 2",
        xlab="Race",ylab="Relative frequency")

par(mfrow = c(1,2))
barplot(table(cluster1$married)/nrow(cluster1),col = "red", ylim = c(0, 1), main="Distribution of variable 'married' in cluster 1",
        xlab="Social status",ylab="Relative frequency")
barplot(table(cluster2$married)/nrow(cluster2),col = "blue", ylim = c(0, 1), main="Distribution of variable 'married' in cluster 2",
        xlab="Social status",ylab="Relative frequency")

par(mfrow = c(1,2))
barplot(table(cluster1$voted)/nrow(cluster1),col = "red", ylim = c(0, 1), main="Distribution of variable 'voted' in cluster 1",
        xlab="Voted in any national elections during the past year",ylab="Relative frequency")
barplot(table(cluster2$voted)/nrow(cluster2),col = "blue", ylim = c(0, 1), main="Distribution of variable 'voted' in cluster 2",
        xlab="Voted in any national elections during the past year",ylab="Relative frequency")

par(mfrow = c(1,2))
barplot(table(cluster1$familysize)/nrow(cluster1),col = "red", ylim = c(0, 1), main="Distribution of variable 'familysize' in cluster 1",
        xlab="Number of children in the family",ylab="Relative frequency")
barplot(table(cluster2$familysize)/nrow(cluster2),col = "blue", ylim = c(0, 1), main="Distribution of variable 'familysize' in cluster 2",
        xlab="Number of children in the family",ylab="Relative frequency")

par(mfrow = c(1,2))
barplot(table(cluster1$continent)/nrow(cluster1),col = "red", ylim = c(0, 1), main="Distribution of variable 'continent' in cluster 1",
        xlab="Continents",ylab="Relative frequency")
barplot(table(cluster2$continent)/nrow(cluster2),col = "blue", ylim = c(0, 1), main="Distribution of variable 'continent' in cluster 2",
        xlab="Continents",ylab="Relative frequency")
