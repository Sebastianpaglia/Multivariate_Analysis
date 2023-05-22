# MVA PROJECT GROUP 4: BANK CHURING RATE - R SCRIPT

## REQUIRED PACKAGES

library(VIM)
library(mice)
library(corrplot)
library(missForest)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(dplyr)
library(fpc)
library(reshape2)
library(tidyr)
library(ggplot2)
library(stats)
library(cluster)
library(colorspace)
library(patchwork)
library(tidyverse)
library(ggpubr)
library(NbClust)
library(DAAG)
library(party)
library(rpart)
library(rpart.plot)
library(mlbench)
library(pROC)
library(tree)
library(C50)
library(printr)
library(randomForest)
library(caret)
library(ROSE)
library(xgboost)
library(DiagrammeR)
library(fabricatr)

## IMPORT DATASET

ds = read.csv('database.csv')

# Remove undesired attributes

ds = ds[-1]

# Convert 'Unknown' into NA

ds[ds == 'Unknown'] = NA


# Convert to Factors the categorical entries

ds$Card_Category = as.factor(ds$Card_Category)
ds$Gender = as.factor(ds$Gender)
ds$Attrition_Flag = as.factor(ds$Attrition_Flag)
ds$Education_Level = as.factor(ds$Education_Level)
ds$Income_Category = as.factor(ds$Income_Category)
ds$Marital_Status = as.factor(ds$Marital_Status)

# Missing Values

## Explore where are the NA

ds_miss = aggr(ds, col=mdc(1:5), numbers=TRUE, sortVars=TRUE, labels=names(ds), cex.axis=.5, gap=1, ylab=c("Proportion missing values","Missing values pattern"))


## Visualization of numeric values correlation

numeric = ds[, sapply(ds, is.numeric)]
correlation <- cor(numeric, method = 'spearman') 
corrplot(correlation, order="AOE", method="color", addCoef.col = "gray",tl.col = "black", tl.cex=.8, number.digits = 2, number.cex = 0.7)

#From this graphic, we can infer the correlation between the different numerical variables, being on a blue scale the positive correlations and on red scale the negative. 

# The most redundant inferations among the positive correlations are:
# * Total_Trans_Ct Vs Total_Trans_Amt (0.88): while more transactions are made, more amount of money are transfered
# * Customer_Age Vs Months_on_book (0.77): the more ages that the Customer has, the more months he remains as client.
# * Total_Revolving_Bal Vs Avg_Utilization_Ratio (0.71):
# * Credit_Limit Vs Avg_Open_To_Buy (0.93):

# The most redundant inferations among the negative correlations are:
# * Avg_Open_To_Buy Vs Avg_Utilization_Ratio (-0.79): Open to Buy Credit Line (Average of last 12 months), less Average Card Utilization Ratio

# LOGICAL IMPUTATION

# Before imputating with KNN/MICE algorithms, we should try to replicate see if we can imput logically. First of all, the three variables
# that have missing values are categorical, which makes things a little bit more difficult, when we want to see if they are correlated. 

dat <- data.frame(table(ds$Income_Category,ds$Education_Level))
names(dat) <- c("Income","Education","Count")
ggplot(data=dat, aes(x=Income, y=Count, fill=Education)) + geom_bar(stat="identity")

# From a general first intuition, we may thing that there exists a correlation between Income and Education Level: 

chisq.test(ds$Income_Category,ds$Education_Level) 

# A chisq-test (p-value=0.22) confirmed the suspicions show in the plot, so unfortunately they are not related.

# Secondly, we should look into Martial_Status and Number of Dependents: 

chisq.test(ds$Marital_Status,ds$Dependent_count) # It is slightly significant so they are slightly dependent

prop.table(table(ds$Marital_Status))
prop.table(table(ds$Marital_Status,ds$Dependent_count),1)

dat <- data.frame(table(ds$Marital_Status,ds$Dependent_count))
names(dat) <- c("Status","Dependents","Count")
ggplot(data=dat, aes(x=Status, y=Count, fill=Dependents, colors='black')) + geom_bar(stat="identity")+scale_fill_brewer(palette = "RdYlBu")


# We were partially right, and we can see slightly some of the expected behaviors: the proportion of people w/ 0 dependence is lower in married,
# and single people tend to have the lowest frequency for large number of dependents.
# Unfortunately,the dependency also turns out to be so slow that we can not define a way to exploit it.

# Imputation KNN

# Imputation Random Forest vs MICE

set.seed(17)
mf_imp <- missForest(ds, variablewise=T, verbose=T) 
ds_mf <- mf_imp$ximp

imputed_df=mice(ds, m=5, method = 'polyreg', seed = 500)
ds_mice<-complete(imputed_df)

## LOOK FOR EDUCATION LEVEL
before = data.frame(table(ds$Education_Level))
after_mf = data.frame(table(ds_mf$Education_Level))
after_mice = data.frame(table(ds_mice$Education_Level))
el_frame = data.frame(before,before[2]/(dim(ds)[1]-sum(is.na(ds$Education_Level))),after_mf[2],after_mf[2]/dim(ds)[1],after_mice[2],after_mice[2]/dim(ds)[1]);
colnames(el_frame)= c('Category','Total_Before','Freq_Before','Total_MF','Freq_MF','Total_MICE','Freq_MICE')
frame
boxplot(el_frame$Freq_Before,el_frame$Freq_MF,el_frame$Freq_MICE)

## LOOK FOR MARITAL STATUS

before = data.frame(table(ds$Marital_Status))
after_mf = data.frame(table(ds_mf$Marital_Status))
after_mice = data.frame(table(ds_mice$Marital_Status))
ms_frame = data.frame(before,before[2]/(dim(ds)[1]-sum(is.na(ds$Marital_Status))),after_mf[2],after_mf[2]/dim(ds)[1],after_mice[2],after_mice[2]/dim(ds)[1]);
colnames(ms_frame)= c('Category','Total_Before','Freq_Before','Total_MF','Freq_MF','Total_MICE','Freq_MICE')
frame
boxplot(ms_frame$Freq_Before,ms_frame$Freq_MF,ms_frame$Freq_MICE)

## LOOK FOR INCOME CATEGORIES

before = data.frame(table(ds$Income_Category))
after_mf = data.frame(table(ds_mf$Income_Category))
after_mice = data.frame(table(ds_mice$Income_Category))
ic_frame = data.frame(before,before[2]/(dim(ds)[1]-sum(is.na(ds$Income_Category))),after_mf[2],after_mf[2]/dim(ds)[1],after_mice[2],after_mice[2]/dim(ds)[1]);
colnames(ic_frame)= c('Category','Total_Before','Freq_Before','Total_MF','Freq_MF','Total_MICE','Freq_MICE')
frame
boxplot(ic_frame$Freq_Before,ic_frame$Freq_MF,ic_frame$Freq_MICE)

# We can see that income category is not good enough. We will explore different new approaches to that specific category

## FREQUENCY PLOT: 

el_matrix = matrix(, nrow = 3, ncol = 6)
el_matrix[1,] = el_frame$Freq_Before
el_matrix[2,] = el_frame$Freq_MF
el_matrix[3,] = el_frame$Freq_MICE

ms_matrix = matrix(, nrow = 3, ncol = 3)
ms_matrix[1,] = ms_frame$Freq_Before
ms_matrix[2,] = ms_frame$Freq_MF
ms_matrix[3,] = ms_frame$Freq_MICE

ic_matrix = matrix(, nrow = 3, ncol = 5)
ic_matrix[1,] = ic_frame$Freq_Before
ic_matrix[2,] = ic_frame$Freq_MF
ic_matrix[3,] = ic_frame$Freq_MICE


par(mfrow=c(1,3))
barplot(el_matrix, beside = TRUE, main = 'Freq. Education Level', names.arg = el_frame$Category, col = c("peachpuff", "skyblue",'orangered4'),legend.text = c("Before", "MissForest","MICE"),args.legend = list(x='topright'))
barplot(ms_matrix, beside = TRUE, main = 'Freq. Marital Status', names.arg = ms_frame$Category, col = c("peachpuff", "skyblue",'orangered4'),legend.text = c("Before", "MissForest","MICE"),args.legend = list(x='topleft'))
barplot(ic_matrix, beside = TRUE, main = 'Freq. Income Categories', names.arg = ic_frame$Category, col = c("peachpuff", "skyblue",'orangered4'),legend.text = c("Before", "MissForest","MICE"),args.legend = list(x='top'))

# Correlation: look if that attribute is extremely correlated to another one, if so, we could imput with that info

# Categorical vs Categorical
chisq.test(ds_mf$Income_Category,ds_mf$Marital_Status)  # Not significant
chisq.test(ds_mf$Income_Category,ds_mf$Education_Level) # Not significant

# Categorical vs numerical
par(mfrow=c(3,3))
boxplot(ds_mf$Customer_Age~ds_mf$Income_Category)
boxplot(ds_mf$Months_on_book~ds_mf$Income_Category)
boxplot(ds_mf$Total_Revolving_Bal~ds_mf$Income_Category)
boxplot(ds_mf$Total_Trans_Ct~ds_mf$Income_Category)
boxplot(ds_mf$Total_Relationship_Count~ds_mf$Income_Category)
boxplot(ds_mf$Total_Amt_Chng_Q4_Q1~ds_mf$Income_Category)
boxplot(ds_mf$Avg_Utilization_Ratio~ds_mf$Income_Category)
boxplot(ds_mf$Total_Trans_Ct~ds_mf$Income_Category)
boxplot(ds_mf$Total_Trans_Amt~ds_mf$Income_Category)

#We can not observe strong correlations with the numerical attributes. 


# OUTLIERS

# DETECTING UNIVARIATE OUTLIERS

X = ds_mice[, sapply(ds_mice, is.numeric)]
names(X)

boxplot(X) # We have outliters with the default range of 1.5 for the following variables:   Credit_Limit, Avg_Open_To_Buy, Total_Trans_Amt
boxplot(X, range = 3) # Applying a range of 3 the number of outliers reduces a lot (Total_Trans_Amt)

## Total_Trans_Amt
out_trans_amt <- boxplot.stats(X$Total_Trans_Amt, coef = 3)$out
out_ind_trans_amt <- which(X$Total_Trans_Amt %in% c(out_trans_amt))
Y_trans_amt <- data.frame(X[out_ind_trans_amt, ],ds[out_ind_trans_amt,1])


# DETECTING MULTIVARIATE OUTLIERS WITH MAHALANOBIS DSTANCE

### X <- ds_mf[, sapply(ds_mf, is.numeric)]
X = scale(X, center = FALSE, scale = TRUE)
mdi = mahalanobis(X,center=apply(X,2,mean),cov=var(X), tol=1e-18). #probar sin "tol"
plot(density(mdi))
cutoff <- qchisq(p = 0.99 , ncol(X))
## Display observation whose distance greater than cutoff value
a = X[mdi>cutoff,]
Z <-data.frame(X[mdi>cutoff,],ds[mdi>cutoff,1])

# ASSESSING THE GROUP INSIDE THE MULTIVARIATE OUTLIERS

Outliers <-data.frame(ds_mice[mdi>cutoff,])
no_outliers <-data.frame(ds_mice[mdi<=cutoff,])
prop.table(table(Outliers$Gender))
prop.table(table(ds_mice$Gender))
aa = table(Outliers$Income_Category)
colnames(aa) = c('')
Outliers$Income_Category = factor(Outliers$Income_Category,levels = c('Less than $40K','$40K - $60K','$60K - $80K','$80K - $120K','$120K +'))
ds_mice$Income_Category = factor(ds_mice$Income_Category,levels = c('Less than $40K','$40K - $60K','$60K - $80K','$80K - $120K','$120K +'))
ii = matrix(, nrow = 2, ncol = 5)
ii[2,] = prop.table(table(Outliers$Income_Category))
ii[1,] = prop.table(table(ds_mice$Income_Category))
par(mfrow=c(1,1))
barplot(ii, beside = TRUE, main = 'Freq. Income Category', names.arg = levels(ds_mice$Income_Category) , col = c("peachpuff", "skyblue"),legend.text = c("Dataset", "Outliers"),args.legend = list(x='top'))


prop.table(table(Outliers$Marital_Status))
prop.table(table(ds_mice$Marital))

prop.table(table(Outliers$Card_Category))
prop.table(table(ds_mice$Card_Category))

##### PCA ######
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/


R <- cor(ds_mice[, sapply(ds_mice, is.numeric)])
corrplot(R, method = "number",number.cex = 0.75, tl.col = "black",tl.cex=.8, number.digits = 2)
colnames(ds_mice)
ds_mice<-ds_mice[,-c(15,18)]

##### PCA 
ds_mice.pca = PCA(ds_mice[, sapply(ds_mice, is.numeric)], scale.unit=TRUE, ncp=2, graph=F) 
summary(ds_mice.pca)

res.desc <- dimdesc(ds_mice.pca, axes = c(1:2), proba = 0.99)
# Description of dimension 1 and 2
res.desc$Dim.1
res.desc$Dim.2

eig.val <- get_eigenvalue(ds_mice.pca)
eig.val
fviz_eig(ds_mice.pca, addlabels = TRUE, ylim = c(0, 20))

var <- get_pca_var(ds_mice.pca)
var$coord
var$cor
var$cos2
var$contrib

####Quality of representation
fviz_cos2(ds_mice.pca, choice = "var", axes = 1, ylim=c(0,1))
fviz_cos2(ds_mice.pca, choice = "var", axes = 2, ylim=c(0,1))
fviz_cos2(ds_mice.pca, choice = "var", axes = 1:2, col="red", ylim= c(0,1))

###Correlation circle
fviz_pca_var(ds_mice.pca, repel=TRUE,gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),col.var = "cos2")

###Contribution
fviz_contrib(ds_mice.pca, choice = "var", axes = 1, top = 20, ylim=c(0,40))
fviz_contrib(ds_mice.pca, choice = "var", axes = 2, top = 20, ylim=c(0,40))
fviz_contrib(ds_mice.pca, choice = "var", axes = (1:2), top = 20, col="red", ylim=c(0,40))

#Contribution circle
fviz_pca_var(ds_mice.pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

####### MCA ########


ds_cat= no_outliers[,c(3,5,6,7,8)]

res.mca = MCA(ds_cat,ncp=5,graph = FALSE)

eig.val = get_eigenvalue(res.mca)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 20))

# CAREFUL AS THERE ARE A LOT OF POINTS
#fviz_mca_biplot(res.mca,repel = TRUE, gtheme = theme_minimal())


var <- get_mca_var(res.mca)
fviz_mca_var(res.mca, repel = TRUE, ggtheme = theme_minimal())
fviz_mca_var(res.mca, choice = "mca.cor",repel = TRUE,ggtheme = theme_minimal())

fviz_mca_var(res.mca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE, ggtheme = theme_minimal())
corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(res.mca, choice = "var", axes = 1:2)

####### MFA ########

ds = read.csv('no_outliers.csv')
ds = ds[,-1]
ds = ds[,c(3,1,2,4:19)]
summary(ds)

res.mfa <- MFA(ds, group = c(2, 3, 2, 4, 7, 1), type = c("s", "n", "n", "s", "s","s"),name.group = c("personal_num","personal_cat","personal_money","interactions", "movements_ind", "move_ratio"), num.group.sup = c(2), graph = FALSE)
fviz_screeplot(res.mfa,addlabels = TRUE)

group <- get_mfa_var(res.mfa, "group")
group
fviz_mfa_var(res.mfa, "group")

# We see that the groups move_ratio, personal_money and movements_ind compose the first dimension, so they are very correlated
# and the dimension 2 is composed mainly by the interactions and personal_num

####

fviz_cos2(res.mfa, choice = "group", axes = 1:2) # cosine is an index about quality representation
# we see that the personal_num has a highest represenation
fviz_contrib(res.mfa, choice = "group", axes = 1:2) # here we check the contribution
# we see that that the first 3 groups contribute a little bit more than the others but not enough


#### Quantitative variables

quanti.var <- get_mfa_var(res.mfa, "quanti.var")
quanti.var 

###Correlation between quantitative variables and dimensions

fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", col.var.sup = "violet", repel = TRUE) 

# Checking the correlation between Credit_Limit and Avg_Open_To_Buy we see there is 
# a cor of 0.99 (tiene sentido porque una sale de la otra, es la resta)
cor.test(ds$Credit_Limit, ds$Avg_Open_To_Buy)
# We also see that both variables are negatively correlated with Avg_utilization_ratio
# it means we will have to remove one of these 3 variables before compute the model (let's keep the credit limit)


fviz_mfa_var(res.mfa, "quanti.var", col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),col.var.sup = "violet", repel = TRUE,geom = c("point", "text"))
fviz_contrib(res.mfa, choice = "quanti.var", axes = 1:2) # here we have the contribution for each feature 

fviz_contrib(res.mfa, choice = "quanti.var", axes = 1,palette = "jco") # the same as the plot above bul only for dim 1
# contribution of the groups 
fviz_contrib(res.mfa, choice = "quanti.var", axes = 2,palette = "jco") # the same but only for dim 2

fviz_mfa_axes(res.mfa) # we see that taste is hightly correlated with Dim 1
# la mayoria de grupos utilizan las dims 1 y 2


## CLUSTERING 

# CLUSTER

df = read.csv('no_outliers.csv')


# Tranformations over the dataset before applying the algorithms
ds= df[, sapply(df,is.numeric)] 
ds = ds[,-c(1)]
ds = scale(ds)
# DETERMINIG THE NUMBER OF CLUSTERS


###   HIERARCHICAL CLUSTERING TREE

distancia<-dist(ds,"euclidean")
fit<-hclust(distancia, "ward.D")
plot(fit, main="H.Clustering with euclidean distance and method WARD")
rect.hclust(fit, k=2, border=6)

res.hcpc <- HCPC(ds)
plot(res.hcpc,choice="bar")



par(mfrow=c(2,1))

## ELBOW METHOD : Observe the whitin group sum of squares
fviz_nbclust(ds, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)+  labs(subtitle = "Elbow method")
## SILHOUETTE METHOD: Observing how far apart are from neighboring clusters. 
fviz_nbclust(ds, kmeans, method = "silhouette")+ labs(subtitle = "Silhouette method")
## GAP METHOD'S
fviz_nbclust(ds, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)  + labs(subtitle = "Gap statistic method")

par(mfrow=c(1,1))
# K-MEANS

## The results are:
## ELBOW METHOD: 4 Clusters Suggested
## SILHOUETTE METHOD: 2 Clusters Suggested
## GAP METHOD: 6 Clusters Suggested


# NBCLUST: Results of a lot of methods
# NbClust(ds, diss = NULL, distance = "euclidean",min.nc = 2, max.nc = 6, method = "kmeans",index='gamma')
a = data.frame(c(2,3,3,3,3,5,3,3,3,6,2,2,3,3,3,4,3,2,4,2,3,6))

barplot((table(a)),main="Suggested Number of Clusters",
        xlab="Clusters",
        ylab="Times Suggested",
        col=rgb(0.8,0.1,0.1,0.6,0.7))

# Do to the size of our dataset, some very useful functions to obtain results with more metrics (such as NBClust='all') could not be
# performed as the expected running was of several hours.

# According to the previous results, we performed the analysis with 3 clusters

km_clusters <- kmeans(x = ds, centers = 3, nstart = 50)


# VISUALIZATION OF THE CLUSTER
fviz_cluster(object = km_clusters, data = ds, show.clust.cent = FALSE,
             ellipse.type = "euclid", star.plot = FALSE, repel = FALSE) + labs(title = "Clustering results with K-means") +
  theme_bw() +  theme(legend.position = "none") 


# CREATE NEW VARIABLE WITH CLSUTER NUMBER
df$cluster = km_clusters$cluster
df$cluster = factor(df$cluster)

# PROFILE THE CLUSTER WITH FACTOMINER TECHNIQUES

profiles = catdes(df,21)
profiles$test.chi2
profiles$quanti.var
profiles$quanti
profiles$category

# Group 1: It is a group that consists of very high earners, with high credit limit, who are typically male (79.7%) 
#and accounting for the larger proportion of not married individuals. This is a group that uses the credit card very
#frequently but it has few contacts with the banking services. It has a high proportion of customers that are not attired, 
#so it could mean that they are content with the credit card services.


# Group 2: It consists of people with low income, and mostly female and married people. This group has obviously 
#low credit limits but more utilization of the credit card than the other two groups: this indicates that people 
#with family in general need to spend money on a wide range of products, activities and services. Additionally, 
#they contact the bank more frequently than the other groups. Additionally, the proportion of attrition is also low.


#Group 3: In this group there is not a significant distinction between genders or marital status, but there is a very 
#clear increase in the number of contacts with the bank. It is a group with an income between the first two ones and with 
#a low utilization ratio. It can be seen that the the number of attrited customers is high among this group


# PERCENTAGE OF THE RESPONSE VARIABLE

#Third group more prone to churn
prop.table(table(df$Attrition_Flag,df$cluster),2)






############################## LDA ##############################
df = read.csv('no_outliers.csv')

# TREATMENT OF THE DATASET
df$Attrition_Flag = factor(df$Attrition_Flag)

df$f.customer_age <-df$Customer_Age
df$f.customer_age[df$Customer_Age<=35]<-"Young_adults"
df$f.customer_age[df$Customer_Age>35 & df$Customer_Age <= 55]<-"Middle-aged_adults"
df$f.customer_age[df$Customer_Age>55]<-"Older adults"

df$Gender = as.factor(df$Gender)
df$Dependent_count = as.factor(df$Dependent_count)
df$Education_Level = as.factor(df$Education_Level)
df$Marital_Status = as.factor(df$Marital_Status)
df$Income_Category <- factor(df$Income_Category, labels = c("High_Income","Low_Income","Medium_Income","Medium_Income","Low_Income"))
df$Card_Category = as.factor(df$Card_Category)
df$f.months_on_book <-as.factor(split_quantile(x = df$Months_on_book, type = 4))
df$Total_Relationship_Count = factor(df$Total_Relationship_Count)
df$Months_Inactive_12_mon = as.factor(df$Months_Inactive_12_mon)
df$Contacts_Count_12_mon = as.factor(df$Contacts_Count_12_mon)
df$Credit_Limit = as.numeric(df$Credit_Limit)
df$Total_Revolving_Bal = as.numeric(df$Total_Revolving_Bal)
df$Avg_Open_To_Buy = as.numeric(df$Avg_Open_To_Buy)
df$Total_Amt_Chng_Q4_Q1 = as.numeric(df$Total_Amt_Chng_Q4_Q1)
df$Total_Trans_Amt = as.numeric(df$Total_Trans_Amt)
df$Total_Trans_Ct = as.numeric(df$Total_Trans_Ct)
df$Total_Ct_Chng_Q4_Q1 = as.numeric(df$Total_Ct_Chng_Q4_Q1)
df$Avg_Utilization_Ratio = as.numeric(df$Avg_Utilization_Ratio)


par(mfrow=c(3,3))
hist(df$Credit_Limit, main="Credit Limit")
hist(df$Total_Revolving_Bal, main="Total Revolving bal")
hist(df$Avg_Open_To_Buy, main="Avg Open to Buy")
hist(df$Total_Amt_Chng_Q4_Q1, main="Total amt chng Q4-Q1")
hist(df$Total_Trans_Amt, main="total trans amt")
hist(df$Total_Trans_Ct, main="total trans amt")
hist(df$Total_Ct_Chng_Q4_Q1, main="total ct chng Q4-Q1")
hist(df$Avg_Utilization_Ratio, main="avg utilization ratio")
par(mfrow=c(1,1))

par(mfrow=c(3,3))
qqPlot(df$Credit_Limit) 
qqPlot(df$Total_Revolving_Bal)
qqPlot(df$Avg_Open_To_Buy) 
qqPlot(df$Total_Amt_Chng_Q4_Q1) 
qqPlot(df$Total_Trans_Amt) 
qqPlot(df$Total_Trans_Ct) 
qqPlot(df$Total_Ct_Chng_Q4_Q1) 
qqPlot(df$Avg_Utilization_Ratio)
par(mfrow=c(1,1))

# we can see that most of them does not follow a normal distribution, specially on Credit_Limit, 
#Avg_Open_To_Buy, Total_Amt_Chng_Q4_Q1, Total_Trans_Amt and Total_Ct_Chng_Q4_Q1, in order to correct this we will 
#try with logarithmic transformation
par(mfrow=c(3,2))
qqPlot(df$Credit_Limit)
qqPlot(log(df$Credit_Limit))
qqPlot(df$Avg_Open_To_Buy)
qqPlot(log(df$Avg_Open_To_Buy)) 
qqPlot(df$Total_Trans_Amt) 
qqPlot(log(df$Total_Trans_Amt))
par(mfrow=c(1,1))

#in the qqplot seems to get closer to a normal distribution, however we will test normality through Shapiro test, 
#but the pvalues obtained are very close to 0 so we have statistical arguments to reject the null hypothesis of normality
shapiro.test(log(df$Credit_Limit)[0:5000])
shapiro.test(log(df$Avg_Open_To_Buy)[0:5000])
shapiro.test(log(df$Total_Trans_Amt)[0:5000])

# Here we check for Multivariate Gaussian Conditions 
royston_test <- mvn(data = df[0:2000,c(13:20)], mvnTest = "royston", multivariatePlot = "qq")
royston_test$multivariateNormality
#we have a value very close to 0, so we can reject our null hypothesis thus our data does not follow a multivariate normality


boxM(data = df[,c(13,14,16:20)], grouping = df[,1])


###############Applying LDA ##################
##Step1: Split Data into training (80%) and test set (20%) (80/20 is a common strategy but not the only one)
set.seed(123)
training.samples <- df$Attrition_Flag %>% createDataPartition(p = 0.8, list = FALSE)
train.data <- df[training.samples, c(1,13,14,16:20)]
test.data <- df[-training.samples, c(1,13,14,16:20)]
##Checking sampling
nrow(train.data)
nrow(test.data)

##Step 2. Calculating LDA function
# Estimate preprocessing parameters
preproc.param <- train.data %>%preProcess(method = c("center", "scale"))
# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)

###LDA
model_lda <- lda(Attrition_Flag ~ ., data = train.transformed)
model_lda

plot(model_lda)

##Step 3. Make predictions
predictions_lda <- model_lda %>% predict(test.transformed)
table(test.transformed$Attrition_Flag, predictions_lda$class, dnn = c("Actual Class", "Predicted Class"))
mean(predictions_lda$class==test.transformed$Attrition_Flag) #accuracy
mean(test.transformed$Attrition_Flag != predictions_lda$class) # error


# We are going to perform QDA because the sample size is bigger and it does not assume the equality of group covariance matrices:
# Fit the model
model_qda <- qda(Attrition_Flag ~ ., data = train.transformed)
model_qda
# Make predictions
predictions_qda <- model_qda %>% predict(test.transformed)
# Model accuracy
mean(predictions_qda$class==test.transformed$Attrition_Flag) #accuracy
mean(test.transformed$Attrition_Flag != predictions_qda$class) # error
table(test.transformed$Attrition_Flag, predictions_qda$class, dnn = c("Actual Class", "Predicted Class"))

##### TREES ################

# TREES
df = read.csv('no_outliers.csv')

df$Card_Category = as.factor(df$Card_Category)
df$Gender = as.factor(df$Gender)
df$Attrition_Flag = as.factor(df$Attrition_Flag)
df$Education_Level = as.factor(df$Education_Level)
df$Income_Category = factor(df$Income_Category,levels = c('Less than $40K','$40K - $60K','$60K - $80K','$80K - $120K','$120K +'))
df$Marital_Status = as.factor(df$Marital_Status)


# BALANCING THE DATASET
df_bal =ovun.sample(Attrition_Flag ~ ., data = df, method = "both",N=10000,p=0.35)$data
table(df_bal$Attrition_Flag)
prop.table(table(df_bal$Attrition_Flag))

# TRAIN TEST SPLIT
set.seed(1234)
ind <- sample(2, nrow(df_bal), replace = T, prob = c(0.7, 0.3))
train <- df_bal[ind == 1,]
test <- df_bal[ind == 2,]

# ONE STANDAR DEVIATIO RULERULE

tree <- rpart(Attrition_Flag~ ., data = train, cp = 0.005)
printcp(tree)
plotcp(tree)
head(tree$cptable, 10)
xerror <- tree$cptable[,"xerror"]
imin.xerror <- which.min(xerror)
tree$cptable[imin.xerror, ]
upper.xerror <- xerror[imin.xerror] + tree$cptable[imin.xerror, "xstd"]
icp <- min(which(xerror <= upper.xerror))
cp <- tree$cptable[icp, "CP"]
tree <- prune(tree, cp = cp) 
rpart.plot(tree)


importance <- tree$variable.importance 
importance <- round(100*importance/sum(importance), 1)
ii = importance[importance >= 1]

pie(ii)


# TRAIN AND TEST CONFUSION MATRICES

p <- predict(tree, train, type = 'class')
confusionMatrix(p, train$Attrition_Flag, positive="Attrited Customer")

p2 <- predict(tree, test, type = 'class')
confusionMatrix(p2, test$Attrition_Flag, positive="Attrited Customer")




# MODELING WITH THE PACKAGE CARET
caret.rpart <- train(Attrition_Flag ~ ., method = "rpart", data = train, 
                     tuneLength = 20,
                     trControl = trainControl(method = "cv", number = 10)) 
ggplot(caret.rpart)
rpart.plot(caret.rpart$finalModel)

caret.rpart <- train(Attrition_Flag ~ ., method = "rpart", data = train, 
                     tuneLength = 20,
                     trControl = trainControl(method = "cv", number = 10,
                                              selectionFunction = "oneSE")) 
var.imp <- varImp(caret.rpart)
plot(var.imp)
pred <- predict(caret.rpart, newdata = test)


# MODELING WITH THE PACKAGE CONDITIONAL TREES

tree2 <- ctree(Attrition_Flag ~ ., data = train) 
plot(tree2)
test2 = predict(tree2, newdata=test, type="response")

# MODELING WITH THE PACKAGE C5.0
model <- C5.0(Attrition_Flag ~., data=train)
plot(model)
results <- predict(object=model, newdata=test, type="class")
table(results, test$Attrition_Flag)


# COMPARISON OF THE 4 PACKAGES
confusionMatrix(p2, test$Attrition_Flag, positive="Attrited Customer") 
confusionMatrix(pred, test$Attrition_Flag,positive="Attrited Customer")
confusionMatrix(results, test$Attrition_Flag, positive="Attrited Customer")
confusionMatrix(test2, test$Attrition_Flag, positive="Attrited Customer")

a = confusionMatrix(results, test$Attrition_Flag, positive="Attrited Customer")

draw_confusion_matrix(a)

## RANDOM FORESTS

library(randomForest)
bagtrees <- randomForest(Attrition_Flag ~ ., data = train, mtry = ncol(train) - 1) 
bagtrees
plot(bagtrees, main = "")
legend("right", colnames(bagtrees$err.rate), lty = 1:5, col = 1:6)
pred2 <- predict(bagtrees, newdata = test)
caret::confusionMatrix(pred2, test$Attrition_Flag, positive="Attrited Customer")
###Random Forest
rf <- randomForest(Attrition_Flag ~ ., data = train,ntree=5000)
plot(rf,main="")
legend("right", colnames(rf$err.rate), lty = 1:5, col = 1:6)
importance(rf)
varImpPlot(rf)
pred3 <- predict(rf, newdata = test)
a = caret::confusionMatrix(pred3, test$Attrition_Flag,positive="Attrited Customer") 


draw_confusion_matrix(a)
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Existing', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Attrited', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Existing', cex=1.2, srt=90)
  text(140, 335, 'Attrited', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  


### XGBOOST ###

# Importing from csv file
df_xgb <- read_csv("no_outliers.csv")

# set a random seed & shuffle data frame
set.seed(1234)
df_xgb <- df_xgb[sample(1:nrow(df_xgb)), ]

## Data preprocessing for xgboost

# Removing target label from df
df_xgb_labelRemoved <- df_xgb %>% select(-Attrition_Flag)
# Creating Boolean vector for target label
df_xgb_labels <- df_xgb %>% select(Attrition_Flag) %>% mutate(Attrition_Flag = ifelse(Attrition_Flag == "Existing Customer",0,1))
# Extracting only numeric attributes from df_labelRemoved
df_xgb_numeric <- df_xgb_labelRemoved %>% select_if(is.numeric)

# convert categorical factors into one-hot encoding
xgb_gender <- model.matrix(~Gender-1, df_xgb_labelRemoved)
xgb_education_level <- model.matrix(~Education_Level-1, df_xgb_labelRemoved)
xgb_marital_status <- model.matrix(~Marital_Status-1, df_xgb_labelRemoved)
xgb_income_category <- model.matrix(~Income_Category-1, df_xgb_labelRemoved)
xgb_card_category <- model.matrix(~Card_Category -1, df_xgb_labelRemoved)

# add our one-hot encoded variables and convert the dataframe into a matrix
df_xgb_numeric <- cbind(df_xgb_numeric, xgb_gender, xgb_education_level, xgb_marital_status, xgb_income_category, xgb_card_category)
df_xgb_matrix <- data.matrix(df_xgb_numeric)
df_xgb_labels <- data.matrix(df_xgb_labels)

# get the 70/30 training test split
numberOfTrainingSamples <- round(length(df_xgb_labels) * .7)

# training data
train_xgb_data <- df_xgb_matrix[1:numberOfTrainingSamples,]
train_xgb_labels <- df_xgb_labels[1:numberOfTrainingSamples]

# testing data
test_xgb_data <- df_xgb_matrix[-(1:numberOfTrainingSamples),]
test_xgb_labels <- df_xgb_labels[-(1:numberOfTrainingSamples)]

# put our testing & training data into two seperates Dmatrix objects
dtrain_xgb <- xgb.DMatrix(data = train_xgb_data, label= train_xgb_labels)
dtest_xgb <- xgb.DMatrix(data = test_xgb_data, label= test_xgb_labels)

## Training xgboost model
model_xgb <- xgboost(data = dtrain_xgb, # the data           
                           max.depth = 3, # the maximum depth of each decision tree
                           nround = 60, # max number of boosting iterations
                           objective = "binary:logistic", # the objective function 
                           scale_pos_weight = 2, # control for imbalanced classes
                           eval_metric = "error")

# generate predictions for our held-out testing data
pred <- predict(model_xgb, dtest_xgb)

# get & print the classification error
err <- mean(as.numeric(pred > 0.5) != test_xgb_labels)
print(paste("test-error=", err))

pred <- as.numeric(pred > 0.5)
pred_xgb_table <- table(test_xgb_labels, pred)
draw_confusion_matrix(confusionMatrix(pred_xgb_table))

# getting information on how important each feature is
importance_matrix_xgb <- xgb.importance(names(df_xgb_matrix), model = model_xgb)
# plotting
xgb.plot.importance(importance_matrix_xgb)

### Tuning xgboost model
# getting the number of negative & positive cases in our data
negative_cases <- sum(train_xgb_labels == 0)
postive_cases <- sum(train_xgb_labels == 1)

model_tuned_xgb <- xgboost(data = dtrain_xgb, # the data
                           max.depth = 3, # the maximum depth of each decision tree
                           nround = 70, # number of boosting rounds
                           eta = 0.3, # learning rate
                           gamma = 0.25, # minimum loss reduction to make further partition
                           lambda = 10, # L2 regularization term on weights
                           early_stopping_rounds = 3, # if we don't see an improvement in this many rounds, stop
                           objective = "binary:logistic", # the objective function
                           scale_pos_weight = 15, # control for imbalanced classes
                           eval_metric = "error")

# generating predictions for our held-out testing data
pred_xgb_tuned <- predict(model_tuned_xgb, dtest_xgb)

# getting & printing the classification error
err <- mean(as.numeric(pred_xgb_tuned > 0.5) != test_xgb_labels)
print(paste("test-error=", err))

### Exploring Results
# Generating Confusion Matrix
pred_xgb_tuned <- as.numeric(pred_xgb > 0.5)
pred_xgb_tuned_table <- table(test_xgb_labels, pred_xgb_tuned)
draw_confusion_matrix(confusionMatrix(pred_xgb_tuned_table))

# getting information on how important each feature is
importance_matrix_xgb <- xgb.importance(names(df_xgb_matrix), model = model_xgb_tuned)
# plotting
xgb.plot.importance(importance_matrix_xgb)

# features contributing most to the model
xgb.plot.multi.trees(feature_names = names(df_xgb_matrix), 
                     model = model_tuned_xgb)



#### ASSOCIATION RULES #####

df_arules= read.csv('no_outliers.csv')
names(df_arules)
df_arules<-df_arules[0:400,]
sub_arules = df_arules[ ,-c(4,9,10,11,12,14,15,16,18,19,20)]

sub_arules$Attrition_Flag = as.factor(sub_arules$Attrition_Flag)
sub_arules$f.customer_age <-df_arules$Customer_Age
sub_arules$f.customer_age[sub_arules$Customer_Age<=35]<-"Young_adults"
sub_arules$f.customer_age[sub_arules$Customer_Age>35 & sub_arules$Customer_Age <= 55]<-"Middle-aged_adults"
sub_arules$f.customer_age[sub_arules$Customer_Age>55]<-"Older adults"
sub_arules = sub_arules[,-c(2)]
sub_arules$f.customer_age= as.factor(sub_arules$f.customer_age)
sub_arules$Gender= as.factor(sub_arules$Gender)
sub_arules$Education_Level = as.factor(sub_arules$Education_Level)
sub_arules$Marital_Status = as.factor(sub_arules$Marital_Status)
sub_arules$Income_Category <- factor(sub_arules$Income_Category, labels = c("High_Income","Low_Income","Medium_Income","Medium_Income","Low_Income"))
sub_arules$Card_Category = as.factor(sub_arules$Card_Category)
sub_arules$Credit_Limit <-as.factor(split_quantile(x = sub_arules$Credit_Limit, type = 3))
sub_arules$Credit_Limit <- factor(sub_arules$Credit_Limit, labels = c("Low_limit","Medium_limit","High_limit"))
sub_arules$Total_Trans_Amt <-as.factor(split_quantile(x = sub_arules$Total_Trans_Amt, type = 3))
sub_arules$Total_Trans_Amt<- factor(sub_arules$Total_Trans_Amt, labels = c("Low_trans_amount","Medium_trans_amount","High_trans_amount"))

trx_arules <- transactions(sub_arules)

itemFrequencyPlot(trx_arules, support = 0.1, cex.names=0.8, col='cadetblue',minlen=2)

rules <- apriori(trx_arules,parameter = list(support = 0.01, confidence = 0.6,  minlen=3))

#Rules related to our Target
rules_existing_customer <- subset(rules, subset = rhs %in% "Attrition_Flag=Existing Customer" & lift > 1.0)
rules_attrited_customer <- subset(rules, subset = rhs %in% "Attrition_Flag=Attrited Customer" & lift > 1.0)


inspect(head(rules_existing_customer, n = 3, by = "confidence"))
#From the output above we can observe that the three most redundant rules evidence that exists a direct 
#relation between clients with a Doctorade and the attrition flag, been single, having Medium or High transaction amount tend to remain their bank accounts open.
inspect(head(rules_attrited_customer, n = 3, by = "confidence"))
#Taking into account that the first three most redundant rules related to the attrited customers show some interactions 
#between Marital Status, Credit Limit, Total Transactions Amount and Customer Age. We can conclude that clients most likely 
#to close their bank accounts are married older adults with low credit limit. 


###writing rules(exporting to a csv file to  work directory)

write(sub_rules.sorted , file = "AssociationRules.csv", sep = ",", col.names = NA)

