#import all the required packages

install.packages("proxy")
if(!require(mlbench)) install.packages("mlbench", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(mice)) install.packages("mice", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(VIM)) install.packages("VIM", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr")

library(mlbench)
library(tidyverse)
library(caret)
library(knitr)
library(mice)
library(corrplot)
library(VIM)
library(Hmisc)
library(ggcorrplot)
library(dplyr)
set.seed(500)

#import the file
library(readr)
pima <- read_csv("\diabetes.csv")
dim(pima)
str(pima)

#summary of data
summary(pima)

#check the data introduction
pima%>%head()

#check duplicate entries
sum(duplicated(pima))

#change the column names 
colnames(pima) <- c('preg','gluc','pres','Skin','insu','mass','pedi','age','class')

#check the null values
sapply(pima, function(x) sum(is.na(x)))

#the summary shows the disguised data e.g. glucose value as 0
#so check number of 0s in each column
sapply(pima, function(x) sum(x==0))

#generate a correlation matirx and plot
correlationMatrix <- round(cor(pima[,1:8]),2)
print(correlationMatrix)
ggcorrplot(cor(pima), hc.order = TRUE, lab = TRUE, lab_size = 3) +
  labs(title = "Correlation Between Variables and outcome",
       subtitle = "Netural and Positive Correlation") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave("CorrelationPlot.png")
rm(correlationMatrix)

#check the outliers using box plot with scatter
# boxplot of continuous features
pima %>%
  gather("key", "value", preg:age) %>%
  ggplot(aes(x=key, y = value, fill = as.factor(class))) +
  facet_wrap(vars(key), ncol = 3, scales = "free") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  geom_boxplot(alpha = 0.8)

#minimal preprocessing
df_control<-pima
df_control[df_control$gluc==0,]$gluc<-NA
df_control[df_control$pres==0,]$pres<-NA
df_control[df_control$Skin==0,]$Skin<-NA
df_control[df_control$mass==0,]$mass<-NA
df_control[df_control$insu==0,]$insu<-NA
if(!require(naniar)) install.packages("naniar")

library(naniar)
df_control <- df_control %>%
  impute_median_all()

df_control$id <- 1:nrow(df_control)
train_med <- df_control %>% dplyr::sample_frac(.70)
test_med  <- dplyr::anti_join(df_control, train_med, by = 'id')


train_med<-train_med[,1:9]
test_med<-test_med[,1:9]

train_median_impute_glm<-train(class~.,data=train_med,method="glm")
pred_glm<-predict(train_median_impute_glm,test_med)
cm_med<-confusionMatrix(pred_glm,test_med$class)
cm_med
summary(train_median_impute_glm)

col <- c("#ed3b3b", "#0099ff")
graphics::fourfoldplot(cm_med$table, color = col, conf.level = 0.95, margin = 1,
                       main = paste("Confusion matrix: Minimal processing", sep = ""))




#remove the clear outliers
pima=pima[pima['Skin']<80,]
pima=pima[pima['insu']<=800,]

#analyze the data based on groups
d<-pima

#Age categories
labs <- c("<20yrs","20-40","40-60 yrs","60-80 yrs",">80 yrs")
d$AgeGroup <- cut(d$age, breaks = 5, labels = labs, right = FALSE)
ggplot(d, aes(x = AgeGroup, fill = as.factor(class)))+
  geom_bar(bins = 6,position = "dodge")+guides(fill=guide_legend(title="Outcome"))+
  scale_fill_discrete(labels=c("Non-diabetic","Diabetic"))

#BP categories 
labsBP <- c("<50","50-100",">100")
d$BPGroup <- cut(d$pres, breaks = 3, labels = labsBP, right = FALSE)
ggplot(d, aes(x = BPGroup, fill = as.factor(class)))+geom_bar(bins = 6,position = "dodge")+guides(fill=guide_legend(title="Outcome"))+
  scale_fill_discrete(labels=c("Non-diabetic","Diabetic"))

d$class=as.factor(d$class)
gluc_density <- ggplot(d, aes(x = gluc, color = class, fill = class)) +
  geom_density(alpha = 0.8) +
  theme(legend.position = "bottom") +
  labs(x = "Glucose", y = "Density", title = "Density plot for glucose")

gluc_box <- ggplot(d, aes(x = class, y = gluc,fill = class)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ggtitle("Boxplot for Diabetic vs Non-diabetic")

gridExtra::grid.arrange(gluc_box, gluc_density, ncol = 2)

ggplot(d,aes(x=mass,y=Skin))+
  geom_point(aes(color=class))+
  guides(fill=guide_legend(title="Outcome"))+
  scale_fill_discrete(labels=c("Non-diabetic","Diabetic"))+
  ggtitle("BMI and Skin Thickness")

rm(d)

#Find the disguised values and replace it with NA
pima[pima$gluc==0,]$gluc<-NA
pima[pima$pres==0,]$pres<-NA
pima[pima$Skin==0,]$Skin<-NA
pima[pima$mass==0,]$mass<-NA
pima[pima$insu==0,]$insu<-NA

#remove all incomplete cases
dim(na.omit(pima))
missing_data_percent <- ((nrow(pima)-(dim(na.omit(pima))[1]))/nrow(pima))*100
print(missing_data_percent)

#apply t-test to get significant dependency
sapply(pima,t.test)


#Density graphs
pima %>%
  gather("key", "value", preg:age) %>%
  ggplot(aes(x = value, fill = as.factor(class))) +
  facet_wrap(vars(key), ncol = 3, scales = "free") +
  geom_histogram(aes(y=..density..), alpha=0.5, 
                 position="identity")+
  geom_density(alpha = 0.4) +
  labs(x = "value of feature in facet", 
       y = "density",
       fill = "Diabetes",
       title = "Density of continuous features", 
       caption = "Source: Pima Indians Diabetes Database")


#find number of missing values in database
library(mice)
#  Exploring cases of missing values
md.pattern(pima,rotate.names = TRUE)

library(VIM)
# Visualizing pattern in missing values (NAs)
mice_plot <- aggr(pima, col=c('brown','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(pima), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
rm(mice_plot)


#remove all the instances with >2 NAs
pima_new=pima[rowSums(is.na(pima[,1:9]))<3, ]

#remove 4 cases of 2 NA attr- Glucose and insulin are highly correlated so if both are missing then remove those records
pima_new=pima_new[!(is.na(pima_new$gluc)& is.na(pima_new$insu)),]

#remove 192 cases of these 2 NA attr as the missing percentage is very high- 25%
#also the insulin and skin thickness are highly correlated so if both are missing remove those records
pima_new=pima_new[!(is.na(pima_new$Skin) & is.na(pima_new$insu)),]

ggcorrplot(cor(pima_new,use="pairwise.complete.obs"), hc.order = TRUE, lab = TRUE, lab_size = 3) +
  labs(title = "Correlation Between Variables and outcome",
       subtitle = "Netural and Positive Correlation") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

missing_data_percent_new <- ((nrow(pima_new)-(dim(na.omit(pima_new))[1]))/nrow(pima_new))*100
print(missing_data_percent_new)

#  Exploring cases of missing values
md.pattern(pima_new,rotate.names = TRUE)
pima<-pima_new
rm(missing_data_percent_new)
rm(pima_new)


#Impute missing values by using SICE
m=10
imputed_Data <- mice(pima, m=m, maxit = 50, method = 'pmm')
summary(imputed_Data)# Impute data
imputed_Data$imp

for(impDF in imputed_Data$imp){
  impDF[,m+1] = impDF[,1]
}



for(i in 1:nrow(imputed_Data$imp$insu)){
  imputed_Data$imp$insu[i,m+1]=as.integer(rowMeans((imputed_Data$imp$insu)[i,1:m]))
}
for(i in 1:nrow(imputed_Data$imp$gluc)){
  imputed_Data$imp$gluc[1,m+1]=as.integer(rowMeans((imputed_Data$imp$gluc)[i,1:m]))
}

for(i in 1:nrow(imputed_Data$imp$pres)){
  imputed_Data$imp$pres[i,m+1]=as.integer(rowMeans((imputed_Data$imp$pres)[i,1:m]))
}

for(i in 1:nrow(imputed_Data$imp$mass)){
  imputed_Data$imp$mass[i,m+1]=as.integer(rowMeans((imputed_Data$imp$mass)[i,1:m]))
}

for(i in 1:nrow(imputed_Data$imp$Skin)){
  imputed_Data$imp$Skin[i,m+1]=as.integer(rowMeans((imputed_Data$imp$Skin)[i,1:m]))
}

for(i in 1:nrow(imputed_Data$imp$pedi)){
  imputed_Data$imp$pedi[i,m+1]=
    as.integer(rowMeans((imputed_Data$imp$pedi)[i,1:m]))
}

for(i in 1:nrow(imputed_Data$imp$class)){
  imputed_Data$imp$class[i,m+1]=
    as.integer(rowMeans((imputed_Data$imp$class)[i,1:m]))
}

for(i in 1:nrow(imputed_Data$imp$age)){
  imputed_Data$imp$age[i,m+1]=
    as.integer(rowMeans((imputed_Data$imp$age)[i,1:m]))
}
for(i in 1:nrow(imputed_Data$imp$preg)){
  imputed_Data$imp$preg[i,m+1]=
    as.integer(rowMeans((imputed_Data$imp$preg)[i,1:m]))
}

imputed_Data$m=m+1
pima_imp_mice <- complete(imputed_Data,2)
pima_imp_sice <- complete(imputed_Data,11)
corrplot(cor(pima_imp_mice[, -9]), type = "upper", method = "number")
ggcorrplot(cor(pima_imp_mice), hc.order = TRUE, lab = TRUE, lab_size = 3) +
  labs(title = "Correlation Between Variables and outcome",
       subtitle = "MICE") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


# rowSums(is.na(pima_imp_sice))
corrplot(cor(pima_imp_sice[, -9]), type = "upper", method = "number")
ggcorrplot(cor(pima_imp_sice), hc.order = TRUE, lab = TRUE, lab_size = 3) +
  labs(title = "Correlation Between Variables and outcome",
       subtitle = "SICE") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#summarize the new df generated after imputation
if(!require(summarytools)) install.packages("summarytools", repos = "http://cran.us.r-project.org")
library(summarytools)
summarytools::descr(pima_imp_sice)

# boxplot of continuous features
pima_imp_sice %>%
  gather("key", "value", preg:age) %>%
  ggplot(aes(x=key, y = value, fill = as.factor(class))) +
  facet_wrap(vars(key), ncol = 3, scales = "free") +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  geom_boxplot(alpha = 0.8)

#remove outliers by analysing the box plot
pima_imp_sice=pima_imp_sice[pima_imp_sice['insu']<=650,]
pima_imp_sice=pima_imp_sice[pima_imp_sice['age']<70,]
pima_imp_sice=pima_imp_sice[pima_imp_sice['mass']<65,]
pima_imp_sice=pima_imp_sice[pima_imp_sice['pedi']<1.5,]

#visualize the imputed values for Insulin
ggplot()+
  geom_point(data = pima_imp_sice, aes(x = insu, y = Skin), shape = 21,color="blue")+
  geom_point(data = pima[!is.na(pima$insu),], aes(x = insu, y = Skin), color = "red" ,shape = 21, show.legend = TRUE)+
  scale_color_manual(values = c("blue","red"), labels = c("imputed_SICE","pima_new"), name = "Label SICE")

#visualize the density graphs
pima_imp_sice %>%
  gather("key", "value", preg:age) %>%
  ggplot(aes(x = value, fill = as.factor(class))) +
  facet_wrap(vars(key), ncol = 3, scales = "free") +
  geom_histogram(aes(y=..density..), alpha=0.5,
                 position="identity")+
  geom_density(alpha = 0.4) +
  labs(x = "value of feature in facet",
       y = "density",
       fill = "Outcome",
       title = "Density of continuous features",
       caption = "Source: Pima Indians Diabetes Database")


#overall analysis 
d=pima_imp_sice
d$age <- ifelse(d$age < 30, "<30 yrs", ">= 30 yrs")

ggplot(d, aes(x = gluc, y = mass)) +
  geom_rect(aes(linetype = "High BMI - Diab"), xmin = 160, ymax = 40, fill = NA, xmax = 200, 
            ymin = 25, col = "black") + 
  geom_rect(aes(linetype = "Low BMI - Not Diab"), xmin = 0, ymax = 30, fill = NA, xmax = 120, 
            ymin = 10, col = "black") + 
  geom_point(aes(col = factor(class), shape = factor(age)), size = 3) +
  scale_color_brewer(name = "class", palette = "Set1") +
  scale_shape(name = "Age") +
  scale_linetype_manual(values = c("High BMI - Diab" = "dotted", "Low BMI - Not Diab" = "dashed"),
                        name = "Segment")

rm(d)

#Data Classification
set.seed(500)
pima_imp_sice$class<-as.factor(pima_imp_sice$class)
#split the data into train and test
split_sice_data=sample(c(rep(0, 0.7 * nrow(pima_imp_sice)), rep(1, 0.3 * nrow(pima_imp_sice))))
train_sice <- pima_imp_sice[split_sice_data == 0, ]
test_sice <- pima_imp_sice[split_sice_data == 1, ]


#### LOGISTIC REGRESSION MODEL ####

# train logistic regression model using train dataset #
# On SICE data
#with all variables
train_sice_glm<-train(class~.,data=train_sice,method="glm")
pred_sice_glm<-predict(train_sice_glm,test_sice)
cm_SICE<-confusionMatrix(pred_sice_glm,test_sice$class)
cm_SICE
summary(train_sice_glm)

#with 4 variables
set.seed(500)
train_sice_glm2<-train(class~gluc+pedi+mass+age,data=train_sice,method="glm")
pred_sice_glm2<-predict(train_sice_glm2,test_sice)
cm_SICE2<-confusionMatrix(pred_sice_glm2,test_sice$class)
cm_SICE2
summary(train_sice_glm2)

#with 3 variables
set.seed(500)
train_sice_glm3<-train(class~gluc+mass+pedi,data=train_sice,method="glm")
pred_sice_glm3<-predict(train_sice_glm3,test_sice)
cm_SICE3<-confusionMatrix(pred_sice_glm3,test_sice$class)
cm_SICE3
# cm_SICE[4]$byClass
summary(train_sice_glm3)

#analyze the model
# par(mfrow = c(2,2))
plot(train_sice_glm3$finalModel)



#with mice
set.seed(500)
pima_imp_mice$class<-as.factor(pima_imp_mice$class)
#split the data into train and test
split_mice_data=sample(c(rep(0, 0.7 * nrow(pima_imp_mice)), rep(1, 0.3 * nrow(pima_imp_mice))))
train_mice <- pima_imp_mice[split_mice_data == 0, ]
test_mice <- pima_imp_mice[split_mice_data == 1, ] 

train_mice_glm<-train(class~.,data=train_mice,method="glm")
pred_mice_glm<-predict(train_mice_glm,test_mice)
mean(pred_mice_glm==test_mice$class)
cm_MICE<-confusionMatrix(pred_mice_glm,test_mice$class)
cm_MICE
cm_MICE[4]$byClass


#CF comparison

col <- c("#ed3b3b", "#0099ff")
graphics::fourfoldplot(cm_med$table, color = col, conf.level = 0.95, margin = 1,
                       main = paste("Confusion matrix: Minimal processing", sep = ""))

graphics::fourfoldplot(cm_SICE3$table, color = col, conf.level = 0.95, margin = 1,
                       main = paste("Confusion matrix: LR SICE", sep = ""))

graphics::fourfoldplot(cm_MICE$table, color = col, conf.level = 0.95, margin = 1,
                       main = paste("Confusion matrix: LR MICE", sep = ""))



#use SMOTE to create new dataset that is more balanced
if(!require(smotefamily)) install.packages("smotefamily", repos = "http://cran.us.r-project.org")
library(smotefamily)
new_df <- pima_imp_sice

new_df$class=as.numeric(new_df$class)-1
new_df <- SMOTE(new_df, new_df$class, K=5)
final_data=new_df$data
final_data <- subset(final_data, select = -c(class))
table(final_data['class'])


set.seed(500)
final_data$class<-as.factor(final_data$class)
#split the data into train and test
split_sice_data_final=sample(c(rep(0, 0.7 * nrow(final_data)), rep(1, 0.3 * nrow(final_data))))
train_sice <- final_data[split_sice_data_final == 0, ]
test_sice <- final_data[split_sice_data_final == 1, ] 

#smote LR
train_siceSMOTE_glm<-train(class~.,data=train_sice,method="glm")
pred_siceSMOTE_glm<-predict(train_sice_glm,test_sice)
mean(pred_siceSMOTE_glm==train_siceSMOTE_glm$class)
cm_SICE_SMOTE<-confusionMatrix(pred_siceSMOTE_glm,test_sice$class)
cm_SICE_SMOTE
cm_SICE_SMOTE[4]$byClass


#Metrics comparison

metrics=rbind(
  
  c(cm_SICE3$overall["Accuracy"],
    cm_SICE3$byClass["Precision"],
    cm_SICE3$byClass["Sensitivity"],
    cm_SICE3$byClass["Specificity"],
    cm_SICE3$byClass["F1"]),
  
  c(cm_MICE$overall["Accuracy"],
    cm_MICE$byClass["Precision"],
    cm_MICE$byClass["Sensitivity"],
    cm_MICE$byClass["Specificity"],
    cm_MICE$byClass["F1"]),
  
  c(cm_SICE_SMOTE$overall["Accuracy"],
    cm_SICE_SMOTE$byClass["Precision"],
    cm_SICE_SMOTE$byClass["Sensitivity"],
    cm_SICE_SMOTE$byClass["Specificity"],
    cm_SICE_SMOTE$byClass["F1"])
  
)

rownames(metrics) = c("SICE", "MICE", "SMOTE")
metrics

barplot(metrics,beside=TRUE,col=c("darkblue","red","green"),legend = rownames(metrics),
        main="Comparison of SICE, MICE and SMOTE with Logistic regression",
        xlab="Statistical values")
