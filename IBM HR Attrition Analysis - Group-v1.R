#########HR Attrition Analysis
####Batch : DSP 16
####Members: Ramprassath T C, MuthuKumar V, Balavignesh, CR


### Installing Packages
install.packages(Boruta)
install.packages(rFerns)
install.packages(dplyr)
install.packages(caret)
install.packages(caTools)
install.packages(e1071) 
install.packages(randomForest)
install.packages(smotefamily)
install.packages(DMwR)
install.packages(ROCR)
install.packages(Metrics)
install.packages(caret)
install.packages(reshape2)
install.packages(corrplot)
install.packages(ggplot2)
install.packages(caTools)
install.packages(gridExtra)

### Adding Libraries
library(Boruta)
library(rFerns)
library(dplyr)
library(caret)
library(caTools)
library(e1071) 
library(randomForest)
library(smotefamily)
library(DMwR)
library(ROCR)
library(Metrics)
library(caret)
library(reshape2)
library(corrplot)
library(ggplot2)
library(caTools)
library(gridExtra)
library(pROC)
library(xgboost)


####################### Reading data file #######################################################

empdata <- read.csv('D:/R LANGUAGE/projects/IBM attr/AttrRuth/Attrition.csv')
empdata_Alt <- empdata
str(empdata)
summary(empdata)
set.seed(123) 

#################################################################################################
####################### Data preparation ########################################################
#################################################################################################

#### (1) Check whether we have any missing values
colSums(is.na(empdata))
# anyNA(data)  - alternate approach

#### (2) Check whether we have any duplicate data
anyDuplicated(empdata)

#### Change data class to factor 
names <- c('Attrition','WorkLifeBalance','BusinessTravel','Department' ,'StockOptionLevel','PerformanceRating','JobSatisfaction','RelationshipSatisfaction','JobLevel','JobRole','MaritalStatus','OverTime','EducationField','Gender','JobInvolvement','EnvironmentSatisfaction','Education')
empdata[,names] <- lapply(empdata[,names], factor)
str(empdata)

#### Remove non significant variables
## EmployeeCount
## EmployeeNumber
## Over18
## StandardHours

empdata<-empdata[,-c(9,10,22,27)]
str(empdata)







#### Find Correlation between the Numerical variables



library(ggcorrplot) # Need to use this library in order to use ggcorrplot function

nums <- select_if(empdata, is.numeric)

corr <- round(cor(nums), 1)

ggcorrplot(corr, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           colors = c("tomato2", "white", "#01A9DB"), 
           title="Correlogram Employee Attritions", 
           ggtheme=theme_minimal())

# Observations - 

# The above Matrix tells us the below correlations - 

# (1) Age variable is correlated with TotalWorkingYears
# (2) TotalWorkingYears correlated with MonthlyIncome
# (3) YearsWithCurrManager also correlated with YearsAtCompany
# (4) YearsWithCurrManger correlated with YearsInCurrentRole
# (5) YearsInCurrentRole correlated with YearsAtCompany

#### Categorical variable analysis
library(ggplot2) # include the package ggplot - grammer of graphics
library(caret)
library(dplyr)

## ************ Graph 1 & 2 : How does 'Age' impact the 'Attrition' factor? ************ ##
# Graph 1
ggplot(empdata,aes(x=Age,fill=Gender))+geom_bar()+facet_grid(".~Attrition")
# Graph 2
age_g<-cut(empdata$Age,breaks = c(18,30,40,60,100),labels = c("18-30","30-40","40-60","60-100"),
           include.lowest = TRUE)
ggplot(empdata,aes(x=age_g,fill=Attrition))+geom_bar()+facet_grid(".~Attrition")

# Observations - Graph 1 and 2
# There is a high attrition rate in the Age group between 18 to 30 irrespective of the Gender
##************************************************************************************** ##


## ************ Graph 3 : How does 'Gender' impact the 'Attrition' factor? ************ ##

ggplot(empdata,aes(x=Gender,fill=Attrition))+geom_bar()+facet_grid(".~Attrition")

# Observations - Graph 3
# There is a high attrition rate in the 'Male' group however if we take the % then there won't be much difference
# between 'Male' and 'Female' gender. This was proven when we saw graph 1 and 2 as well.

##************************************************************************************** ##


## ************ Graph 4 : How does 'Department' impact the 'Attrition' factor? ************ ##

ggplot(empdata,aes(x=Department,fill=Gender))+geom_bar()+facet_grid(".~Attrition")

# Observations - Graph 4
# There is a high attrition rate in the 'Research & Development' department however if we take the % then there won't be much difference
# amoung the group.

##************************************************************************************** ##


## ************ Graph 5 & 6 : How does 'Monthly Income' & Avg Income impact the 'Attrition' factor? ************ ##

# Graph 5 -
ggplot(empdata,aes(y=MonthlyIncome,x=Attrition,col=MonthlyIncome))+geom_jitter(size=2)+facet_grid(".~Gender")

# Graph 6-
avg.income <- empdata %>% select(Department, MonthlyIncome, Attrition) %>% group_by(Attrition, Department) %>%
  summarize(avg.inc=mean(MonthlyIncome)) %>%
  ggplot(aes(x=reorder(Department, avg.inc), y=avg.inc, fill=Attrition)) + geom_bar(stat="identity", position="dodge") + facet_wrap(~Attrition) + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5)) + 
  scale_fill_manual(values=c("lightgreen", "tomato2")) + 
  labs(y="Average Income", x="Department", title="Average Income by Department \n and Attrition Status") + 
  geom_text(aes(x=Department, y=0.01, label= paste0("$ ", round(avg.inc,2))),
            hjust=-0.5, vjust=0, size=3, 
            colour="black", fontface="bold",
            angle=90)

avg.income


# Graph 7 -
library(forcats) # This package contains the function fct_reorder

high.inc <- empdata %>% select(JobSatisfaction, MonthlyIncome, Attrition) %>% group_by(JobSatisfaction, Attrition) %>%
  summarize(med=median(MonthlyIncome)) %>%
  ggplot(aes(x=fct_reorder(JobSatisfaction, -med), y=med, color=Attrition)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=JobSatisfaction, 
                   xend=JobSatisfaction, 
                   y=0, 
                   yend=med)) + facet_wrap(~Attrition) + 
  labs(title="Is Income a Reason for Employees to Leave?", 
       subtitle="by Attrition Status",
       y="Median Income",
       x="Level of Job Satisfaction") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6), plot.title=element_text(hjust=0.5), strip.background = element_blank(),
        strip.text = element_blank()) + 
  coord_flip() + theme_minimal() + scale_color_manual(values=c("#58FA58", "#FA5858")) + 
  geom_text(aes(x=JobSatisfaction, y=0.01, label= paste0("$ ", round(med,2))),
            hjust=-0.5, vjust=-0.5, size=4, 
            colour="black", fontface="italic",
            angle=360)


high.inc



# Graph 8 -
daily_r <- empdata %>% select(JobRole, Attrition, DailyRate) %>% group_by(Attrition, JobRole) %>%
  ggplot(aes(x=JobRole, y=DailyRate, color=Attrition)) + facet_wrap(~Attrition) + coord_flip() + theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90), plot.title=element_text(hjust=0.5, size=10), plot.background=element_rect(fill="#FFF1E0")) + 
  stat_summary(fun.y=mean, fun.ymin = min, fun.ymax = max) + scale_color_manual(values=c("#58FA58", "#FA5858")) + 
  labs(title="Daily Rates by Job Role")

daily_r

# Observations - Graph 5, 6, 7 & 8 -
# It is pretty obvious that the people who has less monthly income is moving out. We are getting a strong 
# evidence for the attrition here. Salary data was negatively skewed.
# Graph 7 tells us that the Median Salary is very low for the people who are moving out when 
# comparing with the other group. Also it had a great impact in the Job Satisfaction
# Graph 8 tells us that the average Daily Rate is low for the people who work in many departments

##************************************************************************************** ##


## ************ Graph 9 - How does 'PerformanceRating' impact the 'Attrition' factor? ************ ##

ggplot(empdata,aes(x=PerformanceRating,fill=Gender))+geom_bar()+facet_grid(".~Attrition")

# Observations - Graph 9
# The above graph tells us that the low rating caused more attrition. People might have disappointed with the 
# low rating.

##************************************************************************************** ##


## ************ Graph 10 - How does 'BusinessTravel' impact the 'Attrition' factor? ************ ##

ggplot(empdata,aes(x=BusinessTravel))+geom_bar()+facet_grid(".~Attrition")

# Observations - Graph 10
# The above graph tells us that the people who travels very rarely prefers to move out. People might try to 
# use this travel experience to enrich their profile and move out to get better opportunity. The amount of People 
# who moves out of the company, either never had a chance to travel or frequently travel.  

##************************************************************************************** ##


## ************ Graph 11 & 12 - How does 'Environment Satisfaction' impact the 'Attrition' factor? ************ ##

# Graph 11 -
library(ggthemes)

empdata$EnvironmentSatisfaction = as.numeric(empdata$EnvironmentSatisfaction)
env.attr <- empdata %>% select(EnvironmentSatisfaction, JobRole, Attrition) %>% group_by(JobRole, Attrition) %>%
  summarize(avg.env=mean(EnvironmentSatisfaction))

ggplot(env.attr, aes(x=JobRole, y=avg.env)) + 
  geom_line(aes(group=Attrition), color="#58ACFA", linetype="dashed") + 
  geom_point(aes(color=Attrition), size=3) +  theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), axis.text.x=element_text(angle=90),
        plot.background=element_rect(fill="#FFF1E0")) + 
  labs(title="Working Environment", y="Average Environment Satisfaction", x="Job Position") + 
  scale_color_manual(values=c("red", "green"))

empdata$EnvironmentSatisfaction = as.factor(empdata$EnvironmentSatisfaction)

# Graph 12 - 
ggplot(empdata,aes(x=JobRole,y=Attrition, col = JobRole))+geom_jitter(size=2)


# Observations - Graph 11 & 12 :
# The above graph tells us that 'Manager' and 'Healthcare Representative' are not satisfied with working 
# Environment. This may be because of the work pressure they may face. Let's see the amount of people who moves 
# out from this position however Graph 12 says that the attrition rate is not high in these 2 positions. 
# Hence we can conclude that "Environment Satisfaction" factor doesn't carry high weitage towards the attrition.


##************************************************************************************** ##


## ************ Graph 13 & 14  - How does 'EducationField' impact the 'Attrition' factor? ************ ##

# Graph 13 - 
ggplot(empdata,aes(x=EducationField,y=Attrition,col=EducationField))+geom_jitter(size=2)
#Graph 14 - 
ggplot(empdata,aes(x=EducationField,y=MonthlyIncome,col=EducationField))+geom_jitter(size=2)+facet_grid(".~Attrition")

# Observations - Graph 13 & 14 :
# The above graphs tells us that the Monthly Income plays a vital role everywhere. People who get less salary
# moves out irrespective of the field


##************************************************************************************** ##


## ************ Graph 15 - How does 'Job Satisfaction' impact the 'Attrition' factor? ************ ##

ggplot(empdata,aes(x=JobSatisfaction,y=MonthlyIncome))+geom_jitter(size=2)+facet_grid(".~Attrition")

# Observations - Graph 15 :
# One more strong evidence to prove that 'Low Salary' is the main factor towards 'High attrition'


##************************************************************************************** ##


## ********* Graph 16, 17 & 18 - How does 'Salary hike' and ' Overtime' impact the 'Attrition' factor? ************ ##

# Graph 16 - 
ggplot(empdata,aes(x=PercentSalaryHike,fill=Attrition))+geom_bar()

# Graph 17 -
ggplot(empdata,aes(x=OverTime,fill=Attrition))+geom_bar()+facet_grid(".~Gender")

# Graph 18 -
overtime_percent <- empdata %>% select(OverTime, Attrition) %>% filter(Attrition == "Yes") %>% group_by(Attrition, OverTime) %>%
  summarize(n=n()) %>% mutate(pct=round(prop.table(n),2) * 100) %>% 
  ggplot(aes(x="", y=pct, fill=OverTime)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
  theme_tufte() + scale_fill_manual(values=c("#2EFE64", "#FE2E2E")) + 
  geom_label(aes(label = paste0(pct, "%")), position = position_stack(vjust = 0.5), colour = "white",  fontface = "italic")+
  theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="white"), plot.subtitle=element_text(color="white"), plot.background=element_rect(fill="#0D7680"),
        axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="white"), 
        legend.background = element_rect(fill="#FFF9F5",
                                         size=0.5, linetype="solid", colour ="black")) + 
  labs(title="Level of Attrition by Overtime Status", subtitle="In Percent", x="", y="") 

overtime_percent

# Observations - Graph 16, 17 & 18 :
# 'Low salary hike' is another factor along with 'Salary'. These 2 are correlated with each other.
# 'Overtime' - There is 50 - 50 distribution here. Considering the total head count, we can say that the people 
# may get some additional OT payout if they get more 'Overtime' opportunities.


##************************************************************************************** ##


## ************ Graph 19 - How does 'Marital Status impact the 'Attrition' factor? ************ ##


ggplot(empdata,aes(x=MaritalStatus,y=Attrition,col=MaritalStatus))+geom_jitter()+facet_grid(".~Gender")

# Observations - Graph 19 :
# Many people who are 'Single' moves out frequently. Their commitments may not be so high as others.

##************************************************************************************** ##


## ************ Graph 20 - How does 'Work Life Balance' impact the 'Attrition' factor? ************ ##

ggplot(empdata,aes(x=WorkLifeBalance,y=Attrition,col=Gender))+geom_jitter()+facet_grid(".~Gender")

# Observations - Graph 20 :
# People who are in the mid category moves out. This is something unusual.

##************************************************************************************** ##


## ************ Graph 21 - How does 'Distance From Home' impact the 'Attrition' factor? ************ ##

ggplot(empdata,aes(x=DistanceFromHome,y=Attrition))+geom_jitter()

# Observations - Graph 21 :
# We can't say that distance may be a reason for attrition by looking at the above graph.


##************************************************************************************** ##

## ************ Graph 22 - Checking 'Median Salary' for each Job Role  ************ ##

# Graph 22 - Create a TreeMap with the number of Employees by JobRole
library(tree)
library(treemapify)

role.amount <- empdata %>% select(JobRole) %>% group_by(JobRole) %>% summarize(amount=n()) %>%
  ggplot(aes(area=amount, fill=JobRole, label=JobRole)) +  geom_treemap() +
  geom_treemap_text(grow = T, reflow = T, colour = "black") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme(legend.position = "none") +
  labs(
    title = "Major Job Roles Inside the Organization",
    caption = "The area of each tile represents the number of
employees by type of job role.",
    fill = "JobRole"
  )

role.amount

# Graph 23 - Median Salary

library(cowplot)
job.sal <- empdata %>% select(JobRole, MonthlyIncome) %>% group_by(JobRole) %>% summarize(med=median(MonthlyIncome), avg=mean(MonthlyIncome))


p1 <- ggplot(job.sal, aes(x=reorder(JobRole,-med), y=med)) +  geom_bar(stat="identity", width=.5, fill="#FE9A2E") + 
  labs(title="Salary by Job Role", 
       subtitle="Median",
       x="Job Role",
       y="Median Income") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6))


p2 <- ggplot(job.sal, aes(x=reorder(JobRole,-avg), y=avg)) +  geom_bar(stat="identity", width=.5, fill="#BE81F7") + 
  labs(title="Salary by Job Role", 
       subtitle="Mean",
       x="Job Role",
       y="Mean Income") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.6))


plot_grid(p1, p2, ncol=2)

# Observations - Graph 22 & 23 :
# Majority of the people are in 'Research scientists', 'Sales executive', 'Lab technician', 'Manufacturing Director', 
# 'Healthcare representatives' roles. Their Mean and Median salary are low.
# Graph 12 clearly tells us the attrition rate in each role. We can see that the people from 'Lab technician',
# 'Sales executive', 'Research scientists' are moving out frequently.


##************************************************************************************** ##

# Few Positive Correlations


options(repr.plot.width=10, repr.plot.height=8) 

p1 <- empdata %>% select(TotalWorkingYears, MonthlyIncome) %>%
  ggplot(aes(x=TotalWorkingYears, y=MonthlyIncome)) + geom_point(col = "orange", alpha=1/2) + 
  geom_smooth(method="auto", col="yellow") + 
  theme_economist() + theme(legend.position="bottom", strip.background = element_blank(), 
                            strip.text.x = element_blank(), 
                            plot.title=element_text(hjust=0.5, color="white"), 
                            plot.background=element_rect(fill="brown"),
                            plot.subtitle=element_text(hjust=0.5, color="yellow"), 
                            axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                            axis.title=element_text(colour="white")) + 
  labs(title="Positive Correlation", subtitle="Monthly Income vs Working Years")

p2 <-  empdata %>% select(PerformanceRating, PercentSalaryHike) %>%
  ggplot(aes(x=factor(PerformanceRating), y=PercentSalaryHike)) + geom_boxplot(colour = "orange", fill="white") + 
  geom_jitter(color="yellow",alpha=1/3)  + 
  theme_economist() + theme(legend.position="bottom", strip.background = element_blank(), 
                            strip.text.x = element_blank(), 
                            plot.title=element_text(hjust=0.5, color="white"), 
                            plot.subtitle=element_text(hjust=0.5, color="white"), 
                            plot.background=element_rect(fill="brown"),
                            axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                            axis.title=element_text(colour="white")) + 
  labs(title="Positive Correlation", subtitle="Percent Salary Hike vs Performance Rating", 
       x="Performance Rating")
# Years with Current Manager, Years since Last Promotion
p3 <-  empdata %>% select(YearsWithCurrManager, YearsSinceLastPromotion) %>%
  ggplot(aes(x=factor(YearsWithCurrManager), y=YearsSinceLastPromotion)) + 
  geom_boxplot(colour = "orange", fill="white") + 
  geom_jitter(color="yellow",alpha=1/3) + geom_smooth(method='loess',aes(group=1),color='yellow',lty=2,size=.5) + 
  theme_economist() + theme(legend.position="bottom", strip.background = element_blank(), 
                            strip.text.x = element_blank(), 
                            plot.title=element_text(hjust=0.5, color="white"),
                            plot.subtitle=element_text(hjust=0.5, color="white"), 
                            plot.background=element_rect(fill="brown"),
                            axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                            axis.title=element_text(colour="white")) + 
  labs(title="Positive Correlation", subtitle="Years since Last Promotions vs Years with Current Manager", 
       x="Years with Current Manager")
# Age and Monthly Income
p4 <-  empdata %>% select(Age, MonthlyIncome) %>%
  ggplot(aes(x=Age, y=MonthlyIncome)) + geom_point(colour = "orange", alpha=1/2) + 
  geom_smooth(method="loess", color="yellow") + 
  theme_economist() + theme(legend.position="bottom", strip.background = element_blank(), 
                            strip.text.x = element_blank(), 
                            plot.title=element_text(hjust=0.5, color="white"), 
                            plot.subtitle=element_text(hjust=0.5, color="white"), 
                            plot.background=element_rect(fill="brown"),
                            axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                            axis.title=element_text(colour="white")) + 
  labs(title="Positive Correlation", subtitle="Monthly Income vs Age")
plot_grid(p1, p2, p3,p4, ncol=2, nrow=2)


#################################################################################################
############################   Model Fitting  - Imbalance Data  #################################
#################################################################################################

View(empdata)
str(empdata)
colSums(is.na(empdata))

#---------Generate Training and Test Data------------------
set.seed(123)
id<-sample(2,nrow(empdata),prob = c(0.7,0.3),replace = TRUE)
empdatatrain<-empdata[id==1,]
empdatatest<-empdata[id==2,]
table(empdatatrain$Attrition)
table(empdatatest$Attrition)
View(empdatatrain)


#-----------------RandomForest----------------------------------------------
library(pROC)
library(randomForest)
library(caret)
forestresult<-randomForest(Attrition~.,empdatatrain,ntree=500,mtry=10,importance=TRUE)
forestresult
varImpPlot(forestresult)


rfpredict<-predict(forestresult,empdatatest)

confusionMatrix(rfpredict,empdatatest$Attrition)

###### Accuracy = 86%% in RandomForest


plot.roc(as.numeric(rfpredict),as.numeric(empdatatest$Attrition),print.auc=TRUE,col="yellow",type="b",lwd=3)


# Applying k-Fold Cross Validation
# install.packages('caret')


ControlParameters <- trainControl(method = 'cv',
                                  number = 5,
                                  savePredictions = T,
                                  classProbs = T)


parameterGrid = expand.grid(mtry = c(1:15))
method = 'cv'
classProbs= TRUE

modelRandom = train(Attrition ~., 
                    data = empdatatrain,
                    method = 'rf',
                    trControl = ControlParameters,
                    tuneGrid = parameterGrid)

modelRandom

predictions <- predict(modelRandom, empdatatest)
confusionMatrix(predictions,empdatatest$Attrition)


###### Accuracy = 85% in RandomForest by using K-Fold CV


#------------------------LogisticRegression----------------------------------

#####################################
##
## Now we can do some quality control by making sure all of the factor
## levels are represented by Attrition with YES and NO
##
## NOTE: We also want to exclude variables that only have 1 or 2 samples in
## a category since +/- one or two samples can have a large effect on the
## odds/log(odds)
##
##
#####################################

xtabs(~Attrition + BusinessTravel, data = empdata)
xtabs(~Attrition + Department, data = empdata)
xtabs(~Attrition + Education, data = empdata)
xtabs(~Attrition + EducationField, data = empdata)
xtabs(~Attrition + EnvironmentSatisfaction, data = empdata)
xtabs(~Attrition + Gender, data = empdata)
xtabs(~Attrition + JobInvolvement, data = empdata)
xtabs(~Attrition + JobLevel, data = empdata)
xtabs(~Attrition + JobRole, data = empdata)
xtabs(~Attrition + JobSatisfaction, data = empdata)
xtabs(~Attrition + MaritalStatus, data = empdata)
xtabs(~Attrition + OverTime, data = empdata)
xtabs(~Attrition + PerformanceRating, data = empdata)
xtabs(~Attrition + RelationshipSatisfaction, data = empdata)
xtabs(~Attrition + StockOptionLevel, data = empdata)
xtabs(~Attrition + WorkLifeBalance, data = empdata)

# Observations : Almost all of the factors have a healthy mixture of Attrition data. Hence we cannot exclude any 
# factor

#####################################
##
## Now we are ready for some logistic regression.
##
#####################################

str(empdatatrain)
logmodel <- glm(Attrition ~., family=binomial, data = empdatatrain)
print(summary(logmodel))



# Accesing the predective ability of the logistic regression model
log_pred <- predict(logmodel,newdata=empdatatest,type='response')
log_pred <- ifelse(log_pred>=0.5,'Yes','No')
log_pred <- factor(log_pred)
caret::confusionMatrix(log_pred,empdatatest$Attrition)

# Plotting the ROC curve
library(ROCR)
res <- predict(logmodel, empdatatest, type = "response")
ROCRPred <- prediction(res, empdatatest$Attrition)
ROCRPerf <- performance(ROCRPred,"tpr","fpr")
plot(ROCRPerf,colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1))


# Chhose the thresold value 0.58 and make prediction
results <- predict(logmodel,newdata=empdatatest,type='response')
results <- ifelse(results>=0.58,'Yes','No')
results <- factor(results)
caret::confusionMatrix(results,empdatatest$Attrition)
print("confusion matrix for Logistic Regression")


table(Actual_value = empdatatest$Attrition,results)
misClasificError1 <- mean(results != empdatatest$Attrition)
print(paste('Logistic Regression Accuracy',1-misClasificError1))
# Accuracy of the Logistic Regression model = 87%



# Applying k-Fold Cross Validation
# install.packages('caret')

ControlParameters <- trainControl(method = 'cv',
                                  number = 10,
                                  repeats = 3,
                                  savePredictions = T,
                                  classProbs = T)


#parameterGrid = expand.grid(mtry = c(1:5))
method = 'cv'
classProbs= TRUE

#logmodel <- glm(Attrition ~., family=binomial, data = empdatatrain)


modelglm = train(Attrition ~., 
                 data = empdatatrain,
                 method = 'glm',
                 trControl = ControlParameters,
                 family = binomial)
                 #preProc=c('center','scale'),
                 #metric = 'ROC')
                 
                 modelglm
                 
                 predictions <- predict(modelglm, empdatatest)
                 confusionMatrix(predictions,empdatatest$Attrition)
                 
                 ## Accuracy of the Logistic Regression model = 87%
                 
                 
                 
                 #################################################################################################
                 ############################ Feature Engineering  ###############################################
                 #################################################################################################
                 
                 empdata <- empdata_Alt
                 ###Finding Median Monthly Salary
                 tapply(empdata$MonthlyIncome,INDEX = empdata$JobLevel,FUN = median)
                 
                 # Grade       1       2       3       4       5 
                 #Median Sal 2670     5340    9980    16154   19232 
                 
                 #####Divide Employees with salary more than Median in their grade
                 empdata <- mutate(empdata,MonSalLevel = case_when((empdata$JobLevel = 1 & empdata$MonthlyIncome >= 2670)~'HighMed', 
                                                                   (empdata$JobLevel = 1 & empdata$MonthlyIncome < 2670)~'LowMed', 
                                                                   (empdata$JobLevel = 2 & empdata$MonthlyIncome >= 5340)~'HighMed',
                                                                   (empdata$JobLevel = 2 & empdata$MonthlyIncome < 5340)~'LowMed', 
                                                                   (empdata$JobLevel = 3 & empdata$MonthlyIncome >= 9980)~'HighMed', 
                                                                   (empdata$JobLevel = 3 & empdata$MonthlyIncome < 9980)~'LowMed', 
                                                                   (empdata$JobLevel = 4 & empdata$MonthlyIncome >= 16154)~'HighMed', 
                                                                   (empdata$JobLevel = 4 & empdata$MonthlyIncome < 16154)~'LowMed', 
                                                                   (empdata$JobLevel = 5 & empdata$MonthlyIncome >= 19232)~'HighMed',
                                                                   (empdata$JobLevel = 5 & empdata$MonthlyIncome < 19232)~'LowMed'
                 ))
                 
                 
                 ####Adding a Age Group
                 empdata$AgeGroup <- as.factor(
                   ifelse(empdata$Age <=25,"Young", ifelse(
                     empdata$Age<=55,"Middle-Age","Adult"
                   ))
                 )
                 
                 
                 ###Adding A Total Satisfaction Variable
                 empdata$TotalSatisfaction <- 
                   as.numeric(empdata$EnvironmentSatisfaction)+
                   as.numeric(empdata$JobInvolvement)+
                   as.numeric(empdata$JobSatisfaction)+
                   as.numeric(empdata$RelationshipSatisfaction)+
                   as.numeric(empdata$WorkLifeBalance)
                 
                 summary(empdata)
                 str(empdata)
                 
                 
                 str(empdata)
                 ##### Changing Numeric to Factors
                 empdata$EnvironmentSatisfaction <- factor(ibmdat$EnvironmentSatisfaction)
                 empdata$JobInvolvement <- factor(ibmdat$JobInvolvement)
                 empdata$JobLevel <- factor(ibmdat$JobLevel)
                 empdata$JobSatisfaction <- factor(ibmdat$JobSatisfaction)
                 empdata$PerformanceRating <- factor(ibmdat$PerformanceRating)
                 empdata$RelationshipSatisfaction <- factor(ibmdat$RelationshipSatisfaction)
                 empdata$WorkLifeBalance <- factor(ibmdat$WorkLifeBalance)
                 empdata$EmployeeNumber <- as.character(ibmdat$EmployeeNumber)
                 empdata$EmployeeCount <- as.character(ibmdat$EmployeeCount) ###EmployeeCount and Hours Shouldn't be scaled
                 empdata$StandardHours <- as.character(ibmdat$StandardHours)
                 empdata$MonSalLevel <- factor(ibmdat$MonSalLevel)
                 empdata$Attrition <- factor(ibmdat$Attrition,levels = c("Yes","No"))
                 
                 empdata <- empdata %>%  mutate_if(is.numeric, scale)###Scale Numeric Variables
                 
                 empdata$EmployeeCount <- as.numeric(ibmdat$EmployeeCount)###Convert EMployeecount and Hours to Numeric
                 empdata$StandardHours <- as.numeric(ibmdat$StandardHours)
                empdata$EmployeeNumber <- as.numeric(ibmdat$EmployeeNumber)
                 #################################################################################################
                 ####################### Exploratory Data Analysis ###############################################
                 #################################################################################################
                 
                 ####Use Boruta Algorithm to find the most Significant variables affecting Attrition
                 ####This Increases Accuracy and also reduces time to process
                 
                 ###Let's list core steps how the Boruta algorithm works:
                 
                 #firstly, the data set is extended by adding copies of all variables (extending information system)
                 #added attributes are shuffled randomly in order to remove any corellation with the response variable
                 #random forest classifier is run on the whole data set and Z-scores are computed for all attributes (another importance measure implemented in basic random forest)
                 #out of all shadow attributes find the one with the maximum Z score and then assign a hit to every attribute that scored better than the one with maximum Z-score
                 #for each attribute with undetermined importance perform a two-sided test of equality with the the one obtained for shadow attribute with maximum Z-score
                 #mark the attributes which have importance signifcantly lower than the shadow with maximum Z-score as `unimportant' and permanently remove them from the dat aset
                 #remove all shadow, artificially added attributes
                 #repeat the procedure until the importance is assigned for all the attributes, or the algorithm has reached the previously set limit of the random forest runs.
                 
                set.seed(123) 
                boruta.train <- Boruta(Attrition ~. -EmployeeNumber, data = ibmdat, getImp=getImpFerns)
                 print(boruta.train)
                 plot(boruta.train)
                 ImpFeat <- attStats(boruta.train)
                 View(ImpFeat)
                 ImpFeat <- getSelectedAttributes(boruta.train,withTentative = T) ##Contains the Significant Variables
                 View(ImpFeat)
                 
                 ImpFeat  <- c(ImpFeat,'Attrition')
                 head(empdata[ImpFeat])
                 
                 ###This is UnBalanced Data with hardly 16% Yes so we are diving Test and Train with equal proportion
                 Attr_Div = table(empdata$Attrition)
                 Attr_Div
                 # Over Sampling
                 over = ( (0.6 * max(Attr_Div)) - min(Attr_Div) ) / min(Attr_Div)
                 over
                 # Under Sampling
                 under = (0.4 * max(Attr_Div)) / (min(Attr_Div) * over)
                 under
                 
                 over = round(over, 1) * 100
                 under = round(under, 1) * 100
                 over
                 under
                 #Generate the balanced data set
                 
                 BalancedData = SMOTE(Attrition~., empdata[ImpFeat], perc.over = over, k = 3, perc.under = under)
                 split <- sample.split(BalancedData,SplitRatio = 0.7)
                 train_attr <- subset(BalancedData,split==TRUE)
                 test <- subset(BalancedData,split==F)
                 
                 ######Balanced SVM
                 #Tune SVM Radial With Best cost
                 tune_out <- tune.svm(Attrition ~ ., data=train_attr, kernel="radial", gamma = 10^(-5:-1), cost =c(0.001, 0.01, 0.1, 1,5,10,12,15,20,22,25,26,27,28,29))
                 tune_out## 1 is best cost gamma 0.1
                 View(test)
                 SVM_classifier = svm(formula = Attrition ~ .,data = train_attr,type = 'C-classification',kernel = 'radial',cost=1,gamma = 0.1) 
                 SVM_Attr_Pred <- predict(SVM_classifier, newdata = test[-22]) 
                 
                 SVM_RES <- confusionMatrix(SVM_Attr_Pred,test$Attrition)
                 SVM_RES
                 
               
                 ##################SVM 86%
                 #################Balanced RF
                 RF_Class <- randomForest(Attrition ~ ., 
                                          data = train_attr,ntree = 5000)
                 RF_Class
                 RF_Attr_Pred <- predict(RF_Class, newdata = test[-22]) 
                 
                 summary(RF_Class)
                 RF_RES <- confusionMatrix(RF_Attr_Pred,test$Attrition)
                 RF_RES
                 ####################RF with K FOLD
                 ControlParameters <- trainControl(method = 'cv',
                                                   number = 10,
                                                   savePredictions = T,
                                                   classProbs = T)
                 
                 
                 parameterGrid = expand.grid(mtry = c(1:15))
                 method = 'cv'
                 classProbs= TRUE
                 
                 modelRandom = train(Attrition ~., 
                                     data = train_attr,
                                     method = 'rf',
                                     trControl = ControlParameters,
                                     tuneGrid = parameterGrid)
                 
                 modelRandom
                 
                 predictions <- predict(modelRandom, test)
                 confusionMatrix(predictions,test$Attrition)
                 
                 
                 #####################XGBOOST
                 
                 
                 indexes = sample(1:nrow(BalancedData), size=0.8*nrow(BalancedData))
                 xgtrain.Data <- BalancedData[indexes,]
                 xgtest.Data <- BalancedData[-indexes,]
                 View(xgtest.Data)
                 
                 fitControl <- trainControl(method="cv", number = 10,classProbs = TRUE )
                 xgbGrid <- expand.grid(nrounds = 500,
                                        max_depth = 30,
                                        eta = .03,
                                        gamma = 0,
                                        colsample_bytree = .7,
                                        min_child_weight = 1,
                                        subsample = 0.9
                 )
                 
                 XGB.model = train(Attrition~., data = xgtrain.Data,
                                   method = "xgbTree"
                                   ,trControl = fitControl
                                   , verbose=0
                                   , maximize=T
                                   ,tuneGrid = xgbGrid
                                   ,na.action = na.pass
                 )
                 
                 
                 XGB.prd <- predict(XGB.model,xgtest.Data)
                 XGB_RES <- confusionMatrix(XGB.prd, xgtest.Data$Attrition)
                 XGB_RES
                 