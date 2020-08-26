# Case Study Solutions : Jobs.csv file
#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 1:
# Reading the CSV file and dropping the first column
data=read.csv('jobs.csv')
# View the data loaded
data
# Dropping the 1st column which is a serial number
data=data[2:18]
# View the dimensions (shape) of the data to be used for the analysis
dim(data)
# There are 899 rows and 17 columns

#---------------------------------------------------------------------------------------------------------------
# Soln. to Question 2:

# The key objective of the case study is : 
# Model to determine the two key outcome variables; a continuous measure of depressive symptoms based on the Hopkins Symptom Checklist, and a binary variable, representing whether the respondent had become employed.
# The response variable : work1 (Indicator variable for employment. 1 = employed) 
# The data collected in the given excel file is grouped under which data class : "Cross Sectional Data"


#-------------------------------------------------------------------------------------------------

# Soln. to Question 3:

#Summarising the dataset : 
summary(data)

# Observations :
# There are no missing values in the dataset
# The mean age is 37.5

# Check the datatypes
str(data)
sapply(data, class)

#-------------------------------------------------------------------------------------------------

# Soln. to Question 4:

data$age<-cut(data$age,seq(0,90,10), right=FALSE)

freq_table <- table(data$age)
freq_table


freq_table1 <- table(data$age,data$work1)
freq_table1

#-------------------------------------------------------------------------------------------------

# Soln. to Question 5:

data$work1 <- ifelse(data$work1=="psyemp", 1, 0)

# Creating a frequency table

freq_table <- table(data$work1)
freq_table

# 606 observations are not having job ( unemployed )
#-------------------------------------------------------------------------------------------------

# Soln. to Question 6:

# Creating a 2 way frequency table

freq_table <- table(data$work1,data$sex)
freq_table

# Here: sex 1 is female ( More females are unemployed compared to males)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 7:

par(mfrow=c(1,1))
# Draw a bar plot to the table created above: 
options(warn=-1)
barplot(freq_table, main="Vote : Gender Wise Distribution",xlab="Female",legend = rownames(freq_table), stacked=TRUE)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 8:

data$job_seek<-ifelse(data$job_seek<=1,1,
                      ifelse(data$job_seek>1&data$job_seek<=2,2,
                             ifelse(data$job_seek>2&data$job_seek<=3, 3,
                                    ifelse(data$job_seek>3&data$job_seek<=4, 4,
                                          5))))
                                                 
data$depress1<-ifelse(data$depress1<=1,1,
                      ifelse(data$depress1>1&data$depress1<=2,2,
                             ifelse(data$depress1>2&data$depress1<=3,3,
                                    ifelse(data$depress1>3&data$depress1<=4, 4,
                                           5))))

data$depress2<-ifelse(data$depress2<=1,1,
                      ifelse(data$depress2>1&data$depress2<=2,2,
                             ifelse(data$depress2>2&data$depress2<=3, 3,
                                    ifelse(data$depress2>3&data$depress2<=4, 4,
                                           5))))

freq_table <- table(data$job_seek)
freq_table

freq_table <- table(data$depress1)
freq_table

freq_table <- table(data$depress2)
freq_table
#-------------------------------------------------------------------------------------------------
# Soln. to Question 9:

# Creating a 2 way frequency table

freq_table <- table(data$sex,data$depress1)
freq_table

# Here: sex 1 is female ( More females are unemployed compared to males)

par(mfrow=c(1,1))
# Draw a bar plot to the table created above: 
options(warn=-1)
barplot(freq_table, main="Depress 1(Pre Treatment) vs Gender Distribution",xlab="Female=1",legend = rownames(freq_table), stacked=TRUE)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 10:

# Creating a 2 way frequency table

freq_table <- table(data$sex,data$depress2)
freq_table

# Here: sex 1 is female ( More females are unemployed compared to males)

par(mfrow=c(1,1))
# Draw a bar plot to the table created above: 
options(warn=-1)
barplot(freq_table, main="Depress 2(Post Treatment) vs Gender Distribution",xlab="Female=1",legend = rownames(freq_table), stacked=TRUE)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 11:

# Creating a 2 way frequency table

freq_table <- table(data$sex,data$control)
freq_table

# Here: sex 1 is female ( More females are unemployed compared to males)

par(mfrow=c(1,1))
# Draw a bar plot to the table created above: 
options(warn=-1)
barplot(freq_table, main="Control group vs Gender Distribution",xlab="Female=1",legend = rownames(freq_table), stacked=TRUE)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 12:

# Creating a 2 way frequency table

freq_table <- table(data$work1,data$control)
freq_table

# Here: sex 1 is female ( More females are unemployed compared to males)

par(mfrow=c(1,1))
# Draw a bar plot to the table created above: 
options(warn=-1)
barplot(freq_table, main="Control group vs Response variable(work)",xlab="Emp=1",legend = rownames(freq_table), stacked=TRUE)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 13:

# Creating a 2 way frequency table

freq_table <- table(data$work1,data$marital)
freq_table

# Here: sex 1 is female ( More females are unemployed compared to males)

par(mfrow=c(1,1))
# Draw a bar plot to the table created above: 
options(warn=-1)
barplot(freq_table, main="Marital vs Response variable(work)",xlab="Emp=1",legend = rownames(freq_table), stacked=TRUE)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 14:

# Creating a 2 way frequency table

freq_table <- table(data$sex,data$marital)
freq_table

# Here: sex 1 is female ( More females are unemployed compared to males)

par(mfrow=c(1,1))
# Draw a bar plot to the table created above: 
options(warn=-1)
barplot(freq_table, main="Marital vs Gender Distribution",xlab="Female=1",legend = rownames(freq_table), stacked=TRUE)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 15:

# Creating a 2 way frequency table

freq_table <- table(data$sex,data$job_seek)
freq_table

# Here: sex 1 is female ( More females are unemployed compared to males)

par(mfrow=c(1,1))
# Draw a bar plot to the table created above: 
options(warn=-1)
barplot(freq_table, main="Job Seek vs Gender Distribution",xlab="Female=1",legend = rownames(freq_table), stacked=TRUE)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 16:


# Fit a logit model for work1 response variable

logit_model <- glm(work1 ~ .,
                   data = data,
                   family = binomial(link = "logit"))
summary(logit_model)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 17:

# As observed : > 99% significant variables are : depress2 (post treatment), age group b/w 50-60,non white group

# Also: AIC is 1118.7 with residual deviance of 1048.7 on 864 degrees of freedom

#-------------------------------------------------------------------------------------------------
# Soln. to Question 18:

# Objective of any logit model is to "maximize likelihood estimation"

# How do we achieve this objective ?

# A probability distribution for the target variable (class label) must be assumed and 
# then a likelihood function defined that calculates the probability of observing the outcome given the input data and the model. 
# This function can then be optimized to find the set of parameters that results in the largest sum likelihood over the training dataset.

#-------------------------------------------------------------------------------------------------
# Soln. to Question 19:

# Fit a logit model for work1 response variable

logit_model2 <- glm(work1 ~ depress1 + depress2 + control+age + sex + comply + job_dich + job_disc + econ_hard,
                    data = data,
                   family = binomial(link = "logit"))
summary(logit_model2)

#-------------------------------------------------------------------------------------------------
# Soln. to Question 20:

# As observed : > 99% significant variables are : depress2 (post treatment), age group b/w 50-60, depress 1 & sex variable

# Also: AIC is 1104.4 with residual deviance of 1074.4 on 884 degrees of freedom

# Lower AIC indicates better fit compared to the previous model