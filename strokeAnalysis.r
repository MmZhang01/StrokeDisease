# OPIM 5603 - Team 5 - Group Project
# Dataset - Heart Disease Predictor

# The TARGET VARIABLE in the dataset is "categorical" hence, we will use LOGISTIC REGRESSION for predicting the outcomes.

# Step 1 - We start by reading the CSV file and storing it as a data frame within R

  getwd()
 # setwd("C:/Users/ayush/Documents/00 Coursework/01 FALL 2021/03 OPIM 5603 - Statistics & R/Group Project")
  
  library(readxl)
  
  strokedata <- data.frame(read.csv("healthcare-dataset-stroke-data.csv"))
  head(strokedata)

## PRE-PROCESSING

# Column Headers - We edit the column headers for clarity
  colnames(strokedata) <- c("ID", "GENDER", "AGE", "Hypertension", "Heart-Disease", "Ever-Married", "Work-Type", "Residence-Type", "Average-Glucose-Level", "BMI", "Smoking-Status", "stroke")
  head(strokedata)

# Check for variable type
  str(strokedata)
  nrow(strokedata)

  ## Variable type update

# Gender - Conversion from CHAR to FACTOR
  strokedata[strokedata$GENDER == "Male",]$GENDER <- "M"
  strokedata[strokedata$GENDER == "Female",]$GENDER <- "F"
  
  # We only had one "Other" in gender in a list of ~5k records, hence we will remove that as an outlier.
  
  strokedata <- strokedata[strokedata$GENDER != "Other",]
  
  strokedata$GENDER <- as.factor(strokedata$GENDER)

# Hypertension - Conversion from INT to FACTOR
  strokedata$Hypertension <- as.factor(strokedata$Hypertension)


# Heart Disease - Conversion from INT to FACTOR
  strokedata$`Heart-Disease` <- as.factor(strokedata$`Heart-Disease`)


# Ever-Married - Conversion from CHAR to FACTOR
  strokedata[strokedata$`Ever-Married` == "Yes",]$`Ever-Married` <- "Y"
  strokedata[strokedata$`Ever-Married` == "No",]$`Ever-Married` <- "N"
  strokedata$`Ever-Married` <- as.factor(strokedata$`Ever-Married`)


# Work-Type - Conversion from CHAR to FACTOR
  strokedata[strokedata$`Work-Type` == "Private",]$`Work-Type` <- "P"
  strokedata[strokedata$`Work-Type` == "Self-employed",]$`Work-Type` <- "S"
  strokedata[strokedata$`Work-Type` == "Govt_job",]$`Work-Type` <- "G"
  strokedata[strokedata$`Work-Type` == "Never_worked",]$`Work-Type` <- "NW"
  strokedata[strokedata$`Work-Type` == "children",]$`Work-Type` <- "C"
  strokedata$`Work-Type` <- as.factor(strokedata$`Work-Type`)


# Residence-Type - Conversion from CHAR to FACTOR
  strokedata[strokedata$`Residence-Type` == "Urban",]$`Residence-Type` <- "U"
  strokedata[strokedata$`Residence-Type` == "Rural",]$`Residence-Type` <- "R"
  strokedata$`Residence-Type` <- as.factor(strokedata$`Residence-Type`)


# BMI - Conversion from CHAR to NUM
  strokedata$BMI <- as.numeric(strokedata$BMI)


# Smoking-Status - Conversion from CHAR to FACTOR
  strokedata[strokedata$`Smoking-Status` == "formerly smoked",]$`Smoking-Status` <- "FS"
  strokedata[strokedata$`Smoking-Status` == "never smoked",]$`Smoking-Status` <- "NS"
  strokedata[strokedata$`Smoking-Status` == "smokes",]$`Smoking-Status` <- "S"
  strokedata$`Smoking-Status` <- as.factor(strokedata$`Smoking-Status`)


# STROKE - Conversion from INT to FACTOR
  strokedata$stroke <- as.factor(strokedata$stroke)


# Checking the updated data types
  str(strokedata)


## Missing values Analysis

# We wanted to understand the number of irrelevant entries in the data set, especially the variables 

  # NA counts in BMI
  NA.count.BMI <- sum(is.na(strokedata$BMI))
  NA.count.BMI
  
  # NA counts in Average-Glucose-Level
  NA.count.AvgGlucLvl <- sum(is.na(strokedata$`Average-Glucose-Level`))
  NA.count.AvgGlucLvl
  
  # Unknown counts in Smoking-Status
  Unknw.count.SmokingStat <- sum(strokedata$`Smoking-Status` == 'Unknown')
  Unknw.count.SmokingStat
  
  # Smoking status for 1,544 patients is "UNKNOWN"


## IMPUTING DATA

# BMI - Imputation of NAs

# We see 201 rows of data where values are missing, we plan to impute the value in these rows 

  # mean.bmi.f.1 = Mean of BMI of all rows where GENDER = FEMALE and Stroke Prediction = 1
  mean.bmi.f.1 <- mean(strokedata$BMI[strokedata$stroke == 1 & strokedata$GENDER == 'F' & !is.na(strokedata$BMI)])
  mean.bmi.f.1
  
  # mean.bmi.f.1 = Mean of BMI of all rows where GENDER = FEMALE and Stroke Prediction = 0
  mean.bmi.f.0 <- mean(strokedata$BMI[strokedata$stroke == 0 & strokedata$GENDER == 'F' & !is.na(strokedata$BMI)])
  mean.bmi.f.0
  
  # mean.bmi.f.1 = Mean of BMI of all rows where GENDER = MALE and Stroke Prediction = 1
  mean.bmi.m.1 <- mean(strokedata$BMI[strokedata$stroke == 1 & strokedata$GENDER == 'M' & !is.na(strokedata$BMI)])
  mean.bmi.m.1
  
  # mean.bmi.f.1 = Mean of BMI of all rows where GENDER = MALE and Stroke Prediction = 0
  mean.bmi.m.0 <- mean(strokedata$BMI[strokedata$stroke == 0 & strokedata$GENDER == 'M' & !is.na(strokedata$BMI)])
  mean.bmi.m.0

# USING NESTED LOOPS TO IMPUTE VALUES

  for(i in 1:nrow(strokedata))
  {
    if(is.na(strokedata$BMI[i]) & strokedata$GENDER[i] == "F" & strokedata$stroke[i] == 1)
    {
      strokedata$BMI[i] <- mean.bmi.f.1
    }
    else if(is.na(strokedata$BMI[i]) & strokedata$GENDER[i] == "F" & strokedata$stroke[i] == 0)
    {
      strokedata$BMI[i] <- mean.bmi.f.0
    }  
    else if(is.na(strokedata$BMI[i]) & strokedata$GENDER[i] == "M" & strokedata$stroke[i] == 1)
    {
      strokedata$BMI[i] <- mean.bmi.m.1
    }
    else if(is.na(strokedata$BMI[i]) & strokedata$GENDER[i] == "M" & strokedata$stroke[i] == 0)
    {
      strokedata$BMI[i] <- mean.bmi.m.0
    } 
  }

  str(strokedata)

## PLOTTING PREDICTOR VARIABLES vs TARGET VARIABLE ("STROKE") - TO check for patterns in correlation
  
  #heatmap
  library(dplyr)
  strokedata.numeric <- select(strokedata, AGE, BMI, `Average-Glucose-Level`)
  cor.mydata<- cor(strokedata.numeric)
  heatmap(x = cor.mydata, col = col, symm = TRUE)
  
  #correlation matrix
  library(corrplot)
  corrplot(cor.mydata)
  
  
  library(ggplot2)
  library(dplyr)
  
# GENDER vs STROKE
   
  strokedata1<- strokedata %>%
    count(stroke,GENDER) %>%
    group_by(stroke)%>%
    arrange(stroke,desc(GENDER))%>%
    mutate(gender_pct=n/sum(n))%>%
    mutate(label_y=cumsum(gender_pct))
  ggplot(strokedata1,aes(x=stroke,fill=GENDER,y=gender_pct))+geom_col(position = "stack")+geom_label(y=strokedata1$label_y,label=round(strokedata1$gender_pct,2))

  # shows very slight increase in MALE proportion for patients with stroke, not relevant enough.
  
# AGE vs STROKE
  plot(strokedata$AGE~strokedata$stroke) 
  ggplot(strokedata, aes(stroke, AGE), overlay = GENDER) + 
    geom_boxplot(aes(color = GENDER))
  # We observe a higher mean age for people who got a stroke which suggests a high correlation between AGE and STROKE
  # High age females are at higher risk of STROKE

# Hypertension vs STROKE
 
  strokedata2<- strokedata %>%
    count(stroke,Hypertension) %>%
    group_by(stroke)%>%
    arrange(stroke,desc(Hypertension))%>%
    mutate(Hypertension_pct=n/sum(n))%>%
    mutate(label_y=cumsum(Hypertension_pct))
  ggplot(strokedata2,aes(x=stroke,fill=Hypertension,y=Hypertension_pct))+geom_col(position = "stack")+geom_label(y=strokedata2$label_y,label=round(strokedata2$Hypertension_pct,2),position =position_nudge(x=0,y = -0.05))

  # People with hypertension have higher proportion of patients having stroke.
  
# Heart-Disease vs STROKE
  
  strokedata3<- strokedata %>%
    count(stroke,`Heart-Disease`) %>%
    group_by(stroke)%>%
    arrange(stroke,desc(`Heart-Disease`))%>%
    mutate(HeartDisease_pct = n/sum(n))%>%
    mutate(label_y=cumsum(HeartDisease_pct))
  ggplot(strokedata3,aes(x=stroke,fill=`Heart-Disease`,y=HeartDisease_pct))+geom_col(position = "stack")+geom_label(y=strokedata3$label_y,label=round(strokedata3$`HeartDisease_pct`,2),position =position_nudge(x=0,y = -0.05))

  # People with HEART-DISEASE have higher proportion of patients having stroke.
  
# Ever-Married vs STROKE

  strokedata4<- strokedata %>%
    count(stroke,`Ever-Married`) %>%
    group_by(stroke)%>%
    arrange(stroke,desc(`Ever-Married`))%>%
    mutate(EverMarried_pct = n/sum(n))%>%
    mutate(label_y=cumsum(EverMarried_pct ))
  ggplot(strokedata4,aes(x=stroke,fill=`Ever-Married`,y=EverMarried_pct))+geom_col(position = "stack")+
    geom_label(y=strokedata4$label_y,label=round(strokedata4$`EverMarried_pct`,2))
  
  # People with a history of marriage have higher proportion of patients having stroke.
  
# Work-type vs STROKE
 
  strokedata5<- strokedata %>%
    count(stroke,`Work-Type`) %>%
    group_by(stroke)%>%
    arrange(stroke,desc(`Work-Type`))%>%
    mutate(WorkType_pct = n/sum(n))%>%
    mutate(label_y=cumsum(WorkType_pct))
  ggplot(strokedata5,aes(x=stroke,y=WorkType_pct,fill=`Work-Type`))+geom_col(position = "stack")+
    geom_label(aes(y=strokedata5$label_y,label=round(strokedata5$`WorkType_pct`,2)))
  help(geom_label)
  
  # Self employed people are more likely to suffer a stroke, as per the dataset

# Residence-type vs STROKE
  
  strokedata6<- strokedata %>%
    count(stroke,`Residence-Type`) %>%
    group_by(stroke)%>%
    arrange(stroke,desc(`Residence-Type`))%>%
    mutate(ResidenceType_pct = n/sum(n))%>%
    mutate(label_y=cumsum(ResidenceType_pct))
  ggplot(strokedata6,aes(x=stroke,y=ResidenceType_pct,fill=`Residence-Type`))+geom_col(position = "stack")+
    geom_label(aes(y=label_y,label=round(strokedata6$`ResidenceType_pct`,2)))
 
  # Not much relevant for stroke prediction
  
# BMI vs STROKE
  ggplot(strokedata, aes(stroke, BMI)) + geom_boxplot()
  # We observe a slightly elevated lower quantile for people who got a stroke
  # This suggests that on average, people who got a stroke had higher a BMI
  
# Average Glucose Level (AGL) vs STROKE
  ggplot(strokedata, aes(stroke, `Average-Glucose-Level`), overlay = GENDER) + 
    geom_boxplot(aes(color = GENDER))
  
  # We observe a higher mean AGL for people who got a stroke along with higher upper quantile which suggests a high correlation between high AGL and STROKE
  # Out of patients who suffered a stroke, Males had a slightly higher AGL 

  
# Smoking Status vs STROKE

   strokedata7<- strokedata %>%
    count(stroke,`Smoking-Status`) %>%
    group_by(stroke)%>%
    arrange(stroke,desc(`Smoking-Status`))%>%
    mutate(SmokingStatus_pct=n/sum(n))%>%
    mutate(label_y=cumsum(SmokingStatus_pct))
  ggplot(strokedata7,aes(x=stroke,fill=`Smoking-Status`,y=SmokingStatus_pct))+geom_col(position = "stack")+geom_label(y=strokedata7$label_y,label=round(strokedata7$SmokingStatus_pct,2))
 
  # We don't observe a noticeable impact of smoking status on the TARGET VARIABLE (i.e. Stroke)
  # Additionally, we calculated that the smoking status for 1,544 patients was UNKNOWN, hence we will not include this column for our LOGISTIC REGRESSION model.
  

## LOGISTIC REGRESSION

  logistic1 <- glm(stroke~GENDER + AGE + BMI + Hypertension + `Heart-Disease` + `Ever-Married` + `Work-Type` + `Average-Glucose-Level`, data = strokedata, family = "binomial")
  summary(logistic1)
  
  # We do not observe any significant relationship between predictors and STROKE (target variable) except for AGE, Hypertension and AVERAGE GLUCOSE LEVEL
  # Hence, we re-run the analysis with just 3 predictors
  
  logistic <- glm(stroke~ AGE + Hypertension + `Average-Glucose-Level`, data = strokedata, family = "binomial")
  summary(logistic)
  
  predicted.data <- data.frame(probability.of.stroke = logistic$fitted.values, stroke = strokedata$stroke)
  predicted.data.plot <- predicted.data[order(predicted.data$probability.of.stroke, decreasing = FALSE),]
  predicted.data.plot$rank <- 1:nrow(predicted.data.plot)
  
  library(ggplot2)
  # install.packages("cowplot")
  library(cowplot)
  
  ggplot(data = predicted.data.plot, aes(x = rank, y = probability.of.stroke)) +
    geom_point(aes(color = stroke), alpha = 1, shape = 3, stroke = 2) + 
    xlab("Index") +
    ylab("Predicted probability of stroke")

  # The graph illustrate higher concentration of 1s towards higher "probability of stroke" inputs.
  # We could still observe that the model does not predict a probability of stroke higher than ~40%.
  # Hence, the predictor variables do not have strong significance from prediction point of view.

