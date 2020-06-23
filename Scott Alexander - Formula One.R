if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")

###########################   READ ME ###################################
## Data is acquired from the Kaggle Formula One repository in a zip file.
# PLEASE NOTE: I had substantial issues downloading and processing the file
# from my laptop. I encountered these same problems in the Movielens project
# Therefore, I downloaded and created my source file manually. It is included in
# my submission. 
#########################################################################

#url <- "https://www.kaggle.com/cjgdev/formula-1-race-data-19502017/download/5571_8322_bundle_archive.zip"
# qualifying <- read.csv(file = 'qualifying.csv') %>% select(raceId,driverId,position)
# races <- read.csv(file = 'races.csv') %>% select(raceId, circuitId, year,name)
# results <- read.csv(file = 'results.csv') %>% select(raceId, driverId, position, statsId)
# drivers <- read.csv(file = 'drivers.csv') %>% select(driverId, surname)
# status <- read.csv(file = 'status.csv') %>% select(statusId, status)

#Set working directory
setwd("C:\\Users\\scott.alexander\\projects\\FormulaOne")

#Read in the primary data file created from the above URL
F1_Results <- read.csv(file = 'F1_Results.csv')


#Build Results Master data set. Filter out NA values and create a new column - Position_diff
F1_Results <- F1_Results %>% filter(!is.na(F1_Results$Results_position)) %>%
  mutate(Position_diff = Qualifying_position - Results_position) %>%
  select(Results_position,Qualifying_position,Position_diff,DidWin,status) 
summary(F1_Results)

#Build dataset for Did Not Finish (DNF). Exclude records who finished the race and were not penalized
F1_DNF <- F1_Results %>% filter(F1_Results$status!="Finished") %>% filter(!str_detect(status, "Lap")) 
summary(F1_DNF)

#set up our random seed
set.seed(123)

#Create a test and train set consisting of 75% of the data for training
indxTrain <- createDataPartition(y = F1_Results$Qualifying_position,p = 0.75,list = FALSE)
training <- F1_Results[indxTrain,]
testing <- F1_Results[-indxTrain,]

#Check the training and test sets to ensure the distribution for each position is valid
nrow(testing)
prop.table(table(testing$Qualifying_position)) * 100
nrow(training)
prop.table(table(training$Qualifying_position)) * 100

#Run the model on the training data. We have two models, lm and svm.  

################################################
## to run the lm model uncomment the following three lines of code and comment out
## the 5 lines of code in the svn section
################################################

#lm_model <- lm(Results_position ~ Qualifying_position, data = training)
#summary(lm_model)
#pred <- round(predict(lm_model, data.frame(Qualifying_position=1:26)), digits =0)

################################################
## to run the svm model uncomment the following 5 lines of code and comment out
## the previous 3 lines of code in the lm section above
##
##    NOTE - RUNNING SVM WILL TAKE 30 to 60 MINUTES!!!!!!!!! 
##           TO REDUCE THIS RUN FEWER INTERATIONS SUCH AS
##          seq(0,1,0.25), cost = (1:3)))
################################################

model <- tune(svm, Results_position ~ Qualifying_position, data = training,
       ranges = list(epsilon = seq(0,1,0.1), cost = (1:10)))
summary(model)
print(model$best.model)
pred <- round(predict(model$best.model, data.frame(Qualifying_position=1:26)), digits =0)


pred <- as.data.frame(pred)
pred$Qualifying_position <- 1:nrow(pred)

TestingOutput <- inner_join(testing, pred, by="Qualifying_position")
FinalResults <- TestingOutput %>% mutate(pred_diff = pred-Results_position) %>%
select(Results_position,pred, pred_diff)


###############################################################
####      OUTPUTS
###############################################################

# Qualifying vs. Results Boxplot

sp <- ggplot(F1_Results, aes(x = factor(Results_position), y = Qualifying_position)) + 
  geom_boxplot() +
  ggtitle("Qualifying vs. Results Boxplot") 
sp

scatter.smooth(x=F1_Results$Results_position, y=F1_Results$Qualifying_position, main="Scatter Plot with smoothed line") #, main="Dist ~ Speed")

## Linear Model Fitted to Data
ggplot(data = training, aes(x = Results_position, y = Qualifying_position)) +
  geom_point() +
  stat_smooth(method = "lm", col = "royalblue4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Linear Model Fitted to Data") +
  theme_stata()

#Actual Qualifying vs. Results by Count
F1_Summary <- F1_Results %>%
  select(Results_position,Qualifying_position) %>%
  group_by(Results_position,Qualifying_position) %>%
  tally(name="count")
p <- ggplot(data=F1_Summary, aes(Results_position, Qualifying_position)) +
  geom_point(aes(size=count)) +
  ggtitle("Actual Qualifying vs. Results by Count") + 
  theme_stata()


#First Place Finishes by Qualifying Position
ft <- F1_Results %>% filter(Results_position == 1)
ggplot(data=ft,aes(Qualifying_position)) + 
  geom_histogram(binwidth = 1,color = "black", fill = "royalblue4") +
  ggtitle("First Place Finishes by Qualifying Position") +
  theme_stata()

#Actual Finish vs. Predicted by Count
Temp <- FinalResults %>%
  select(Results_position,pred) %>%
  group_by(Results_position,pred) %>%
  tally(name="count")
p <- ggplot(data=Temp, aes(Results_position,pred)) +
  geom_point(aes(size=count)) +
  ggtitle("Actual Finish vs. Predicted by Count") + 
  theme_stata()
p

#Differences in Predicted Position vs. Actual Position
ggplot(data=FinalResults, aes(pred_diff)) +
  geom_bar(color = "black", fill = "royalblue4") +
  ggtitle("Differences in Predicted Position vs. Actual Position")

## Histogram DNF
ggplot(data=F1_DNF,aes(status)) + 
  geom_bar(color = "black", fill = "royalblue4") +
  ggtitle("Cause of DNF") +
  theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2)) 

F1_PosDiff <- F1_Results %>%
  select(Results_position,Position_diff) %>%
  group_by(Results_position,Position_diff) %>%
  tally(name="count")

q <- ggplot(data=F1_PosDiff, aes(Position_diff)) +
  geom_bar(color = "black", fill = "royalblue4") +
  ggtitle("Differences between Start and Finish Positions")  
q

#create the detailed output 
write.csv(FinalResults, "C:\\Users\\scott.alexander\\projects\\FormulaOne\\F1Output.csv", na = "", row.names=FALSE)














