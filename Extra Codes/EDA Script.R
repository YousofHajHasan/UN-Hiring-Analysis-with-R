library(tidyverse)
library(dplyr)
library(readxl)
Recruitment <- read_excel("G:/My Drive/دراسة/2022-2023/Data Visualisation/Project/Phase 1/Final Datasets/Recruitment.xlsx")
View(Recruitment)

library(readxl)
Roster <- read_excel("G:/My Drive/دراسة/2022-2023/Data Visualisation/Project/Phase 1/Final Datasets/Roster.xlsx")
View(Roster)

#############################################################################

Recruitment[sapply(Recruitment, is.character)] <- 
  lapply(Recruitment[sapply(Recruitment, is.character)],as.factor)

glimpse(Recruitment)
Recruitment$Level <- as.character(Recruitment$Level)
Recruitment$Level <- factor(Recruitment$Level,
                            levels = c("P-3","P-4","P-5"),
                            ordered = TRUE)
Recruitment$SelectionMonth <- as.Date(Recruitment$SelectionMonth)
Recruitment$PostingMonth <- as.Date(Recruitment$PostingMonth)
#############################################################################

summary(Recruitment)


#############################################################################
#Dropping the similar Columns : As written in my report : 
#Dropping Entity;'Entity Type’ has more meaningful names
Recruitment <- subset(Recruitment, select = -c(Entity))
#Dropping Job_Family; Job Family has the same values : 
Recruitment <- subset(Recruitment, select = -c(`Job_Family`))
Recruitment <- subset(Recruitment, select = -c(`Job Family`))
Recruitment <- subset(Recruitment, select = -c(`JO Number`))
#Recruitment <- subset(Recruitment, select = -c(`Applicant Number`))
Recruitment <- subset(Recruitment, select = -c(`Job Code`))

dim(Recruitment)
#############################################################################

view(Recruitment[Recruitment$`Applicant Number` == 19111809,])
ApplicantDesc <- Recruitment$`Applicant Number`[order(Recruitment$`Applicant Number`, decreasing = TRUE)]
head(ApplicantDesc)

#############################################################################
#Selection Year relation with Level

Recruitment %>%
  count(`Selection Year`)


SelectYear1 <- Recruitment %>%
  group_by(`Selection Year`,Level) %>%
  summarize(Count = n()) %>%
  mutate(Per = Count/sum(Count)) %>%
  ggplot(aes(x = Level,y = Per,fill = Level,label = scales::percent(Per))) +
  geom_col(position = "dodge") +
  facet_wrap(vars(`Selection Year`)) + 
  labs(title = "Levels over Years",x = "Level",y = "Percentage") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 100/100), labels = scales::percent) +
  geom_text(position = position_dodge(width = 0.8))
SelectYear1

SelectYear2 <- Recruitment %>%
  group_by(`Selection Year`,Level) %>%
  mutate(Count =  n()) %>%
  ggplot(aes(x = `Selection Year`,y = Count,fill = Level)) +
  geom_col(position = "dodge") +
  facet_wrap(vars(`Level`)) +
  labs(title = "Years per Levels",x = "Level",y = "Percentage") +
  theme_classic()
SelectYear2



#We can conclude from the above Charts that there are reduction in Number of 
#people employed in 2020, this maybe caused by the Corona pandemic,
#In my opinion, there's no clear relation between Level and the Selection Year
#But the highest year in percentage of hiring P-3 was 2020 50%
#The highest year in percentage of hiring P-4 is 2018 39%
#The highest year in percentage of hiring p-5 is 2021 28%

#############################################################################
#JO Type relation with Level
Recruitment %>%
  count(`JO Type`)


JobType1 <- Recruitment %>%
  group_by(`JO Type`,Level) %>%
  summarize(Count = n()) %>%
  mutate(Per = Count/sum(Count)) %>%
  ggplot(aes(x = Level,y = Per,fill = Level,label = scales::percent(Per))) +
  geom_col(position = "dodge") +
  facet_wrap(vars(`JO Type`)) + 
  labs(title = "Types of Job types of different Levels",x = "Level",y = "Percentage") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 100/100), labels = scales::percent) +
  geom_text(position = position_dodge(width = 0.8))
JobType1

#We can conclude from this:
#The jobs that require only rostered candidates can apply for it "RfR" is lower 
#than Jobs that can select candidates who are rostered or not rostered
#and the RfR jobs most likely to be at level P-3
#############################################################################
#Job Network relation with Level:

Recruitment %>%
  count(`Job Network`)


JobNetwork1 <- Recruitment %>%
  group_by(`Job Network`,Level) %>%
  summarize(Count = n()) %>%
  mutate(Per = Count/sum(Count)) %>%
  ggplot(aes(x = `Job Network`,y = Per,fill = Level,label = scales::percent(Per))) +
  geom_col(position = "dodge") +
  facet_wrap(vars(`Level`)) + 
  labs(title = "Job Network on different Levels",x = "Job Network",y = "Percentage") +
  theme_classic() + 
  scale_y_continuous(limits = c(0, 100/100), labels = scales::percent) +
  geom_text(position = position_dodge(width = 0.8)) +
  coord_flip()
JobNetwork1

#This plot contains useful information:
#SAFETYNET is more likely to be located at the P-3 level.
#SCINET appeared always at level P-4.
#The most Job Network that could be P-5 is DEVNET.
#SCINET and SAFETYNET are not appear at the P-5 level.
#SCINET and SAFETYNET are not found at the P-5 level.

############################################################################
#Gender
Recruitment %>%
  count(Gender)

Gender1 <- Recruitment %>%
  group_by(Level,Gender) %>%
  summarize(Count = n()) %>%
  mutate(Per = Count/sum(Count)) %>%
  ggplot(aes(x = `Level`,y = Per,fill = Gender,label = scales::percent(Per))) +
  geom_col(position = "dodge") +
  labs(title = "Genders and Levels",x = "Level",y = "Percentage") +
  theme_classic() + 
  scale_y_continuous(limits = c(0, 100/100), labels = scales::percent) +
  geom_text(position = position_dodge(width = 0.8))
Gender1

#There's No clear relation between Gender and levels

#Gender and Job Network:
GenderWithJobNet <- Recruitment %>%
  group_by(`Job Network`,Level,Gender) %>%
  summarize(Count = n()) %>%
  mutate(Per = Count/sum(Count)) %>%
  ggplot(aes(x = `Job Network`,y = Per,fill = Gender,label = scales::percent(Per))) +
  geom_col(position = "dodge") +
  facet_wrap(vars(`Level`)) + 
  labs(title = "Job Network on different Levels",x = "Job Network",y = "Percentage") +
  theme_classic() + 
  scale_y_continuous(limits = c(0, 100/100), labels = scales::percent) +
  geom_text(position = position_dodge(width = 0.8)) +
  coord_flip()
GenderWithJobNet

#So, Now we got more information:
#The LEGALNET Females don't appear at P-4 and P-5.
#All SCINET are Females.
#ITECNET has more males, also P-5 consists of Males.


contingency_table <- table(Recruitment$`Gender`,Recruitment$`Level`)
contingency_table
chisq_test <- chisq.test(contingency_table)
chisq_test


############################################################################
#Nationality
x <- Recruitment %>%
  count(Nationality) %>%
  filter(n > 10)
x[order(x$`n`,decreasing = TRUE),]


#The most five nation in hiring: 
#1- United States of America: 53 applicants
#2- France: 44 applicants
#3- Germany: 34 applicants
#4- China: 28 applicants
#5- Canada: 26 applicants
#5- Italy: 26 applicants

#Can't tell a real information from this column
############################################################################
#Region
Recruitment %>%
  count(Region)

#The most number of applicants are from Western Europe and Others

Recruitment %>%
  group_by(Region,Level,Gender) %>%
  summarize(Count = n()) %>%
  mutate(Per = Count/sum(Count)) %>%
  ggplot(aes(x = `Level`,y = Per,fill = Gender,label = scales::percent(Per))) +
  geom_col(position = "dodge") +
  facet_wrap(vars(`Region`)) + 
  labs(title = "Region and Levels",x = "Region",y = "Percentage") +
  theme_classic() + 
  scale_y_continuous(limits = c(0, 100/100), labels = scales::percent) +
  geom_text(position = position_dodge(width = 0.8))


#Eastern European are rarely to be at level P-5 
#Most of African applicants at different levels are from males.

############################################################################
col1 <- Recruitment$`Applicant Number`
col2 <- Roster$`Applicant Number`

# Get the similar records
similar_records <- as.factor(intersect(col1, col2))

# View the similar records
n_distinct(similar_records)

#571 Records
############################################################################  
Recruitment %>%
  count(`Posting Year`)
Recruitment$NumOfYears <- as.numeric(as.character(Recruitment$`Selection Year`)) - 
                         as.numeric(as.character(Recruitment$`Posting Year`))

Recruitment %>%
  group_by(NumOfYears,Level) %>%
  summarize(Count = n()) %>%
  mutate(Per = Count/sum(Count)) %>%
  ggplot(aes(x = `NumOfYears`,y = Per,fill = Level,label = scales::percent(Per))) +
  geom_col(position = "dodge") +
  facet_wrap(vars(`Level`)) + 
  labs(title = "Region and Levels",x = "Region",y = "Percentage") +
  theme_classic() + 
  scale_y_continuous(limits = c(0, 100/100), labels = scales::percent) +
  geom_text(position = position_dodge(width = 0.8))





