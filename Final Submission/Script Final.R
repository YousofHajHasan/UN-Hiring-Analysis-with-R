library(tidyverse)

library(readxl)
Recruitment <- read_excel("Cleaned Data.xlsx", 
                          sheet = "Recruitment")

Roster <- read_excel("Cleaned Data.xlsx", 
                     sheet = "Roster")


sum(is.na(Roster))


Roster <- na.omit(Roster) ## Dropping Nulls


########################################################################
UniqueRecruitment <- Recruitment %>% 
  group_by(`Applicant Number`) %>% 
  filter(`Selection Year` == max(`Selection Year`),)

n_distinct(UniqueRecruitment$`Applicant Number`)
nrow(UniqueRecruitment)
#As we can see here, There's a record that has the same applicant number and the same Selction Year 
#So we decided to keep it.

UniqueRoster <- Roster %>% 
  group_by(`Applicant Number`) %>% 
  filter(`RosterDate` == max(`RosterDate`),)
#There's an issue : There are applicants have the same number and the same Roster Date

duplicate_rows <- duplicated(UniqueRoster$`Applicant Number`)

UniqueRoster <- UniqueRoster[!duplicate_rows,]
#nrow(UniqueRoster)

Rostered <- UniqueRecruitment %>%
  filter(`JO Selection` == "Roster")
#nrow(Rostered)

unRostered <- Recruitment %>%
  filter(`JO Selection` == "Non-roster")

df <- merge(Rostered,UniqueRoster,by = "Applicant Number")
#n_distinct(df$`Applicant Number`)
#nrow(df)
#########################################################################
df <- subset(df, select = -c(`JO Type.y`,`Job Network.y`,`Gender.y`,`Job Code.y`
                             ,`Job Family.y`,`Nationality.y`,Entity,`Job_Family`
                             ,`Job Family.x`,`JO Number`,`Job Code.x`,Grade))
view(df)
########################################################################
#Firstly : Start with Recruitment DataFrame:
Recruitment[sapply(Recruitment, is.character)] <- 
  lapply(Recruitment[sapply(Recruitment, is.character)],as.factor)

glimpse(Recruitment)
Recruitment$Level <- as.character(Recruitment$Level)
Recruitment$Level <- factor(Recruitment$Level,
                            levels = c("P-3","P-4","P-5"),
                            ordered = TRUE)
Recruitment$SelectionMonth <- as.Date(Recruitment$SelectionMonth,format = "%d/%m/%Y")
Recruitment$PostingMonth <- as.Date(Recruitment$PostingMonth,format = "%d/%m/%Y")

#######################################################################
#Preparing the data : 
Recruitment <- subset(Recruitment, select = -c(Entity))
#Dropping Job_Family; Job Family has the same values : 
Recruitment <- subset(Recruitment, select = -c(`Job_Family`))
Recruitment <- subset(Recruitment, select = -c(`Job Family`))
Recruitment <- subset(Recruitment, select = -c(`JO Number`))
Recruitment <- subset(Recruitment, select = -c(`Applicant Number`))
Recruitment <- subset(Recruitment, select = -c(`Job Code`))

dim(Recruitment)

#######################################################################

Recruitment %>%
  group_by(`Selection Year`) %>%
  count() %>%
  ggplot(aes(x = `Selection Year`,y = n)) +
  geom_line(color = "Blue",size = 1.5, alpha = 0.5) + 
  labs(title = "Hired over Years",x = "Years",y = "Number of Hired") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_point()

#The number of hired applicants is decreasing in 2020, this is maybe caused by the Corona pandemic

Recruitment %>%
  group_by(`JO Selection`) %>%
  count()
#The Numbers of Non-Roster applicants is little higher than Roster applicants 
view(Recruitment)

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

#SAFETYNET is more likely to be located at the P-3 level.
#SCINET appeared always at level P-4.
#The most Job Network that could be P-5 is DEVNET.
#SCINET and SAFETYNET are not appear at the P-5 level.
#SCINET and SAFETYNET are not found at the P-5 level.

########################################################################
africa <- sum(Roster$`Nationality Region` == "African")
Asia <- sum(Roster$`Nationality Region` == "Asia-Pacific")
EasternEuropean <- sum(Roster$`Nationality Region` == "Eastern European")
Latin<- sum(Roster$`Nationality Region` == "Latin American and Caribbean")
WesternEurope<- sum(Roster$`Nationality Region` == "Western Europe and Others")

x <- df %>%
    group_by(Region) %>%
    summarize(Count = n()) %>%
    mutate(Per = ifelse(Region == "African", Count/africa, 
                 ifelse(Region == "Asia-Pacific",Count/Asia,
                 ifelse(Region == "Eastern European",Count/EasternEuropean,
                 ifelse(Region == "Latin American and Caribbean",Count/Latin,
                 ifelse(Region == "Western Europe and Others",Count/WesternEurope,0
                       ))))))
x %>%
  ggplot(aes(x = Region,y = Per,fill = Region,label = scales::percent(Per))) +
  geom_col(position = "dodge") +
  labs(title = "Percentage of get hired from roster based on Region",x = "Region",y = "Percentage") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(limits = c(0, 10/100), labels = scales::percent) +
  geom_text(position = position_dodge(width = 1),hjust = 0.1) +
  coord_flip()
#We can conclude that Africa has the highest probability of being hired from roster
#Applicants from Africa are almost twice as likely to get a job as those who are
#from Asia
########################################################################
Male <- sum(Roster$Gender == "Male")
Female <- sum(Roster$Gender == "Female")

unique_values <- unique(Roster$Gender)

x <- df %>%
    group_by(Gender.x) %>%
    summarize(Count = n()) %>%
    mutate(Per = ifelse(Gender.x == "Male",Count/Male,
                 ifelse(Gender.x == "Female",Count/Female,0)))

x %>%
  ggplot(aes(x = Gender.x,y = Per,label = scales::percent(Per))) +
  geom_col(position = "dodge") +
  labs(title = "Percentage of get hired based on Gender",x = "Gender",y = "Percentage") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 10/100), labels = scales::percent) +
  geom_text(position = position_dodge(width = 0.8)) +
  coord_flip()

#There is no big difference between Male and Females in getting hired
#######################################################################
#Age over levels
df %>%
  group_by(Level,Age) %>%
  ggplot(aes(x = Level,y = Age,fill = Level)) +
  labs(title = "Levels and ages",x = "Level",y = "Age") +
  geom_boxplot() +
  theme_classic()

#There's a positive relation between Age and level 
#Higher Age = Higher Level 
#The lowest age to be in Level 5 is nearly 40
#######################################################################

one <- sum(Roster$`Fluent Language(s)` == "1 language")
two <- sum(Roster$`Fluent Language(s)` == "2 languages")
three <- sum(Roster$`Fluent Language(s)` == "3 languages")
four <- sum(Roster$`Fluent Language(s)` == "4 languages")
five <- sum(Roster$`Fluent Language(s)` == "5 languages")


x <- df %>%
    group_by(`Fluent Language(s)`) %>%
    summarize(Count = n()) %>%
    mutate(Per = ifelse(`Fluent Language(s)` == "1 language",Count/one,
                 ifelse(`Fluent Language(s)` == "2 languages",Count/two,
                 ifelse(`Fluent Language(s)` == "3 languages",Count/three,
                 ifelse(`Fluent Language(s)` == "4 languages",Count/four,
                 ifelse(`Fluent Language(s)` == "5 languages",Count/five,0))))))

x %>%
  ggplot(aes(x = `Fluent Language(s)`,y = Per,fill = `Fluent Language(s)`,label = scales::percent(Per))) +
  geom_col(position = "dodge") +
  labs(title = "Percentage of get hired from roster based on Number of Languages",x = "# Languages",y = "Percentage") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 20/100), labels = scales::percent) +
  geom_text(position = position_dodge(width = 0.8))

#So, We can clearly see that the increase in your fluent languages will increase
#the probability of being hired 
#The applicants with 5 languages are six times more likely to be hired than applicants 
#with 4 languages 

glimpse(df)

Yes <- sum(Roster$`English-Fluent` == "Y")
No <- sum(Roster$`English-Fluent` == "N")

#Applicant Must be Fluent in English to get hired
###########################################################################

internal <- sum(Roster$`Account Status` == "Internal")
external <- sum(Roster$`Account Status` == "External")

  x <- df %>%
    group_by(`Account Status`) %>%
    summarize(Count = n()) %>%
    mutate(Per = ifelse(`Account Status` == "Internal",Count/internal,
                 ifelse(`Account Status` == "External",Count/external,0)))

x %>%
  ggplot(aes(x = `Account Status`,y = Per,fill = `Account Status`,label = scales::percent(Per))) +
  geom_col(position = "dodge") +
  labs(title = "Percentage of get hired based on Account Status",x = "Account Status",y = "Percentage") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 10/100), labels = scales::percent) +
  geom_text(position = position_dodge(width = 0.8))

#The chances of being hired are 2.5 times higher for internal applicants as compared to external applicants.
##########################################################################
#Comparison between most common nationalities : 

Rostered %>%
  group_by(Nationality) %>%
  count() %>%
  filter(n > 10) %>%
  ggplot(aes(x = Nationality, y = n,fill = Nationality)) +
  geom_col(position = "dodge") + 
  labs(title = "Most common recruited applicants from roster based on nation",x = "Nation",y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()


usa <- sum(Roster$Nationality == "United States of America")
France <- sum(Roster$Nationality == "France")
Germany <- sum(Roster$Nationality == "Germany")
Canada <- sum(Roster$Nationality == "Canada")
uk <- sum(Roster$Nationality == "United Kingdom")

Nations <- c("United States of America","France","Germany","Canada","United Kingdom")




x <- df %>%
    group_by(Nationality.x) %>%
    filter(Nationality.x %in% Nations) %>%
    summarize(Count = n()) %>%
    mutate(Per = ifelse(Nationality.x == "United States of America",Count/usa,
                 ifelse(Nationality.x == "France",Count/France,
                 ifelse(Nationality.x == "Germany",Count/Germany,
                 ifelse(Nationality.x == "Canada",Count/Canada,
                 ifelse(Nationality.x == "United Kingdom",Count/uk,0))))))

x %>%
  ggplot(aes(x = Nationality.x,y = Per,fill = Nationality.x,label = scales::percent(Per))) +
  geom_col(position = "dodge") +
  labs(title = "Percentage of get hired from roster based on Nationality",x = "Nation",y = "Percentage") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 10/100), labels = scales::percent) +
  geom_text(position = position_dodge(width = 0.8)) + 
  coord_flip()






