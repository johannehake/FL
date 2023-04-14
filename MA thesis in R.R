
# 1.0. Preparations -------------------------------------------------------

# Loading the required libraries
library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(readxl)
library(ggpubr)
library(DescTools)
library(hhi)
library(scales)
library(hrbrthemes)
library(tidyr)
library(wordcloud)
library(tm)
library(grDevices)
library(RColorBrewer)
library(pdftools)
library(SnowballC)
library(tidytext)
library(Hmisc)
library(mice)
library(car)
library(sandwich)
library(gridExtra)
library(ggthemes)


# 1.1. Setting working directory and loading data set ----------------------


setwd("C:/Users/johanhak/OneDrive - Universitetet i Oslo/Documents/R")
getwd()


fl <- read_xlsx("FL 2006-2020.xlsx",
                  col_types = c("text","date", "date", "numeric", "text", 
                                "numeric", "text", "text", "text", "text", "text", 
                                "numeric"))



# 1.2. Cleaning data set --------------------------------------------------


#Converting the title_eng column to lowercase
fl$title_eng <- tolower(fl$title_eng)

#Removing punctuation and spaces from the title_eng column
fl$title_eng <- str_replace_all(fl$title_eng, "[[:punct:]]", "")
fl$title_eng <- gsub(" ", "", fl$title_eng)




# 2.0. Recoding the variables ------------------------------------------------


# 2.1. Merge --------------------------------------------------------------


#To capture that VEN and BUS merged from 2017 and onwards
fl_recoded <- fl %>% 
  mutate(publisher_code_new = ifelse(fl_year >= 2017 & publisher_code == "VEN", 
                                     "BUS", 
                                     publisher_code)) %>%
  filter(is.na(fl_removed))




# 2.2. Creating dummy -----------------------------------------------------


#To capture that I measure subjects social sciences+history against the other ones
publishing_house_data <- fl_recoded %>% 
  mutate(subject_type_aggregated = ifelse(subject_ru == "History" | subject_ru == "Fundamentals of life safety", 
                                          "fund_history", 
                                          "other")) %>% 
  group_by(fl_year, publisher_code_new, subject_type_aggregated) %>% 
  summarise(count = n()) %>% 
  group_by(fl_year, subject_type_aggregated) %>% 
  mutate(overall_count_per_year = sum(count)) %>% 
  mutate(publishing_house_share = count / overall_count_per_year) %>% 
  arrange(fl_year,subject_type_aggregated, desc(publishing_house_share))




# 3.0. Descriptives -------------------------------------------------------

# 3.1. Plotting number of books overall -----------------------------------

# Grouping the data by publisher code and count the number of books
book_counts <- fl %>%
  group_by(publisher_code) %>%
  summarise(num_books = n()) %>%
  filter(num_books > 100) %>%
  arrange(desc(num_books))

# Creating a bar chart showing the overall number of books per publisher
overall_barplot <- ggplot(book_counts, aes(x = reorder(publisher_code, -num_books), y = num_books)) +
  geom_col(fill = "lightsteelblue") +
  xlab("Publisher code") +
  ylab("Number of books") +
  ggtitle("Total no. books by 16 largest publishers 2006-2020") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_tufte() + 
  theme(panel.grid.minor = element_line(colour = "lightgrey", size = 0.2)) +
  scale_y_continuous(breaks = seq(0, 7000, by = 500))


overall_barplot


# 3.2. Plotting the publishing houses' share by the dummy --------

#Plot of share in subjects:
pub_share_plot <- publishing_house_data %>%
  filter(publishing_house_share > 0.2) %>%
  ggplot(., aes(x = fl_year,
                y = publishing_house_share,
                color = subject_type_aggregated)) +
  geom_line() +
  geom_vline(xintercept = 2014) +
  facet_grid(~publisher_code_new)+
  theme_tufte() + 
  theme(panel.grid.minor = element_line(colour = "lightgrey", size = 0.2)) +
  scale_x_continuous(breaks = seq(2006, 2020, by = 2))+
  scale_y_continuous(labels=percent)+
  scale_color_discrete(name = "Subject", labels = c("Other", "History & FLS"))+
  ggtitle("Publishing house share (above 20%), before/after Crimea")+
  xlab("Year")+
  ylab("Share")+
  theme(plot.title = element_text(size = 13),
        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9))


pub_share_plot


# 3.3. Showing EDU --------------------------------------------------

#Filtering data for the publishing house of interest
pub_house <- "BUS" # change to the publishing house code of interest
pub_data <- fl %>% 
  filter(publisher_code == pub_house)

#Grouping the data by year and count the number of books
year_counts <- pub_data %>%
  group_by(fl_year) %>%
  summarise(num_books = n())

#Creating a line plot showing the number of books published by year
BUS_plot <- ggplot(year_counts, aes(x = fl_year, y = num_books)) +
  geom_line(color = "darkgrey", size = 1) +
  xlab("Year") +
  ylab("Books Published") +
  ggtitle(paste("Books published by BUS, before/after Crimea")) +
  theme_tufte() + 
  theme(panel.grid.minor = element_line(colour = "lightgrey", size = 0.2)) +
  scale_x_continuous(breaks=c(2006:2020))+
  geom_vline(xintercept = 2014)


BUS_plot


#See overall number of books per publisher
subject_counts <- fl_recoded %>%
  group_by(subject_code) %>%
  summarise(num_books = n()) %>%
  arrange(desc(num_books))

# Creating a bar chart showing the overall number of books per publisher
overall_barplot <- ggplot(subject_counts, aes(x = reorder(subject_code, -num_books), y = num_books)) +
  geom_col(fill = "skyblue4") +
  xlab("Publisher") +
  ylab("Number of books") +
  ggtitle("Books in each subject category") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme_tufte() + 
  theme(panel.grid.minor = element_line(colour = "lightgrey", size = 0.2)) +
  scale_x_discrete(labels=c("Natural sciences", "Interdisciplinary", "Foreign language", "Russian language",
                            "Literature", "History", "Art", "Practical", "Social sciences", "Moral/Religion"))+
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))


overall_barplot



# 3.5 Plotting the distribution of books on grades ----------------------------


# Creating a histogram of book counts by grade
grade_hist <- ggplot(fl, aes(x = grade)) +
  geom_histogram(binwidth = 1, fill = "cadetblue", color = "white") +
  theme_tufte() + 
  theme(panel.grid.minor = element_line(colour = "lightgrey", size = 0.2)) +
  scale_x_continuous(breaks=1:11)+
  labs(title = "Distribution of books by grade", x = "Grade", y = "Number of Books")

grade_hist


# 4.0 Hypotheses --------------------------------------------------------------

# 4.1 Hypothesis 1a ------------------------------------------------------------

#Grouping the data by year and publisher code and 
#counting the number of books for each publisher in each year
grouped_fl <- fl_recoded %>% 
  group_by(fl_year, publisher_code_new) %>%
  summarise(num_books_per_publisher = n())

#I want the sum of all books per year for all publishers
grouped_fl <- grouped_fl %>% 
  group_by(fl_year) %>% 
  mutate(all_books_per_year = sum(num_books_per_publisher))

#Creating a new column that calculates the concentration of each publisher
grouped_fl <- grouped_fl %>%
  group_by(fl_year) %>%
  mutate(share = num_books_per_publisher / all_books_per_year)

#Summarizing the mean concentration of publishers for each year, HHI
fl_summary <- grouped_fl %>%
  group_by(fl_year) %>%
  summarise(concentration_simple = mean(share), 
            concentration_hhi = sum(share^2))

#Plot
fl_hhi_plot <- fl_summary %>% 
  ggplot(., aes(x = fl_year, concentration_hhi)) + 
  geom_line()+
  ggtitle("HHI-index among publishers before/after Crimea") +
  xlab("Year")+
  geom_point()+
  ylab("HHI")+
  scale_x_continuous(breaks=c(2006:2020))+
  theme_tufte()+
  theme(panel.grid.minor = element_line(colour = "lightgrey", size = 0.2))+
  geom_vline(xintercept = 2014, col="black")

fl_hhi_plot


# 4.2 Hypothesis 1b ------------------------------------------------------------

#Grouping the data by year and dummy-variable and 
#counting the number of books for each publisher in each year
grouped_fl <- fl_recoded %>% 
  mutate(history_subject_dummy = ifelse(subject_code == 8, 1, 0)) %>% 
  group_by(fl_year, publisher_code, history_subject_dummy) %>%
  summarise(num_books_per_publisher = n()) 

#I want the sum of all books per year for all publishers
grouped_fl <- grouped_fl %>% 
  group_by(fl_year, history_subject_dummy) %>% 
  mutate(all_books_per_year = sum(num_books_per_publisher))

#Creating a new column that calculates the concentration of each publisher
grouped_fl <- grouped_fl %>%
  group_by(fl_year, history_subject_dummy) %>%
  mutate(share = num_books_per_publisher / all_books_per_year)

#Summarizing the mean concentration of publishers for each year per subject
fl_summary1 <- grouped_fl %>%
  group_by(fl_year, history_subject_dummy) %>%
  summarise(concentration_simple = mean(share), 
            concentration_hhi = sum(share^2))

#Plot
his_other_plot <- fl_summary1 %>% 
  ggplot(., aes(x = fl_year, concentration_hhi, 
                group = factor(history_subject_dummy), 
                color = factor(history_subject_dummy))) + 
  geom_line()+
  geom_point()+
  theme_grey() + 
  ggtitle("HHI-index for publishing houses by subjects before/after Crimea")+
  ylab("HHI-index")+
  xlab("Year")+
  theme(panel.grid.minor = element_line(colour="grey", size=0.2, linetype="solid"))+
  scale_x_continuous(breaks=c(2006:2020))+
  scale_color_discrete(name = "Subject", labels = c("Other", "History"))
  

his_other_plot




# Hypothesis 1b --------




#### Natural sciences vs. fund/his

#Fund/His.
fl_summary_hisfund <- fl_recoded %>%
  filter(subject_ru %in% c("History", "Fundamentals of life safety")) %>%
  group_by(fl_year, publisher_code, subject_ru) %>%
  summarise(num_books_per_publisher = n())

fl_summary_hisfund <- fl_summary_hisfund %>%
  group_by(fl_year, subject_ru) %>%
  mutate(all_books_per_year = sum(num_books_per_publisher))

fl_summary_hisfund <- fl_summary_hisfund %>%
  group_by(fl_year, subject_ru) %>%
  mutate(share = num_books_per_publisher / all_books_per_year)

fl_summary_hisfund <- fl_summary_hisfund %>%
  group_by(fl_year, subject_ru) %>%
  summarise(concentration_simple = mean(share),
            concentration_hhi = sum(share^2))


# Natural sciences
fl_summary_nat <- fl_recoded %>%
  filter(subject_ru %in% c("Biology", "Mathematics", "Chemistry", "Physics")) %>%
  group_by(fl_year, publisher_code, subject_ru) %>%
  summarise(num_books_per_publisher = n())

fl_summary_nat <- fl_summary_nat %>%
  group_by(fl_year, subject_ru) %>%
  mutate(all_books_per_year = sum(num_books_per_publisher))

fl_summary_nat <- fl_summary_nat %>%
  group_by(fl_year, subject_ru) %>%
  mutate(share = num_books_per_publisher / all_books_per_year)

fl_summary_nat <- fl_summary_nat %>%
  group_by(fl_year, subject_ru) %>%
  summarise(concentration_simple = mean(share),
            concentration_hhi = sum(share^2))



fl_hhi_plot_subject <- inner_join(fl_summary_nat, fl_summary_hisfund, by = "fl_year")

fl_hhi_plot_subject$subject_ru.x <- ifelse(fl_hhi_plot_subject$subject_ru.x %in% c("Biology", "Mathematics", "Chemistry", "Physics"), "Natural Sciences", "His/FLS")
fl_hhi_plot_subject$subject_ru.y <- ifelse(fl_hhi_plot_subject$subject_ru.y %in% c("Biology", "Mathematics", "Chemistry", "Physics"), "Natural Sciences", "His/FLS")


fl_hhi_plot_subject <- fl_hhi_plot_subject %>%
  group_by(fl_year) %>%
  summarise(concentration_nat = mean(c(concentration_hhi.x, concentration_hhi.x)),
            concentration_hisfund = mean(c(concentration_hhi.y, concentration_hhi.y)))



#Plot
fund_his_nat_plot <- ggplot(fl_hhi_plot_subject, aes(x = fl_year)) +
  geom_line(aes(y = concentration_hisfund, color = "cadetblue")) +
  geom_line(aes(y = concentration_nat, color = "lightsteelblue")) +
  geom_point(aes(y = concentration_nat), color="lightsteelblue") +
  geom_point(aes(y = concentration_hisfund), color="cadetblue")+
  ggtitle("HHI for History/FLS vs. Natural Sciences") +
  ylab("HHI-index") +
  xlab("Year") +
  scale_x_continuous(breaks = c(2006:2020)) +
  scale_color_manual(values = c("cadetblue", "lightsteelblue"),
                     name = "Subject",
                     labels = c("History/FLS", "Natural Sciences")) +
  theme(legend.title = element_text(hjust = 0.2),
        legend.justification = c(0, 0),
        legend.position = c(0, 1)) +
  theme_tufte() +
  geom_vline(xintercept=2014, color="black") +
  theme(panel.grid.minor = element_line(colour = "lightgrey", size = 0.2)) +
  scale_color_manual(values = c("cadetblue", "lightsteelblue"),
                     name = "Subject",
                     labels = c("History/FLS", "Natural Sciences")) +
  scale_fill_manual(values = c("grey"), name = "Subject", labels = c("HHI-index difference")) +
  theme(legend.position = c(0.2, 0.9))


fund_his_nat_plot

# Fundamentals vs. History ------------------------------------------------


#Fund
fl_summary_fund <- fl_recoded %>%
  filter(subject_ru == "Fundamentals of life safety") %>%
  group_by(fl_year, publisher_code, subject_ru) %>%
  summarise(num_books_per_publisher = n())

fl_summary_fund <- fl_summary_fund %>%
  group_by(fl_year, subject_ru) %>%
  mutate(all_books_per_year = sum(num_books_per_publisher))

fl_summary_fund <- fl_summary_fund %>%
  group_by(fl_year, subject_ru) %>%
  mutate(share = num_books_per_publisher / all_books_per_year)

fl_summary_fund <- fl_summary_fund %>%
  group_by(fl_year, subject_ru) %>%
  summarise(concentration_simple = mean(share),
            concentration_hhi = sum(share^2))


fl_summary_history <- fl_recoded %>%
  filter(subject_ru == "History") %>%
  group_by(fl_year, publisher_code, subject_ru) %>%
  summarise(num_books_per_publisher = n())

fl_summary_history <- fl_summary_history %>%
  group_by(fl_year, subject_ru) %>%
  mutate(all_books_per_year = sum(num_books_per_publisher))

fl_summary_history <- fl_summary_history %>%
  group_by(fl_year, subject_ru) %>%
  mutate(share = num_books_per_publisher / all_books_per_year)

fl_summary_history <- fl_summary_history %>%
  group_by(fl_year, subject_ru) %>%
  summarise(concentration_simple = mean(share),
            concentration_hhi = sum(share^2))



fl_hhi_plot_subject <- full_join(fl_summary_history, fl_summary_fund, by = "fl_year")


#Plot
his_fund_plot <- ggplot(fl_hhi_plot_subject, aes(x = fl_year)) +
  geom_line(aes(y = concentration_hhi.x, color="cadetblue")) +
  geom_point(aes(y = concentration_hhi.y, color="lightsteelblue")) +
  geom_point(aes(y = concentration_hhi.x, color="cadetblue")) +
  geom_line(aes(y = concentration_hhi.y, color="lightsteelblue")) +
  theme(legend.position = c(0.3, 1.06)) +
  ggtitle("HHI for History and FLS") +
  ylab("HHI-index") +
  xlab("Year") +
  theme_tufte() + 
  theme(panel.grid.minor = element_line(colour = "lightgrey", size = 0.2)) +
  scale_x_continuous(breaks = c(2006:2020)) +
  scale_color_manual(values = c("cadetblue", "lightsteelblue"), 
                     name = "Subject", 
                     labels = c("History", "FLS"))+
  geom_vline(xintercept=2014, color="black")+
  theme(legend.position = c(0.2, 0.9))

his_fund_plot



# Math and history --------------------------------------------------------

fl_his <- fl_recoded %>%
  filter(subject_ru=="History") %>%
  group_by(fl_year, publisher_code) %>%
  summarise(num_books_per_publisher = n())

fl_his <- fl_his %>%
  group_by(fl_year) %>%
  mutate(all_books_per_year = sum(num_books_per_publisher))


fl_his <- fl_his %>% 
  group_by(fl_year) %>%
  mutate(share = num_books_per_publisher / all_books_per_year) %>%
  summarise(concentration_simple = mean(share),
            concentration_hhi = sum(share^2))


fl_math <- fl_recoded %>%
  filter(subject_ru=="Mathematics") %>%
  group_by(fl_year, publisher_code) %>%
  summarise(num_books_per_publisher = n())


fl_math <- fl_math %>%
  group_by(fl_year) %>%
  mutate(all_books_per_year = sum(num_books_per_publisher))

fl_math <- fl_math %>% 
  group_by(fl_year) %>%
  mutate(share = num_books_per_publisher / all_books_per_year) %>%
  summarise(concentration_simple = mean(share),
            concentration_hhi = sum(share^2))


fl_hhi_plot_subject <- full_join(fl_math, fl_his, by = "fl_year")

math_his_plot <- ggplot(fl_hhi_plot_subject, aes(x = fl_year, y=concentration_hhi.x)) +
  geom_line(aes(y = concentration_hhi.y, color="cadetblue")) +
  geom_point(aes(y = concentration_hhi.y, color="cadetblue")) +
  geom_point(aes(y = concentration_hhi.x, color="lightsteelblue")) +
  geom_line(aes(y = concentration_hhi.x, color="lightsteelblue")) +
  theme(legend.position = c(0.3, 1.06)) +
  ggtitle("HHI for Mathematics and History") +
  ylab("HHI-index") +
  xlab("Year") +
  theme_tufte() + 
  theme(panel.grid.minor = element_line(colour = "lightgrey", size = 0.2)) +
  scale_x_continuous(breaks = c(2006:2020)) +
  scale_color_manual(values = c("cadetblue", "lightsteelblue"), 
                     name = "Subject", 
                     labels = c("History", "Mathematics"))+
  geom_vline(xintercept=2014, color="black") +
  theme(legend.position = c(0.2, 0.9))

math_his_plot




## Together

plots_list <- list(math_his_plot, his_fund_plot, fund_his_nat_plot)
grid.arrange(grobs = plots_list, ncol = 3, widths = c(1,1,1), bottom = "Year", left = "HHI-index")





# Fundamentals vs other subjects + sanctions ---------------------------------------------------


### Cleaning "sanctions"

sanctions <- read_xls("globsanc.xls") %>% 
  filter(sanctioned_state == "Russia") %>%
  filter(begin > 2005)

#Years
years_seq <- function(start, end) seq(start, end, by = 1)

# Expand sanctions 
sanctions_expanded <- sanctions %>%
  rowwise() %>%
  mutate(year = map2(begin, end, years_seq)) %>%
  unnest(year)

sanctions_summary <- sanctions_expanded %>%
  group_by(year) %>%
  summarise(n=sum(n_distinct(case_id)))







# Hypothesis 2a -----------------------------------------------------------


#Grouping the data by year and publisher code and 
#counting the number of books for each publisher in each year
grouped_fl <- fl_recoded %>% 
  group_by(fl_year, publisher_code_new) %>%
  summarise(num_books_per_publisher = n())

#I want the sum of all books per year for all publishers
grouped_fl <- grouped_fl %>% 
  group_by(fl_year) %>% 
  mutate(all_books_per_year = sum(num_books_per_publisher))

#Creating a new column that calculates the concentration of each publisher
grouped_fl <- grouped_fl %>%
  group_by(fl_year) %>%
  mutate(share = num_books_per_publisher / all_books_per_year)

#Summarizing the mean concentration of publishers for each year, HHI
fl_summary <- grouped_fl %>%
  group_by(fl_year) %>%
  summarise(concentration_simple = mean(share), 
            concentration_hhi = sum(share^2))

#Plot
fl_hhi_plot <- fl_summary %>% 
  ggplot(., aes(x = fl_year, concentration_hhi)) + 
  geom_line()+
  ggtitle("HHI-index among publishers before/after Crimea") +
  xlab("Year")+
  ylab("HHI")+
  scale_x_continuous(breaks=c(2006:2020))+
  theme_tufte()+
  geom_point()+
  theme(panel.grid.minor = element_line(colour = "lightgrey", size = 0.2))+
  geom_vline(xintercept = 2014, col="black")

fl_hhi_plot

#### Now, compare with sanctions

p2 <- ggplot(sanctions_summary, aes(x = year, y = n)) +
  geom_bar(stat = "identity") +
  theme_tufte() +
  theme(panel.grid.minor = element_line(colour = "lightgrey", size = 0.2))+
  ggtitle("Number of sanctions") +
  ylab("Count") +
  xlab("Year") +
  scale_x_continuous(limits = c(2006, 2020), breaks = c(2006:2020))+
  scale_y_continuous(limits=c(0, 10), breaks=c(0:10))



grid.arrange(fl_hhi_plot, p2, ncol = 1, heights = c(5, 3))




p2 <- ggplot(sanctions_summary, aes(x = year, y = n)) +
  geom_bar(stat = "identity") +
  theme_tufte() +
  theme(panel.grid.minor = element_line(colour = "lightgrey", size = 0.2))+
  ggtitle("Number of sanctions") +
  ylab("Count") +
  xlab("Year") +
  scale_x_continuous(limits = c(2006, 2020), breaks = c(2006:2020))+
  scale_y_continuous(limits=c(0, 10), breaks=c(0:10))



grid.arrange(fund_his_nat_plot, p2, ncol = 1, heights = c(5, 3))


  
  
  


# HERE --------------------------------------------------------------------

#Grouping the data by year and dummy-variable and 
#counting the number of books for each publisher in each year

grouped_fl <- fl_recoded %>% 
  filter(subject_ru %in% c("History", "Mathematics")) %>%
  mutate(subject_dummy = ifelse(subject_ru == "History", 1, 0)) %>% 
  group_by(fl_year, publisher_code, subject_dummy) %>%
  summarise(num_books_per_publisher = n()) 

#I want the sum of all books per year for all publishers
grouped_fl <- grouped_fl %>% 
  group_by(fl_year, subject_dummy) %>% 
  mutate(all_books_per_year = sum(num_books_per_publisher))

#Creating a new column that calculates the concentration of each publisher
grouped_fl <- grouped_fl %>%
  group_by(fl_year, subject_dummy) %>%
  mutate(share = num_books_per_publisher / all_books_per_year)

#Summarizing the mean concentration of publishers for each year per subject
fl_summary <- grouped_fl %>%
  group_by(fl_year, subject_dummy) %>%
  summarise(concentration_simple = mean(share), 
            concentration_hhi = sum(share^2))



#Plot

p3 <- ggplot(fl_summary, aes(x = fl_year, y = concentration_hhi, color = factor(subject_dummy))) + 
  geom_line() + 
  geom_point() + 
  theme_tufte() + 
  theme(panel.grid.minor = element_line(colour = "lightgrey", size = 0.2))+
  scale_color_manual(name = "Subject", values = c("lightsteelblue", "cadetblue"), labels = c("Mathematics", "History")) +
  theme(legend.direction = "horizontal", legend.box = "horizontal", legend.position = c(0.3, 1.025), 
        legend.text = element_text(size = 8)) +
  ggtitle("HHI-index") +
  ylab("HHI-index") +
  xlab("Year") +
  scale_x_continuous(breaks = c(2006:2020)) +
  geom_vline(xintercept=2014, col="black")


p4 <- ggplot(sanctions_summary, aes(x = year, y = n)) +
  geom_bar(stat = "identity") +
  theme_tufte() +
  theme(panel.grid.minor = element_line(colour = "lightgrey", size = 0.2))+
  ggtitle("Number of sanctions") +
  ylab("Count") +
  xlab("Year") +
  scale_x_continuous(limits = c(2006, 2020), breaks = c(2006:2020))+
  scale_y_continuous(limits=c(0, 10), breaks=c(0:10))



grid.arrange(p3, p4, ncol = 1, heights = c(5, 3))



##### Confirming h2a


# Filter the data to include only subject_code 2 and 8 since 2014
# filter the data for years since 2014
fl_summary_since_2014 <- fl_summary %>% 
  filter(fl_year >= 2014) %>%
  group_by(fl_year) %>%
  mutate(diff_concentration_hhi = concentration_hhi[subject_dummy == 0] - concentration_hhi[subject_dummy == 1])





# cORRELATIONS ----------------------------------------------------




# Load the necessary packages
library(ggplot2)
library(gridExtra)

history_df <- fl_summary_hisfund %>%
  filter(subject_ru == "History") %>%
  select(fl_year, concentration_hhi) %>%
  rename(year = fl_year, value = concentration_hhi)

fls_df <- fl_summary_hisfund %>%
  filter(subject_ru == "Fundamentals of life safety") %>%
  select(fl_year, concentration_hhi) %>%
  rename(year = fl_year, value = concentration_hhi)

natural_sciences_df <- fl_summary_nat %>%
  filter(subject_ru %in% c("Biology", "Mathematics", "Chemistry", "Physics")) %>%
  select(fl_year, concentration_hhi) %>%
  rename(year = fl_year, value = concentration_hhi)



fl_summary_eco <- fl_recoded %>%
  filter(subject_ru == "Economics") %>%
  group_by(fl_year, publisher_code, subject_ru) %>%
  summarise(num_books_per_publisher = n())

fl_summary_eco <- fl_summary_eco %>%
  group_by(fl_year, subject_ru) %>%
  mutate(all_books_per_year = sum(num_books_per_publisher))

fl_summary_eco <- fl_summary_eco %>%
  group_by(fl_year, subject_ru) %>%
  mutate(share = num_books_per_publisher / all_books_per_year)

fl_summary_eco <- fl_summary_eco %>%
  group_by(fl_year, subject_ru) %>%
  summarise(concentration_simple = mean(share),
            concentration_hhi = sum(share^2))

economics_df <- fl_summary_eco %>%
  select(fl_year, concentration_hhi) %>%
  rename(year=fl_year, value=concentration_hhi)

subjects_df <- rbind(history_df, fls_df, natural_sciences_df, economics_df)

subjects_df$subject_ru <- c(rep("History", 19), rep("Fundamentals of life safety", 19),
                            rep("Natural Sciences", 19), rep("Economics", 20))


history_values <- history_df$value
fls_values <- fls_df$value
natural_sciences_values <- natural_sciences_df$value
eco_values <- economics_df$value

correlation_matrix <- cor(cbind(history_values, eco_values, fls_values, natural_sciences_values))
round(correlation_matrix, 2)



# 4.3.1 Added/removed - Crimea --------------------------------------------------------------


FL_2 <- read_xlsx("FL 2006-2020.xlsx",
                  col_types = c("text","date", "date", "numeric", "text", 
                                "numeric", "text", "text", "text", "text", "text", "numeric"))

# Filtering the data set to include only books added after 2014

# Subsetting added books
added_books <- subset(FL_2, !is.na(fl_added))

# Subsetting removed books
removed_books <- subset(FL_2, !is.na(fl_removed))

# Combining the two data frames
fl_removed_added <- rbind(added_books, removed_books)


# Filtering the data to include only books in the field of history+fund
fl_hisfund <- fl_removed_added %>%
  filter(subject_ru== c("History", "FLS"))


# Grouping by publisher code and calculate the number of books added and removed
publisher_counts <- fl_hisfund %>% 
  group_by(publisher_code) %>% 
  summarise(num_books_added = sum(ifelse(!is.na(fl_added), 1, 0)),
            num_books_removed = sum(ifelse(!is.na(fl_removed), 1, 0))) %>%
  ungroup()


# Reshaping the data to long 
publisher_counts_long <- publisher_counts %>%
  pivot_longer(cols = c(num_books_added, num_books_removed), 
               names_to = "book_type", values_to = "num_books") %>%
  mutate(book_type = ifelse(book_type == "num_books_added", "Added", "Removed"))

# Creating a stacked barplot
barplot_add_rem <- ggplot(publisher_counts_long, aes(x = publisher_code, y = num_books, fill = book_type)) +
  coord_flip() +
  geom_col() +
  scale_fill_manual(values = c(Added = "lightsteelblue", Removed = "cadetblue"), 
                    labels = c("Books Added", "Books Removed"),
                    name = "Book type") +
  theme_ipsum() +
  labs(x = "Publisher House", y = "Number of Books", 
       title = "Added and removed history/FLS books after Crimea")+
  geom_text(aes(label = num_books), size = 3, position = position_stack(vjust = 0.5))


barplot_add_rem

