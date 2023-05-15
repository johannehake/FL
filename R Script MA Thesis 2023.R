
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








# 2.0 Hypotheses --------------------------------------------------------------




# 2.1 Hypothesis 1 ------------------------------------------------------------

#Grouping the data by year and publisher code and 
#counting the number of books for each publisher in each year
grouped_fl <- fl %>% 
  group_by(fl_year, publisher_code) %>%
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

#Plot; first plot
fl_hhi_plot <- fl_summary %>% 
  ggplot(., aes(x = fl_year, concentration_hhi)) + 
  geom_line()+
  ggtitle("HHI-levels before/after Crimea") +
  xlab("Year")+
  geom_point()+
  ylab("HHI")+
  scale_x_continuous(breaks=c(2006:2020))+
  scale_y_continuous(labels = number_format(accuracy = 0.01))+
  theme_tufte()+
  geom_vline(xintercept = 2014, col="black")

fl_hhi_plot


p2 <- ggplot(sanctions_summary, aes(x = year, y = n)) +
  geom_bar(stat = "identity") +
  theme_tufte() +
  ggtitle("Number of sanctions") +
  ylab("Count") +
  xlab("Year") +
  scale_x_continuous(limits = c(2006, 2020), breaks = c(2006:2020))+
  scale_y_continuous(limits=c(0, 10), breaks=c(0:10))


# First plot; H1
grid.arrange(fl_hhi_plot, p2, ncol = 1, heights = c(5, 3))




# 2.2 Hypothesis 2 ------------------------------------------------------------


#### Natural sciences vs. soc/his/fls

#soc/His.
fl_summary_hissocfls <- fl %>%
  filter(subject_ru %in% c("History", "Social studies", "Fundamentals of life safety")) %>%
  group_by(fl_year, publisher_code, subject_ru) %>%
  summarise(num_books_per_publisher = n())

fl_summary_hissocfls <- fl_summary_hissocfls %>%
  group_by(fl_year, subject_ru) %>%
  mutate(all_books_per_year = sum(num_books_per_publisher))

fl_summary_hissocfls <- fl_summary_hissocfls %>%
  group_by(fl_year, subject_ru) %>%
  mutate(share = num_books_per_publisher / all_books_per_year)

fl_summary_hissocfls <- fl_summary_hissocfls %>%
  group_by(fl_year, subject_ru) %>%
  summarise(concentration_simple = mean(share),
            concentration_hhi = sum(share^2))


# Natural sciences
fl_summary_nat <- fl %>%
  filter(subject_ru %in% c("Biology", "Physics", "Mathematics", "Chemistry")) %>%
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



fl_hhi_plot_subject <- inner_join(fl_summary_nat, fl_summary_hissocfls, by = "fl_year")

fl_hhi_plot_subject$subject_ru.x <- ifelse(fl_hhi_plot_subject$subject_ru.x %in% c("Biology", "Mathematics", "Chemistry", "Physics"), "Natural Sciences", "His/Soc")
fl_hhi_plot_subject$subject_ru.y <- ifelse(fl_hhi_plot_subject$subject_ru.y %in% c("Biology", "Mathematics", "Chemistry", "Physics"), "Natural Sciences", "His/Soc")


fl_hhi_plot_subject <- fl_hhi_plot_subject %>%
  group_by(fl_year) %>%
  summarise(concentration_nat = mean(c(concentration_hhi.x, concentration_hhi.x)),
            concentration_hissocfls = mean(c(concentration_hhi.y, concentration_hhi.y)))


#Plot
hissocfund_nat_plot <- ggplot(fl_hhi_plot_subject, aes(x = fl_year)) +
  geom_line(aes(y = concentration_hissocfls, color = "darkorange")) +
  geom_point(aes(y = concentration_hissocfls), color="darkorange")+
  geom_line(aes(y = concentration_nat, color = "deepskyblue")) +
  geom_point(aes(y = concentration_nat), color="deepskyblue") +
  ggtitle("HHI for Societal Disciplines vs. Non-Societal Disciplines") +
  ylab("HHI-index") +
  xlab("Year") +
  scale_x_continuous(breaks = c(2006:2020)) +
  theme(legend.title = element_text(hjust = 0.2),
        legend.justification = c(0, 0),
        legend.position = c(0, 1)) +
  theme_tufte() +
  theme(text=element_text(size=13))+
  geom_vline(xintercept=2014, color="black") +
  scale_color_manual(values = c("darkorange", "deepskyblue"),
                     name = "Subject",
                     labels = c("History/Social Studies/FLS", "Biology/Mathematics/Physics/Chemistry")) +
  scale_fill_manual(values = c("grey"), name = "Subject", labels = c("HHI-index difference")) +
  theme(legend.position = c(0.2, 0.9))

hissocfund_nat_plot


### NUANCES: 



#History vs. natural sciences.
fl_summary_his <- fl %>%
  filter(subject_ru %in% c("History")) %>%
  group_by(fl_year, publisher_code, subject_ru) %>%
  summarise(num_books_per_publisher = n())

fl_summary_his <- fl_summary_his %>%
  group_by(fl_year, subject_ru) %>%
  mutate(all_books_per_year = sum(num_books_per_publisher))

fl_summary_his <- fl_summary_his %>%
  group_by(fl_year, subject_ru) %>%
  mutate(share = num_books_per_publisher / all_books_per_year)

fl_summary_his <- fl_summary_his %>%
  group_by(fl_year, subject_ru) %>%
  summarise(concentration_simple = mean(share),
            concentration_hhi = sum(share^2))


# Natural sciences
fl_summary_nat <- fl %>%
  filter(subject_ru %in% c("Biology", "Physics", "Mathematics", "Chemistry")) %>%
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



fl_hhi_plot_subject <- inner_join(fl_summary_nat, fl_summary_his, by = "fl_year")

fl_hhi_plot_subject$subject_ru.x <- ifelse(fl_hhi_plot_subject$subject_ru.x %in% c("Biology", "Mathematics", "Chemistry", "Physics"), "Natural Sciences", "His/Soc")
fl_hhi_plot_subject$subject_ru.y <- ifelse(fl_hhi_plot_subject$subject_ru.y %in% c("Biology", "Mathematics", "Chemistry", "Physics"), "Natural Sciences", "His/Soc")


fl_hhi_plot_subject <- fl_hhi_plot_subject %>%
  group_by(fl_year) %>%
  summarise(concentration_nat = mean(c(concentration_hhi.x, concentration_hhi.x)),
            concentration_his = mean(c(concentration_hhi.y, concentration_hhi.y)))



#Plot
his_nat_plot <- ggplot(fl_hhi_plot_subject, aes(x = fl_year)) +
  geom_line(aes(y = concentration_his, color = "darkorange")) +
  geom_point(aes(y = concentration_his), color="darkorange")+
  geom_line(aes(y = concentration_nat, color = "deepskyblue")) +
  geom_point(aes(y = concentration_nat), color="deepskyblue") +
  ggtitle("HHI for History vs. Non-Societal Disciplines") +
  ylab("HHI-index") +
  xlab("Year") +
  scale_x_continuous(breaks = c(2006:2020)) +
  theme(legend.title = element_text(hjust = 0.2),
        legend.justification = c(0, 0),
        legend.position = c(0, 1)) +
  theme_tufte() +
  theme(text=element_text(size=13))+
  geom_vline(xintercept=2014, color="black") +
  scale_color_manual(values = c("darkorange", "deepskyblue"),
                     name = "Subject",
                     labels = c("History", "Biology/Mathematics/
Physics/Chemistry")) +
  scale_fill_manual(values = c("grey"), name = "Subject", labels = c("HHI-index difference")) +
  theme(legend.position = c(0.2, 0.9))

his_nat_plot


#Soc vs. nat
fl_summary_soc <- fl %>%
  filter(subject_ru %in% c("Social studies")) %>%
  group_by(fl_year, publisher_code, subject_ru) %>%
  summarise(num_books_per_publisher = n())

fl_summary_soc <- fl_summary_soc %>%
  group_by(fl_year, subject_ru) %>%
  mutate(all_books_per_year = sum(num_books_per_publisher))

fl_summary_soc <- fl_summary_soc %>%
  group_by(fl_year, subject_ru) %>%
  mutate(share = num_books_per_publisher / all_books_per_year)

fl_summary_soc <- fl_summary_soc %>%
  group_by(fl_year, subject_ru) %>%
  summarise(concentration_simple = mean(share),
            concentration_hhi = sum(share^2))


# Natural sciences
fl_summary_nat <- fl %>%
  filter(subject_ru %in% c("Biology", "Physics", "Mathematics", "Chemistry")) %>%
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



fl_hhi_plot_subject <- inner_join(fl_summary_nat, fl_summary_soc, by = "fl_year")

fl_hhi_plot_subject$subject_ru.x <- ifelse(fl_hhi_plot_subject$subject_ru.x %in% c("Biology", "Mathematics", "Chemistry", "Physics"), "Natural Sciences", "His/Soc")
fl_hhi_plot_subject$subject_ru.y <- ifelse(fl_hhi_plot_subject$subject_ru.y %in% c("Biology", "Mathematics", "Chemistry", "Physics"), "Natural Sciences", "His/Soc")


fl_hhi_plot_subject <- fl_hhi_plot_subject %>%
  group_by(fl_year) %>%
  summarise(concentration_nat = mean(c(concentration_hhi.x, concentration_hhi.x)),
            concentration_soc = mean(c(concentration_hhi.y, concentration_hhi.y)))



#Plot
soc_nat_plot <- ggplot(fl_hhi_plot_subject, aes(x = fl_year)) +
  geom_line(aes(y = concentration_soc, color = "darkorange")) +
  geom_point(aes(y = concentration_soc), color="darkorange")+
  geom_line(aes(y = concentration_nat, color = "deepskyblue")) +
  geom_point(aes(y = concentration_nat), color="deepskyblue") +
  ggtitle("HHI for Social studies vs. Non-Societal Disciplines") +
  ylab("HHI-index") +
  xlab("Year") +
  scale_x_continuous(breaks = c(2006:2020)) +
  theme(legend.title = element_text(hjust = 0.2),
        legend.justification = c(0, 0),
        legend.position = c(0, 1)) +
  theme_tufte() +
  theme(text=element_text(size=13))+
  geom_vline(xintercept=2014, color="black") +
  scale_color_manual(values = c("darkorange", "deepskyblue"),
                     name = "Subject",
                     labels = c("Social studies", "Biology/Mathematics/
Physics/Chemistry")) +
  scale_fill_manual(values = c("grey"), name = "Subject", labels = c("HHI-index difference")) +
  theme(legend.position = c(0.9, 0.2))

soc_nat_plot

# FLS vs nat


#fls.
fl_summary_fls <- fl %>%
  filter(subject_ru %in% c("Fundamentals of life safety")) %>%
  group_by(fl_year, publisher_code, subject_ru) %>%
  summarise(num_books_per_publisher = n())

fl_summary_fls <- fl_summary_fls %>%
  group_by(fl_year, subject_ru) %>%
  mutate(all_books_per_year = sum(num_books_per_publisher))

fl_summary_fls <- fl_summary_fls %>%
  group_by(fl_year, subject_ru) %>%
  mutate(share = num_books_per_publisher / all_books_per_year)

fl_summary_fls <- fl_summary_fls %>%
  group_by(fl_year, subject_ru) %>%
  summarise(concentration_simple = mean(share),
            concentration_hhi = sum(share^2))


# Natural sciences
fl_summary_nat <- fl %>%
  filter(subject_ru %in% c("Biology", "Physics", "Mathematics", "Chemistry")) %>%
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



fl_hhi_plot_subject <- inner_join(fl_summary_nat, fl_summary_fls, by = "fl_year")

fl_hhi_plot_subject$subject_ru.x <- ifelse(fl_hhi_plot_subject$subject_ru.x %in% c("Biology", "Mathematics", "Chemistry", "Physics"), "Natural Sciences", "His/Soc")
fl_hhi_plot_subject$subject_ru.y <- ifelse(fl_hhi_plot_subject$subject_ru.y %in% c("Biology", "Mathematics", "Chemistry", "Physics"), "Natural Sciences", "His/Soc")


fl_hhi_plot_subject <- fl_hhi_plot_subject %>%
  group_by(fl_year) %>%
  summarise(concentration_nat = mean(c(concentration_hhi.x, concentration_hhi.x)),
            concentration_fls = mean(c(concentration_hhi.y, concentration_hhi.y)))



#Plot
fls_nat_plot <- ggplot(fl_hhi_plot_subject, aes(x = fl_year)) +
  geom_line(aes(y = concentration_fls, color = "darkorange")) +
  geom_point(aes(y = concentration_fls), color="darkorange")+
  geom_line(aes(y = concentration_nat, color = "deepskyblue")) +
  geom_point(aes(y = concentration_nat), color="deepskyblue") +
  ggtitle("HHI for Fundamentals of Life Safety vs. Non-Societal Disciplines") +
  ylab("HHI-index") +
  xlab("Year") +
  scale_x_continuous(breaks = c(2006:2020)) +
  theme(legend.title = element_text(hjust = 0.2),
        legend.justification = c(0, 0),
        legend.position = c(0, 1)) +
  theme_tufte() +
  theme(text=element_text(size=13))+
  geom_vline(xintercept=2014, color="black") +
  scale_color_manual(values = c("darkorange", "deepskyblue"),
                     name = "Subject",
                     labels = c("FLS", "Biology/Mathematics/
Physics/Chemistry")) +
  scale_fill_manual(values = c("grey"), name = "Subject", labels = c("HHI-index difference")) +
  theme(legend.position = c(0.2, 0.9))

fls_nat_plot


