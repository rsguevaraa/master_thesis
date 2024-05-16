# Master thesis 
# Title: Representing the “Other”: How do Peruvian right-leaning newspapers represent Venezuelan immigrants (2017-2023)?
# Author: Renato Guevara Ayón

##### Loading data #####
#Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries
pacman::p_load(rio, tidyverse, ggplot2,zoo, purrr, prediction, mfx, texreg)

# Load data
data <- import("Final-Corpus-2017-2023.csv")

######### Part 1: Data cleaning and data exploration ########

##### 1.1. Recoding predictor variables
# 1) Newspaper name
data$newspaper_name <- as.factor(data$`Newspaper Name`)
table(data$newspaper_name) # correo=6444, ojo=2430, peru21 = 2408, trome=3135

#2) Time variables
## 2.1) Year variable
data$Year <- as.numeric(format(data$News_Date, "%Y"))
table(data$Year)
# Create Year as factor
data$Year_category = as.factor(data$Year)

## 2.2) Month-Year Variable
data$Month_Year <- as.yearmon(data$News_Date)
table(data$Month_Year)

# 3) Government variable
data <- data %>%
  mutate(Government = as.factor(case_when(
    Month_Year >= as.yearmon("Jan 2017") & Month_Year <= as.yearmon("Mar 2018") ~ "Kuczynski",
    Month_Year >= as.yearmon("Apr 2018") & Month_Year <= as.yearmon("Nov 2020") ~ "Vizcarra",
    Month_Year >= as.yearmon("Dec 2020") & Month_Year <= as.yearmon("Jul 2021") ~ "Sagasti",
    Month_Year >= as.yearmon("Aug 2021") & Month_Year <= as.yearmon("Dec 2022") ~ "Castillo",
    Month_Year >= as.yearmon("Jan 2023") & Month_Year <= as.yearmon("Dec 2023") ~ "Boluarte",
    TRUE ~ "Other"  # Default case
  )))
data$Government <- factor(data$Government, levels = c("Kuczynski", "Vizcarra", "Sagasti", "Castillo", "Boluarte"))
table(data$Government)

# 5) Gender variable
data <- data %>% rename("Female" = "if_female_full")

##### 1.2 Distribution of predictor variables

# 1) Frequency table for presence of women in the corpus
Female_frequency_table=table(data$Female)
Female_percentage_table=round(prop.table(table(data$Female))*100,1)
Female_table <- cbind(Frequency = Female_frequency_table, Percentage = Female_percentage_table)
Female_table<- addmargins(Female_table, margin = 1)

# 2) Frequency table for Years
Year_frequency_table = table(data$Year_category)
Year_percentage_table = round(prop.table(table(data$Year_category))*100,1)
Year_table <- cbind(Frequency = Year_frequency_table, Percentage = Year_percentage_table)

# 3) Frequency table for Government
Government_frequency_table= table(data$Government)
Government_percentage_table= round(prop.table(table(data$Government))*100,1)
Government_table <- cbind(Frequency = Government_frequency_table, Percentage = Government_percentage_table)

# 4) Frequency table for newspapers
newspaper_frequency_table = table(data$newspaper_name)
newspaper_percentage_table = round(prop.table(table(data$newspaper_name))*100,1)
newspaper_table <- cbind(Frequency = newspaper_frequency_table, Percentage = newspaper_percentage_table)

#### 1.3) Distribution of newspaper articles
#Distribution of articles per newspaper
data %>% 
  group_by(newspaper_name) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(prop = count/sum(count),
         percentage = count/sum(count)*100) %>% 
  ggplot(aes(newspaper_name, y = prop)) + 
  geom_bar(stat= "identity", aes(fill = newspaper_name), show.legend = F) +
  geom_text(aes(label=paste0(round(percentage), "%", ",",count)),
            vjust = -0.3, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x= "Newspaper name", y= "Number of articles",
       title = "Number of articles per newspaper") +
  theme_classic() +
  theme(plot.title = element_text(hjust= 0.5, size=12, face = "bold"),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "lines"),
        legend.title = element_text(size = 8))

######### Part 2: Descriptive statistics of the frames######

# 2.1) Tables for presence/absense of each frame
prop.table(table(data$crime_frame_dummy))*100 #75% of news contain a crime frame
prop.table(table(data$migration_frame_dummy))*100 #19% of news contain migration frame
prop.table(table(data$violence_frame_dummy))*100 #17% of news contain a  violence frame
prop.table(table(data$work_frame_dummy))*100 #15% of news contain work frame
prop.table(table(data$sexvi_frame_dummy))*100 #15% of news contain a sexual violence frame
prop.table(table(data$ilegal_frame_dummy))*100 #12.7% of news contain a ilegal frame
prop.table(table(data$aes_frame_dummy))*100 #6.1% of news contain a aesthethic frame
prop.table(table(data$entertainment_frame_dummy))*100 #4.5% of news contain an enterntainment frame
prop.table(table(data$no_frame)) *100 #12.5% has no identified frame

# 2.2) Table with total number of frames (count)
#Filter only the articles that contain a frame (frame == T)
dummy_vars = c("crime_frame_dummy", "violence_frame_dummy", "sexvi_frame_dummy",
               "migration_frame_dummy", "ilegal_frame_dummy", "entertainment_frame_dummy",
               "aes_frame_dummy", "work_frame_dummy")

# Define a function to filter the presence of frame (value TRUE) per article and combine it into a unified dataframe
summarize_data <- function(dummy_var) {
  data %>%
    filter(!!rlang::sym(dummy_var) == TRUE) %>%
    group_by(Month_Year) %>%
    summarise(!!paste0(sub("_dummy", "", dummy_var)) := sum(!!rlang::sym(dummy_var)))
}

months_data_list <- map(dummy_vars, summarize_data)
months_data_combined <- reduce(months_data_list, full_join, by = "Month_Year")
months_data_combined<- months_data_combined %>% replace(is.na(.), 0)
names(months_data_list) <- gsub("_dummy", "", dummy_vars)

# Show final table with presence of frames across time (count frames)
print(months_data_combined)

# 2.3) Table showing the percentage of frames (Table 8 reported in the thesis)
frame_table <- months_data_combined %>%
  pivot_longer(
    cols = ends_with("_frame"), 
    names_to = "thematic_frame", 
    values_to = "count"
  ) %>%
  group_by(thematic_frame) %>%
  summarize(
    Total = sum(count), 
    Percentage = round(sum(count) / 14417 * 100,1)
  ) %>% 
  arrange(desc(Total)) %>% 
  rename(
    `Thematic Frame` = thematic_frame,
    `Number of articles with the frame` = Total,
    `Percentage of articles containning the frame` = Percentage)

# 2.4) Thematic frames identified in the period 2017-2023 (Figure 1 in the thesis report)
ggplot(months_data_combined, aes(x = Month_Year)) +
  geom_line(aes(y = crime_frame, color= "crime_frame")) +
  geom_line(aes(y = migration_frame, color = "migration_frame")) +
  geom_line(aes(y = sexvi_frame, color = "sexvi_frame")) +
  geom_line(aes(y = violence_frame, color = "violence_frame")) +
  geom_line(aes(y = work_frame, color = "work_frame")) +
  geom_line(aes(y = ilegal_frame, color = "ilegal_frame")) +
  geom_line(aes(y = aes_frame, color = "aes_frame")) + 
  geom_line(aes(y = entertainment_frame, color = "entertainment_frame")) +
  labs(y = "Number of articles reporting the frame", color = "Thematic frame",
       title = "Thematic frames in Peruvian right-leaning newspapers (2017-2023)") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(), # no title for x axis
        axis.text.x=element_text(angle=90),
        plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        plot.title.position = "plot",
        legend.box.background = element_rect(color = "black", size = 0.5)) +
  #scale_color_hue() +
  scale_color_manual(values = c("crime_frame" = "red","migration_frame" = "blue",
                                "sexvi_frame" = "orange", "violence_frame" = "#4DAF4A",
                                "work_frame" = "#984EA3", "ilegal_frame" = "#A65628",
                                "aes_frame" = "#FFFF33","entertainment_frame" = "#984EA3"),
                     labels = c("Crime","Migration", "Sexual Violence", "Violence",
                                "Work", "Ilegal", "Physical appearance", "Entertainment")) +
  scale_x_yearmon(format = "%Y-%m", n = 20) + 
  scale_y_continuous(limits=c(0, 525)) +
  geom_rect(xmin = as.yearmon("Jan 2017"), xmax = as.yearmon("Mar 2018"), ymin = 505, ymax = 525, 
            fill = "lightblue", color = "black", alpha = 0.2) +
  annotate("text", x = as.yearmon("Aug 2017"), y = 530 * 0.97, 
           label = "P.P. Kuczynski", color = "black", size = 2.5, hjust = 0.5) +
  geom_rect(xmin = as.yearmon("Mar 2018"), xmax = as.yearmon("Nov 2020"), ymin = 505, ymax = 525, 
            fill = "lightblue",color = "black", alpha = 0.2) +
  annotate("text", x = as.yearmon("Jul 2019"), y = 530 * 0.97, 
           label = "M. Vizcarra", color = "black", size = 3, hjust = 0.5) +
  geom_rect(xmin = as.yearmon("Nov 2020"), xmax = as.yearmon("Jul 2021"), ymin = 505, ymax = 525, 
            fill = "lightblue",color = "black", alpha = 0.2) +
  annotate("text", x = as.yearmon("Mar 2021"), y = 530 * 0.97, 
           label = "F. Sagasti", color = "black", size = 2.5, hjust = 0.5) +
  geom_rect(xmin = as.yearmon("Jul 2021"), xmax = as.yearmon("Dec 2022"), ymin = 505, ymax = 525, 
            fill = "lightblue", color = "black", alpha = 0.2) +
  annotate("text", x = as.yearmon("Mar 2022"), y = 530 * 0.97, 
           label = "P. Castillo", color = "black", size = 3, hjust = 0.5) +
  geom_rect(xmin = as.yearmon("Dec 2022"), xmax = as.yearmon("Dec 2023"), ymin = 505, ymax = 525, 
            fill = "lightblue", color = "black", alpha = 0.2) +
  annotate("text", x = as.yearmon("Jun 2023"), y = 530 * 0.97, 
           label = "D. Boluarte", color = "black", size = 3, hjust = 0.5) #+
  # ggsave("Figure 1 - Thematic frames (Methods).jpeg", 
  #        width = 730/96 , height = 450/96)
  
# 2.5) TOP 3 thematic frames for 2017-2023 (Figure 2)
ggplot(months_data_combined, aes(x = Month_Year)) +
  geom_line(aes(y = crime_frame, color= "crime_frame")) +
  geom_line(aes(y = migration_frame, color = "migration_frame")) +
  geom_line(aes(y = sexvi_frame, color = "sexvi_frame")) +
  labs(y = "Number of articles reporting the frame", color = "Thematic frame",
       title = "Thematic frames in Peruvian right-leaning newspapers (2017-2023)") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(), # no title for x axis
        axis.text.x=element_text(angle=90),
        plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        plot.title.position = "plot",
        legend.box.background = element_rect(color = "black", size = 0.5)) +
  #scale_color_hue() +
  scale_color_manual(values = c("crime_frame" = "red","migration_frame" = "blue",
                                "sexvi_frame" = "orange"),
                     labels = c("Crime","Migration", "Sexual Violence")) +
  scale_x_yearmon(format = "%Y-%m", n = 20) + 
  scale_y_continuous(limits=c(0, 525)) +
  geom_rect(xmin = as.yearmon("Jan 2017"), xmax = as.yearmon("Mar 2018"), ymin = 505, ymax = 525, 
            fill = "lightblue", color = "black", alpha = 0.2) +
  annotate("text", x = as.yearmon("Aug 2017"), y = 530 * 0.97, 
           label = "P.P. Kuczynski", color = "black", size = 2.5, hjust = 0.5) +
  geom_rect(xmin = as.yearmon("Mar 2018"), xmax = as.yearmon("Nov 2020"), ymin = 505, ymax = 525, 
            fill = "lightblue",color = "black", alpha = 0.2) +
  annotate("text", x = as.yearmon("Jul 2019"), y = 530 * 0.97, 
           label = "M. Vizcarra", color = "black", size = 3, hjust = 0.5) +
  geom_rect(xmin = as.yearmon("Nov 2020"), xmax = as.yearmon("Jul 2021"), ymin = 505, ymax = 525, 
            fill = "lightblue",color = "black", alpha = 0.2) +
  annotate("text", x = as.yearmon("Mar 2021"), y = 530 * 0.97, 
           label = "F. Sagasti", color = "black", size = 2.5, hjust = 0.5) +
  geom_rect(xmin = as.yearmon("Jul 2021"), xmax = as.yearmon("Dec 2022"), ymin = 505, ymax = 525, 
            fill = "lightblue", color = "black", alpha = 0.2) +
  annotate("text", x = as.yearmon("Mar 2022"), y = 530 * 0.97, 
           label = "P. Castillo", color = "black", size = 3, hjust = 0.5) +
  geom_rect(xmin = as.yearmon("Dec 2022"), xmax = as.yearmon("Dec 2023"), ymin = 505, ymax = 525, 
            fill = "lightblue", color = "black", alpha = 0.2) +
  annotate("text", x = as.yearmon("Jun 2023"), y = 530 * 0.97, 
           label = "D. Boluarte", color = "black", size = 3, hjust = 0.5) #+ 
  #ggsave("Figure 2 - TOP3: Thematic frames.jpeg", width = 730/96 , height = 450/96)

# 2.6) Barplot of TOP-3 thematic frames per year
frame_types <- list(
  "Crime" = "crime_frame_dummy",
  "Migration" = "migration_frame_dummy",
  "Sexual Violence" = "sexvi_frame_dummy"
)

# Iterate over the list, processing each frame type to generate a df with the percentage of frame presence per year
combined_data2 <- map_df(names(frame_types), function(frame_label) {
  dummy_var <- frame_types[[frame_label]]
  data %>%
    group_by(Year, !!rlang::sym(dummy_var)) %>%
    summarise(n = n(), .groups = 'drop') %>%
    group_by(Year) %>%
    mutate(proportion = n / sum(n),
           percentage = n/sum(n) * 100) %>%
    filter(!!rlang::sym(dummy_var) == TRUE) %>%
    ungroup() %>%
    dplyr::select(-!!rlang::sym(dummy_var)) %>% # Remove the dummy variable column
    mutate(frame = frame_label)  # Add a column with the frame label
}, .id = "frame_type")

#Graph: Barplot
ggplot(combined_data2, aes(x=as.factor(Year), y=proportion, fill = frame)) +
  geom_bar(stat= "identity", position = "dodge", colour = "black") +
  geom_text(aes(label = paste0(round(percentage), "%")),
            position = position_dodge(width= 1),
            size = 2.5, hjust= 0.3, vjust = -0.3, face = "bold") +
  labs(x= "Year", y= "Percentage of frame presence in articles (%)", fill = "Frame",
       title = "Thematic frames in right-leaning Peruvian newspapers (2017-2023)") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_classic() +
  theme(plot.title = element_text(size = 11, face= "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "lines"),
        legend.title = element_text(size = 8),
        legend.box.background = element_rect(color = "black", size = 0.5),
        axis.title.y = element_text(margin = margin(r = 9, unit = "pt"),
                                    size = 8)) #+
  #ggsave("Figure 3 - Barplot 3 frames.jpeg", width = 600/96 , height = 360/96)

# 2.7) Barplots for presence of frames (Yes/No) for the period 2017-2023
# Define a function to automatize the creation of the graph
frame_presence_barplot <- function(data,variable, save_filename = NULL){
  frame_var <- enquo(variable)
  frame_var_name <- as_label(frame_var)
  var_name_short <- str_split(frame_var_name, "_")[[1]][1]
  
  plot <- data %>%
    group_by(Year, !!frame_var) %>% 
    summarise(count = n()) %>% 
    ungroup() %>% 
    group_by(Year) %>% 
    summarise(prop = count/sum(count),
              percentage = (count/sum(count))*100,
              frame_crime = !!frame_var) %>% 
    ggplot(aes(x= as.factor(Year), y=prop, fill= frame_crime)) + 
    geom_bar(stat= 'identity', position = "dodge") +
    geom_text(aes(label = paste0(round(percentage), "%")),
              position = position_dodge(width= 1),
              size = 3, vjust= -0.3) +
    labs(x = "Years", y = "Percentage of frame presence",
         fill = "Criminal Frame",
         title = paste0("Presence of ", var_name_short, " frame in peruvian right-leaning newspapers (2017-2023)")) +
    scale_y_continuous(labels= scales::percent_format()) +
    theme_classic() +
    theme(plot.title = element_text(size = 10, face= "bold", hjust = 0.5),
          legend.position = "bottom",
          legend.key.size = unit(0.5, "lines"),
          legend.title = element_text(size = 8))
  
  if(!is.null(save_filename)){ #If parameter is not NULL apply the following
    ggsave(plot, filename = save_filename, device = "jpeg", 
           width =10, height = 8, units = "in", dpi = 300)
    print(plot)
  } else { # if parameter is not active, just print the plot
    print(plot)
  }
  
}

frame_presence_barplot (data,crime_frame_dummy)
frame_presence_barplot (data,migration_frame_dummy)
frame_presence_barplot (data,sexvi_frame_dummy)

# frame_presence_barplot (data,crime_frame_dummy, save_filename = "crime_frame_years.jpeg")
# frame_presence_barplot (data,migration_frame_dummy, save_filename = "migration_frame_years.jpeg")
# frame_presence_barplot (data,sexvi_frame_dummy, save_filename = "sexualviolence_frame_years.jpeg")


# 2.8) Time series Plot - Presence of frame (Only TRUE is reported) - Figures 4, 5 and 6 in the Thesis report
#Define a function to automatize the graphs
frame_presence_timeplot <- function(data,variable, save_filename = NULL){
  frame_var <- enquo(variable)
  frame_var_name <- as_label(frame_var)
  var_name_short <- str_split(frame_var_name, "_")[[1]][1]
  
  data_summary <- data %>%
    group_by(Month_Year, !!frame_var) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(Month_Year) %>%
    summarise(
      prop = count/sum(count),
      percentage = (count/sum(count)) * 100,
      frame_crime = !!frame_var,
      .groups = 'drop') %>% 
    filter(frame_crime == T)
  
  # Calculate y_max for annotations
  y_max <- max(data_summary$prop, na.rm = TRUE) * 1.1
  
  #Plot
  plot <- ggplot(data_summary, aes(x= Month_Year, y=prop, color= frame_crime, group = frame_crime)) +
    geom_line() +
    geom_point() +
    geom_text(aes(label = paste0(round(percentage), "%")), 
              nudge_y = 0.01, 
              check_overlap = TRUE, 
              size = 2) +
    geom_rect(xmin = as.yearmon("Jan 2017"), xmax = as.yearmon("Mar 2018"), ymin = y_max * 0.95, ymax = y_max, 
              fill = "lightblue", color = "black", alpha = 0.2) +
    annotate("text", x = as.yearmon("Aug 2017"), y = y_max * 0.975, 
             label = "P.P. Kuczynski", color = "black", size = 3, hjust = 0.5) +
    geom_rect(xmin = as.yearmon("Mar 2018"), xmax = as.yearmon("Nov 2020"), ymin = y_max * 0.95, ymax = y_max, 
              fill = "lightblue",color = "black", alpha = 0.2) +
    annotate("text", x = as.yearmon("Jul 2019"), y = y_max * 0.975, 
             label = "M. Vizcarra", color = "black", size = 3, hjust = 0.5) +
    geom_rect(xmin = as.yearmon("Nov 2020"), xmax = as.yearmon("Jul 2021"), ymin = y_max * 0.95, ymax = y_max, 
              fill = "lightblue",color = "black", alpha = 0.2) +
    annotate("text", x = as.yearmon("Mar 2021"), y = y_max * 0.975, 
             label = "F. Sagasti", color = "black", size = 2.5, hjust = 0.5) +
    geom_rect(xmin = as.yearmon("Jul 2021"), xmax = as.yearmon("Dec 2022"), ymin = y_max * 0.95, ymax = y_max, 
              fill = "lightblue", color = "black", alpha = 0.2) +
    annotate("text", x = as.yearmon("Mar 2022"), y = y_max * 0.975, 
             label = "P. Castillo", color = "black", size = 3, hjust = 0.5) +
    geom_rect(xmin = as.yearmon("Dec 2022"), xmax = as.yearmon("Dec 2023"), ymin = y_max * 0.95, ymax = y_max, 
              fill = "lightblue", color = "black", alpha = 0.2) +
    annotate("text", x = as.yearmon("Jun 2023"), y = y_max * 0.975, 
             label = "D. Boluarte", color = "black", size = 3, hjust = 0.5) +
    labs(x = "Month-Years", y = "Percentage of frame presence (%)",
         fill = paste0(var_name_short,"Frame"),
         title = paste0("Presence of ", var_name_short ," frame in peruvian right-leaning newspapers (2017-2023)")) +
    scale_y_continuous(labels= scales::percent_format()) +
    theme_classic() +
    theme(plot.title = element_text(size = 10, face= "bold", hjust = 0.5),
          legend.position = "none",
          legend.key.size = unit(0.5, "lines"),
          legend.title = element_text(size = 8),
          axis.title.y = element_text(margin = margin(r = 9, unit = "pt")),
          axis.title.x = element_blank())
  
  if(!is.null(save_filename)){ #If parameter is not NULL apply the following
    ggsave(plot, filename = save_filename, device = "jpeg", 
           width =731/96, height = 361/96)
    print(plot)
  } else { # if parameter is not active, just print the plot
    print(plot)
  }
  
}

frame_presence_timeplot (data, crime_frame_dummy) # Figure 4
frame_presence_timeplot (data, migration_frame_dummy) # Figure 5
frame_presence_timeplot (data, sexvi_frame_dummy) # Figure 6

# frame_presence_timeplot (data, crime_frame_dummy, save_filename = "Figure 4 - Time series crime frames.jpeg")
# frame_presence_timeplot (data, migration_frame_dummy, save_filename = "Figure 5 - Time series migration frames.jpeg")
# frame_presence_timeplot (data, sexvi_frame_dummy, save_filename = "Figure 6 - Time series sexvi frames.jpeg")

# 2.9) Tables to obtain the percentage of frames presence per Month/Year
# Table to compare crime frame through years
crime_frame_p_monts <- data %>%
  group_by(Month_Year, crime_frame_dummy) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Month_Year) %>%
  summarise(
    prop = count/sum(count),
    percentage = (count/sum(count)) * 100,
    frame_crime = crime_frame_dummy,
    .groups = 'drop') %>% 
  filter(frame_crime == T)

# Table for migration frame
migration_frame_p_monts <- data %>%
  group_by(Month_Year, migration_frame_dummy) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Month_Year) %>%
  summarise(
    prop = count/sum(count),
    percentage = (count/sum(count)) * 100,
    frame_crime = migration_frame_dummy,
    .groups = 'drop') %>% 
  filter(frame_crime == T)

# Table for sexual violence frame
sexvi_frame_p_monts <- data %>%
  group_by(Month_Year, sexvi_frame_dummy) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Month_Year) %>%
  summarise(
    prop = count/sum(count),
    percentage = (count/sum(count)) * 100,
    frame_crime = sexvi_frame_dummy,
    .groups = 'drop') %>% 
  filter(frame_crime == T)


######### Part 3: Statistical analysis ####################

#### 3.1) Logistic Model 1: Outcome = Frames / Predictor = Years / Control = newspapers
# Logit CRIME Model 1
# Model with RSE and AME
logit_crime_1<- mfx::logitmfx(crime_frame_dummy ~ Year_category + newspaper_name, data = data, 
                              robust = T)
#Extract Coefficients and SE (AME)
logit_crime_1_coef <- logit_crime_1$mfxest[,1]  
logit_crime_1_se <- logit_crime_1$mfxest[,2]
logit_crime_1_p <- logit_crime_1$mfxest[,4]
# Report table see in the console (to textreg to display it in latext)
screenreg(logit_crime_1, override.coef = logit_crime_1_coef, override.se = logit_crime_1_se,
          override.pvalues = logit_crime_1_p)

# Logit MIGRATION Model 1
logit_migration_1 <- mfx::logitmfx(migration_frame_dummy ~ Year_category + newspaper_name, data = data, 
                                   robust = T)

#Extract Coefficients and SE (AME)
logit_migration_1_coef <- logit_migration_1$mfxest[,1]  
logit_migration_1_se <- logit_migration_1$mfxest[,2]
logit_migration_1_p <- logit_migration_1$mfxest[,4]

# For a single report in latext
screenreg(logit_migration_1, override.coef = logit_migration_1_coef, override.se = logit_migration_1_se,
          override.pvalues =logit_migration_1_p)

# Logit SEXUAL VIOLENCE Model 1
logit_sexvi_1 <- mfx::logitmfx(sexvi_frame_dummy ~ Year_category + newspaper_name, data = data, 
                               robust = T)
#Extract Coefficients and SE (AME)
logit_sexvi_1_coef <- logit_sexvi_1$mfxest[,1]  
logit_sexvi_1_se <- logit_sexvi_1$mfxest[,2]
logit_sexvi_1_p <- logit_sexvi_1$mfxest[,4]

# For a single report in latext
screenreg(logit_sexvi_1, override.coef = logit_sexvi_1_coef, override.se = logit_sexvi_1_se,
          override.pvalues = logit_sexvi_1_p)

#### 3.2) Logistic Model 2: Outcome = Frames / Predictor = Government / Control = newspapers
# Logit CRIME Model 2
logit_crime_2 <- mfx::logitmfx(crime_frame_dummy ~ Government + newspaper_name, data = data, 
                               robust = T)
#Extract Coefficients and SE (AME)
logit_crime_2_coef <- logit_crime_2$mfxest[,1]  
logit_crime_2_se <- logit_crime_2$mfxest[,2]
logit_crime_2_p <- logit_crime_2$mfxest[,4]
# For a single report in latext
screenreg(logit_crime_2, override.coef = logit_crime_2_coef, override.se = logit_crime_2_se,
          override.pvalues = logit_crime_2_p)

# Logit MIGRATION Model 2
logit_migration_2 <- mfx::logitmfx(migration_frame_dummy ~ Government + newspaper_name, data = data, 
                                   robust = T)
#Extract Coefficients and SE (AME)
logit_migration_2_coef <- logit_migration_2$mfxest[,1]  
logit_migration_2_se <- logit_migration_2$mfxest[,2]
logit_migration_2_p <- logit_migration_2$mfxest[,4]

# For a single report in latext
screenreg(logit_migration_2, override.coef = logit_migration_2_coef, override.se = logit_migration_2_se,
          override.pvalues =logit_migration_2_p)

# Logit SEXUAL VIOLENCE Model 2
logit_sexvi_2 <- mfx::logitmfx(sexvi_frame_dummy ~ Government + newspaper_name, data = data, 
                               robust = T)
#Extract Coefficients and SE (AME)
logit_sexvi_2_coef <- logit_sexvi_2$mfxest[,1]  
logit_sexvi_2_se <- logit_sexvi_2$mfxest[,2]
logit_sexvi_2_p <- logit_sexvi_2$mfxest[,4]
# For a single report in latext
screenreg(logit_sexvi_2, override.coef = logit_sexvi_2_coef, override.se = logit_sexvi_2_se,
          override.pvalues = logit_sexvi_2_p)

#### 3.2) Logistic Model 3: Outcome = Frames / Predictor = Female / Controls = Government + newspapers
# Logit CRIME Model 3
logit_crime_3 <- mfx::logitmfx(crime_frame_dummy ~ Female+ Government + newspaper_name, data = data, 
                               robust = T)
#Extract Coefficients and SE (AME)
logit_crime_3_coef <- logit_crime_3$mfxest[,1]  
logit_crime_3_se <- logit_crime_3$mfxest[,2]
logit_crime_3_p <- logit_crime_3$mfxest[,4]
# For a single report in latext
screenreg(logit_crime_3, override.coef = logit_crime_3_coef, override.se = logit_crime_3_se,
          override.pvalues = logit_crime_3_p)

# Logit MIGRATION Model 3
logit_migration_3 <- mfx::logitmfx(migration_frame_dummy ~ Female+ Government + newspaper_name, data = data, 
                                   robust = T)
#Extract Coefficients and SE (AME)
logit_migration_3_coef <- logit_migration_3$mfxest[,1]  
logit_migration_3_se <- logit_migration_3$mfxest[,2]
logit_migration_3_p <- logit_migration_3$mfxest[,4]
# For a single report in latext
screenreg(logit_migration_3, override.coef = logit_migration_3_coef, override.se = logit_migration_3_se,
          override.pvalues = logit_migration_3_p)

# Logit SEXUAL VIOLENCE Model 3
logit_sexvi_3 <- mfx::logitmfx(sexvi_frame_dummy ~ Female+ Government + newspaper_name, data = data, 
                               robust = T)

#Extract Coefficients and SE (AME)
logit_sexvi_3_coef <- logit_sexvi_3$mfxest[,1]  
logit_sexvi_3_se <- logit_sexvi_3$mfxest[,2]
logit_sexvi_3_p <- logit_sexvi_3$mfxest[,4]
# For a single report in latext
screenreg(logit_sexvi_3, override.coef = logit_sexvi_3_coef, override.se = logit_sexvi_3_se,
          override.pvalues = logit_sexvi_3_p)

#### 3.3) Reporting the models
ordered_names <- c(
  "2018" , "2019", "2020",
  "2021", "2022", "2023",
  "Ojo", "Peru21", "Trome",
  "Vizcarra", "Sagasti", "Castillo", "Boluarte"
)

ordered_names_2 <- c(
  "Female - True" ,
  "Vizcarra", "Sagasti", "Castillo", "Boluarte",
  "Ojo", "Peru21", "Trome"
)

#Table 1: For Crime (changed to screenreg() so the output can be displayed in the console)
texreg(list(logit_crime_1, logit_crime_2),
       override.coef = list(logit_crime_1_coef, logit_crime_2_coef),
       override.se = list(logit_crime_1_se, logit_crime_2_se),
       override.pvalues = list(logit_crime_1_p, logit_crime_2_p),
       custom.model.names = c("Crime", "Crime"),
       stars = c(0.05, 0.01, 0.001), custom.header = list("Model 1" = 1, "Model 2" = 2), 
       custom.coef.names = ordered_names,
       reorder.coef=c(1,2,3,4,5,6,10,11,12,13,7,8,9), groups = list("Year (ref = 2017)" = 1:6,
                                                                    "Government (ref = Kuczynski)"= 7:10,
                                                                    "Newspaper (ref = Correo)" = 11:13))
#Table 2: For Migration
texreg(list(logit_migration_1, logit_migration_2),
       override.coef = list(logit_migration_1_coef, logit_migration_2_coef),
       override.se = list(logit_migration_1_se, logit_migration_2_se),
       override.pvalues = list(logit_migration_1_p, logit_migration_2_p),
       custom.model.names = c("Migration", "Migration"),
       stars = c(0.05, 0.01, 0.001), custom.header = list("Model 1" = 1, "Model 2" = 2), 
       custom.coef.names = ordered_names,
       reorder.coef=c(1,2,3,4,5,6,10,11,12,13,7,8,9),groups = list("Year (ref = 2017)" = 1:6,
                                                                   "Government (ref = Kuczynski)"= 7:10,
                                                                   "Newspaper (ref = Correo)" = 11:13))
#Table 3: For Sexual Violence
texreg(list(logit_sexvi_1, logit_sexvi_2),
       override.coef = list(logit_sexvi_1_coef, logit_sexvi_2_coef),
       override.se = list(logit_sexvi_1_se, logit_sexvi_2_se),
       override.pvalues = list(logit_sexvi_1_p, logit_sexvi_2_p),
       custom.model.names = c("Sexual Violence", "Sexual Violence"),
       stars = c(0.05, 0.01, 0.001), custom.header = list("Model 1" = 1, "Model 2" = 2), 
       custom.coef.names = ordered_names,
       reorder.coef=c(1,2,3,4,5,6,10,11,12,13,7,8,9),groups = list("Year (ref = 2017)" = 1:6,
                                                                   "Government (ref = Kuczynski)"= 7:10,
                                                                   "Newspaper (ref = Correo)" = 11:13))
#Table 4: Gender
texreg(list(logit_crime_3, logit_migration_3, logit_sexvi_3),
       override.coef = list(logit_crime_3_coef, logit_migration_3_coef, logit_sexvi_3_coef),
       override.se = list(logit_crime_3_se, logit_migration_3_se, logit_sexvi_3_se),
       override.pvalues = list(logit_crime_3_p, logit_migration_3_p, logit_sexvi_3_p),
       custom.model.names = c("Crime", "Migration", "Sexual Violence"),
       stars = c(0.05, 0.01, 0.001), custom.header = list("Model 1" = 1, "Model 2" = 2, "Model 3" = 3),
       custom.coef.names = ordered_names_2, groups = list("Government (ref = Kuczynski) " = 2:5,
                                                          "Newspaper (ref = Correo)" = 6:8))


