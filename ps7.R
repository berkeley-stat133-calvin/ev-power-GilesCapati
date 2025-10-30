### ----- PROBLEM SET 7 ----- ###

### -- Explanatory Data Analysis -- ###
# packages for EDA
library(tidyverse)
library(dplyr)
library(ggplot2)
storms

### PROBLEM 1 ----------
# they want us to make a plot from storms package
# using hint of using the case_when(cond1 ~ value1, cond2 ~ value2, TRUE ~ default)
# used the solution sheet

storms2 <- storms |>
  mutate(class = case_when(wind < 36 ~ "Tropical Depression",
                            wind < 64 ~ "Tropical Storm",
                          TRUE ~ "Hurricane")) |>
  mutate(class = factor(class, levels = c("Tropical Depression", "Tropical Storm", "Hurricane")))

ggplot(storms2, aes(x = pressure,
                    y = wind,
                  color = class)) +
  geom_point(alpha = 0.5, size = 1.3) +
  scale_color_brewer(palette = "RdPu") +
  theme_bw(base_size = 13) +
  labs(title = "Wind Speed and Pressure in Storms (1975-2022)",
        x = "Air Pressure (mb)",
        y = "Maximum Wind Speed (knots)",
      color = "Storm Type")

### PROBLEM 2 ----------
# are there any changes you would make 
# i would change the color palette, it is not very clear

### PROBLEM 3 ----------


### -- Joins -- ###

# These tibbles are needed for problems 6 to 10
library(tibble)

A <- tibble(
  student_id = c(1L,2L,3L,4L),
  name = c("Ada", "Bruno", "Chandra", "Diego"),
  major = c("Stat", "DS", "Stat", "Econ")
)

B <- tibble (
  student_id = c(2L, 3L, 4L, 5L),
  quiz_score = c(88L, 94L, 77L, 91L)
)

### PROBLEM 6 ----------
# all students who took the quiz
# we see that not every student id has a name, and vice versa, so we must combine those who are registered, 
# meaning that they have a name and student ID, and those who quiz scores
quiz_taken <- inner_join(x = A, y = B, join_by(student_id == student_id))
quiz_taken

### PROBLEM 7 ----------
# the quiz scores for all registered students
# we are interested in the students with student id's and with a quiz score
quiz_scores <- left_join(x = A, y = B, join_by(student_id == student_id))
quiz_scores

### PROBLEM 8 ----------
# those with names and majors of all students who took the quiz
names_majors <- 
names_majors 