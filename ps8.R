### ----- PROBLEM SET 8 ----- ### 

### -- STRINGS I -- ###
# packages for STRINGS I
library(tidyverse)
library(dplyr)
library(stringr)
# this tells us the first few
head(fruit) 

### PROBLEM 1 ----------
# this tells us the structure of the package
str(fruit)

# this tells us the length
str_length(fruit)

### PROBLEM 2 ----------
# making the package into a data frame
fruitdf <- data.frame(fruit)
fruitdf

# now that it is a df I can use the mutate function
# nchar = number of character
fruitdf |>
  mutate(nchar = nchar(fruit)) |>
  arrange(desc(nchar))

# purple mangosteen has the longest character string
# nut has the shortest character string

### PROBLEM 3 ----------
# renaming an object in a df using stringr function
str_replace(fruit, "currant", "red currant")

### PROBLEM 4 ----------
# paste0 function does not automatically add spaces between arguments
# sep connect within each pair
# collapse connects between the results
# example: sep = "are" and collapase = ";" --- paste(fruits, colors, sep="are", collapse=";") ----- result: "apples are red; bananas are yellow; cherries are red"
paste0("My favorite fruits are ", paste(paste(fruit[0:(length(fruit)-1)], collapse = ", "), fruit[length(fruit)], sep = ", and "), ".") # review this, takes a while to digest

### PROBLEM 5 ----------
# literally prints emoji, my favorite fruit from the vector, and the emoji again
paste("\U0001F600", fruit[length(fruit)], "\U0001F600")

### PROBLEM 6 ----------
# we want to print out only the fruits that have berry in its name
str_view(fruit, "berry")

### PROBLEM 7 ----------
# we want the fruits names that contain the letter "o"
length(str_view(fruit,"o"))

### PROBLEM 8 ----------
# we want the fruit names with the most "o's"
# this is adding a new column in the df
fruitdf$o_count <- str_count(fruitdf$fruit,"o")

# this arranges the fruit with the most "o's" in descending order
fruitdf |>
  arrange(desc(o_count))

### PROBLEM 9 ----------
# we want to find the fruits with names that have two "a's" separated with another chr
# making a new column for the data frame that has the pattern
fruitdf$a.a_pattern <- str_detect(fruitdf$fruit, "a.a")

# print where the pattern is true in the df
fruitdf$fruit[fruitdf$a.a_pattern]

# find the number of fruit with the pattern
length(fruitdf$fruit[fruitdf$a.a_pattern])

### -- STRINGS II -- ###
# packages for STRINGS II
library(babynames)
babynames

### PROBLEM 10 ----------
# code that helped me find the info to the following questions
?babynames

# what does every row refer to? 
# i believe it shares the information regarding a babyname during a specific year

# what is the unit observed?
# the unit is number of live births, rounded to nearest 1000

# what is the time range that is covered in this data?
# it seems like the data is in chronological order so i print the first few to get the earliest year
head(babynames)
# and now i print the last few
tail(babynames)
# the year range is 1880 to 2017

# how is prop calculate? (what is the numerator and denominator?)
# it is calculated by n number of babies with that name divided by the total number of applicants that year

### PROBLEM 11 ----------
# in 2010, what is the proportion of boy names that ended with a vowel?
# here i created a new dataframe with the desired info
vowelboys_2010 <- babynames |>
  filter(year == "2010",
          sex == "M",
          str_detect(name, "[aeiou]$")
)

# here is to sum up the prop
sum(vowelboys_2010$prop)
# we get prop = 0.139581

# how about girl names?
# used the same code as earlier
vowelgirls_2010 <- babynames |>
  filter(year == "2010",
          sex == "F",
          str_detect(name, "[aeiou]$")
)
sum(vowelgirls_2010$prop)
# prop = 0.5374653

# how do these proportions differ when calculated in 1910?
# used the same code as earlier
vowelboys_1910 <- babynames |>
  filter(year == "1910",
          sex == "M",
          str_detect(name, "[aeiou]$")
)

vowelgirls_1910 <- babynames |>
  filter(year == "1910",
          sex == "F",
          str_detect(name, "[aeiou]$")
)
sum(vowelboys_1910$prop)
sum(vowelgirls_1910$prop)
# prop for boys = 0.1589482
# prop for girls = 0.5805035
# in comparison they both increased b/w ~0.2-0.5 of a margin

### PROBLEM 12 ----------
# we are asked to narrow the data frame to names that end with a diminuitive
# for ours, we will collect those that end with "-ito" or "-ita"
babynames |>
  filter(str_detect(name, "ita$|ito$"))

### PROBLEM 13 ----------
# how many distinct names rhyme with Aiden?
aiden_rhyme <- babynames |>
  filter(str_detect(name, "a+y*i*den$")) |>
  distinct(name)

length(aiden_rhyme$name)
# 136 distinct names that rhyme with Aiden

### PROBLEM 14 ----------
# plot a graph using the info about your name
library(ggplot2)
library(dplyr)
library(stringr)

giles <- babynames |>
  filter(str_detect(name, "Giles")) |>
  ggplot(aes(x = year, y = prop)) +
  geom_line(color = "blue") +
  labs(title = "Popularity of the Name Giles",
       x = "Year",
       y = "Popularity")

giles

### PROBLEM 15 ----------
# create a plot that shows the relative frequency of a name of a historic figure over time
ryan <- babynames |>
  filter(str_detect(name, "Ryan"),
          sex == "M") |>
  ggplot(aes(x = year, y = prop)) +
  geom_line(color = "yellow") +
  labs(title = "Popularity of the Name Ryan",
      x = "Year",
      y = "Popularity")

ryan
# i did the same code as before, just with a different name, and restricted this to
# just males since it is a gender neutral name

### PROBLEM 16 ----------
# optional challenge: for each year, which names have been the most gender neutral?
# my plan is to make a for loop for the year 2000-2017, then filter, then list
neutral <- babynames|>
  group_by(name)
  filter(year >= 2000, year <= 2017)

neutral
bggb
bgtr
rgtbr
rbtgtr
brtg
rb
rgbr
bbtg

### PROBLEM 17 ----------
# optional challenge: plot the proportion of diminutive names over time
# i simply combined my work of the ita/ito and graphing problem
ita_ito <- babynames |>
  filter(str_detect(name, "ita$|ito$")) |>
  ggplot(aes(x = year, y = prop)) +
  geom_line(color = "red") +
  labs(title = "Popularity of the Name w/ 'ita' or 'ito'",
       x = "Year",
       y = "Popularity")

ita_ito

### PROBLEM 18 ----------

### PROBLEM 19 ----------
# this is a character string required for the remaining questions
x <- "We're goin' up, up, up, it's our moment
You know together we're glowing
Gonna be, gonna be golden
Oh, up, up, up, with our voices
\uc601\uc6d0\ud788 \uae68\uc9c8 \uc218 \uc5c6\ub294
Gonna be, gonna be golden"

# how many lines are in the chorus
str_split(x)

### PROBLEM 20 ----------
