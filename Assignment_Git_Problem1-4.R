#BAN400 Assignment Git

#Problem 1 -4

##############################################################################
#Problem1 
# I did it by clone, update the local file, add and commit, and push


##############################################################################
#Problem2
# read data "suites_dw_Table1.txt" to a tidy data frame in R, 
# without changing the raw data file and without hard coding line numbers.

#set working directory

#load packages
library(tidyverse)
library(ggplot2)

#Read text data
raw_text <- readLines(con = "C:/Users/littl/ban400/git-Tomoko-Yamagata/suites_dw_Table1.txt")

#Convert the text file to data frame
#separate with "|"
#skip first 14 rows

df_galaxy<-
  read.delim("suites_dw_Table1.txt",
           header = FALSE,
           sep = "|",
           skip = 14
           ) 

#Add column's names from row 13
colnames(df_galaxy)<-
  readLines("suites_dw_Table1.txt")[13] %>%
  strsplit(split = "\\|") %>% unlist()


##############################################################################
# Problem3
# The tendency: There are some signs that the smaller objects 
# are under-represented in the sample
# Make a plot that reveals the tendency and a likely explanation


# For example, define "smaller objects" as the galaxies that have 
# smaller values in "a_26"(linear diameter)

median(df_galaxy$` a_26  `, na.rm = TRUE)
mean(df_galaxy$` a_26  `, na.rm = TRUE)

# mean of 'a_26' > median of a_26, which means that 
# the sample regarding the diameter is right-skewed distribution
# and some large size galaxies are pushing up the mean.

df_galaxy %>% 
  ggplot(aes(x=` a_26  `))+
  geom_histogram(aes(y= ..density..),
                 binwidth=0.5,
                 colour="black", fill="white")+
  geom_density(alpha=0.05, fill="pink")+
  geom_vline(aes(xintercept=mean(` a_26  `, na.rm=TRUE)),
             color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=median(` a_26  `, na.rm=TRUE)),
             color="blue", linetype="dashed", size=1)

  
#limit the data to the 1st 25% of "a_26"
q1_a_26 <- 
  df_galaxy %>% 
  filter (` a_26  ` <= quantile(df_galaxy$` a_26  `, na.rm=TRUE)[1:2])
  
q1_a_26 %>% 
  ggplot(aes(x= ` a_26  `))+
  geom_histogram(aes(y= ..density..),
                 binwidth=0.5,
                 colour="black", fill="white")+
  geom_density(alpha=0.05, fill="pink")+
  geom_vline(aes(xintercept=mean(` a_26  `, na.rm=TRUE)),
             color="red", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=median(` a_26  `, na.rm=TRUE)),
             color="blue", linetype="dashed", size=1)

# from this plot, despite there are lot of galaxies 
# whose diameters are between 0.5 to 5, whilst the numbers of 
# galaxy whose sizes are smaller than 0.5 are very limited.
# It seems the Hubble cannot observe these size of galaxies.
# therefore, this research could be said that the data has limitation 
# in representatives regarding very small size of galaxies. 


##############################################################################
#Problem4

# Read the txt file
raw_text2 <- readLines(con = "C:/Users/littl/ban400/git-Tomoko-Yamagata/UCNG_Table4.txt")

# Convert the txt file into a dataframe

df_galaxy2 <-
  read.delim("UCNG_Table4.txt",
             header = FALSE,
             sep = "|",
             skip = 2
  ) 

# use the the 1st row as a column name

colnames(df_galaxy2)<-
  readLines("UCNG_Table4.txt")[1] %>%
  strsplit(split = "\\|") %>% unlist()


# Join this information with the table from Problem 2 


df_galaxy3 <- left_join(x= df_galaxy2, y = df_galaxy)

# Plot the velocity of each galaxy (cz) against their distance from us (D). 

df_galaxy3 %>% 
  ggplot(aes(x=`   D   `, y=`  cz  `))+
  geom_point()+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)


# the expansion of space is given by v = HD, 
# where v is the velocity, D is the distance, 
# and H is Hubble's constant.

# I replaced D and cz (x-axis=D, y axis= cz)
# In this note, the expansion can be said v= HD
# except some very large velocity galaxies.
# Also, the the galaxies where the distance between 
# 0.2 to 1.0 seem also not following the trend... 


