


# Read the FIFA file
fifa <- read_excel("C:/Users/user/Downloads/football_players_FIFA_simplified.xlsx")


# 2.1(a) Show variable names and types
variable_info <- data.frame(
  Index = 1:ncol(fifa),
  Variable_Name = names(fifa),
  Class = sapply(fifa, class)
)
print(variable_info)
 
#2.1b)
nrow(fifa)

# Number of columns (variables)
ncol(fifa)

#2.1c)
View(head(fifa))  

#2.2a)duplicates
duplicate <- fifa[duplicated(fifa), ]
write.csv(duplicate, "duplicates.csv")

#2.2b)contract dates , contract dates r 4 digits, if its not 4 digits, the contract dates r false and theirs no years
no_years <- fifa[
  nchar(fifa$Contract_start) != 4 | 
    nchar(fifa$Contract_end) != 4, 
]
write.csv(no_years, "no_years.csv")
View(head(no_years, 5))

#2.2c zero salary
zero_salary <- fifa[fifa$Wage == 0, ]
write.csv(zero_salary, "zero_salary.csv")
View(head(zero_salary, 5))   #head shows first 6 but i changed n = 5

#2.2d 
zero_value <- fifa[fifa$Value == 0, ]
write.csv(zero_value, "zero_value.csv")
View(head(zero_value, 5))

# 2.2e) Records with fake players
fake_players <- fifa[grep("Fake", fifa$Player_name, ignore.case = TRUE), ]   #grep function is used to search for patterns, searching for players that have their name as fake
write.csv(fake_players, "fake_players.csv", row.names = FALSE) #ignore.case = true makes the code insenstive to capital and non-capital
View(fake_players)

#2.3a) Clean the data 

# Combining all bad records from previous steps
bad_records <- unique(rbind(
  duplicate,
  no_years,
  zero_salary,
  zero_value,
  fake_players
))  #rbind is a function which combines multiple datasets,unique() makes sure theirs no duplicates

# Remove bad records from original data
clean_fifa <- fifa[!rownames(fifa) %in% rownames(bad_records), ] # checks for each row in the fifa dataset if its row name does not appear in the row names of bad_records

#2.3b) Save  data
write.csv(clean_fifa, "clean_fifa.csv", row.names = FALSE)

# Clear everything and reload clean data
rm(list = ls())  #ls()lists all objects, rm() removes
clean_fifa <- read.csv("clean_fifa.csv")

#2.4a)
all_players <- clean_fifa

View(head(all_players, 5))
write.csv(all_players, "all_players.csv", row.names = FALSE)


# b) Players with CAM in any position
cam_players <- clean_fifa[grepl("CAM", clean_fifa$Position.1) | 
                            grepl("CAM", clean_fifa$Position.2) | 
                            grepl("CAM", clean_fifa$Position.3), ]
write.csv(cam_players, "cam_players.csv", row.names = FALSE)
View(cam_players) # | represents OR so it searches for players who play CAM in pos1 or players who play CAM in pos2 or players who play CAM in pos3



# c) Players with CAM as primary position, we want players who only have 1 position which is CAM , pos2 and pos3 need to be NA so we search for that, & is AND
cam_only <- clean_fifa[clean_fifa$Position.1 == "CAM" &is.na(clean_fifa$Position.2) &is.na(clean_fifa$Position.3),]
View(head(cam_only, 5))
write.csv(cam_only, "cam_only.csv", row.names = FALSE)


# 2.4d) Players with BOTH CAM AND CM in any positions
cam_cm_players <- clean_fifa[
  (grepl("CAM", clean_fifa$Position.1) | 
     grepl("CAM", clean_fifa$Position.2) | 
     grepl("CAM", clean_fifa$Position.3)) &
    (grepl("CM", clean_fifa$Position.1) | 
       grepl("CM", clean_fifa$Position.2) | 
       grepl("CM", clean_fifa$Position.3)), 
] #finding players that have cam in position1,2 or 3 AND have cm in position 1,2 or 3 


# View all results (not just first 5)
View(cam_cm_players)

# Export to CSV
write.csv(cam_cm_players, "cam_cm_players.csv", row.names = FALSE)

# 2.5) Wage Distribution Analysis
par(mfrow = c(2, 2)) # Set up 2x2 grid for plots, par() is a function for graphical parameters

# a) All Players
hist(all_players$Wage, breaks = 10, main = "All Players Wages", 
     xlab = "Wage", col = "blue") #breaks = 10 Divides the wage values into 10 bins (intervals).


# b) Players with CAM in any position
hist(cam_players$Wage, breaks = 10, main = "CAM Players Wages", 
     xlab = "Wage", col = "green")


# c) CAM-only players
hist(cam_only$Wage, breaks = 10, main = "CAM-Only Players Wages", 
     xlab = "Wage", col = "pink")


# d) Players with both CAM and CM
hist(cam_cm_players$Wage, breaks = 10, main = "CAM+CM Players Wages", 
     xlab = "Wage", col = "yellow")


# Reset plot layout
par(mfrow = c(1, 1))

#2.6
# Set up plot layout
par(mfrow = c(2, 2))

# a) All Players
boxplot(all_players$Wage, main = "All Players Wages", 
        ylab = "Wage", col = "blue", outline = TRUE)

# b) Players with CAM in any position
boxplot(cam_players$Wage, main = "CAM Players Wages", 
        ylab = "Wage", col = "green", outline = TRUE)

# c) CAM-only players
boxplot(cam_only$Wage, main = "CAM-Only Players Wages", 
        ylab = "Wage", col = "pink", outline = TRUE)

# d) Players with both CAM and CM
boxplot(cam_cm_players$Wage, main = "CAM+CM Players Wages", 
        ylab = "Wage", col = "yellow", outline = TRUE)

# Reseting plot layout to 1 by 1
par(mfrow = c(1, 1))

#2.7
# a) Counting players per primary position
position_counts <- table(clean_fifa$Position.1)
View(position_counts)

# b) Frequency table (relative to total count)
total_players <- nrow(clean_fifa)
position_freq <- data.frame(
  Position = names(position_counts),
  Count = as.numeric(position_counts),
  Percentage = round(as.numeric(position_counts)/total_players * 100, 1)
)#names() gets name of each position
#Count = as.numeric(position_counts):  Converts the frequency counts into numeric form , round() rounds to 1dp

# Sort by frequency
position_freq <- position_freq[order(-position_freq$Count), ] #- gives u descending order 
print(position_freq)

# c) Pie chart ,
pie(position_counts, main = "Distribution of Primary Positions", 
    col = rainbow(length(position_counts)), cex = 0.5)  #creating pie chart using position counts
#cex = 0.5 Controls the size of text labels on the chart, half the normal size

# Adding legend
legend("topright", legend = names(position_counts), 
       fill = rainbow(length(position_counts)), cex = 0.6)
#adds legend at top right , legend = names(position_counts) uses the names of the position counts
#fill = rainbow(length(position_counts)), cex = 0.6)- makes the legend colours and the pie chart the same

