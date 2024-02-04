# install.packages("tidyverse") 
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

#load in dataset

spbwl <- read.csv(file = "superbowl.csv", header = TRUE,  sep = ",", row.names = NULL,  stringsAsFactors = FALSE)

#change all instances of washington redskins to washington commanders
spbwl$Winner <- gsub("Washington Redskins", "Washington Commanders", spbwl$Winner)
spbwl$Loser <- gsub("Washington Redskins", "Washington Commanders", spbwl$Loser)

#inspect data
#str(spbwl)
#print(head(spbwl,10))

sb_2021 <- c("Feb 7 2021","LV (55)", "Tampa Bay Buccaneers", 31, "Kansas City Chiefs", 9, "Tom Brady", "Raymond James Stadium", "Tampa", "Florida")
sb_2022 <- c("Feb 13 2022","LVI (56)", "Los Angeles Rams", 23, "Cincinatti Bengals", 20, "Cooper Kupp", "SoFi Stadium", "Los Angeles", "California")
sb_2023 <- c("Feb 12 2023","LVII (57)", "Kansas City Chiefs", 38, "Philadelphia Eagles", 35, "Patrick Mahomes", "State Farm Stadium", "Glendale", "Arizona")
#sb_2024 <- c("Feb 11, 2024", "LVIII (58)", "Kansas City Chiefs",0,"San Francisco 49ers",0,"","Allegiant Stadium","Las Vegas","Nevada")

#adding recent superbowl data to dataframe
spbwl <- rbind(spbwl,sb_2021)
spbwl <- rbind(spbwl,sb_2022)
spbwl <- rbind(spbwl,sb_2023)
#spbwl <- rbind(spbwl,sb_2024)
#print(tail(spbwl,10))

#see who has won the most superbowls
winner_counts <- spbwl %>%
    count(Winner) %>%
    filter(n>=3)

#make plot
png("freq_sb.png")
freq_winners <- ggplot(winner_counts, aes(x = Winner, y = n)) +
    geom_col(fill = brewer.pal(n = nrow(winner_counts),name = "Reds" )) + labs(
    title = "Frequent Superbowl Winners",
    x = "Team",
    y = "Wins"
    ) +
    theme(axis.text.x = element_text(angle = 35, vjust = 0.5, hjust=0.8))

print(freq_winners)
dev.off() 

#create a subset for only chiefs and 49ers history
sp2024_lineup <- select(spbwl, Winner,Winner.Pts, Loser,Loser.Pts) %>% 
    filter(Winner == "Kansas City Chiefs" | 
        Winner == "San Francisco 49ers" |  
        Loser == "Kansas City Chiefs" |
        Loser == "San Francisco 49ers")

#convert winner and loser points to numeric
sp2024_lineup$Winner.Pts <- as.numeric(sp2024_lineup$Winner.Pts)
sp2024_lineup$Loser.Pts <- as.numeric(sp2024_lineup$Loser.Pts)

chief_avg <- sp2024_lineup %>%
    filter(Winner == "Kansas City Chiefs") %>%
    summarise("Average Winning Score" = mean(Winner.Pts),"Max Score" = max(Winner.Pts),"Min Score" = min(Winner.Pts), "Avg Score Difference" = mean((Winner.Pts-Loser.Pts)))

sf_avg <- sp2024_lineup %>%
    filter(Winner == "San Francisco 49ers") %>%
    summarise("Average Winning Score" = mean(Winner.Pts),"Max Score" = max(Winner.Pts),"Min Score" = min(Winner.Pts),"Avg Score Difference" = mean((Winner.Pts-Loser.Pts)))

chiefs_sf <- select(spbwl, Date, Winner, Winner.Pts,Loser,Loser.Pts) %>%
    filter(((Winner == "Kansas City Chiefs") & (Loser == "San Francisco 49ers")) | 
        ((Loser == "Kansas City Chiefs") & (Winner == "San Francisco 49ers")))

#combine chiefs and niners stats into one dataframe
# add name of team as a column
chief_avg <- chief_avg %>% 
    mutate(Team = "Chiefs")

sf_avg <- sf_avg %>%
    mutate(Team = "49ers")

sb_pred <- full_join(chief_avg,sf_avg) %>%
    select("Team",everything())


