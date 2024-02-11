# install.packages("tidyverse") 
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(knitr)
library(broom)
library(purrr)
library(rpart)


#load in dataset

spbwl <- read.csv(file = "superbowl.csv", header = TRUE,  sep = ",", row.names = NULL,  stringsAsFactors = FALSE)

#change all instances of washington redskins to washington commanders
spbwl$Winner <- gsub("Washington Redskins", "Washington Commanders", spbwl$Winner)
spbwl$Loser <- gsub("Washington Redskins", "Washington Commanders", spbwl$Loser)

#inspect data
#str(spbwl)
#print(head(spbwl,10))

#add recent superbowls
sb_2021 <- c("Feb 7 2021","LV (55)", "Tampa Bay Buccaneers", 31, "Kansas City Chiefs", 9, "Tom Brady", "Raymond James Stadium", "Tampa", "Florida")
sb_2022 <- c("Feb 13 2022","LVI (56)", "Los Angeles Rams", 23, "Cincinatti Bengals", 20, "Cooper Kupp", "SoFi Stadium", "Los Angeles", "California")
sb_2023 <- c("Feb 12 2023","LVII (57)", "Kansas City Chiefs", 38, "Philadelphia Eagles", 35, "Patrick Mahomes", "State Farm Stadium", "Glendale", "Arizona")
#sb_2024 <- c("Feb 11, 2024", "LVIII (58)", "Kansas City Chiefs",0,"San Francisco 49ers",0,"","Allegiant Stadium","Las Vegas","Nevada")

spbwl <- rbind(spbwl, sb_2021, sb_2022, sb_2023)
#spbwl <- rbind(spbwl,sb_2024)
#print(tail(spbwl,10))

#see who has won the most superbowls
winner_counts <- spbwl %>%
    count(Winner) %>%
    filter(n>=3)

#make plot
png("freq_sb.png")
freq_winners <- ggplot(winner_counts, aes(x = Winner, y = n)) +
    geom_col(fill = brewer.pal(n = nrow(winner_counts),name = "Paired" )) + labs(
    title = "Frequent Superbowl Winners",
    x = "Team",
    y = "Wins"
    ) +
    theme(axis.text.x = element_text(angle = 35, vjust = 0.5, hjust=0.8), panel.grid = element_blank(), panel.background = element_rect(fill = "white"))

#print(freq_winners)
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

games_html <- kable(sb_pred, format = "markdown")
print(games_html)

#Stats with TS in Attendance
tkts <- data.frame(
    Date = c("Sept. 17","Sept. 24", "Oct. 1","Oct. 8","Oct. 12","Oct. 22","Oct. 29", "Nov. 5","Nov. 20","Nov. 26","Dec. 3","Dec. 10","Dec. 17","Dec. 25","Dec. 31","Jan. 13","Jan. 21","Jan. 28"),
    Opponent = c("Jacksonville Jaguars","Chicago Bears","New York Jets","Minnesota Vikings","Denver Broncos","Los Angeles Chargers","Denver Broncos","Miami Dolphins","Philadelphia Eagles","Las Vegas Raiders","Green Bay Packers","Buffalo Bills","New England Patriots","Las Vegas Raiders","Cincinnati Bengals","Miami Dolphins","Buffalo Bills","Baltimore Ravens"), 
    Catches = c(4,7,6,10,9,12,6,3,7,6,4,6,5,5,3,7,5,11),
    Yards = c(26,69,60,67,124,179,58,14,44,91,81,83,28,44,16,71,75,116),
    Touchdowns = c(1,1,0,1,0,1,0,0,1,0,0,0,0,0,0,0,2,1),
    TS_Present = c(FALSE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
)

#summary of stats for all games
summary(tkts)

#exclude date and TS_Present column for boxplot representation
tkts_2 <- select(tkts,-Date,-TS_Present)

#boxplots for each column by putting data into long format
tkts_long <- tkts_2 %>%
    pivot_longer(cols = -Opponent, 
                names_to = "variable",
                values_to = "value")

# Plot 
boxy <- ggplot(tkts_long, aes(variable, value), outlier.shape= NA) +
    geom_boxplot(aes(group = variable, fill = variable),alpha = 0.7) +
    facet_wrap(~ variable, scales = "free") +
    labs(title = "Distribution of Player Statistics", x = "Player Statistics", y = "Value") + 
    theme(axis.text.x = element_blank()) + theme_classic() +
    scale_fill_brewer(palette = "Reds")



#Statistical Analysis
columns = c("Catches","Yards","Touchdowns")

t_test_results <- map_df(columns, ~tidy(t.test(reformulate("TS_Present", response = .x), data = tkts)) %>% 
    mutate(Variable = .x)) %>%
    rename("Estimated Difference" = estimate,"Average without TS" = estimate1,"Average with TS" = estimate2) %>%
    select("Variable", "Average without TS","Average with TS","Estimated Difference",everything(), -method,-alternative) 

T_Test <- kable(t_test_results, format = "markdown")

print(T_Test)

#Stats for Chiefs and Niners Games
#create new data frame for chiefs
chiefs <- data.frame(
    Winner = c("Saints","Chiefs","Chiefs","Lions","Chiefs","Chiefs","Chiefs","Chiefs","Chiefs","Chiefs","Broncos","Chiefs","Eagles","Chiefs","Packers","Bills","Chiefs","Raiders","Chiefs","Chiefs","Chiefs","Chiefs","Chiefs"),
    Winner_Pts = c(26,38,32,21,17,41,23,27,19,31,24,21,21,31,27,20,27,20,25,13,26,27,17),
    Loser = c("Chiefs","Cardinals","Browns","Chiefs","Jaguars","Bears","Jets","Viking","Broncos","Chargers","Chiefs","Dolphins","Chiefs","Raiders","Chiefs","Chiefs","Patriots","Chiefs","Bengals","Chargers","Dolphins","Bills","Ravens"),
    Loser_Pts = c(24,10,32,20,9,10,20,20,8,17,9,14,17,17,19,17,17,14,17,12,7,24,10),
    Turf = c("Away","Away","Home","Home","Away","Home","Away","Away","Home","Home","Away","Home","Home","Away","Away","Home","Away","Home","Home","Away","Home","Away","Away"),
    Season = c("Pre","Pre","Pre","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Post","Post","Post")
)

niners <- data.frame(
    Winner = c("Raiders","49ers","Chargers","49ers","49ers","49ers","49ers","49ers","Browns","Vikings","Bengals","49ers","49ers","49ers","49ers","49ers","49ers","Ravens","49ers","Rams","49ers","49ers"),
    Winner_Pts = c(34,21,23,30,30,30,35,42,19,22,31,34,27,31,42,28,45,33,27,21,24,34),
    Loser = c("49ers","Broncos","49ers","Steelers","Rams","Giants","Cardinals","Cowboys","49ers","49ers","49ers","Jaguars","Buccaneers","Seahawks","Eagles","Seahawks","Cardinals","49ers","Commanders","49ers","Packers","Lions"),
    Loser_Pts = c(7,20,12,7,23,12,16,10,17,17,17,3,14,13,19,16,29,19,10,20,21,31),
    Turf = c("Away","Home","Home","Away","Away","Home","Home","Home","Away","Away","Home","Away","Home","Away","Away","Home","Away","Home","Away","Home","Home","Home"),
    Season = c("Pre","Pre","Pre","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Regular","Post","Post")
)

turf_stats_chiefs <- chiefs %>%
    filter(Winner == "Chiefs") %>%
    select(Turf,everything()) %>%
    group_by(Turf) %>%
    summarize("Avg Win Pts" = mean(Winner_Pts),"Max Win Pts" = max(Winner_Pts),"Min Win Pts" = min(Winner_Pts),"Avg Diff in Pts" = mean(Winner_Pts-Loser_Pts))

turf_stats_niners <- niners %>%
    filter(Winner == "49ers") %>%
    select(Turf,everything()) %>%
    group_by(Turf) %>%
    summarize("Avg Win Pts" = mean(Winner_Pts),"Max Win Pts" = max(Winner_Pts),"Min Win Pts" = min(Winner_Pts),"Avg Diff in Pts" = mean(Winner_Pts-Loser_Pts))

turf_stats_chiefs <- turf_stats_chiefs %>%
    mutate(Team = "Chiefs")

turf_stats_niners <- turf_stats_niners %>% 
    mutate(Team = "49ers")
#combine statistics for comparison
turf_stats <- full_join(turf_stats_chiefs, turf_stats_niners) %>%
    select(Team,everything())

turf_stats_html <- kable(turf_stats, format = "markdown")
print(turf_stats_html)

#season stats
season_stats_chiefs <- chiefs %>%
    filter(Winner == "Chiefs") %>%
    select(Season,everything()) %>%
    group_by(Season) %>%
    summarize("Avg Win Pts" = mean(Winner_Pts),"Max Win Pts" = max(Winner_Pts),"Min Win Pts" = min(Winner_Pts),"Avg Diff in Pts" = mean(Winner_Pts-Loser_Pts))

season_stats_niners <- niners %>%
    filter(Winner == "49ers") %>%
    select(Season,everything()) %>%
    group_by(Season) %>%
    summarize("Avg Win Pts" = mean(Winner_Pts),"Max Win Pts" = max(Winner_Pts),"Min Win Pts" = min(Winner_Pts),"Avg Diff in Pts" = mean(Winner_Pts-Loser_Pts))

season_stats_chiefs <- season_stats_chiefs %>%
    mutate(Team = "Chiefs")

season_stats_niners <- season_stats_niners %>% 
    mutate(Team = "49ers")

#combine statistics for comparison
season_stats <- full_join(season_stats_chiefs, season_stats_niners) %>%
    select(Team,everything())

season_stats_html <- kable(season_stats, format = "markdown")
print(season_stats_html)

#Stats exluding turf and season
stats_chiefs <- chiefs %>%
    filter(Winner == "Chiefs") %>%
    summarize("Avg Win Pts" = mean(Winner_Pts),"Max Win Pts" = max(Winner_Pts),"Min Win Pts" = min(Winner_Pts),"Avg Diff in Pts" = mean(Winner_Pts-Loser_Pts))

stats_niners <- niners %>%
    filter(Winner == "49ers") %>%
    summarize("Avg Win Pts" = mean(Winner_Pts),"Max Win Pts" = max(Winner_Pts),"Min Win Pts" = min(Winner_Pts),"Avg Diff in Pts" = mean(Winner_Pts-Loser_Pts))

stats_chiefs <- stats_chiefs %>%
    mutate(Team = "Chiefs")

stats_niners <- stats_niners %>% 
    mutate(Team = "49ers")

#combine statistics for comparison
stats <- full_join(stats_chiefs, stats_niners) %>%
    select(Team,everything())

stats_html <- kable(stats, format = "markdown")
print(stats_html)

# Prediction
# Assuming 'chiefs' is your Chiefs dataset
chiefs$Outcome <- ifelse(chiefs$Winner == "Chiefs", "Win", "Lose")

# Assuming 'niners' is your Niners dataset
niners$Outcome <- ifelse(niners$Winner == "49ers", "Win", "Lose")

# Combine the datasets
combined_data <- rbind(chiefs, niners)

# Filter data for when the Chiefs won
chiefs_wins <- combined_data[combined_data$Winner == "Chiefs", ]

# Filter data for when the 49ers won
niners_wins <- combined_data[combined_data$Winner == "49ers", ]

# Combine the datasets for Chiefs' and 49ers' wins
combined_wins_data <- rbind(chiefs_wins, niners_wins)

# Train a decision tree model using the combined wins data
tree_model_wins <- rpart(Winner_Pts - Loser_Pts ~ Winner_Pts + Loser_Pts, data = combined_wins_data)

# Plot the decision tree for wins
plot(tree_model_wins)
text(tree_model_wins)

# Create real data for a hypothetical matchup
hypothetical_data_wins <- data.frame(
    Winner_Pts = c(31),  
    Loser_Pts = c(20)    
)

# Make predictions on the hypothetical matchup for wins
predicted_margin_wins <- predict(tree_model_wins, newdata = hypothetical_data_wins)

# Determine the winner based on the predicted margin for wins
predicted_winner_wins <- ifelse(predicted_margin_wins > 0, "Chiefs", "49ers")
predicted_margin_wins <- abs(predicted_margin_wins)

# Display the predicted outcome for wins
print(paste("Predicted Winner for Wins:", predicted_winner_wins))
print(paste("Predicted Margin of Victory for Wins:", predicted_margin_wins))