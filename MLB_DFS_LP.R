library(tidyverse)
library(ggimage)
library(gt)
library(gtExtras)
library(ggplot2)
library(dplyr)
library(tidymodels)
library(lpSolve)

MLB_DFS_8_3 <- MLB_DFS_8_3[!is.na(salary) & !is.na(fantasy) & !is.na(position), ]
MLB_DFS_8_3$binary <- 1  # decision variable for each player
MLB_DFS_8_3$salary <- gsub("[$,]", "", MLB_DFS_8_3$salary)  # remove $ and commas
MLB_DFS_8_3$salary <- as.numeric(MLB_DFS_8_3$salary)

n <- nrow(MLB_DFS_8_3)

# Build constraint matrix
position_constraints <- matrix(0, nrow=8, ncol=n)

position_map <- list(
  SP = 1,
  `1B` = 2,
  `2B` = 3,
  `3B` = 4,
  SS = 5,
  OF = 6,
  HITTER = 7,
  TOTAL = 8
)

# Fill in position constraints
for (i in 1:n) {
  pos <- MLB_DFS_8_3$position[i]

  if (isTRUE(pos == "SP")) position_constraints[1, i] <- 1
  if (isTRUE(pos == "1B")) position_constraints[2, i] <- 1
  if (isTRUE(pos == "2B")) position_constraints[3, i] <- 1
  if (isTRUE(pos == "3B")) position_constraints[4, i] <- 1
  if (isTRUE(pos == "SS")) position_constraints[5, i] <- 1
  if (isTRUE(pos == "OF")) position_constraints[6, i] <- 1

  # Utility (non-SP hitters only)
  if (!isTRUE(pos == "SP")) position_constraints[7, i] <- 1

  # Total player count (include all players)
  position_constraints[8, i] <- 1
}
# Right-hand side for each constraint
rhs <- c(1, 1, 1, 1, 1, 3, 1, 9)

# All constraints are equalities
directions <- rep("=", 8)

# Add salary constraint
salary_constraint <- MLB_DFS_8_3$salary
constraint_matrix <- rbind(position_constraints, MLB_DFS_8_3$salary)
rhs <- c(1, 1, 1, 1, 1, 3, 1, 9, 35000)
directions <- c("=", "=", "=", "=", "=", "=", "=", "=", "<=")
rhs <- as.numeric(rhs)
directions <- as.character(directions)

# Objective: maximize fantasy points
objective <- MLB_DFS_8_3$fantasy

# Solve LP
result <- lp("max", objective, constraint_matrix, directions, rhs, binary.vec=1:n)

# Output
if (result$status == 0) {
  MLB_DFS_8_3[result$solution == 1, ]
} else {
  print("No feasible lineup found under constraints.")
}








# Clean and prep
MLB_DFS_8_3$salary <- gsub("[$,]", "", MLB_DFS_8_3$salary)
MLB_DFS_8_3$salary <- as.numeric(MLB_DFS_8_3$salary)
MLB_DFS_8_3$fantasy <- as.numeric(MLB_DFS_8_3$fantasy)
MLB_DFS_8_3 <- MLB_DFS_8_3[!is.na(MLB_DFS_8_3$salary) &
                             !is.na(MLB_DFS_8_3$fantasy) &
                             !is.na(MLB_DFS_8_3$position), ]

n <- nrow(MLB_DFS_8_3)
position_constraints <- matrix(0, nrow=8, ncol=n)

for (i in 1:n) {
  pos <- MLB_DFS_8_3$position[i]

  if (pos == "SP") position_constraints[1, i] <- 1
  if (pos == "1B") position_constraints[2, i] <- 1
  if (pos == "2B") position_constraints[3, i] <- 1
  if (pos == "3B") position_constraints[4, i] <- 1
  if (pos == "SS") position_constraints[5, i] <- 1
  if (pos == "OF") position_constraints[6, i] <- 1
  if (pos != "SP") position_constraints[7, i] <- 1  # utility hitter
  position_constraints[8, i] <- 1  # total players
}

rhs <- c(1, 1, 1, 1, 1, 3, 1, 9, 45000)
directions <- c(rep("=", 8), "<=")
constraint_matrix <- rbind(position_constraints, MLB_DFS_8_3$salary)

# Run LP
result <- lp("max", MLB_DFS_8_3$fantasy, constraint_matrix, directions, rhs, binary.vec=1:n)

# Output result
if (result$status == 0) {
  MLB_DFS_8_3[result$solution == 1, ]
} else {
  print("No feasible lineup found under constraints.")
}












MLB_DFS_8_3$salary <- gsub("[$,]", "", MLB_DFS_8_3$salary)
MLB_DFS_8_3$salary <- as.numeric(MLB_DFS_8_3$salary)
MLB_DFS_8_3$fantasy <- as.numeric(MLB_DFS_8_3$fantasy)

# Remove players with missing data
MLB_DFS_8_3 <- MLB_DFS_8_3[!is.na(MLB_DFS_8_3$salary) &
                             !is.na(MLB_DFS_8_3$fantasy) &
                             !is.na(MLB_DFS_8_3$position), ]

n <- nrow(MLB_DFS_8_3)

# Constraint matrix: 8 rows for positions + 1 row for salary
position_constraints <- matrix(0, nrow = 7, ncol = n)

# Build the position constraints (no utility)
for (i in 1:n) {
  pos <- MLB_DFS_8_3$position[i]

  if (pos == "SP") position_constraints[1, i] <- 1
  if (pos == "1B") position_constraints[2, i] <- 1
  if (pos == "2B") position_constraints[3, i] <- 1
  if (pos == "3B") position_constraints[4, i] <- 1
  if (pos == "SS") position_constraints[5, i] <- 1
  if (pos == "OF") position_constraints[6, i] <- 1
  position_constraints[7, i] <- 1  # total players
}

# Add salary constraint as a new row
salary_row <- matrix(MLB_DFS_8_3$salary, nrow = 1)
constraint_matrix <- rbind(position_constraints, salary_row)

# Right-hand sides:
# 1 SP, 1 1B, 1 2B, 1 3B, 1 SS, 3 OFs, 8 total players, ≤ $35,000
rhs <- c(1, 1, 1, 1, 1, 3, 8, 35000)

# Direction of constraints: all equalities, salary is a ≤
directions <- c("=", "=", "=", "=", "=", "=", "=", "<=")

# Objective: maximize fantasy points
objective <- MLB_DFS_8_3$fantasy

# Solve LP
result <- lp("max", objective, constraint_matrix, directions, rhs, binary.vec = 1:n)

# Output result
if (result$status == 0) {
  optimal_team <- MLB_DFS_8_3[result$solution == 1, ]
  print(optimal_team)
} else {
  print("No feasible lineup found under constraints.")
}


















MLB_DFS_8_3$salary <- gsub("[$,]", "", MLB_DFS_8_3$salary)
MLB_DFS_8_3$salary <- as.numeric(MLB_DFS_8_3$salary)
MLB_DFS_8_3$fantasy <- as.numeric(MLB_DFS_8_3$fantasy)

# Filter out incomplete rows
MLB_DFS_8_3 <- MLB_DFS_8_3[!is.na(MLB_DFS_8_3$salary) &
                             !is.na(MLB_DFS_8_3$fantasy) &
                             !is.na(MLB_DFS_8_3$position), ]

n <- nrow(MLB_DFS_8_3)

# 9 constraints: SP, C, 1B, 2B, 3B, SS, OF (x3), total players
position_constraints <- matrix(0, nrow = 8, ncol = n)

for (i in 1:n) {
  pos <- MLB_DFS_8_3$position[i]

  if (pos == "SP") position_constraints[1, i] <- 1
  if (pos == "C")  position_constraints[2, i] <- 1
  if (pos == "1B") position_constraints[3, i] <- 1
  if (pos == "2B") position_constraints[4, i] <- 1
  if (pos == "3B") position_constraints[5, i] <- 1
  if (pos == "SS") position_constraints[6, i] <- 1
  if (pos == "OF") position_constraints[7, i] <- 1
  position_constraints[8, i] <- 1  # total player count
}

# Add salary constraint
salary_row <- matrix(MLB_DFS_8_3$salary, nrow = 1)
constraint_matrix <- rbind(position_constraints, salary_row)

# RHS for each constraint
rhs <- c(1, 1, 1, 1, 1, 1, 3, 9, 35000)  # 1 of each, 3 OF, 9 players, ≤ $35k
directions <- c("=", "=", "=", "=", "=", "=", "=", "=", "<=")

# Objective: maximize fantasy points
objective <- MLB_DFS_8_3$fantasy

# Solve LP
result <- lp("max", objective, constraint_matrix, directions, rhs, binary.vec = 1:n)

# Output result
if (result$status == 0) {
  optimal_team <- MLB_DFS_8_3[result$solution == 1, ]
  print(optimal_team)
} else {
  print("No feasible lineup found under constraints.")
}


library(scales)  # for dollar formatting

# Optional: select and rename columns for display
optimal_team_gt <- optimal_team %>%
  select(Player = player, Position = position, Team = team, Salary = salary, Game = gameInfo,`Fantasy Pts` = fantasy) %>%
  arrange(match(Position, c("SP", "C", "1B", "2B", "3B", "SS", "OF"))) %>%
  gt() %>%
  tab_header(
    title = "Optimized MLB DFS Lineup (Fanduel)- August 3",
    subtitle = "Maximized Fantasy Points Under $35,000 Salary Cap"
  ) %>%
  fmt_number(columns = `Fantasy Pts`, decimals = 1) %>%
  fmt_currency(columns = Salary, currency = "USD") %>%
  cols_label(
    Player = "Player",
    Position = "Pos",
    Team = "Team",
    Salary = "Salary",
    Game = "Game",
    `Fantasy Pts` = "Fantasy"
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.align = "left"
  )

optimal_team_gt
