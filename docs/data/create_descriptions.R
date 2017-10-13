readme_txt <- readLines("http://seanlahman.com/files/database/readme2016.txt", warn = FALSE)


headers_pos <- grep("^2\\.[0-9]+[ ]+[a-zA-Z ]+([Tt]able)$", readme_txt)
headers <- readme_txt[headers_pos]
headers <- gsub("^2\\.[0-9]+[ ]+", "", headers)
headers <- gsub("( [Tt]able)$", "", headers)

headers <- gsub("MASTER", "Master", headers)
headers <- gsub("TeamFranchises", "TeamsFranchises", headers)
headers_pos1 <- c(headers_pos, length(readme_txt))

column_descriptions <-
  lapply(seq_along( headers_pos), function(i) {
    lines <- readme_txt[
      (headers_pos1[i]+1):(headers_pos1[i+1]-2)
      ]
    lines <- lines[lines != ""]
    lines <- gsub("\\t", " ", lines)
    map(lines, function(l) {
      words <- stringr::word(l, 1:20)
      words <- words[complete.cases(words) & words != ""]
      data_frame(
        column = words[1],
        description = paste(words[-1], collapse = " ")
      )
    }) %>%
      bind_rows
  }) %>%
  set_names(headers) %>%
  bind_rows(.id = "table")

column_descriptions$description[column_descriptions$column == "playerID"] <-
  "Player ID code"

write.table(
  column_descriptions,
  file = "docs/data/column_descriptions.csv",
  row.names = TRUE,
  sep = ",",
  quote = TRUE
)
any(grepl('"',column_descriptions$description))
#save(column_descriptions, file = "docs/data/column_descriptions.Rdata")


table_descriptions <-
  read.table(header = TRUE, sep = ",", text = '
"table", "description"
"Master", "Player names, DOB, and biographical info"
"Batting", "Batting statistics"
"Pitching", "Pitching statistics"
"Fielding", "Fielding statistics"
"AllStarFull", "All-Star appearances"
"HallofFame", "Hall of Fame voting data"
"Managers", "Managerial statistics"
"Teams", "Yearly stats and standings "
"BattingPost", "Post-season batting statistics"
"PitchingPost", "Post-season pitching statistics"
"TeamFranchises", "Franchise information"
"FieldingOF", "Outfield position data  "
"FieldingPost", "Post-season fielding data"
"FieldingOFsplit", "LF/CF/RF splits"
"ManagersHalf", "Split season data for managers"
"TeamsHalf", "Split season data for teams"
"Salaries", "Player salary data"
"SeriesPost", "Post-season series information"
"AwardsManagers", "Awards won by managers "
"AwardsPlayers", "Awards won by players"
"AwardsShareManagers", "Award voting for manager awards"
"AwardsSharePlayers", "Award voting for player awards"
"Appearances", "Details on the positions a player appeared at"
"Schools", "List of colleges that players attended"
"CollegePlaying", "List of players and the colleges they attended"
"Parks", "List of major league ballparls"
"HomeGames", "Number of homegames played by each team in each ballpark"
')

write.table(
  table_descriptions,
  file = "docs/data/table_descriptions.csv",
  quote = TRUE,
  row.names = FALSE,
  sep = ","
)
