read_rookies <- function() read_parquet("data/rookies.parquet") %>% arrange(Position)

read_teams <- function() read_parquet("data/teams.parquet") %>% pull(team)

read_entries <- function() {
  arrow::open_dataset("storage") %>%
    collect() %>%
    group_by(entry_nickname,entry_email) %>%
    slice_max(entry_date) %>%
    ungroup() %>%
    mutate(entry_nickname = case_when(entry_nickname == "[no account]" ~ entry_name, TRUE ~ entry_nickname))
}

read_correct <- function(){
  read.csv("data/correct.csv",stringsAsFactors = FALSE) %>%
    rename(Top64 = top64,
           DraftedBy = team) %>%
    mutate(Top64 = as.logical(Top64) %>% replace_na(FALSE))
}

compare_entries <- function(){
  entries <- read_entries()
  correct <- read_correct()

  check <- entries %>%
    left_join(correct, by = c("Player","Position"),suffix = c("_entry","_correct")) %>%
    mutate(
      Top64 = as.numeric(Top64_entry == Top64_correct),
      DraftedBy = as.numeric(DraftedBy_entry == DraftedBy_correct),
      Score = Top64 + DraftedBy * 2
    ) %>%
    select(
      EntryNickname = entry_nickname,
      Player,
      Position,
      TeamGuess = DraftedBy_entry,
      TeamActual = DraftedBy_correct,
      Top64Score = Top64,
      DraftedByScore = DraftedBy,
      Score
    )

  return(check)
}

summarise_entries <- function(check){
  check %>%
    group_by(EntryNickname) %>%
    summarise(
      Top64Score = sum(Top64Score,na.rm = TRUE),
      DraftedByScore = sum(DraftedByScore, na.rm = TRUE),
      Score = sum(Score, na.rm = TRUE)
    ) %>%
    arrange(-Score,-DraftedByScore)
}

