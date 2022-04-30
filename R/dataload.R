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
  data.table::fread("data/correct.csv") %>%
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
      Miss64 = -as.numeric(Top64_entry != Top64_correct),
      DraftedBy = as.numeric(DraftedBy_entry == DraftedBy_correct),
      Score = Top64 + DraftedBy * 2 + Miss64
    ) %>%
    select(
      EntryNickname = entry_nickname,
      EntryTimestamp = entry_date,
      Player,
      Position,
      TeamGuess = DraftedBy_entry,
      TeamActual = DraftedBy_correct,
      Top64Score = Top64,
      Miss64Score = Miss64,
      DraftedByScore = DraftedBy,
      Score
    )

  return(check)
}

summarise_entries <- function(check){
  check %>%
    group_by(EntryNickname, EntryTimestamp) %>%
    summarise(
      Top64 = sum(Top64Score,na.rm = TRUE),
      Miss64 = sum(Miss64Score, na.rm = TRUE),
      TeamCorrect = sum(DraftedByScore, na.rm = TRUE),
      Score = sum(Score, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    arrange(-Score, -TeamCorrect, EntryTimestamp)
}

entry_display <- function(entries,scores){
  entry_ranks <- levels(scores %>% pull(EntryNickname) %>% as_factor())
  entries %>%
    mutate(EntryNickname = EntryNickname %>%
             as_factor() %>%
             forcats::fct_relevel(entry_ranks)) %>%
    arrange(EntryNickname)
}

