
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            ~~
##              BUILDING SANKEY DIAGRAMS FOR UCP LEADERSHIP RACE            ----
##                                                                            ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------ LOAD LIBRARIES---------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(magrittr)
library(readxl)

# ggsankey Needs to be installed from GitHub using devtools
# https://github.com/davidsjoberg/ggsankey
library(ggsankey)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------- READ AND PARSE DATA-------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

read_scenarios <- function(
    xlsx_path = "UCP Leadership Scenarios.xlsx",
    sheet_to_read = "Base Model",
    rows_to_skip = 0,
    n_candidates = 7
){
  scenario_data <- read_excel(
    path = xlsx_path,
    sheet = sheet_to_read,
    skip = rows_to_skip,
    n_max = n_candidates + rows_to_skip + 1
  ) %>% 
    rename(
      "Candidate" = "...1"
    )
  
  colnames(scenario_data)[2:dim(scenario_data)[2]] <- paste(
    "Round",
    1:(dim(scenario_data)[2]-1),
    sep = "_"
  )
  
  candidates_in_order <- scenario_data %>% 
    pull(Candidate) %>% 
    rev()
  
  scenario_data %<>%
    pivot_longer(
      cols = starts_with("Round_"),
      names_to = "Round",
      names_prefix = "Round_",
      values_to = "Support"
    ) %>% 
    filter(
      !is.na(Support)
    ) %>% 
    mutate(
      Round = as.numeric(Round),
      Candidate = factor(Candidate, levels = candidates_in_order, ordered = TRUE)
    )
  
  return(scenario_data)

}

read_distributions <- function(
    xlsx_path = "UCP Leadership Scenarios.xlsx",
    sheet_to_read = "Base Model",
    rows_to_skip = 0,
    n_candidates = 7
){
  transition_data <- read_excel(
    path = xlsx_path,
    sheet = sheet_to_read,
    col_names = FALSE,
    skip = n_candidates + rows_to_skip + 2,
    n_max = 2*n_candidates + rows_to_skip + 3,
    na = c("", "n/a")
  ) %>% 
    select(-`...1`)
  
  colnames(transition_data) <- c(
    "Candidate",
    paste(
    "PortionReceivedForRound",
    2:dim(transition_data)[2],
    sep = "_"
    )
  )
  
  candidates_in_order <- transition_data %>% 
    pull(Candidate) %>% 
    rev()
  
  transition_data %<>%
    pivot_longer(
      cols = starts_with("PortionReceivedForRound"),
      names_to = "ToRound",
      names_prefix = "PortionReceivedForRound_",
      values_to = "PortionTransferred"
    ) %>% 
    mutate(
      ToRound = as.numeric(ToRound),
      FromRound = ToRound - 1
    )
  
  # return(transition_data) 
  
  round_eliminated <- transition_data %>% 
    filter(is.na(PortionTransferred)) %>% 
    group_by(Candidate) %>% 
    summarise(
      RoundEliminated = min(FromRound),
      .groups = "drop"
    ) %>% 
    rename(
      "FromCandidate" = "Candidate"
    )
  
  #return(round_eliminated)
  
  transition_data %<>%
    filter(!is.na(PortionTransferred)) %>% 
    rename(
      "ToCandidate" = "Candidate"
    ) %>% 
    left_join(
      round_eliminated,
      by = c("FromRound" = "RoundEliminated")
    ) %>% 
    select(
      FromRound,
      ToRound,
      FromCandidate,
      ToCandidate,
      PortionTransferred
    ) %>% 
    mutate(
      FromCandidate = factor(FromCandidate, levels = candidates_in_order, ordered = TRUE),
      ToCandidate = factor(ToCandidate, levels = candidates_in_order, ordered = TRUE)
    )
  
  return(transition_data)
}

# test_support <- read_scenarios()
# test_transfer <- read_distributions()

#...............................................................................
#                                                                              .
#  Using the conventions from ggsankey::geom_sankey, FromRound = x, ToRound =  .
#  next_x, FromCanddiate = node, ToCandidate = next_node, Value = Support      .
#                                                                              .
#...............................................................................

create_sankey_data <- function(
    xlsx_path = "UCP Leadership Scenarios.xlsx",
    sheet_to_read = "Base Model",
    rows_to_skip = 0,
    n_candidates = 7
){
  support_by_round <- read_scenarios(
    xlsx_path, sheet_to_read, rows_to_skip, n_candidates
  )
  transitions <- read_distributions(
    xlsx_path, sheet_to_read, rows_to_skip, n_candidates
  )
  
  candidate_to_self <- support_by_round %>% 
    rename(
      "ToCandidate" = "Candidate",
      "FromRound" = "Round"
    ) %>% 
    mutate(
      FromCandidate = ToCandidate,
      ToRound = FromRound + 1
    )
  
  num_rounds <- candidate_to_self %>% 
    pull(ToRound) %>% 
    max()
  
  # Filter out the support to self for candidates that are eliminated
  
  candidate_to_self %<>%
    group_by(FromCandidate) %>% 
    filter(
      (ToRound < max(ToRound) | ToRound == num_rounds)
    ) %>% 
    ungroup()
  
  candidate_to_others <- transitions %>% 
    left_join(
      support_by_round,
      by = c(
        "FromCandidate" = "Candidate",
        "FromRound" = "Round"
        )
    ) %>% 
    mutate(
      Support = Support * PortionTransferred
    ) %>% 
    filter(Support > 0) %>% 
    select(
      -PortionTransferred
    )
  
  final_rows <- candidate_to_self %>% 
    filter(
      ToRound == num_rounds
    ) %>% 
    mutate(
      FromRound = num_rounds,
      ToRound = NA_integer_,
      ToCandidate = NA_character_
    )
  
  sankey_df <- bind_rows(
    candidate_to_self,
    candidate_to_others,
    final_rows
  ) %>% 
    select(
      FromRound,
      ToRound,
      FromCandidate,
      ToCandidate,
      Support
    ) %>% 
    mutate(
      ToCandidate = factor(ToCandidate, levels = levels(FromCandidate), ordered = TRUE),
    )
  
  sankey_labels_df <- sankey_df %>% 
    group_by(
      FromCandidate, FromRound
    ) %>% 
    summarise(
      SupportLabel = sum(Support),
      .groups = "drop"
    ) 
  # %>% 
  #   mutate(
  #     ToCandidate = FromCandidate
  #   )
  
  sankey_df %<>%
    left_join(
      sankey_labels_df
    ) %>% 
    mutate(
      Label = paste(round(SupportLabel), "%", sep = ""),
      Label = case_when(
        (FromRound == max(FromRound)) & (Support == max(Support)) ~ paste("Winner", "\n", round(Support), "%", sep = ""),
        FromRound == max(FromRound) ~ paste(round(Support), "%", sep = ""),
        (FromRound == 1) ~ paste(as.character(FromCandidate), "\n", round(SupportLabel), "%", sep = ""),
        # FromRound == 1 ~ as.character(FromCandidate),
        is.na(SupportLabel) ~ NA_character_,
        FromRound < num_rounds ~ Label,
      )
    )
  
  return(sankey_df)
}

# test_sankey_data <- create_sankey_data()


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------ VISUALIZE DATA---------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

visualize_sankey_data <- function(
    sankey_data,
    main_title
){
  out_plot <- ggplot(
    data = sankey_data,
    mapping = aes(
      x = FromRound,
      next_x = ToRound,
      node = FromCandidate,
      next_node = ToCandidate,
      value = Support,
      fill = FromCandidate,
      label = Label
    )
  ) + 
    geom_sankey(
      flow.alpha = 0.6,
      node.colour = "gray30"
    ) +
    geom_sankey_label(
      size = 3,
      colour = "white",
      fill = "grey40"
    ) +
    scale_fill_viridis_d() +
    guides(fill = "none") + 
    labs(
      x = NULL,
      title = main_title
    ) +
    theme_sankey(base_size = 18) + 
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(hjust = .5)
    )
  
  
  return(out_plot)
}

# test_plot <- visualize_sankey_data(test_sankey_data, main_title = "Test")