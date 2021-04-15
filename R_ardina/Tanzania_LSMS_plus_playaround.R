# LSMS+ Tanzania Paradata #
# ardinah@umich.edu
# Base code used from susopara James Arthur Shaw

setwd("C:/Users/ardin/OneDrive/LSMS+ World Bank Report/Paradata Research/")

df1 <-  readr::read_tsv("Para_April_3_21/NPS-SDD-Main-Questionnaire-Paradata_English/TZNPS5_3_Paradata_All_English 01-30-2019/paradata.tab")
df2 <-  readr::read_tsv("Para_April_3_21/NPS-SDD-Main-Questionnaire-Paradata_English/TZNPS5_5_Paradata_All_English 01-30-2019/paradata.tab")
df3 <-  readr::read_tsv("Para_April_3_21/NPS-SDD-Main-Questionnaire-Paradata_English/TZNPS5_6_Paradata_All_English 02-19-2019/paradata.tab")
df4 <-  readr::read_tsv("Para_April_3_21/NPS-SDD-Main-Questionnaire-Paradata_English/TZNPS5_8_Paradata_All_English 03-29-2019/paradata.tab")
df5 <-  readr::read_tsv("Para_April_3_21/NPS-SDD-Main-Questionnaire-Paradata_English/TZNPS5_9_Paradata_All_English 03-29-2019/paradata.tab")
df6 <-  readr::read_tsv("Para_April_3_21/NPS-SDD-Main-Questionnaire-Paradata_English/TZNPS5_10_Paradata_All_English 04-08-2019/paradata.tab")
df7 <-  readr::read_tsv("Para_April_3_21/NPS-SDD-Main-Questionnaire-Paradata_English/TZNPS5_11_Paradata_All_English 04-08-2019/paradata.tab")
df8 <-  readr::read_tsv("Para_April_3_21/NPS-SDD-Main-Questionnaire-Paradata_English/TZNPS5_12_Paradata_All_English 04-08-2019/paradata.tab")

df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8)
rm(df1, df2, df3, df4, df5, df6, df7, df8)

length(unique(df$interview__id))
# 993 household for LSMS+ Tanzania in this paradata (less than 1184 ceited in report)

unique(df$event) # All possible events
unique(df$role)
table(df$role)

# compile the types of events to exclude
excluded_events <- c(
  "ApproveByHeadquarter",
  "ApproveBySupervisor",
  "ClosedBySupervisor",
  "KeyAssigned",
  "OpenedBySupervisor",
  "QuestionDeclaredInvalid",
  "QuestionDeclaredValid",
  "ReceivedByInterviewer",
  "ReceivedBySupervisor",
  "RejectedByHeadquarter",
  "RejectedBySupervisor",
  "SupervisorAssigned",
  "TranslationSwitched",
  "UnapproveByHeadquarters",
  "VariableDisabled",
  "VariableSet"
)

# check inputs
# `df` has expected columns
columns_expected <- c("interview__id", "order", "event", "responsible", "role", "timestamp", "offset", "parameters")
columns_found <- names(df)
assertthat::assert_that(
  all(columns_expected %in% columns_found),
  msg = glue::glue(
    "The paradata is missing some expected columns\n",
    "Columns expected: ", glue::glue_collapse(columns_expected, sep = ", "), "\n",
    "Columns found: ", glue::glue_collapse(columns_found, sep = ", ")
  )
)

# determine how the actor's role is stored
# so that filtering can be done correctly below
how_role_stored <- dplyr::case_when(
  "1" %in% df$role[1] ~ 1,
  "Interviewer" %in% df$role[1] ~ 2
)

event_duration <- df %>%
  # limit to actions by the interview or an unknown actor
  {if (how_role_stored == 1) dplyr::filter(., .data$role %in% c(0, 1)) else .} %>%
  {if (how_role_stored == 2) dplyr::filter(., .data$role == "Interviewer" | role == "UNKNOWN ROLE") else .} %>%
  # remove non-interview events
  dplyr::filter(!.data$event %in% excluded_events) %>%
  # within each interview...
  dplyr::group_by(.data$interview__id) %>%
  # ... compute elapsed time as difference between two sequential active events
  dplyr::mutate(
    # parse timestamps as date-time
    # time = lubridate::ymd_hms(as.character(.data$timestamp)),
    time = .data$timestamp,
    # compute elapsed time between each event as difftime expressed in seconds
    elapsed_sec = lubridate::make_difftime(.data$time - dplyr::lag(.data$time), units = "seconds"),
    # remove (i.e., set to 0) durations that are not active interviewing
    # time between pause and resume
    elapsed_sec = dplyr::if_else(.data$event == "Resumed" & dplyr::lag(.data$event) == "Paused",
                                 true = lubridate::make_difftime(0, units = "seconds"),
                                 false = .data$elapsed_sec,
                                 missing = .data$elapsed_sec),
    # time between event and resumed (i, e., when "Paused" event missing)
    elapsed_sec = dplyr::if_else(.data$event == "Resumed",
                                 true = lubridate::make_difftime(0, units = "seconds"),
                                 false = .data$elapsed_sec,
                                 missing = .data$elapsed_sec),
    # time between pause and next event (i.e., when "Resumed" event missing)
    elapsed_sec = dplyr::if_else(dplyr::lag(.data$event) == "Paused",
                                 true = lubridate::make_difftime(0, units = "seconds"),
                                 false = .data$elapsed_sec,
                                 missing = .data$elapsed_sec),
    # time between an event and pause (i.e., assume time between last active event and pause is not active)
    elapsed_sec = dplyr::if_else(.data$event == "Paused",
                                 true = lubridate::make_difftime(0, units = "seconds"),
                                 false = .data$elapsed_sec,
                                 missing = .data$elapsed_sec),
    # remove time between event and Restarted or between restarting and event
    elapsed_sec = dplyr::if_else(.data$event == "Restarted" | dplyr::lag(.data$event) == "Restarted",
                                 true = lubridate::make_difftime(0, units = "seconds"),
                                 false = .data$elapsed_sec,
                                 missing = .data$elapsed_sec),
    # compute time in seconds and minutes as numbers
    # coerce difftime into number of seconds
    elapsed_sec = as.numeric(.data$elapsed_sec),
    # convert elapsed time from seconds to minutes
    elapsed_min = .data$elapsed_sec/60
  ) %>%
  dplyr::ungroup()

# Getting key statistics and checking results
mosaic::favstats(event_duration$elapsed_sec)
# The missing is important information here since it tells you how many HH there are.
length(which(event_duration$elapsed_sec<0))
quantile(event_duration$elapsed_sec, na.rm=TRUE)
quantile(event_duration$elapsed_sec, c(.75, .80, .90, .99), na.rm=TRUE)
mosaic::favstats(event_duration$elapsed_sec[event_duration$elapsed_sec>96])

mosaic::favstats(event_duration$elapsed_min)
length(which(event_duration$elapsed_min<0))
round(quantile(event_duration$elapsed_min, na.rm=TRUE),3)
quantile(event_duration$elapsed_min, c(.75, .80, .90, .99), na.rm=TRUE)
mosaic::favstats(event_duration$elapsed_min[event_duration$elapsed_min>1.6])
   # The mean here of 1300 minutes is 21 hours, might be a definition that shouldn't be counted.

round(prop.table(table(event_duration$event[event_duration$elapsed_min<0])),3)
round(prop.table(table(event_duration$event[event_duration$elapsed_min>1.6])),3)
round(prop.table(table(event_duration$event[event_duration$elapsed_min<1.6 | event_duration$elapsed_min>0])),3)

which(event_duration$elapsed_min>1300)


# Next Steps:
# Try to get module sections or questions.
# Sum up by module, by question.

# section = str_extract(variable, pattern = "(?<=s)[0-9A-Za-z]{2,3}(?=q)")

#### Just looking at the raw data.

df <-  {if (how_role_stored == 1) dplyr::filter(df, role %in% c(0, 1)) else .}
df <-  dplyr::filter(df, !df$event %in% excluded_events)

df <- df %>%  dplyr::group_by(.data$interview__id) %>%
  # ... compute elapsed time as difference between two sequential active events
  dplyr::mutate(
    # parse timestamps as date-time
    time = .data$timestamp,
    # compute elapsed time between each event as difftime expressed in seconds
    elapsed_sec = lubridate::make_difftime(.data$time - dplyr::lag(.data$time), units = "seconds"))

mosaic::favstats(df$elapsed_sec)
length(which(df$elapsed_sec<0)) # Even worse without cleaning

#### Just looking at df4 to understand each column
library(dbplyr)

df4 <- readr::read_tsv("Para_April_3_21/NPS-SDD-Main-Questionnaire-Paradata_English/TZNPS5_8_Paradata_All_English 03-29-2019/paradata.tab")
df4 <- filter(df4, !df4$event %in% excluded_events)
