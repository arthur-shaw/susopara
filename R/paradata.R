#' Calculate time between active paradata events
#' 
#' Determines active event durations by:
#' 
#' - Filtering out passive and non-interview events
#' - Computing difference between active events
#' 
#' Passive events include:
#' 
#' - Events of the following types in `event`: c("ApproveByHeadquarter", "ApproveBySupervisor", "ClosedBySupervisor", "KeyAssigned", "OpenedBySupervisor", "QuestionDeclaredInvalid", "QuestionDeclaredValid", "ReceivedByInterviewer", "ReceivedBySupervisor", "RejectedByHeadquarter", "RejectedBySupervisor", "SupervisorAssigned", "TranslationSwitched", "UnapproveByHeadquarters", "VariableDisabled", "VariableSet")
#' - Pauses in the interview--that is, time between `Pause` and `Resume` events
#' - End and resumption of the interview--that is between events and `Restarted` or between `Restarted` and an event
#' 
#' All other events are considered active
#' 
#' @param df Data frame that is Survey Solutions' paradata
#' 
#' @return Data frame. Adds a few columns to the input data set: event duration in elapsed seconds (`elapsed_sec`) and minutes (`elapsed_min`). Removes passive events.
#' 
#' @importFrom assertthat assert_that
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom lubridate ymd_hms make_difftime
#' @importFrom glue glue glue_collapse
#' 
#' @export 
calc_time_btw_active_events <- function(df) {

    # check inputs
    # `df` has expected columns
    columns_expected <- c("interview__id", "order", "event", "responsible", "role", "timestamp", "offset", "parameters")
    columns_found <- names(df)
    assertthat::assert_that(
        all(columns_found %in% columns_expected),
        msg = glue::glue(
            "The paradata is missing some expected columns\n",
            "Columns expected: ", glue::glue_collapse(columns_expected, sep = ", "), "\n", 
            "Columns found: ", glue::glue_collapse(columns_found, sep = ", ") 
        ) 
    )

    # determine how the actor's role is stored
    # so that filtering can be done correctly below
    how_role_stored <- dplyr::case_when(
        "1" %in% df$role ~ 1,
        "Interviewer" %in% df$role ~ 2
    )

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

    event_duration <- df %>% 
        # limit to actions by the interview or an unknown actor
        {if (how_role_stored == 1) dplyr::filter(., .data$role %in% c(0, 1)) else .} %>%
        {if (how_role_stored == 2) dplyr::filter(., .data$role == "Interviewer") else .} %>%
        # remove non-interview events
        dplyr::filter(!.data$event %in% excluded_events) %>% 
        # within each interview...
        dplyr::group_by(.data$interview__id) %>%
        # ... compute elapsed time as difference between two sequential active events
        dplyr::mutate(
            # parse timestamps as date-time
            time = lubridate::ymd_hms(as.character(.data$timestamp)), 
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

    return(event_duration)

}


#' Summarize answer changes
#'
#' Produces a data frame that summarizes both the frequency and details of answer changes.
#' An answer change may involve one, many, or a combination of the following interviewer actions: answer changed, answer removed.
#' The summary reduces the answer change events for a question into a formatted string of the following format: 
#' `(order) (+/-) value`, where `order` is the event order number in the paradata, 
#' `(+/-)` indicates an addition/change (`(+)`) or a deletion (`(-)`), 
#' and `value` shows answer value(s) due to that event. 
#' The summary will consist of several such event blocks separated by a semi-colon (i.e., `;`).
#' 
#' @param df Data frame that is Survey Solutions' paradata
#' 
#' @return Data frame.
#'
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom tidyr separate
#' @importFrom glue glue glue_collapse
#'
#' @export
summarize_answer_changes <- function(df) {

    # check inputs
    # find expected column names

    summary <- df %>%
        # limit answers being set and variables recorded
        dplyr::filter(
            .data$event == "AnswerSet" & 
            stringr::str_detect(.data$parameters, "^.+(?=\\|\\|)")
        ) %>%
        # split `parameters` into its constituent parts: variable, value, and address
        tidyr::separate(
            col = .data$parameters, 
            into = c("variable", "value", "address"), 
            sep = "\\|\\|"
        ) %>%
        # count events per variable-address
        dplyr::group_by(.data$interview__id, .data$variable, .data$address) %>%
        dplyr::summarise(
            n_events = dplyr::n_distinct(.data$value),
            values = glue::glue_collapse(
                glue::glue("({order}) (+) {value}"),
                sep = "; "
            )
        ) %>%
        dplyr::ungroup() %>%
        # compute changes as number of events minus one (i.e., initial answer recording)
        dplyr::mutate(n_changes = .data$n_events - 1) %>%
        # retain cases with more than 1 change
        dplyr::filter(.data$n_changes >= 1)

    return(summary)

}

#' Count the number of answer changes per interview
#' 
#' @param df Data frame that is Survey Solutions' paradata
#' 
#' @return Data frame.
#' 
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom tidyr separate
#' 
#' @export 
count_answer_changes <- function(df) {

    counts <- df %>%
        # limit answers being set and variables recorded
        dplyr::filter(
            .data$event == "AnswerSet" & .data$role == "Interviewer" &
            stringr::str_detect(.data$parameters, "^.+(?=\\|\\|)")
        ) %>%
        # split `parameters` into its constituent parts: variable, value, and address
        tidyr::separate(
            col = .data$parameters, 
            into = c("variable", "value", "address"), 
            sep = "\\|\\|"
        ) %>%
        # flag and exclude list and multi-select questions
        dplyr::mutate(
            is_list = stringr::str_detect(.data$value, "^.+\\|"),
            is_multiselect = stringr::str_detect(.data$value, "^[0-9]+\\.*0*, [0-9]+\\.*0*")
        ) %>%
        dplyr::filter(.data$is_list == FALSE & .data$is_multiselect == FALSE) %>%
        # count answer set events per interview-variable-address
        dplyr::group_by(.data$interview__id, .data$variable, .data$address) %>%
        dplyr::summarise(
            n_answers_changed = dplyr::n()
        ) %>%
        dplyr::ungroup() %>%
        # compute answers changed as n_events - 1 and question answered as 1 per group
        dplyr::mutate(
            n_answers_changed = .data$n_answers_changed - 1,
            n_questions_answered = 1
        ) %>%
        # sum these variables by interview
        dplyr::group_by(.data$interview__id) %>%
        dplyr::summarize(dplyr::across(
            .cols = dplyr::matches("^n_"),
            .fns = ~ sum(.x, na.rm = TRUE)
        )) %>%
        dplyr::ungroup()

    return(counts)

}
