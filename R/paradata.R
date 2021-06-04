#' Read pardata from disk
#' 
#' Reads paradata using a fast method
#' 
#' @param file Full file path to the paradata file.
#' 
#' @return Data table
#' 
#' @importFrom data.table fread
#' 
#' @export 
read_paradata <- function(file) {

    # check that file exists
    # if not, fail with error

    dt <- data.table::fread(
        input = file,
        encoding = "UTF-8"  # recognize that contents are already UTF-8 encoded
    )

    return(dt)

}

#' Parse paradata file
#' 
#' Transforms paradata into a more usable format by splitting the `parameters` into its constituent pieces: `variable`, `value`, and `row`
#' 
#' @param dt Data table or data frame
#' 
#' @return Data table of pradata with three new columns: `variable`, `value`, and `row`.
#' 
#' @import data.table
#' @importFrom lubridate ymd_hms
#' 
#' @export 
parse_paradata <- function(dt) {

    # avoid R CMD check warning by binding variable name to NULL
    parameters <- time <- timestamp <- NULL

    # check that is data.table
    # if not, convert to data.table and issue message to that effect
    if (!data.table::is.data.table(dt)) {
        dt <- data.table::as.data.table(dt)
    }

    # split "parameters" column into its constituent pieces: "variable", "value", "row"
    dt <- dt[,c("variable", "value", "row") := data.table::tstrsplit(parameters, split = "||", fixed = TRUE)]

    # convert timestamp into time
    # TODO: test: how much time this adds, whether this is ever useful
    dt <- dt[, time := lubridate::ymd_hms(as.character(timestamp))]

    return(dt)

}

#' Calculate time between active paradata events
#' 
#' Computes duration as the difference between the timestamps of active events. To do so, this subsets to active events, computes durations, and handles some edge cases.
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
#' @param dt Data table of the form produced by `parse_paradata`.
#' 
#' @return Data table of active events with two new columns: `elapsed_sec` and `elapsed_min`
#' 
#' @importFrom assertthat assert_that
#' @importFrom glue glue glue_collapse
#' @import data.table
#' 
#' @export 
calc_time_btw_active_events <- function(dt) {

    # avoid R CMD check warning by binding variable name to NULL
    role <- event <- elapsed_sec <- time <- interview__id <- elapsed_min <- NULL

    # check inputs
    # `df` has expected columns
    columns_expected <- c("interview__id", "order", "event", "responsible", "role", "timestamp", "offset", "parameters", "variable", "value", "row", "time")
    columns_found <- names(dt)
    assertthat::assert_that(
        all(columns_expected %in% columns_found),
        msg = glue::glue(
            "The paradata is missing some expected columns\n",
            "Columns expected: ", glue::glue_collapse(columns_expected, sep = ", "), "\n", 
            "Columns found: ", glue::glue_collapse(columns_found, sep = ", ") 
        ) 
    )

    # check that is data.table
    # if not, convert to data.table and issue message to that effect
    if (!is.data.table(dt)) {
        dt <- as.data.table(dt)
    }

    # determine how the actor's role is stored
    # so that filtering can be done correctly below
    how_role_stored <- data.table::fcase(
        "1" %in% dt$role, 1,
        "Interviewer" %in% dt$role, 2
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

    # limit to actions by the interview or an unknown actor
    if (how_role_stored == 1) {
        dt <- dt[role %in% c(0, 1),]
    } else if (how_role_stored == 2) {
        dt <- dt[role == "Interviewer",]
    }

    # remove non-interview events
    dt <- dt[!event %in% excluded_events,]
    
    # compute elapsed time between each event as difftime expressed in seconds
    dt <- dt[, elapsed_sec := time - data.table::shift(time, type = "lag"), by = interview__id]

    # write durations to 0 for time intervals that aren't of meaningful time spans
    dt <- dt[
        # between pause and resume
        (event == "Resumed" & data.table::shift(event, type = "lag") == "Paused"), elapsed_sec := 0, by = interview__id][
        # time between event and resumed (i, e., when "Paused" event missing)
        event == "Resumed", elapsed_sec := 0, by = interview__id][
        # time between pause and next event (i.e., when "Resumed" event missing)
        data.table::shift(event, type = "lag") == "Paused", elapsed_sec := 0, by = interview__id][
        # time between an event and pause (i.e., assume time between last active event and pause is not active)
        event == "Paused", elapsed_sec := 0, by = interview__id][
        # remove time between event and Restarted or between restarting and event
        (event == "Restarted" | data.table::shift(event, type = "lag") == "Restarted"), elapsed_sec := 0, by = interview__id]

    # convert elaped time to numeric and compute time in minutes
    dt <- dt[, 
        elapsed_sec := as.numeric(elapsed_sec)][,
        elapsed_min := elapsed_sec/60]

    return(dt)

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
#' @importFrom dplyr `%>%` filter group_by ungroup summarize n_distinct mutate
#' @importFrom rlang .data
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
#' @importFrom dplyr `%>%` filter mutate group_by ungroup summarise across
#' @importFrom rlang .data
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
