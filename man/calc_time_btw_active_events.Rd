% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paradata.R
\name{calc_time_btw_active_events}
\alias{calc_time_btw_active_events}
\title{Calculate time between active paradata events}
\usage{
calc_time_btw_active_events(dt)
}
\arguments{
\item{dt}{Data table of the form produced by \code{parse_paradata}.}
}
\value{
Data table of active events with two new columns: \code{elapsed_sec} and \code{elapsed_min}
}
\description{
Computes duration as the difference between the timestamps of active events. To do so, this subsets to active events, computes durations, and handles some edge cases.
}
\details{
Determines active event durations by:
\itemize{
\item Filtering out passive and non-interview events
\item Computing difference between active events
}

Passive events include:
\itemize{
\item Events of the following types in \code{event}: c("ApproveByHeadquarter", "ApproveBySupervisor", "ClosedBySupervisor", "KeyAssigned", "OpenedBySupervisor", "QuestionDeclaredInvalid", "QuestionDeclaredValid", "ReceivedByInterviewer", "ReceivedBySupervisor", "RejectedByHeadquarter", "RejectedBySupervisor", "SupervisorAssigned", "TranslationSwitched", "UnapproveByHeadquarters", "VariableDisabled", "VariableSet")
\item Pauses in the interview--that is, time between \code{Pause} and \code{Resume} events
\item End and resumption of the interview--that is between events and \code{Restarted} or between \code{Restarted} and an event
}

All other events are considered active
}
