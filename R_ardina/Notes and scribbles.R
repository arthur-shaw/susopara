# Notes from Arthur:
#
# The function of interest for this project is calc_time_btw_active_events().
# This can be used to compute the time span between each active event.
# But note: this function does not read the paradata from file to memory. To do this, consider using readr::read_tsv(). This separation is on purpose. First, I want to figure out the fastest way to read these large data files. Second, I want us to see and handle any reading issues (e.g.,
# if there are any observations that arent parsed correctly) before computing any stats.

# Questions for Heather/Wilbert/Arthur:
# What is 0 unknown role in the role column?
# What is the split folders?
# What are the parameters again, seems different from what Arthur has sent?
              # In Arthur's there are 3, but for Tanzania seems to only have 2.

# There are still negative values of seconds, and there are crazy outliers using the Tanzania data.
# Most of the negative value is becuase of InterviewerAssigned event
