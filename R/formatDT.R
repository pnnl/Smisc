##' Converts date or datetime strings into alternate formats
##'
##' Can be used to convert date-time character vectors into other types of
##' date-time formats.  It is designed to automatically find the appropriate
##' date and time informats without the user having to specify them.
##'
##' If the input vector contains times, \code{formatDT} assumes that the dates
##' and times are separated by at least one space.  The date format and the
##' time format of the input vector must be the same for all cells in the
##' vector. The input format is determined by the first non-missing entry of
##' the \code{dt} vector. Missing values (\code{NA} or \code{""}) are carried
##' through to the output vectors without error.
##'
##' In chosing the informat, \code{formatDT} first checks if the datetime
##' string has a format of "dd/mm/yyyy hh:mm:ss pm".  If so, it moves directly
##' to the datetime conversions.  Otherwise, it searches through the date and
##' time informats listed below for a suitable match.
##'
##' Acceptable date informats for \code{dt}: \code{mm/dd/yyyy},
##' \code{mm-dd-yyyy}, \code{yyyy-mm-dd}, \code{yyyymmdd}, \code{ddmonyyyy},
##' \code{dd-mon-yyyy}
##'
##' Acceptable time informats for \code{dt}: \code{hh:mm:sspm}, \code{hh:mm:ss
##' pm}, \code{hh:mm:ss} (24 hour time), \code{hh:mmpm}, \code{hh:mm pm},
##' \code{hh:mm} (24 hour time), \code{hhmm} (24 hour time), \code{hhmmss} (24
##' hour time)
##'
##' @export
##' @param dt A character vector of date values or datetime values
##'
##' @param date.outformat A character string requesting the date format to be
##' returned.  The following date outformats are supported: "mm/dd/yyyy",
##' "mm-dd-yyyy", "yyyy-mm-dd", "yyyymmdd", "ddmonyyyy", and "dd-mon-yyyy".  If
##' \code{date.outformat = NULL}, then "mm/dd/yyyy" is used.
##'
##' @param time.outformat A character string requesting the time format to be
##' returned.  The following time outformats are supported: "hh:mm:sspm",
##' "hh:mm:ss pm", "hh:mm:ss", "hh:mmpm", "hh:mm pm", and "hh:mm".  If
##' \code{time.outformat = NULL}, then "hh:mm:ss pm" is used.
##'
##' @param posix \code{= TRUE} returns date and datetime vectors of class
##' POSIXct that can be used for time calculations.
##'
##' @param weekday \code{= TRUE} returns a character vector denoting the day of
##' the week.
##'
##' @return A list with these components: \item{date}{A character vector of the
##' form requested by \code{date.outformat}.} \item{time}{A character vector of
##' the form requested by \code{time.outformat} or an empty character vector of
##' the form "" if the time is not present in the input vector \code{dt}.}
##' \item{dt}{A character vector containing the combined datetime using the
##' requested formats.  If time is not present in the input vector \code{dt},
##' then simply the date is returned.} \item{date.posix}{A vector of class
##' "POSIXt POSIXct" containing the date.  This is only returned if
##' \code{posix = TRUE}.} \item{dt.posix}{A vector of class "POSIXt POSIXct"
##' containing the datetime.  This is only returned if \code{posix = TRUE} and
##' time values are present in the argument \code{dt}.} \item{weekday}{A
##' character vector indicating the days of the week.  This is only returned if
##' \code{weekday = TRUE}.}
##'
##' @author Landon Sego
##'
##' @keywords misc
##'
##' @examples
##' # Demonstrates conversion of different datetime informats
##' formatDT("03/12/2004 04:31:17pm", posix = FALSE)
##' formatDT("12Mar2004 04:31pm", posix = FALSE)
##' formatDT("2004-3-12 16:31:17", posix = FALSE)
##' formatDT("7-5-1998 22:13")
##'
##' # Specifying different types of outformats
##' formatDT("03/12/2004", date.outformat = "dd-mon-yyyy", posix = FALSE)
##' formatDT("17-Sep-1782 12:31am", date.outformat = "yyyy-mm-dd",
##'          time.outformat = "hh:mm", posix = FALSE)
##'
##' # Processing datetime vectors
##' formatDT(c("03/12/2004 04:31pm","03/12/2005 04:32:18pm"), posix = FALSE)
##' formatDT(c("03/12/2004 04:31:17pm","03/12/2005 04:32:18pm"))
##' formatDT(c("03/12/2004 04:31:17pm","03/12/2005 04:32:18pm"), weekday = TRUE)
##'
##' # An incorrect date (will produce an error)
##' try(formatDT("29-Feb-2001"))
##'
##' # An incorrect time will also produce an error
##' try(formatDT("28-Feb-2001 00:00:00 AM"))
##' formatDT("28-Feb-2001 12:00:00 AM")
##'
##' # Illustrate the handling of missing values
##' formatDT(c(NA,"","2010-10-23 3:47PM"), weekday = TRUE)

formatDT <- function(dt,
                     date.outformat = NULL,
                     time.outformat = NULL,
                     posix = TRUE,
                     weekday = FALSE) {

  # Assumptions:
  # 'dt' is a character vector with a date or datetime.
  # If it is a datetime, the date and time are separated
  # by a space.  The date and time must follow one of the
  # informats specified below, or an Error is returned.

  # NOTE:  The algorithm assumes that the date format
  # and the time format of the input vector ARE THE SAME
  # FOR ALL THE CELLS in the vector.  The input format is
  # determined by using the first entry of the vector.
  # Missing values are set to NA.

  if (class(dt) != "character") stop("Date or datetime is not a character string.\n")

  # Identify the requested date outformats
  if (is.null(date.outformat)) date.outformat <- "%m/%d/%Y" # <-- the Excel format
  else {
     # User can intuitively request date outformats
          if (tolower(date.outformat) == "mm/dd/yyyy")  date.outformat <- "%m/%d/%Y"
     else if (tolower(date.outformat) == "yyyy-mm-dd")  date.outformat <- "%Y-%m-%d"
     else if (tolower(date.outformat) == "mm-dd-yyyy")  date.outformat <- "%m-%d-%Y"
     else if (tolower(date.outformat) == "ddmonyyyy")   date.outformat <- "%d%b%Y"
     else if (tolower(date.outformat) == "dd-mon-yyyy") date.outformat <- "%d-%b-%Y"
     else if (tolower(date.outformat) == "yyyymmdd")    date.outformat <- "%Y%m%d"
     else {
        cat("Warning in formatDT(): '",date.outformat,"' is not ",
            "a supported date outformat. 'mm/dd/yyyy' will be used ",
            "instead.\n",sep = "")
        date.outformat <- "%m/%d/%Y"
      }
  }

  # Identify the requested time outformats
  if (is.null(time.outformat)) time.outformat <- "%I:%M:%S %p"
  else {
      # User can intuitively request time outformats
          if (tolower(time.outformat) == "hh:mm:sspm")  time.outformat <- "%I:%M:%S%p"
     else if (tolower(time.outformat) == "hh:mm:ss pm") time.outformat <- "%I:%M:%S %p"
     else if (tolower(time.outformat) == "hh:mm:ss")    time.outformat <- "%H:%M:%S"
     else if (tolower(time.outformat) == "hh:mmpm")     time.outformat <- "%I:%M%p"
     else if (tolower(time.outformat) == "hh:mm pm")    time.outformat <- "%I:%M %p"
     else if (tolower(time.outformat) == "hh:mm")       time.outformat <- "%H:%M"
     else if (tolower(time.outformat) == "hhmm")        time.outformat <- "%H%M"
     else if (tolower(time.outformat) == "hhmmss")      time.outformat <- "%H%M%S"
     else {
        cat("Warning in formatDT(): '",time.outformat,"' is not ",
            "a supported time outformat. 'hh:mm pm' will be used ",
            "instead.\n",sep = "")
        time.outformat <- "%I:%M %p"
     }
  }


  # Find the first non missing entry in dt, fill in all missing values with that first one
  # Keep record of missing values
  missing.ind <- is.na(dt) | (nchar(dt) == 0)

  if (any.missing.ind <- any(missing.ind)) {

    if (all(missing.ind))
      stop("All values of 'dt' were '' or NA")

    # Backup of dt
    original.dt <- dt

    # Use the first non-missing value for the time stamp
    dt.one <- dt[!missing.ind]
    dt.one <- dt.one[1]

    # Fill in the missing values with the first non-missing one, will remove them later
    dt[missing.ind] <- dt.one

    # Vector of missing values that will be used to fill in output vectors
    original.dt.missing <- original.dt[missing.ind]

  }


  # Preproccessing to account for some obs having no seconds and others having seconds
  # Count the number of colons
  num.colons <- unlist(lapply(gregexpr(":", dt), function(x) length(x[x != -1])))
  u.num.colons <- unique(num.colons)

  if (!all(u.num.colons %in% 0:2))
    stop("There should only be 0, 1 or 2 colons in the date time\n")


  # If some elements of the time string have seconds and others do not, then add the 00's in for the seconds
  # There have to be only 2 colon counts:  1 and 2
  if (length(u.num.colons == 2)) {
    if (all(1:2 == sort(u.num.colons))) {

      # Change time string to upper case for easier processing
      dt <- toupper(dt)

      # A function to insert the 00 seconds
      sub.func <- function(x) {

        if (grepl(" AM", x))
          out <- gsub(" AM", ":00 AM", x)
        else if (grepl("AM", x))
          out <- gsub("AM", ":00AM", x)
        else if (grepl(" PM", x))
          out <- gsub(" PM", ":00 PM", x)
        else if (grepl("PM", x))
          out <- gsub("PM", ":00PM", x)
        else
          out <- paste(x, "00", sep = ":")

        return(out)

      } # sub.func

      # Now make the substitutions as needed in the time vector
      for (i in 1:length(dt)) {

        if (num.colons[i] == 1)
          dt[i] <- sub.func(dt[i])

      } # for

    } # if (all(1:2 == sort(u.num.colons))) {
  } # if (length(u.num.colons == 2)) {


  # We assume that time is present in dt
  time.present <- TRUE


  # Check if the string matches the standard "mm/dd/yyy hh:mm:ssam" format.
  # If so, then by-pass all the splitting...
  if ((!is.na(strptime(dt[1],"%m/%d/%Y %I:%M:%S %p"))) &
      (nchar(dt[1]) <= 22))
     dt.POSIXlt <- strptime(dt,"%m/%d/%Y %I:%M:%S %p")

  # Otherwise, split the string of the first element and identify the correct informat
  else {

     # Split the first element of 'dt' using " " as the delimiter
     dt.split <- strsplit(dt[1]," ")[[1]]

     dtl <- length(dt.split)
     time.string <- date.string <- character(1)

     if (dtl > 3)
        stop("Date or datetime '",dt[1],"' has more than 3 strings",
             " that are separated by spaces.\n")

     else if (dtl == 3) {
        if (!(tolower(dt.split[3]) %in% c("am","pm")))
           stop("Date or datetime '",dt[1],"' has 3 strings that ",
                "are separated by spaces\nand the 3rd string ",
                "is not AM or PM.\n")
        # Grab time (assuming time is separated from "AM" or "PM" by a space)
        else  time.string <- paste(dt.split[2],dt.split[3],sep = " ")
     }

     # Grab time (assuming time is not separated from "AM" or "PM" by a space)
     else if (dtl == 2) time.string <- dt.split[2]

     # Grab date
     date.string <- dt.split[1]

     date.informat <- NULL
     date.informats <- c("%m/%d/%Y",  # mm/dd/yyyy
                         "%m-%d-%Y",  # mm-dd-yyyy
                         "%Y-%m-%d",  # yyyy-mm-dd
                         "%Y%m%d",    # yyymmdd
                         "%d%b%Y",    # ddmonyyyy
                         "%d-%b-%Y")  # dd-mon-yyyy
     easy.date.informats <- c("mm/dd/yyyy",
                              "mm-dd-yyyy",
                              "yyyy-mm-dd",
                              "yyyymmdd",
                              "ddmonyyyy",
                              "dd-mon-yyyy")

     # Selects the first date format that works
     for (j in 1:length(date.informats)) {
         if ((!(is.na(strptime(date.string,date.informats[j])))) &
             (nchar(date.string) <= nchar(easy.date.informats[j]))) {
            date.informat <- date.informats[j]
#            if (date.informat != "%m/%d/%Y")
#               cat("Note in formatDT():  date.informat that will be used is '",
#                   easy.date.informats[j],
#                   "'.\n\tPlease verify that this is the correct date format.\n", sep = "")
            break
         }
     }

     if (is.null(date.informat))
        stop("The first non-missing date ", date.string,
             " is incorrect or has an invalid format.\n")

     # If there is a time then search for its format
     if (time.string != "") {


       time.informat <- NULL
       time.informats <- c("%I:%M:%S %p",  # hh:mm:sspm AND hh:mm:ss pm
                           "%H:%M:%S",     # hh:mm:ss (24 hour time)
                           "%I:%M %p",     # hh:mmpm AND hh:mm pm
                           "%H:%M")        # hh:mm (24 hour time)
       easy.time.informats <- c("hh:mm:ss pm",
                                "hh:mm:ss",
                                "hh:mm pm",
                                "hh:mm")

       # Uses the first time format that works on the first element
       for (j in 1:length(time.informats)) {
           if ((!(is.na(strptime(time.string, time.informats[j])))) &
               (nchar(time.string) <= nchar(easy.time.informats[j]))){
              time.informat <- time.informats[j]
#               if (time.informat != "%I:%M:%S %p")
#                  cat("Note in formatDT():  time.informat that will be used is '",
#                      easy.time.informats[which(time.informat == time.informats)],
#                      "'.\n\tPlease verify that this is the correct time format.\n", sep = "")
              break
           }
       }

       if (is.null(time.informat))
          stop("The first non-missing time ", time.string,
               " is incorrect or has an invalid format.\n")

       # Create the POSIXlt object
       dt.POSIXlt <- strptime(dt, paste(date.informat,time.informat))

     } # if (time.string != ""))

     # If time is not present (dates only)
     else {
       time.informat <- ""
       time.present <- FALSE
     }

     # Create the POSIXlt object
     dt.POSIXlt <- strptime(dt, paste(date.informat, time.informat))

   } # else if (is.na(strptime(dt[1],"%m/%d/%Y %I:%M:%S%p")))

   if (any(is.na(dt.POSIXlt)))
      cat("Warning in formatDT():", sum(is.na(dt.POSIXlt)),
          "of the formatted dates are incorrect or have invalid formats.\n")

   # Produce requested output
   out <- NULL

   # Create output for date, time, and dt

   out$date <- format(dt.POSIXlt, date.outformat)

   if (time.present){
     out$time <- format(dt.POSIXlt, time.outformat)
     out$dt <- paste(out$date, out$time)
     # If NA's are present due to bad formatting of oen or more obs, fill in with
     # NA's--otherwise, "NA NA" will appear
     if (any(missing.date.time <- is.na(out$date) | is.na(out$time)))
       out$dt[missing.date.time] <- NA
   }
   else {
     out$time <- character(length(out$date))
     out$dt <- out$date
   }

   # Insert the original missing values if there were any
   if (any.missing.ind) {
     out$date[missing.ind] <- original.dt.missing
     out$time[missing.ind] <- original.dt.missing
     out$dt[missing.ind] <- original.dt.missing
   }


   # POSIX output
   if (posix) {

     # Date times
     if (time.present) {
       out$dt.posix <- as.POSIXct(dt.POSIXlt)
       if (any.missing.ind)
         out$dt.posix[missing.ind] <- NA
     }

     # Dates
     out$date.posix <- as.POSIXct(trunc(dt.POSIXlt, "days"))
     if (any.missing.ind)
       out$date.posix[missing.ind] <- NA

   }

   # Weekday output
   if (weekday) {
     out$weekday <- weekdays(dt.POSIXlt)
     if (any.missing.ind)
       out$weekday[missing.ind] <- original.dt.missing
   }


   return(out)

} # end formatDT()
