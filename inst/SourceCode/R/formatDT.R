formatDT <- function(dt,
                     date.outformat=NULL,
                     time.outformat=NULL,
                     posix=TRUE,
                     weekday=FALSE) {
  
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

  if (class(dt)!="character") stop("Date or datetime is not a character string.\n")

  # Identify the requested date outformats
  if (is.null(date.outformat)) date.outformat <- "%m/%d/%Y" # <-- the Excel format
  else {
     # User can intuitively request date outformats
          if (tolower(date.outformat)=="mm/dd/yyyy")  date.outformat <- "%m/%d/%Y"
     else if (tolower(date.outformat)=="yyyy-mm-dd")  date.outformat <- "%Y-%m-%d"
     else if (tolower(date.outformat)=="mm-dd-yyyy")  date.outformat <- "%m-%d-%Y"
     else if (tolower(date.outformat)=="ddmonyyyy")   date.outformat <- "%d%b%Y"   
     else if (tolower(date.outformat)=="dd-mon-yyyy") date.outformat <- "%d-%b-%Y"
     else if (tolower(date.outformat)=="yyyymmdd")    date.outformat <- "%Y%m%d"
     else {
        cat("Warning in formatDT(): '",date.outformat,"' is not ",
            "a supported date outformat. 'mm/dd/yyyy' will be used ",
            "instead.\n",sep="")
        date.outformat <- "%m/%d/%Y"
      }
  }

  # Identify the requested time outformats
  if (is.null(time.outformat)) time.outformat <- "%I:%M:%S %p" 
  else {
      # User can intuitively request time outformats
          if (tolower(time.outformat)=="hh:mm:sspm")  time.outformat <- "%I:%M:%S%p"  
     else if (tolower(time.outformat)=="hh:mm:ss pm") time.outformat <- "%I:%M:%S %p"  
     else if (tolower(time.outformat)=="hh:mm:ss")    time.outformat <- "%H:%M:%S"   
     else if (tolower(time.outformat)=="hh:mmpm")     time.outformat <- "%I:%M%p"
     else if (tolower(time.outformat)=="hh:mm pm")    time.outformat <- "%I:%M %p"
     else if (tolower(time.outformat)=="hh:mm")       time.outformat <- "%H:%M"
     else if (tolower(time.outformat)=="hhmm")        time.outformat <- "%H%M"
     else if (tolower(time.outformat)=="hhmmss")      time.outformat <- "%H%M%S"        
     else {
        cat("Warning in formatDT(): '",time.outformat,"' is not ",
            "a supported time outformat. 'hh:mm pm' will be used ",
            "instead.\n",sep="")
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
          out <- paste(x, "00", sep=":")
  
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

     else if (dtl==3) {
        if (!(tolower(dt.split[3]) %in% c("am","pm")))
           stop("Date or datetime '",dt[1],"' has 3 strings that ",
                "are separated by spaces\nand the 3rd string ",
                "is not AM or PM.\n")
        # Grab time (assuming time is separated from "AM" or "PM" by a space)
        else  time.string <- paste(dt.split[2],dt.split[3],sep=" ")
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
#                   "'.\n\tPlease verify that this is the correct date format.\n", sep="")
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
#                      easy.time.informats[which(time.informat==time.informats)],
#                      "'.\n\tPlease verify that this is the correct time format.\n", sep="")
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
