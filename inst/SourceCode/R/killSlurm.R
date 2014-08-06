# Kill a SLURM job from R

# Landon Sego 18 Nov 2011

killSlurm <- function(job.id) {

  # Convert the job.id to numeric if possible
  job.id <- as.numeric.silent(job.id)

  # Verify it's a valid job that can be an integer
  job.id.bad <- TRUE
  if (is.numeric(job.id)) {
    if ((job.id > 0) & (job.id %% 1 == 0))
      job.id.bad <- FALSE
  }
  if (job.id.bad)
    stop("'job.id' must be a numeric value or string that refers to a SLURM job number")

  # Verify we're on olympus or one of its nodes
  usr.host <- system("/bin/hostname -s", intern = TRUE)
  if (!(gsub("[0-9]", "", usr.host) %in% c("olympus", "olympus-e", "node", "gpu", "fat")))
    stop("'killSlurm' was written for use on olympus or its computing nodes only\n",
         "Your host name is '", usr.host, "'.")
  
  # Get a list of the current SLURM jobs
  jobs <- as.numeric(system("squeue | awk '{print $1}'", intern = TRUE)[-1])

  if (job.id %in% jobs) {
    cat("Killing SLURM job", job.id, "\n")
    system(paste("scancel", job.id))
  }
  else
    cat("SLURM job", job.id, "not in the queue\n")
  
} # killSlurm

