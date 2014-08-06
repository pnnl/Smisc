# Write and compile the MPI code used to launch multiple jobs
# This function is used to support piclapply()

compileMPI <- function(wkdir, rscript.file, mpi.compiler = NULL, verbose = FALSE) {

  # NOTE: rscript.file is presumed to include it's full path

  # Automatically assign the compiler, if the hostname is appropriate
  if (is.null(mpi.compiler)) {

    # Verify we're on olympus or one of its nodes
    usr.host <- system("/bin/hostname -s", intern = TRUE)
    usr.host.nonumber <- gsub("[0-9]", "", usr.host)
    
    if (!(usr.host.nonumber %in% c("olympus", "olympus-e", "node", "gpu", "fat", "nwiceb")))
      stop("A value for 'mpi.compiler' must be supplied for host '", usr.host, "'")

    # nwiceb has been decomissioned---but I've left it in as an example of including another
    # machine...
    mpi.compiler <- switch(usr.host.nonumber,
                           "olympus"         = "/share/apps/mvapich2/1.7/gcc/4.6.0/bin/mpicc",
                           "olympus-e"       = "/share/apps/mvapich2/1.7/gcc/4.6.0/bin/mpicc",
                           "node"            = "/share/apps/mvapich2/1.7/gcc/4.6.0/bin/mpicc",
                           "gpu"             = "/share/apps/mvapich2/1.7/gcc/4.6.0/bin/mpicc",
                           "fat"             = "/share/apps/mvapich2/1.7/gcc/4.6.0/bin/mpicc", 
                           "nwiceb.pnl.gov"  = "/opt/openmpi/bin/mpicc")
  }

  
  # Remake the wkdir.logs variable  
  wkdir.logs <- paste(wkdir, "logs", sep = "/")
  
  # Create the string that will be used to make the R system call in the c code
  str1 <- paste(Sys.getenv("R_HOME"), "/bin/R CMD BATCH --no-restore --no-save '--args %d' ",
                rscript.file, " ", wkdir.logs, "/", stripPath(stripExtension(rscript.file)),
               "_%d.Rout", sep = "")
  str2 <- paste("    sprintf(str, \"", str1, "\", myid, myid);\n", sep = "")
  

  # Write wkdir/launch.c
  cat("#include \"mpi.h\"\n",
      "#include <stdio.h>\n",
      "#include <math.h>\n",
      "\n",
      "int main(int argc, char *argv[])\n",
      "{\n",
      "    int done = 0, n, myid, numprocs, i;\n",
      "    double startwtime = 0.0, endwtime;\n",
      "    int  namelen;\n",
      "    char processor_name[MPI_MAX_PROCESSOR_NAME];\n",
      "    char str[512];\n",
      "\n",
      "    MPI_Init(&argc,&argv);\n",
      "    MPI_Comm_size(MPI_COMM_WORLD,&numprocs);\n",
      "    MPI_Comm_rank(MPI_COMM_WORLD,&myid);\n",
      "    MPI_Get_processor_name(processor_name,&namelen);\n",
      "\n",
      "    fprintf(stdout, \"Process %d on %s\\n\", myid, processor_name);\n",
      "\n",
      "    // Issue the command\n",
      str2,
      "    system(str);\n",
      "    \n",
      "    MPI_Finalize();\n",
      "\n",
      "    return 0;\n",
      "}\n",
  
      sep = "",
      file = paste(wkdir, "launch.c", sep = "/"))

  # Now compile launch.c code
  compilation.failed <- system(paste(mpi.compiler, " ", wkdir, "/launch.c -o ",
                                     wkdir, "/launch.o", sep = ""))
    
  if (compilation.failed)
    stop("MPI code '", wkdir, "/launch.c' did not compile correctly\n")
    
  if (verbose)
    cat("\nMPI code 'launch.c' written and compiled\n")

} # compileMPI
