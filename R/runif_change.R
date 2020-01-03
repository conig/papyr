get_objects = function(code){
  stringr::str_extract(as.character(code),"(\\S+)\\s*<-") %>%
    gsub(" <-","",.) %>%
    stats::na.omit() %>%
    as.character()
}

#' run_if_change
#'
#'Takes in R commands and the path to a location to store temporary files. The function will create the file if it doesn't already exist, store the objects there, and only re-run if there has been a change to a reference object.
#' @param code a code chunk wrapped in\ \{\}
#' @param object.names a character vector. The names to be saved and loaded
#' @param compare an object. The object which if changed, will cause the code to run.
#' @param path a string. The path to the tempory file to be used. Must end in '.rData'
#' @param run a bool. If True, the analyses will be re-run even if the comparitor is identical.
#' @details Runs analyses and then caches them. Analyses are returned in a list.
#' @export run_if_change

run_if_change = function(code = NULL,
                         #the code to be run
                         object.names = NULL,
                         #the object names to be saved/loaded
                         compare = NULL,
                         #the comparator object to test update required
                         path = NULL,
                         run = "auto") {
  comparison_command = substitute(compare)
  comparison_string = as.character(comparison_command)
  code_to_run = substitute(code)
  #return(code_to_run)
  #return(comparison_command)
  #declare flags
  run_anyway = F
  no_file = F
  cached = NULL

  if(is.null(object.names)){
    object.names = get_objects(code_to_run)
  }

  if(force){
    run_anyway = T
  }

  if (!file.exists(path)) {
    #if there is no save file
    previous = "no file exists_"
    no_file = T #record no file
    current = compare

    #run code - then create the save file
    if (!dir.exists(dirname(path))) {
      #make sure folder exists
      #if the folder doesnt exist
      dir.create(dirname(path), recursive = T) #make it
    }

  } else{
    #message("fileexists")
    #if there is the cached file.
    cached = readRDS(path) %>% as.environment() #load it up
    if (comparison_string %in% names(cached)) {
      current = eval(comparison_command, envir = globalenv()) #get current
      previous = eval(comparison_command, envir = cached) #and previous versions of the file
    } else{
      previous = "_previous is absent"
      current = "previous is absent_"
      message(previous)
    }

    if (identical(current, previous)) {
      #if they are identical
      inls = all(object.names %in% ls(envir = cached)) #make sure all the required objects are in there
      if (!inls) {
        #if they're not there

        run_anyway = T #of mpt rim amyway

      } else{
        message("No change detected.") #if the comparator is identical, and files are in there

      }
    }
  }

  if (!identical(current, previous) |
      run_anyway | no_file) {
    #if code needs to be re-run
    if (run_anyway) {
      if(force){
        message("Force is equal to TRUE.\nAnalysing...")
      }else{
      message("required objects are not in temporary file.\nAnalysing...") #tell the user
      }
    } else if (no_file) {
      message("Temp file not found.\nCreating and analysing...")
    } else{
      message("changes have been made to the dataset.\nAnalysing...") #tell everyone
    }
    #message("running analyses")

    duplicate = new.env()

    if(is.null(cached)){
      cached = new.env()
    }else{
    old_cached  = cached
    cached = new.env()
    for(n in ls(old_cached, all.names=TRUE)) assign(n, get(n, old_cached), cached) #copy contents of the old cache to the new_cache
    }


    for(n in ls(globalenv(), all.names=TRUE)) assign(n, get(n, globalenv()), duplicate) #copy contents of global environment to duplicate
    #return(code_to_run)
    eval(code_to_run, envir = duplicate)

    #run the code
    if (!all(object.names %in% ls(envir=duplicate))) {
      ls_names = ls()
      #return(ls_names)
      notall = object.names[!object.names %in% ls()]
      stop(paste0(
        "not all object names are in the enviornment:",
        paste(notall, collapse = ", ")
      ))
    } else{
      #assign values within cached
      assign(comparison_string, current, envir = cached)
      #move to duplicate
      secret = lapply(object.names, function(i) {
        assign(i, eval(as.name(i), envir = duplicate) , envir = cached)
      })

      #print(othernames)
      saveRDS(object = as.list(cached), #save all objects
              file = path)
    }
  }else{
    message("Previous analyses returned")
  }
  out_cached = as.list(cached)
  out = out_cached[names(out_cached) %in% object.names]

  return(out)
}


