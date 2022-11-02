#' initiate_targets
#'
#' Creates a boilerplate targets directory
#' @param path path to targets directory

initiate_targets <- function(path){
  dir.create(paste0(path,"/R"))
  dir.create(paste0(path,"/data"))
  if(file.exists(paste0(path,"/_targets.R"))) stop("HAULT! _targets.R already exists.")
  write('library(targets)
library(future)
plan("multisession")

tar_option_set(packages = c("data.table"))

scripts <- list.files("R",
            full.names = TRUE,
            pattern = ".r$",
            ignore.case = TRUE,
            recursive = TRUE)
for(s in scripts) source(s)

list(
tar_target(target, functions())
)
',paste0(path,"/_targets.R"))
  write("",paste0(path,"/R/functions.R"))
}
