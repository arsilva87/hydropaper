#' Analyze Multiple Images
#'
#' Detect the water-sensitive paper from multiple images and do all the
#' analyses in a run. Parallel processing is allowed.
#'
#' @param path A character giving the path to the folder containing the
#' images (only!) of the papers.
#'
#' @param paper_dim A numeric vector of length 2 indicating the length and
#' width (in mm) of the water-sensitive paper.
#'
#' @param distance A numeric value indicating the distance from the nozzle
#' to the target, in order to estimate the spray drift (%) using the
#' 'German model' for field crops.
#'
#' @param display.it (Logical) Should the image with the bounding box of the
#' paper be displayed? Default is \code{TRUE}.
#'
#' @param cl An integer indicating the number of parallel processes or an
#' object created by [parallel::makeCluster()]. Default if \code{NULL}.
#'
#' @return A list of objects of class \code{hydropaper}. See more in
#'  [analyzePaper()].
#'
#' @examples
#' path <- system.file('images', package = 'hydropaper')
#' list.files(path)
#' analyzeImages(path)
#'
#' @importFrom pbapply pblapply
#' @importFrom parallel detectCores makeCluster clusterExport
#'
#' @aliases analyzeImages
#'
#' @export
analyzeImages <- function(path,
                          paper_dim = c(76, 26), distance = 0.7,
                          display.it = FALSE,
                          cl = NULL)
{
  img_names <- list.files(path)
  files <- paste0(path, "/", img_names)
  if(!is.null(cl)) {
    if(is.integer(cl)) {
      ncores <- parallel::detectCores()
      cl <- ifelse(cl > ncores, ncores, cl)
      cl <- parallel::makeCluster(cl)
    }
    parallel::clusterExport(cl, c("detectPaper",
                                  "analyzePaper"))
  }
  l1 <- pblapply(files, function(x) {
     detectPaper(x, paper_dim, display.it)
  }, cl = cl)
  out <- pblapply(l1, analyzePaper,
                  paper_dim, distance, display.it,
                  cl = cl)
  if(!is.null(cl)) parallel::stopCluster(cl)
  names(out) <- img_names
  return(out)
}
