#' Analyze Multiple Images
#'
#' Detect the water-sensitive paper from multiple images and do all the
#' analyses in a run.
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
#' @return A list of objects of class \code{hydropaper}. See more in
#'  [analyzePaper()].
#'
#' @examples
#' path <- system.file('images', package = 'hydropaper')
#' analyzeImages(path)
#'
#' @importFrom pbapply pblapply
#'
#' @aliases analyzeImages
#'
#' @export
analyzeImages <- function(path,
                          paper_dim = c(76, 26), distance = 0.7,
                          display.it = FALSE)
{
  img_names <- list.files(path)
  files <- paste0(path, "/", img_names)
  l1 <- pblapply(files, function(x) {
     detectPaper(x, paper_dim, display.it)
  })
  out <- pblapply(l1, analyzePaper,
                  paper_dim, distance, display.it)
  names(out) <- img_names
  return(out)
}
