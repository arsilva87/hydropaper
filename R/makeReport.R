#' Report Maker
#'
#' Create a document to report the results of the analyses of one or
#' more water-sensitive paper.
#'
#' @param x An object of class \code{hydropaper}, created with the function
#' [analyzePaper()].
#'
#' @return A HTML will be generated and saved in the working directory.
#'
#' @examples
#' path <- system.file('images', 'field1.jpg', package = 'hydropaper')
#' p1 <- detectPaper(path)
#' a <- analyzePaper(p1)
#' makeReport(a)  # check your working directory
#'
#' @importFrom rmarkdown render
#'
#' @aliases makeReport
#'
#' @export
makeReport <- function(x)
{
  path <- system.file('exdata', 'report.Rmd', package = 'hydropaper')
  render(path,
         params = list(x = x),
         output_file = "hydropaper_report.html",
         output_dir = getwd(),
         encoding = "UTF-8")
}
