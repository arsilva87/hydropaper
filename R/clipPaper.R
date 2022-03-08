#' Clip Image to Extract the Water-Sensitive Paper
#'
#' Click on three points on the image to clip the water-sensitive paper,
#' for further analysis.
#'
#' @param x Either an object of class \code{Image} from package [EBImage]
#' or a character indicating the path to the image file (.jpg, .jpeg, .png, .tif).
#'
#' @return An object of class \code{Image}.
#'
#' @seealso [detectPaper()]
#'
#' @examples
#' path <- system.file('images', 'field1.jpg', package = 'hydropaper')
#' p1 <- clipPaper(path)
#' display(p1, "raster")
#'
#' @importFrom EBImage readImage display
#'
#' @aliases clipPaper
#'
#' @export
clipPaper <- function(x)
{
   if(inherits(x, "Image")) im <- x else im <- readImage(x)
   X11()
   display(im, method = "raster")
   getGraphicsEvent("Click on 3 points to clip the image...",
      onMouseDown = function(buttons, x, y) NA)
   loc <- locator(type = "o", col = "red", lwd = 2, n = 3L)
   xint <- min(loc$x):max(loc$x)
   yint <- min(loc$y):max(loc$y)
   polygon(x = c(min(xint), max(xint), max(xint), min(xint)),
      y = c(min(yint), min(yint), max(yint), max(yint)),
      col = adjustcolor("red", alpha = 0.5))
   Sys.sleep(3)
   dev.off()
   return(im[xint, yint, ])
}
