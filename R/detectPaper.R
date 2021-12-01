#' Detect the Water-Sensitive Paper
#'
#' Detect the water-sensitive paper in RGB images using a pre-trained SURF
#' (Speed Up Robust Features) descriptor.
#'
#' @param x Either an object of class \code{Image} from package [EBImage]
#' or a character indicating the path to the image file (.jpg, .jpeg, .png, .tif).
#'
#' @param paper_dim A numeric vector of length 2 indicating the length and
#' width (in mm) of the water-sensitive paper.
#'
#' @param display.it (Logical) Should the image with the bounding box of the
#' paper be displayed? Default is \code{TRUE}.
#'
#' @return An object of class \code{Image}. An additional attribute containing
#' the bounding box for the paper is stored. See Examples.
#'
#' @seealso
#'
#' @examples
#' path <- system.file('images', 'field1.jpg', package = 'hydropaper')
#' p1 <- detectPaper(path)
#' attr(p1, 'paper_bbox')
#'
#'
#' @importFrom EBImage readImage display imageData
#' @importFrom image.dlib image_surf
#' @importFrom FNN get.knnx
#'
#' @aliases detectPaper detectCard
#'
#' @export
detectPaper <- function(x, paper_dim = c(76, 26), display.it = TRUE)
{
   if(inherits(x, "Image")) img <- x else img <- readImage(x)
   im. <- img*255
   storage.mode(im.) <- "integer"
   dat <- imageData(im.)
   a <- round(aperm(dat, perm = c(3, 1, 2)))
   data(trained_surf)
   surfs <- trained_surf
   paper_asp_ratio <- max(paper_dim)/min(paper_dim)
   sf <- image_surf(a, detection_threshold = 5)
   k <- get.knnx(surfs, sf$surf, k = 1)
   npts <- seq.int(10, 100)
   os <- lapply(npts, function(x) head(order(k$nn.dist), x))
   # objective func 1: aspect ratio
   asp <- sapply(os, function(w) {
      dx <- diff(range(sf$x[w], na.rm = TRUE))
      dy <- diff(range(sf$y[w], na.rm = TRUE))
      max(dx, dy)/min(dx, dy)
   })
   w <- which.min(abs(asp - paper_asp_ratio))
   opt_npts1 <- npts[w]
   o <- head(order(k$nn.dist), opt_npts1)
   xlims <- range(sf$x[o], na.rm = TRUE)
   ylims <- range(sf$y[o], na.rm = TRUE)
   if (display.it) {
      display(img, method = "raster")
      rect(xlims[1], ylims[1], xlims[2], ylims[2],
         border = "red", lwd = 2)
   }
   bb <- c(xmin = xlims[1], ymin = ylims[1],
            xmax = xlims[2], ymax = ylims[2])
   paper <- img[bb[1]:bb[3], bb[2]:bb[4], ]
   attr(paper, "paper_bbox") <- bb
   return(paper)
}
