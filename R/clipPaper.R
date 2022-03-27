#' Clip Image to Extract the Water-Sensitive Paper
#'
#' Click on four points on the image or use the package
#' trained neural net to automatically clip the water-sensitive paper,
#' for further analysis.
#'
#' @param x Either an object of class \code{Image} from package [EBImage]
#' or a character indicating the path to the image file (.jpg, .jpeg, .png, .tif).
#' @param method One of the two characters: \code{"auto"} or \code{"manual"}
#' to be used to clip the paper from the image.
#' @param align.x \code{Logical}. If \code{FALSE} (default), the paper will
#' be rotated to align with the x-axis.
#' @param display.it \code{Logical}. If \code{TRUE} (default), the clipped
#' paper will be displayed.
#'
#' @return An object of class \code{Image}.
#'
#' @seealso [analyzePaper()]
#'
#' @examples
#' path <- system.file('images', 'field1.jpg', package = 'hydropaper')
#' p1 <- clipPaper(path, method = "auto", display.it = FALSE)
#' display(p1, "raster")
#'
#' @importFrom EBImage readImage display imageData rotate ocontour
#' bwlabel
#' @importFrom nnet nnet
#'
#' @aliases clipPaper
#'
#' @export
clipPaper <- function(x, method = c("auto", "manual"),
                      align.x = FALSE, display.it = TRUE)
{
   if(inherits(x, "Image")) img <- x else img <- readImage(x)
   method = match.arg(method)
   if(method == "manual") {
      X11()
      display(img, method = "raster")
      getGraphicsEvent("Click on 4 points to clip the paper...",
                       onMouseDown = function(buttons, x, y) NA)
      loc <- locator(type = "o", col = "red", lwd = 2, n = 4L)
      corners1 <- as.data.frame(loc)
      cen. <- colMeans(corners1)
      ww <- apply(corners1, 1, function(z) {
        p2 <- z[1] > cen.[1] & z[2] < cen.[2]
        p1 <- z[1] > cen.[1] & z[2] > cen.[2]
        p4 <- z[1] < cen.[1] & z[2] > cen.[2]
        p3 <- z[1] < cen.[1] & z[2] < cen.[2]
        cbind(p1, p2, p3, p4)
      })
      op <- apply(ww, 2, which.max)
      corners <- corners1[op,]
      Sys.sleep(1)
      dev.off()
   } else {
     dat <- imageData(img)
     all_px <- data.frame(r = as.integer(dat[, , 1] * 255),
                          g = as.integer(dat[, , 2] * 255),
                          b = as.integer(dat[, , 3] * 255))
     data(model_nnet, package = 'hydropaper')
     pred_nnet <- nnet:::predict.nnet(model_nnet, all_px,
                                      type = "class")
     cl <- matrix(pred_nnet, nrow = nrow(dat), ncol = ncol(dat))
     seg_f <- cl != "fundo"
     bw_f <- bwlabel(seg_f)
     w_f <- as.integer(names(sort(table(bw_f), decreasing=TRUE)))[2L]
     pap <- bw_f == w_f
     co <- ocontour(pap)[[1]]
     cen <- apply(co, 2L, mean)
     wQ <- apply(co, 1L, function(z) {
       if(z[1] > cen[1] & z[2] > cen[2]) {
         wq <- "Q1"
       } else if(z[1] > cen[1] & z[2] < cen[2]) {
         wq <- "Q2"
       } else if(z[1] < cen[1] & z[2] < cen[2]) {
         wq <- "Q3"
       } else { wq <- "Q4" }
       wq
     })
     corners. <- by(co, wQ, function(z) {
       dis_cen <- apply(sweep(z, 2L, cen)^2, 1, sum)
       z[which.max(dis_cen),]
     })
     corners <- do.call("rbind", corners.)
   }
   comb <- list(1:2, 2:3, 3:4, c(4,1))
   sapply(comb, function(z) {
       X <- cbind(1, corners[z, 1])
       Y <- corners[z, 2]
       ab <- solve(t(X) %*% X, t(X) %*% Y)
       ab
   }) -> ab
   gr <- expand.grid(1:nrow(img), 1:ncol(img))
   inside <- inout(as.matrix(gr), ab)
   ss <- matrix(inside, nrow = nrow(img), ncol = ncol(img))
   im <- img
   im[ss == 0] <- 0
   bb <- apply(corners, 2L, range)
   paper <- im[bb[1,1]:bb[2,1], bb[1,2]:bb[2,2], ]
   if(align.x) {
     ang <- 180*atan(ab[2, 4])/pi
     paper_rot <- rotate(paper, -ang)
     paper <- paper_rot
   }
   if (display.it) {
     display(paper, method = "raster")
   }
   return(paper)
}
