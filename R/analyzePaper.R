#' Analyze the Water-Sensitive Paper
#'
#' Evaluate the spray quality by analyzing an RGB-image of the water-sensitive
#' paper.
#'
#' @param x Either an object of class \code{Image} from package [EBImage]
#' or a character indicating the path to the image file (.jpg, .jpeg, .png, .tif).
#'
#' @param paper_dim A numeric vector of length 2 indicating the length and
#' width (in mm) of the water-sensitive paper.
#'
#' @param distance A numeric value indicating the distance from the nozzle(s)
#' to the target, in order to estimate the spray drift (%) using the
#' 'German model' for field crops.
#'
#' @param display.it (Logical) Should the image with the bounding box of the
#' paper be displayed? Default is \code{TRUE}.
#'
#' @return Am object of class \code{hydropaper}, consisting of a \code{list}
#' of the following:
#' \describe{
#'   \item{ndrops}{ An integer indicating the number of droplets found.}
#'   \item{coverage}{ The percentage of wet area of the paper.}
#'   \item{density}{ The number of droplets per squared centimeter.}
#'   \item{vol}{ The volume of product deposited in the paper.
#'   If \code{paper_dim}is given in milimeters, then \code{vol} is given in
#'   microliters.}
#'   \item{vol_ha}{ The extrapolation of volume deposited for one hectare.
#'   If \code{paper_dim} is given in milimeters, then \code{vol_ha} is given
#'   in L/ha.}
#'   \item{md}{ The mean diameter of droplets. If \code{paper_dim} is given
#'   in milimeters, then \code{md} is given in micrometers.}
#'   \item{nmd}{ The numeric median diameter of droplets.
#'   If \code{paper_dim} is given in milimeters, then \code{nmd} is given in
#'   micrometers.}
#'   \item{vmd}{ The volumetric median diameter of droplets.
#'   If \code{paper_dim} is given in milimeters, then \code{vmd} is given in
#'   micrometers.}
#'   \item{d1}{ The 10th percentile of the volumetric median diameter of droplets.
#'   If \code{paper_dim} is given in milimeters, then \code{d1} is given in
#'   micrometers.}
#'   \item{d9}{ The 90th percentile of the volumetric median diameter of droplets.
#'   If \code{paper_dim} is given in milimeters, then \code{d9} is given in
#'   micrometers.}
#'   \item{RA}{ The relative amplitude: \code{(d9 - d1)/vmd}.}
#'   \item{maxdrop}{ The maximum diameter of droplets.
#'   If \code{paper_dim} is given in milimeters, then \code{maxdrop} is given in
#'   micrometers.}
#'   \item{mindrop}{ The minimum diameter of droplets.
#'   If \code{paper_dim} is given in milimeters, then \code{mindrop} is given in
#'   micrometers.}
#'   \item{size_class}{ The percentage of droplets in each of the following
#'   three classes of diameter: <200, 200-400, >400 micrometers.}
#'   \item{CVa}{ The coeficient of variation (%) of droplet areas.}
#'   \item{CVd}{ The coeficient of variation (%) of droplet diameters.}
#'   \item{r}{ A character giving a naive recomendation of which products
#'   are suitable to be sprayed, based on \code{density}.}
#'   \item{drift}{ A prediction of drift (%) based on the 'German model' for
#'   field crops (Rautmann et al., 2001), as function of \code{distance}.
#'   This quantifies a 90th percentile of the drift values.}
#'   \item{areas}{ A numeric vector containing area of each droplet found.
#'   If \code{paper_dim} is given in milimeters, then \code{areas} is given in
#'   squared milimeters.}
#'   \item{diams}{ A numeric vector containing the diameter of each droplet
#'   found. If \code{paper_dim} is given in milimeters, then \code{diams}
#'   is given in squared micrometers.}
#'   \item{binary}{ The binary mask of the segmentation, a numeric matrix.}
#' }
#'
#' @references
#' Rautmann, D., M. Streloke, R. Winkler (2001) New basic drift values in
#' the authorization procedure for plant protection products.
#' In: R. Forster & M. Streloke, Workshop on Risk Assessment and Risk
#' Mitigation measures in the context of the authorization of plant
#' protection products (WORMM) 27.-29. September 1999. Mitteilungen
#' aus der Biologischen Bundesanstalt für Land- und Forstwirtschaft,
#' Berlin-Dahlem, Heft 381. 2001. 133-141.
#'
#' @seealso [detectPaper()]
#'
#' @examples
#' path <- system.file('images', 'field1.jpg', package = 'hydropaper')
#' p1 <- detectPaper(path)
#'
#' a <- analyzePaper(p1)
#' print(a)
#' plot(a)
#'
#' @importFrom EBImage readImage display imageData rotate as.Image ocontour
#' watershed distmap bwlabel
#' @importFrom nnet nnet
#'
#' @aliases analyzePaper
#'
#' @exportS3Method print hydropaper
#' @exportS3Method plot hydropaper
#'
#' @export
analyzePaper <- function(x, paper_dim = c(76, 26),
  distance = 0.7, display.it = TRUE)
{
   if(inherits(x, "Image")) im <- x else im <- readImage(x)
   dat <- imageData(im)
   totalpix <- nrow(im)*ncol(im)
   areapapel <- prod(paper_dim) # mm2
   # segmentation
   all_px <- data.frame(r = as.integer(dat[,,1]*255),
                        g = as.integer(dat[,,2]*255),
                        b = as.integer(dat[,,3]*255))
   data(model_nnet, package = 'hydropaper')
   pred_nnet <- nnet:::predict.nnet(model_nnet, all_px, type = "class")
   cl <- matrix(pred_nnet, nrow = nrow(dat), ncol = ncol(dat))
   seg <- cl == "gotas"
   mind <- 50   # diam to be detected, in microm
   rs <- sqrt(areapapel*10^6)/mind
   s <- round(sqrt(totalpix)/rs)
   distm <- distmap(seg)
   bin <- watershed(distm, tolerance = 0.6, ext = s)
   # calculations
   bin <- bwlabel(bin)
   oo <- ocontour(bin)
   res <- paper_dim / dim(bin)  # mm/px
   A <- sapply(oo, function(x) {
     g <- sweep(x, 2L, res, "*")
     n <- nrow(g)
     cen <- apply(g, 2L, mean)
     g <- rbind(g, g[1,])
     sum(sapply(1:n, function(k) {
       x1y2 <- abs(g[k,1] - cen[1]) * abs(g[k+1,2] - cen[2])
       x2y1 <- abs(g[k+1,1] - cen[1]) * abs(g[k,2] - cen[2])
       abs(x1y2 - x2y1)
     })) * 0.5
   })
   asp <- sapply(oo, function(w) {
     d <- apply(apply(w, 2L, range), 2L, diff)
     max(d)/min(d)
   })
   non_null <- A > 0 & asp < 3
   areas <- A[non_null]  # mm2
   n <- length(areas)
   cob <- 100*sum(areas)/areapapel  # %
   diams <- 1000*2*sqrt(areas/pi)
   vols <- (diams/1000)^3 * pi/6
   vol <- sum(vols)   # microL
   vol_ha <- 10000*vol/areapapel
   wdmv <- which.min(cumsum(sort(vols)) <= vol*0.5)
   dmv <- sort(diams)[wdmv]
   wdv1 <- which.min(cumsum(sort(vols)) <= vol*0.1)
   d1 <- sort(diams)[wdv1]
   wdv9 <- which.min(cumsum(sort(vols)) <= vol*0.9)
   d9 <- sort(diams)[wdv9]
   AR <- (d9 - d1)/dmv
   dmn <- median(diams)
   dm <- mean(diams)
   maxdrop <- max(diams)
   mindrop <- min(diams)
   if(mindrop < min(res)*1000) mindrop <- min(res)*1000
   size_cut <- cut(diams, breaks = c(1, 105, 340, Inf),
                   right = FALSE)
   size_class <- 100*table(size_cut)/length(diams)
   dens <- round(n/(prod(paper_dim)/100)) # drops/cm2
   if(dens >= 50 & dens <= 70) {
      r <- "contact fungicides"
   } else if(dens >= 30 & dens <= 40) {
      r <- "contact herbicides"
   } else if (dens >= 20 & dens < 30) {
      r <- "insecticides/pre-emergent herbicides"
   } else {r = NULL}
   # drift (German model)
   drift <- 100 - 2.7705*distance^-0.9787
   # graphics
   if (display.it) {
      display(im, method = "raster")
      lapply(oo[non_null], lines, col = "red") -> null.obj
   }
   # output
   cv <- function(x) 100*sd(x)/mean(x)
   out <- list(ndrops = n, coverage = cob, density = dens,
               vol = vol, vol_ha = vol_ha,
               md = dm, nmd = dmn, vmd = dmv, d1 = d1, d9 = d9,
               RA = AR, maxdrop = maxdrop, mindrop = mindrop,
               size_class = size_class,
               CVa = cv(areas), CVd = cv(diams),
               r = r, drift = drift,
               areas = areas, diams = diams, binary = bin)
   class(out) <- "hydropaper"
   invisible(out)
}

# print method --------------------
print.hydropaper <- function(obj, ...)
{
   cat("\n              Water-Sensitive Paper Analysis\n",
      "\n                            N drops:", obj$ndrops,
      "\n                 Spray coverage (%):", round(obj$coverage, 1),
      "\n                Density (drops/cm2):", obj$density,
      "\n         Vol. applied (microliters):", round(obj$vol, 2),
      "\n                   L/ha (predicted):", round(obj$vol_ha, 2),
      "\n       Mean diameter (micronmeters):", round(obj$md),
      "\n                  NMD (micrometers):", round(obj$nmd),
      "\n                  VMD (micrometers):", round(obj$vmd),
      "\n                  D.1 (micrometers):", round(obj$d1),
      "\n                  D.9 (micrometers):", round(obj$d9),
      "\n                 Relative amplitude:", round(obj$RA, 2),
      "\n         Largest drop (micrometers):", round(obj$maxdrop),
      "\n        Smallest drop (micrometers):", round(obj$mindrop),
      "\nDiam. class % (<105, 105-340, >340):", round(obj$size_class, 1),
      "\n                        CV area (%):", round(obj$CVa, 1),
      "\n                       CV diam. (%):", round(obj$CVd, 1),
      "\n                      Good to spray:", obj$r,
      "\n                          Drift (%):", round(obj$drift, 1),
      "\n\n")
}

# plot method ----------------------
plot.hydropaper <- function(obj, ...)
{
   ml <- matrix(c(1,1,2,2, 1,1,2,2, 1,1,3,3), nrow = 4)
   layout(ml)
   par(mar = c(5, 4.5, 3, 1))
   h <- hist(obj$diams, breaks = 30, freq = FALSE,
      col = "yellow", main = "", ylab = "Rel. frequency",
      xlab = "Diameter (micrometers)", border = "gray", ...)
   abline(h = 0, col = "gray")
   box()
   with(obj, abline(v = c(d1, nmd, vmd, d9), lwd = 3,
      col = c("gray", "magenta", "green", "cyan")))
   with(obj, mtext(c("D.1", "NMD", "VMD", "D.9"),
      side = 3, at = c(d1, nmd, vmd, d9),
      padj = c(-1.5, 0, 0, -1.5),
      col = c("gray", "magenta", "green", "cyan"),
      font = 2, cex = 0.9))
   vols <- (h$mids/1000)^3 * pi/6 * h$counts
   # vol vs diam
   par(mar = c(4.5, 4.5, 3, 1))
   plot(h$mids, vols, type = "n",
      ylab = "Vol. (microliters)",
      xlab = "Diameter (micrometers)", ...)
   abline(h = 0, col = "gray")
   polygon(c(h$mids[1], h$mids, max(h$mids)),
      c(min(vols), vols, min(vols)),
      col = "yellow", border = "gray")
   with(obj, abline(v = c(d1, nmd, vmd, d9),
      col = c("gray", "magenta", "green", "cyan"), lwd = 3))
   with(obj, mtext(c("D.1", "NMD", "VMD", "D.9"),
      side = 3, at = c(d1, nmd, vmd, d9),
      padj = c(-1.5, 0, 0, -1.5),
      col = c("gray", "magenta", "green", "cyan"),
      font = 2, cex = 0.9))
   par(mar = c(1, 1, 1, 1))
   with(obj, plot(rotate(as.Image(binary), 90)))
}
