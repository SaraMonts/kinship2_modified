
ped.legend <- function(x, ped_id, adopted = NULL, labels = NULL, cex = 1, col = 1, symbolsize = 1, 
                   width = 8, density = -1, angle = 90)
{
  ped <- as.data.frame.pedigree(x[as.character(ped_id)])
  
  if (!is.null(ped$affected1)) aff1 <- ped$affected1
  else aff1 <- rep(0, nrow(ped))
  if (!is.null(ped$affected2)) aff2 <- ped$affected2
  else aff2 <- rep(0, nrow(ped))
  if (!is.null(ped$affected3)) aff3 <- ped$affected3
  else aff3 <- rep(0, nrow(ped))
  if (!is.null(ped$affected4)) aff4 <- ped$affected4
  else aff4 <- rep(0, nrow(ped))
  
  df <- data.frame(sex = ped$sex, status = ped$status, adopted = adopted, affected1 = aff1, 
                   affected2 = aff2, affected3 = aff3, affected4 = aff4)
  
  df <- unique(df)
  
  
  sexes <- c()
  if ("female" %in% df$sex) sexes <- c(sexes, 2)
  else if ("male" %in% df$sex) sexes <- c(sexes, 1)
  else if ("unknown" %in% df$sex) sexes <- c(sexes, 3)
  
  deceased <- c()
  if (any(df$sex == "female" & df$status == 1)) deceased <- c(deceased, 2)
  else if (any(df$sex == "male" & df$status == 1)) deceased <- c(deceased, 1)
  else if (any(df$sex == "unknown" & df$status == 1)) deceased <- c(deceased, 3)
  
  pregnancies <- c()
  if (any(df$sex == "female" & df$status == 2)) pregnancies <- c(pregnancies, 2)
  else if (any(df$sex == "male" & df$status == 2)) pregnancies <- c(pregnancies, 1)
  else if (any(df$sex == "unknown" & df$status == 2)) pregnancies <- c(pregnancies, 3)
  
  abortions <- c()
  if (any(df$sex == "terminated" & df$status == 0)) abortions <- c(abortions, 0)
  else if (any(df$sex == "terminated" & df$status == 1)) abortions <- c(abortions, 1)
  
  adopteds <- c()
  if (any(df$sex == "female" & !is.na(df$adopted))) adopteds <- c(adopteds, 2)
  else if (any(df$sex == "male" & !is.na(df$adopted))) adopteds <- c(adopteds, 1)
  else if (any(df$sex == "unknown" & !is.na(df$adopted))) adopteds <- c(adopteds, 3)
  
  phen1 <- c()
  if (any(df$sex == "female" & df$affected1 == 1)) phen1 <- c(phen1, 2)
  else if (any(df$sex == "male" & df$affected1 == 1)) phen1 <- c(phen1, 1)
  else if (any(df$sex == "unknown" & df$affected1 == 1)) phen1 <- c(phen1, 3)
  else if (any(df$sex == "terminated" & df$affected1 == 1)) phen1 <- c(phen1, 4)
  
  carriers1 <- c()
  if (any(df$sex == "female" & df$affected1 == 2)) carriers1 <- c(carriers1, 2)
  else if (any(df$sex == "male" & df$affected1 == 2)) carriers1 <- c(carriers1, 1)
  else if (any(df$sex == "unknown" & df$affected1 == 2)) carriers1 <- c(carriers1, 3)
  else if (any(df$sex == "terminated" & df$affected1 == 2)) carriers1 <- c(carriers1, 4)
  
  presymp1 <- c()
  if (any(df$sex == "female" & df$affected1 == 3)) presymp1 <- c(presymp1, 2)
  else if (any(df$sex == "male" & df$affected1 == 3)) presymp1 <- c(presymp1, 1)
  else if (any(df$sex == "unknown" & df$affected1 == 3)) presymp1 <- c(presymp1, 3)
  else if (any(df$sex == "terminated" & df$affected1 == 3)) presymp1 <- c(presymp1, 4)
  
  phen2 <- c()
  if (any(df$sex == "female" & df$affected2 == 1)) phen2 <- c(phen2, 2)
  else if (any(df$sex == "male" & df$affected2 == 1)) phen2 <- c(phen2, 1)
  else if (any(df$sex == "unknown" & df$affected2 == 1)) phen2 <- c(phen2, 3)
  else if (any(df$sex == "terminated" & df$affected2 == 1)) phen2 <- c(phen2, 4)
  
  carriers2 <- c()
  if (any(df$sex == "female" & df$affected2 == 2)) carriers2 <- c(carriers2, 2)
  else if (any(df$sex == "male" & df$affected2 == 2)) carriers2 <- c(carriers2, 1)
  else if (any(df$sex == "unknown" & df$affected2 == 2)) carriers2 <- c(carriers2, 3)
  else if (any(df$sex == "terminated" & df$affected2 == 2)) carriers2 <- c(carriers2, 4)
  
  presymp2 <- c()
  if (any(df$sex == "female" & df$affected2 == 3)) presymp2 <- c(presymp2, 2)
  else if (any(df$sex == "male" & df$affected2 == 3)) presymp2 <- c(presymp2, 1)
  else if (any(df$sex == "unknown" & df$affected2 == 3)) presymp2 <- c(presymp2, 3)
  else if (any(df$sex == "terminated" & df$affected2 == 3)) presymp2 <- c(presymp2, 4)
  
  phen3 <- c()
  if (any(df$sex == "female" & df$affected3 == 1)) phen3 <- c(phen3, 2)
  else if (any(df$sex == "male" & df$affected3 == 1)) phen3 <- c(phen3, 1)
  else if (any(df$sex == "unknown" & df$affected3 == 1)) phen3 <- c(phen3, 3)
  else if (any(df$sex == "terminated" & df$affected3 == 1)) phen3 <- c(phen3, 4)
  
  carriers3 <- c()
  if (any(df$sex == "female" & df$affected3 == 2)) carriers3 <- c(carriers3, 2)
  else if (any(df$sex == "male" & df$affected3 == 2)) carriers3 <- c(carriers3, 1)
  else if (any(df$sex == "unknown" & df$affected3 == 2)) carriers3 <- c(carriers3, 3)
  else if (any(df$sex == "terminated" & df$affected3 == 2)) carriers3 <- c(carriers3, 4)
  
  presymp3 <- c()
  if (any(df$sex == "female" & df$affected3 == 3)) presymp3 <- c(presymp3, 2)
  else if (any(df$sex == "male" & df$affected3 == 3)) presymp3 <- c(presymp3, 1)
  else if (any(df$sex == "unknown" & df$affected3 == 3)) presymp3 <- c(presymp3, 3)
  else if (any(df$sex == "terminated" & df$affected3 == 3)) presymp3 <- c(presymp3, 4)
  
  phen4 <- c()
  if (any(df$sex == "female" & df$affected4 == 1)) phen4 <- c(phen4, 2)
  else if (any(df$sex == "male" & df$affected4 == 1)) phen4 <- c(phen4, 1)
  else if (any(df$sex == "unknown" & df$affected4 == 1)) phen4 <- c(phen4, 3)
  else if (any(df$sex == "terminated" & df$affected4 == 1)) phen4 <- c(phen4, 4)
  
  carriers4 <- c()
  if (any(df$sex == "female" & df$affected4 == 2)) carriers4 <- c(carriers4, 2)
  else if (any(df$sex == "male" & df$affected4 == 2)) carriers4 <- c(carriers4, 1)
  else if (any(df$sex == "unknown" & df$affected4 == 2)) carriers4 <- c(carriers4, 3)
  else if (any(df$sex == "terminated" & df$affected4 == 2)) carriers4 <- c(carriers4, 4)
  
  presymp4 <- c()
  if (any(df$sex == "female" & df$affected4 == 3)) presymp4 <- c(presymp4, 2)
  else if (any(df$sex == "male" & df$affected4 == 3)) presymp4 <- c(presymp4, 1)
  else if (any(df$sex == "unknown" & df$affected4 == 3)) presymp4 <- c(presymp4, 3)
  else if (any(df$sex == "terminated" & df$affected4 == 3)) presymp4 <- c(presymp4, 4)
  
  
  
  col1 <- 0
  if (length(sexes) > 0) col1 <- col1 + 1
  if (length(deceased) > 0) col1 <- col1 + 1
  if (length(pregnancies) > 0) col1 <- col1 + 1
  if (length(abortions) > 0) col1 <- col1 + 1
  if (length(adopteds) > 0) col1 <- col1 + 1
  
  col2 <- 0
  if (length(phen1) > 0) col2 <- col2 + 1
  if (length(carriers1) > 0) col2 <- col2 + 1
  if (length(presymp1) > 0) col2 <- col2 + 1
  if (length(phen2) > 0) col2 <- col2 + 1
  if (length(carriers2) > 0) col2 <- col2 + 1
  if (length(presymp2) > 0) col2 <- col2 + 1
  if (length(phen3) > 0) col2 <- col2 + 1
  if (length(carriers3) > 0) col2 <- col2 + 1
  if (length(presymp3) > 0) col2 <- col2 + 1
  if (length(phen4) > 0) col2 <- col2 + 1
  if (length(carriers4) > 0) col2 <- col2 + 1
  if (length(presymp4) > 0) col2 <- col2 + 1
  
  
  
  
  xrange <- c(1, 2)
  maxlev <- max(col1, col2)
  frame()
  oldpar <- par(mar=c(4.1, 1, 4.1, 1), pin=c(width-2, maxlev), xpd=TRUE)
  psize <- par('pin')  # plot region in inches
  stemp1 <- strwidth("ABC", units='inches', cex=1)* 2.5/3
  stemp2 <- strheight('1g', units='inches', cex=1)
  stemp3 <- max(strheight("HPO", units='inches', cex=1))
  
  ht1 <- psize[2]/maxlev - (stemp3 + 1.5*stemp2)
  if (ht1 <=0) stop("Labels leave no room for the graph, reduce cex")
  ht2 <- psize[2]/(maxlev + (maxlev-1)/2)
  wd2 <- .8*psize[1]/(.8 + diff(xrange))
  
  boxsize <- symbolsize* min(ht1, ht2, stemp1, wd2) # box size in inches
  hscale <- (psize[1]- boxsize)/diff(xrange)  #horizontal scale from user-> inch
  vscale <- (psize[2]-(stemp3 + stemp2/2 + boxsize))/ max(1, maxlev-1)
  boxw  <- boxsize/hscale  # box width in user units
  boxh  <- boxsize/vscale   # box height in user units
  labh  <- stemp2/vscale   # height of a text string
  
  par(usr=c(xrange[1] - 0.5, xrange[2] + 0.5, 
            maxlev + boxh + 0.1 , 0.9))
  
  circfun <- function(nslice, n=50) {
    nseg <- ceiling(n/nslice)  #segments of arc per slice
    
    theta <- -pi/2 - seq(0, 2*pi, length=nslice +1)
    out <- vector('list', nslice)
    for (i in 1:nslice) {
      theta2 <- seq(theta[i], theta[i+1], length=nseg)
      out[[i]]<- list(x=c(0, cos(theta2)/2),
                      y=c(0, sin(theta2)/2) + .5)
    }
    out
  }
  polyfun <- function(nslice, object) {
    # make the indirect segments view
    zmat <- matrix(0,ncol=4, nrow=length(object$x))
    zmat[,1] <- object$x
    zmat[,2] <- c(object$x[-1], object$x[1]) - object$x
    zmat[,3] <- object$y
    zmat[,4] <- c(object$y[-1], object$y[1]) - object$y
    
    # Find the cutpoint for each angle
    #   Yes we could vectorize the loop, but nslice is never bigger than
    # about 10 (and usually <5), so why be obscure?
    ns1 <- nslice+1
    theta <- -pi/2 - seq(0, 2*pi, length=ns1)
    x <- y <- double(ns1)
    for (i in 1:ns1) {
      z <- (tan(theta[i])*zmat[,1] - zmat[,3])/
        (zmat[,4] - tan(theta[i])*zmat[,2])
      tx <- zmat[,1] + z*zmat[,2]
      ty <- zmat[,3] + z*zmat[,4]
      inner <- tx*cos(theta[i]) + ty*sin(theta[i])
      indx <- which(is.finite(z) & z>=0 &  z<=1 & inner >0)
      if (length(indx) > 1) indx <- indx[1]    
      x[i] <- tx[indx]
      y[i] <- ty[indx]
    }
    nvertex <- length(object$x)
    temp <- data.frame(indx = c(1:ns1, rep(0, nvertex)),
                       theta= c(theta, object$theta),
                       x= c(x, object$x),
                       y= c(y, object$y))
    temp <- temp[order(-temp$theta),]
    out <- vector('list', nslice)
    for (i in 1:nslice) {
      rows <- which(temp$indx==i):which(temp$indx==(i+1))
      out[[i]] <- list(x=c(0, temp$x[rows]), y= c(0, temp$y[rows]) +.5)
    }
    out
  }
  

  polylist <- list(
    square = list(list(x=c(-1, -1, 1, 1)/2,  y=c(0, 1, 1, 0))),
    circle = list(list(x=.5* cos(seq(0, 2*pi, length=50)),
                       y=.5* sin(seq(0, 2*pi, length=50)) + .5)),
    diamond = list(list(x=c(0, -.5, 0, .5), y=c(0, .5, 1, .5))),
    triangle= list(list(x=c(0, -.56, .56),  y=c(0, 0.82, 0.82))))


  
  drawbox <- function(x, y, sex, status, affected, polylist, col, density, angle, boxw, boxh, adopted) {
    
    if (affected == 0) {
      polygon(x + polylist[[sex]][[1]]$x *boxw,
              y + polylist[[sex]][[1]]$y *boxh,
              col=NA, border=1)
    }
    
    else if (affected == 1) {
      polygon(x + polylist[[sex]][[1]]$x * boxw,
              y + polylist[[sex]][[1]]$y * boxh,
              col=col, border=1, density=density, angle=angle)
    }

    else if (affected == 2) {
      polygon(x + polylist[[sex]][[1]]$x * boxw,
              y + polylist[[sex]][[1]]$y * boxh,
              col=NA, border=1)
        
      midx <- x + mean(range(polylist[[sex]][[1]]$x*boxw))
      midy <- y + mean(range(polylist[[sex]][[1]]$y*boxh))
        
      points(midx, midy, pch=16, cex=symbolsize, col=col)
    }
    
    else if (affected == 3) {
      polygon(x + polylist[[sex]][[1]]$x * boxw,
              y + polylist[[sex]][[1]]$y * boxh,
              col=NA, border=1)
        
      midx <- x + mean(range(polylist[[sex]][[1]]$x*boxw))
      supy <- y + min(range(polylist[[sex]][[1]]$y*boxh))
      infy <- y + max(range(polylist[[sex]][[1]]$y*boxh))
        
      segments(midx, supy, midx, infy, col = col, lwd = symbolsize*2, lend = 1)
    }
      
    # Es dibuixa la ratlla diagonal per a indicar si un individu està mort
    if (status==1) segments(x- .6*boxw, y+1.1*boxh, 
                            x+ .6*boxw, y- .1*boxh)
    
    # Afegeixo aquest condicional per a indicar si es tracta d'un embaras
    else if (status == 2) {
      points(x + boxw*0.02, y + mean(range(polylist[[sex]][[1]]$y*boxh)), pch="P", cex=symbolsize*0.8)
    }
    
    # Afegeixo claudàtors si l'individu és adoptat
    if (!is.null(adopted)) {
      if (!is.na(adopted)) {
        if (adopted == "in" | adopted == "out") {
          segments(x - 0.6*boxw, y - 0.1*boxh, x - 0.6*boxw, y + 1.1*boxh)
          segments(x - 0.6*boxw, y - 0.1*boxh, x - 0.3*boxw, y - 0.1*boxh)
          segments(x - 0.6*boxw, y + 1.1*boxh, x - 0.3*boxw, y + 1.1*boxh)
          segments(x + 0.6*boxw, y - 0.1*boxh, x + 0.6*boxw, y + 1.1*boxh)
          segments(x + 0.6*boxw, y - 0.1*boxh, x + 0.3*boxw, y - 0.1*boxh)
          segments(x + 0.6*boxw, y + 1.1*boxh, x + 0.3*boxw, y + 1.1*boxh)
        }
      }
    }
  }
  
  
  
  
  for (i in 1:col1) {
    if (!is.null(sexes)) {
      if (length(sexes) == 3) {
        drawbox(1-0.3, i, sexes[1], 0, 0, NA, polylist, NA, NA, boxw, boxh, NA)
        drawbox(1-0.2, i, sexes[2], 0, 0, NA, polylist, NA, NA, boxw, boxh, NA)
        drawbox(1-0.1, i, sexes[3], 0, 0, NA, polylist, NA, NA, boxw, boxh, NA)
      }
      else if (length(sexes) == 2) {
        drawbox(1-0.2, i, sexes[1], 0, 0, NA, polylist, NA, NA, boxw, boxh, NA)
        drawbox(1-0.1, i, sexes[2], 0, 0, NA, polylist, NA, NA, boxw, boxh, NA)
      }
      else if (length(sexes) == 1) {
        drawbox(1-0.1, i, sexes[1], 0, 0, NA, polylist, NA, NA, boxw, boxh, NA)
      }
      sexes <- NULL
    }
    
    else if (!is.null(deceased)) {
      if (length(deceased) == 3) {
        drawbox(1-0.3, i, deceased[1], 1, 0, NA, polylist, NA, NA, boxw, boxh, NA)
        drawbox(1-0.2, i, deceased[2], 1, 0, NA, polylist, NA, NA, boxw, boxh, NA)
        drawbox(1-0.1, i, deceased[3], 1, 0, NA, polylist, NA, NA, boxw, boxh, NA)
      }
      else if (length(deceased) == 2) {
        drawbox(1-0.2, i, deceased[1], 1, 0, NA, polylist, NA, NA, boxw, boxh, NA)
        drawbox(1-0.1, i, deceased[2], 1, 0, NA, polylist, NA, NA, boxw, boxh, NA)
      }
      else if (length(deceased) == 1) {
        drawbox(1-0.1, i, deceased[1], 1, 0, NA, polylist, NA, NA, boxw, boxh, NA)
      }
      deceased <- NULL
    }
    
    else if (!is.null(pregnancies)) {
      if (length(pregnancies) == 3) {
        drawbox(1-0.3, i, pregnancies[1], 2, 0, NA, polylist, NA, NA, boxw, boxh, NA)
        drawbox(1-0.2, i, pregnancies[2], 2, 0, NA, polylist, NA, NA, boxw, boxh, NA)
        drawbox(1-0.1, i, pregnancies[3], 2, 0, NA, polylist, NA, NA, boxw, boxh, NA)
      }
      else if (length(pregnancies) == 2) {
        drawbox(1-0.2, i, pregnancies[1], 2, 0, NA, polylist, NA, NA, boxw, boxh, NA)
        drawbox(1-0.1, i, pregnancies[2], 2, 0, NA, polylist, NA, NA, boxw, boxh, NA)
      }
      else if (length(pregnancies) == 1) {
        drawbox(1-0.1, i, pregnancies[1], 2, 0, NA, polylist, NA, NA, boxw, boxh, NA)
      }
      pregnancies <- NULL
    }
    
    else if (!is.null(abortions)) {
      if (length(abortions) == 2) {
        drawbox(1-0.2, i, 4, abortions[1], 0, NA, polylist, NA, NA, boxw, boxh, NA)
        drawbox(1-0.1, i, 4, abortions[2], 0, NA, polylist, NA, NA, boxw, boxh, NA)
      }
      else if (length(abortions) == 1) {
        drawbox(1-0.1, i, 4, abortions[1], 0, NA, polylist, NA, NA, boxw, boxh, NA)
      }
      abortions <- NULL
    }
    
    else if (!is.null(adopteds)) {
      if (length(adopteds) == 3) {
        drawbox(1-0.3, i, adopteds[1], 0, 0, NA, polylist, NA, NA, boxw, boxh, "in")
        drawbox(1-0.2, i, adopteds[2], 0, 0, NA, polylist, NA, NA, boxw, boxh, "in")
        drawbox(1-0.1, i, adopteds[3], 0, 0, NA, polylist, NA, NA, boxw, boxh, "in")
      }
      else if (length(adopteds) == 2) {
        drawbox(1-0.2, i, adopteds[1], 0, 0, NA, polylist, NA, NA, boxw, boxh, "in")
        drawbox(1-0.1, i, adopteds[2], 0, 0, NA, polylist, NA, NA, boxw, boxh, "in")
      }
      else if (length(adopteds) == 1) {
        drawbox(1-0.1, i, adopteds[1], 0, 0, NA, polylist, NA, NA, boxw, boxh, "in")
      }
      adopteds <- NULL
    }
  }
  
  
  
  for (i in 1:col2) {
    if (!is.null(phen1)) {
      if (length(phen1) == 4) {
        drawbox(2-0.4, i, phen1[1], 0, 1, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.3, i, phen1[2], 0, 1, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.2, i, phen1[3], 0, 1, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.1, i, phen1[4], 0, 1, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
      }
      else if (length(phen1) == 3) {
        drawbox(2-0.3, i, phen1[1], 0, 1, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.2, i, phen1[2], 0, 1, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.1, i, phen1[3], 0, 1, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
      }
      else if (length(phen1) == 2) {
        drawbox(2-0.2, i, phen1[1], 0, 1, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.1, i, phen1[2], 0, 1, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
      }
      else if (length(phen1) == 1) {
        drawbox(2-0.1, i, phen1[1], 0, 1, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
      }
      phen1 <- NULL
    }
    
    else if (!is.null(carriers1)) {
      if (length(carriers1) == 4) {
        drawbox(2-0.4, i, carriers1[1], 0, 2, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.3, i, carriers1[2], 0, 2, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.2, i, carriers1[3], 0, 2, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.1, i, carriers1[4], 0, 2, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
      }
      else if (length(carriers1) == 3) {
        drawbox(2-0.3, i, carriers1[1], 0, 2, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.2, i, carriers1[2], 0, 2, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.1, i, carriers1[3], 0, 2, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
      }
      else if (length(carriers1) == 2) {
        drawbox(2-0.2, i, carriers1[1], 0, 2, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.1, i, carriers1[2], 0, 2, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
      }
      else if (length(carriers1) == 1) {
        drawbox(2-0.1, i, carriers1[1], 0, 2, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
      }
      carriers1 <- NULL
    }
    
    else if (!is.null(presymp1)) {
      if (length(presymp1) == 4) {
        drawbox(2-0.4, i, presymp1[1], 0, 3, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.3, i, presymp1[2], 0, 3, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.2, i, presymp1[3], 0, 3, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.1, i, presymp1[4], 0, 3, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
      }
      else if (length(presymp1) == 3) {
        drawbox(2-0.3, i, presymp1[1], 0, 3, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.2, i, presymp1[2], 0, 3, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.1, i, presymp1[3], 0, 3, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
      }
      else if (length(presymp1) == 2) {
        drawbox(2-0.2, i, presymp1[1], 0, 3, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
        drawbox(2-0.1, i, presymp1[2], 0, 3, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
      }
      else if (length(presymp1) == 1) {
        drawbox(2-0.1, i, presymp1[1], 0, 3, col[1], polylist, density[1], angle[1], boxw, boxh, NA)
      }
      presymp1 <- NULL
    }
    
    else if (!is.null(phen2)) {
      if (length(phen2) == 4) {
        drawbox(2-0.4, i, phen2[1], 0, 1, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.3, i, phen2[2], 0, 1, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.2, i, phen2[3], 0, 1, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.1, i, phen2[4], 0, 1, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
      }
      else if (length(phen2) == 3) {
        drawbox(2-0.3, i, phen2[1], 0, 1, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.2, i, phen2[2], 0, 1, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.1, i, phen2[3], 0, 1, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
      }
      else if (length(phen2) == 2) {
        drawbox(2-0.2, i, phen2[1], 0, 1, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.1, i, phen2[2], 0, 1, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
      }
      else if (length(phen2) == 1) {
        drawbox(2-0.1, i, phen2[1], 0, 1, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
      }
      phen2 <- NULL
    }
    
    else if (!is.null(carriers2)) {
      if (length(carriers2) == 4) {
        drawbox(2-0.4, i, carriers2[1], 0, 2, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.3, i, carriers2[2], 0, 2, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.2, i, carriers2[3], 0, 2, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.1, i, carriers2[4], 0, 2, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
      }
      else if (length(carriers2) == 3) {
        drawbox(2-0.3, i, carriers2[1], 0, 2, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.2, i, carriers2[2], 0, 2, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.1, i, carriers2[3], 0, 2, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
      }
      else if (length(carriers2) == 2) {
        drawbox(2-0.2, i, carriers2[1], 0, 2, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.1, i, carriers2[2], 0, 2, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
      }
      else if (length(carriers2) == 1) {
        drawbox(2-0.1, i, carriers2[1], 0, 2, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
      }
      carriers2 <- NULL
    }
    
    else if (!is.null(presymp2)) {
      if (length(presymp2) == 4) {
        drawbox(2-0.4, i, presymp2[1], 0, 3, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.3, i, presymp2[2], 0, 3, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.2, i, presymp2[3], 0, 3, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.1, i, presymp2[4], 0, 3, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
      }
      else if (length(presymp2) == 3) {
        drawbox(2-0.3, i, presymp2[1], 0, 3, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.2, i, presymp2[2], 0, 3, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.1, i, presymp2[3], 0, 3, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
      }
      else if (length(presymp2) == 2) {
        drawbox(2-0.2, i, presymp2[1], 0, 3, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
        drawbox(2-0.1, i, presymp2[2], 0, 3, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
      }
      else if (length(presymp2) == 1) {
        drawbox(2-0.1, i, presymp2[1], 0, 3, col[2], polylist, density[2], angle[2], boxw, boxh, NA)
      }
      presymp2 <- NULL
    }
    
    else if (!is.null(phen3)) {
      if (length(phen3) == 4) {
        drawbox(2-0.4, i, phen3[1], 0, 1, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.3, i, phen3[2], 0, 1, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.2, i, phen3[3], 0, 1, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.1, i, phen3[4], 0, 1, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
      }
      else if (length(phen3) == 3) {
        drawbox(2-0.3, i, phen3[1], 0, 1, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.2, i, phen3[2], 0, 1, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.1, i, phen3[3], 0, 1, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
      }
      else if (length(phen3) == 2) {
        drawbox(2-0.2, i, phen3[1], 0, 1, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.1, i, phen3[2], 0, 1, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
      }
      else if (length(phen3) == 1) {
        drawbox(2-0.1, i, phen3[1], 0, 1, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
      }
      phen3 <- NULL
    }
    
    else if (!is.null(carriers3)) {
      if (length(carriers3) == 4) {
        drawbox(2-0.4, i, carriers3[1], 0, 2, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.3, i, carriers3[2], 0, 2, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.2, i, carriers3[3], 0, 2, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.1, i, carriers3[4], 0, 2, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
      }
      else if (length(carriers3) == 3) {
        drawbox(2-0.3, i, carriers3[1], 0, 2, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.2, i, carriers3[2], 0, 2, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.1, i, carriers3[3], 0, 2, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
      }
      else if (length(carriers3) == 2) {
        drawbox(2-0.2, i, carriers3[1], 0, 2, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.1, i, carriers3[2], 0, 2, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
      }
      else if (length(carriers3) == 1) {
        drawbox(2-0.1, i, carriers3[1], 0, 2, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
      }
      carriers3 <- NULL
    }
    
    else if (!is.null(presymp3)) {
      if (length(presymp3) == 4) {
        drawbox(2-0.4, i, presymp3[1], 0, 3, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.3, i, presymp3[2], 0, 3, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.2, i, presymp3[3], 0, 3, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.1, i, presymp3[4], 0, 3, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
      }
      else if (length(presymp3) == 3) {
        drawbox(2-0.3, i, presymp3[1], 0, 3, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.2, i, presymp3[2], 0, 3, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.1, i, presymp3[3], 0, 3, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
      }
      else if (length(presymp3) == 2) {
        drawbox(2-0.2, i, presymp3[1], 0, 3, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
        drawbox(2-0.1, i, presymp3[2], 0, 3, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
      }
      else if (length(presymp3) == 1) {
        drawbox(2-0.1, i, presymp3[1], 0, 3, col[3], polylist, density[3], angle[3], boxw, boxh, NA)
      }
      presymp3 <- NULL
    }
    
    else if (!is.null(phen4)) {
      if (length(phen4) == 4) {
        drawbox(2-0.4, i, phen4[1], 0, 1, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.3, i, phen4[2], 0, 1, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.2, i, phen4[3], 0, 1, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.1, i, phen4[4], 0, 1, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
      }
      else if (length(phen4) == 3) {
        drawbox(2-0.3, i, phen4[1], 0, 1, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.2, i, phen4[2], 0, 1, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.1, i, phen4[3], 0, 1, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
      }
      else if (length(phen4) == 2) {
        drawbox(2-0.2, i, phen4[1], 0, 1, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.1, i, phen4[2], 0, 1, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
      }
      else if (length(phen4) == 1) {
        drawbox(2-0.1, i, phen4[1], 0, 1, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
      }
      phen4 <- NULL
    }
    
    else if (!is.null(carriers4)) {
      if (length(carriers4) == 4) {
        drawbox(2-0.4, i, carriers4[1], 0, 2, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.3, i, carriers4[2], 0, 2, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.2, i, carriers4[3], 0, 2, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.1, i, carriers4[4], 0, 2, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
      }
      else if (length(carriers4) == 3) {
        drawbox(2-0.3, i, carriers4[1], 0, 2, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.2, i, carriers4[2], 0, 2, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.1, i, carriers4[3], 0, 2, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
      }
      else if (length(carriers4) == 2) {
        drawbox(2-0.2, i, carriers4[1], 0, 2, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.1, i, carriers4[2], 0, 2, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
      }
      else if (length(carriers4) == 1) {
        drawbox(2-0.1, i, carriers4[1], 0, 2, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
      }
      carriers4 <- NULL
    }
    
    else if (!is.null(presymp4)) {
      if (length(presymp4) == 4) {
        drawbox(2-0.4, i, presymp4[1], 0, 3, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.3, i, presymp4[2], 0, 3, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.2, i, presymp4[3], 0, 3, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.1, i, presymp4[4], 0, 3, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
      }
      else if (length(presymp4) == 3) {
        drawbox(2-0.3, i, presymp4[1], 0, 3, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.2, i, presymp4[2], 0, 3, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.1, i, presymp4[3], 0, 3, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
      }
      else if (length(presymp4) == 2) {
        drawbox(2-0.2, i, presymp4[1], 0, 3, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
        drawbox(2-0.1, i, presymp4[2], 0, 3, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
      }
      else if (length(presymp4) == 1) {
        drawbox(2-0.1, i, presymp4[1], 0, 3, col[4], polylist, density[4], angle[4], boxw, boxh, NA)
      }
      presymp4 <- NULL
    }
    
  }
  
  
  
  
  
  
  
  
  
} 