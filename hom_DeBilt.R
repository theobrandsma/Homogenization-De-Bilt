#-------------------------------------------------------------------------------
# Homogenization of station De Bilt with Eelde as a reference using 
# quantile matching (based on three-month running intervals)
#-------------------------------------------------------------------------------

rm(list=ls(all=TRUE))

# Import the data --------------------------------------------------------------

Eelde <- read.table(file="Eelde.txt",header=TRUE, skip=0,sep=',')
DeBilt_prehom <- read.table(file="DeBilt_prehom.txt",header=TRUE, skip=0,sep=',')


# Select the column for Tg (tcol = 5), Tn (tcol = 6), Tx (tcol = 7) ------------
tcol <- 5

# Specify the output file names 
output_filename <- c("DeBiltTg_hom_v0.txt", "DeBiltTn_hom_v0.txt", "DeBiltTx_hom_v0.txt") 


# Calculate the smoothed quantiles for Eelde (pre - post)-----------------------
# use a three month running window and smooth the quantile differences

xnew <- Eelde
xnew[,5:7] <- xnew[,5:7]/10

yout <- matrix(rep(0,19*5),nc=5,nr=19)
quan <- seq(0.05,0.95,0.05)
yout[,1] <- quan*100

# Jan (Eelde)
x <- xnew
x <- x[x[,3] >= 12 | x[,3] <= 2,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x1 <- x1[!(x1[,2] == 1950 & x1[,3] == 12),]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]
x2 <- x2[!(x2[,2] == 1956 & x2[,3] == 12),]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.1 <- yout

# Feb (Eelde)
x <- xnew
x <- x[x[,3] >= 1 & x[,3] <= 3,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.2 <- yout

# Mar (Eelde)
x <- xnew
x <- x[x[,3] >= 2 & x[,3] <= 4,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.3 <- yout

# Apr (Eelde)
x <- xnew
x <- x[x[,3] >= 3 & x[,3] <= 5,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.4 <- yout

# May (Eelde)
x <- xnew
x <- x[x[,3] >= 4 & x[,3] <= 6,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.5 <- yout

# Jun (Eelde)
x <- xnew
x <- x[x[,3] >= 5 & x[,3] <= 7,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.6 <- yout

# Jul (Eelde)
x <- xnew
x <- x[x[,3] >= 6 & x[,3] <= 8,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.7 <- yout

# Aug (Eelde)
x <- xnew
x <- x[x[,3] >= 7 & x[,3] <= 9,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x1 <- x1[!(x1[,2] == 1950 & x1[,3] == 9),]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]
x2 <- x2[!(x2[,2] == 1956 & x2[,3] == 9),]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.8 <- yout

# Sep (Eelde)
x <- xnew
x <- x[x[,3] >= 8 & x[,3] <= 10,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x1 <- x1[!(x1[,2] == 1950 & x1[,3] >= 9),]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]
x2 <- x2[!(x2[,2] == 1956 & x2[,3] >= 9),]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.9 <- yout

# Oct (Eelde)
x <- xnew
x <- x[x[,3] >= 9 & x[,3] <= 11,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1949,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1955,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.10 <- yout

# Nov (Eelde)
x <- xnew
x <- x[x[,3] >= 10 & x[,3] <= 12,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1949,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1955,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.11 <- yout

# Dec (Eelde)
x <- xnew
x <- x[x[,3] >= 10 & x[,3] <= 12,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x1 <- x1[!(x1[,2] == 1950 & x1[,3] >= 11),]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]
x2 <- x2[!(x2[,2] == 1956 & x2[,3] >= 11),]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.12 <- yout

y <- rbind(yout.1,yout.2, yout.3,yout.4,yout.5,yout.6,yout.7,yout.8,yout.9,
           yout.10,yout.11,yout.12)

month <- rep(1:12,rep(19,12))
y <- cbind(y, month)
names(y) <- c("quan.a","T.pre.a","T.after.a","jump.a","jump.sm.a","month")
yout.Eelde <- y


# Calculate the smoothed quantiles for De Bilt (pre - post)-----------------------

xnew <- DeBilt_prehom

# fill a matrix yout with 5,10,15,..,95
yout <- matrix(rep(0,19*5),nc=5,nr=19)
quan <- seq(0.05,0.95,0.05)
yout[,1] <- quan*100

# Jan (DB)
x <- xnew
x <- x[x[,3] >= 12 | x[,3] <= 2,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x1 <- x1[!(x1[,2] == 1950 & x1[,3] == 12),]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]
x2 <- x2[!(x2[,2] == 1956 & x2[,3] == 12),]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.1 <- yout

# Feb (DB)
x <- xnew
x <- x[x[,3] >= 1 & x[,3] <= 3,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.2 <- yout

# Mar (DB)
x <- xnew
x <- x[x[,3] >= 2 & x[,3] <= 4,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.3 <- yout

# Apr (DB)
x <- xnew
x <- x[x[,3] >= 3 & x[,3] <= 5,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.4 <- yout

# May (DB)
x <- xnew
x <- x[x[,3] >= 4 & x[,3] <= 6,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.5 <- yout

# Jun (DB)
x <- xnew
x <- x[x[,3] >= 5 & x[,3] <= 7,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.6 <- yout

# Jul (DB)
x <- xnew
x <- x[x[,3] >= 6 & x[,3] <= 8,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.7 <- yout

# Aug (DB)
x <- xnew
x <- x[x[,3] >= 7 & x[,3] <= 9,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x1 <- x1[!(x1[,2] == 1950 & x1[,3] == 9),]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]
x2 <- x2[!(x2[,2] == 1956 & x2[,3] == 9),]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.8 <- yout

# Sep (DB)
x <- xnew
x <- x[x[,3] >= 8 & x[,3] <= 10,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x1 <- x1[!(x1[,2] == 1950 & x1[,3] >= 9),]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]
x2 <- x2[!(x2[,2] == 1956 & x2[,3] >= 9),]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.9 <- yout

# Oct (DB)
x <- xnew
x <- x[x[,3] >= 9 & x[,3] <= 11,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1949,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1955,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.10 <- yout

# Nov (DB)
x <- xnew
x <- x[x[,3] >= 10 & x[,3] <= 12,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1949,]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1955,]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.11 <- yout

# Dec (DB)
x <- xnew
x <- x[x[,3] >= 10 & x[,3] <= 12,]
x1 <- x[x[,2] >= 1946 & x[,2] <= 1950,]
x1 <- x1[!(x1[,2] == 1950 & x1[,3] >= 11),]
x2 <- x[x[,2] >= 1952 & x[,2] <= 1956,]
x2 <- x2[!(x2[,2] == 1956 & x2[,3] >= 11),]

yout[,2] <- quantile(x1[,tcol],probs=quan)
yout[,3] <- quantile(x2[,tcol],probs=quan)
yout[,4] <- yout[,2]-yout[,3]

y <- loess(yout[,4]~yout[,1],span=0.6,degree=2,family="gaussian")
yout[,5] <- y$fitted
yout <- data.frame(yout)
yout.12 <- yout

y <- rbind(yout.1,yout.2, yout.3,yout.4,yout.5,yout.6,yout.7,yout.8,yout.9,
           yout.10,yout.11,yout.12)

month <- rep(1:12,rep(19,12))
y <- cbind(y, month)
names(y) <- c("quan.a","T.pre.a","T.after.a","jump.a","jump.sm.a","month")
yout.DeBilt <- y


# Smooth each quantile difference (De Bilt[pre - post] - Eelde[pre - post]) between the months

x <- yout.DeBilt[,c(1,5,6)]
x[,2] <- x[,2] - yout.Eelde[,5]
names(x) <- c("quan","jump","month")

y <- x
y$month1 <- c(rep(1:12,rep(19,12)))
y$quan <- as.integer(y$quan)

#add months ND as -1 and 0 and JF as 13 and 14
y1 <- y[y[,4]==11,]
y1[,4] <- -1
y2 <- y[y[,4]==12,]
y2[,4] <- 0
y3 <- y[y[,4]==1,]
y3[,4] <- 13
y4 <- y[y[,4]==2,]
y4[,4] <- 14
y <-rbind(y1,y2,y,y3,y4)

#add the smoothed jump to y as a new column
y <- cbind(y,y[,2])
names(y)[5] <- 'jump.sm2'
for(i in 1:19){
  qa <- 5*i
  yi <- y[,1]==qa
  out <- loess(y[yi,2]~y[yi,4],span=0.6,degree=2,family="gaussian")
  y[yi,5] <- out$fitted
}

y <- y[y[,4] >= 1 & y[,4]<=12,]

yout.correction <- y[,c(1,5,4)]


# Calculate the homogenized series----------------------------------------------

# first correct the <= Aug 1950 part
x <- DeBilt_prehom

y <- yout.correction
x <- cbind(x,x[,tcol])
names(x)[8] <- 'T.hom'
x <- x[x[,2] < 1951,]
x <- x[!(x[,2] == 1950 & x[,3] > 8),]

for(i in 1:12){
  ind <- x[,3] == i
  yquan <- quantile(x[ind,tcol],probs=c(1:19/20))
  yquan <- as.numeric(yquan)
  dT <- approx(yquan,y[y[,3]==i,2],x[ind,tcol],rule=2)$y
  x[ind,8] <- x[ind,8] - dT
}
x1 <- x


# second correct the > Aug 1950 <= Aug 1951 part (divide the corrections by 2)
x <- DeBilt_prehom

y <- yout.correction
y[,2] <- y[,2]/2
x <- cbind(x,x[,tcol])
names(x)[8] <- 'T.hom'
x <- x[x[,2] >= 1950 & x[,2] <= 1951,]
x <- x[!(x[,2] == 1950 & x[,3] <= 8),]
x <- x[!(x[,2] == 1951 & x[,3] > 8),]


for(i in 1:12){
  ind <- x[,3] == i
  yquan <- quantile(x[ind,tcol],probs=c(1:19/20))
  yquan <- as.numeric(yquan)
  dT <- approx(yquan,y[y[,3]==i,2],x[ind,tcol],rule=2)$y
  x[ind,8] <- x[ind,8] - dT
}
x2 <- x

# Combine the files and add the modern data
x <- rbind(x1,x2)
x1 <- DeBilt_prehom
x1 <- cbind(x1,x1[,tcol])
names(x1)[8] <- 'T.hom'
x1 <- x1[x1[,2] >= 1951,]
x1 <- x1[!(x1[,2] == 1951 & x1[,3] <= 8),]

x <- rbind(x,x1)


# Write the homogenized data to disk--------------------------------------------
write.table(round(x[,c(2:4,tcol,8)],1), file = output_filename[tcol-4],sep = ",", row.names = FALSE)



