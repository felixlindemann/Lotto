
    
    getNextLotto<-function(scatter, prev, n, replace=FALSE){ # nolint

        sample(subset(scatter, x == prev)$y, n, replace = replace) # nolint
    }
    
    


getEuroJackpot<-function(n = 0, m = 0, t = 0, r=0){

    euro.csv <- read.csv(url("https://www.lottozahlenonline.com/eurojackpot/eurojackpot_archiv.csv"), header = FALSE) # nolint

    euro.gewinnzahl <- strsplit(euro.csv[,2], split ="-")
    euro.zusatzzahl <- strsplit(euro.csv[,3], split ="-")
    euro.datum <- as.Date(euro.csv[,1])
    euro.einsatz <- as.numeric(euro.csv[,4])

    euro.gewinnzahl <- matrix(as.numeric(unlist(euro.gewinnzahl)), nrow=length(euro.gewinnzahl), byrow=TRUE) # nolint
    euro.zusatzzahl <- matrix(as.numeric(unlist(euro.zusatzzahl)), nrow=length(euro.zusatzzahl), byrow=TRUE) # nolint

    colnames(euro.gewinnzahl) <- paste("g", 1:5, sep="")
    colnames(euro.zusatzzahl) <- paste("z", 1:2, sep="")
 
    euro.df<- data.frame(datum=euro.datum, einsatz = euro.einsatz, euro.gewinnzahl, euro.zusatzzahl) # nolint

    scatter <- data.frame(x= c(euro.df$g1,euro.df$g2,euro.df$g3,euro.df$g4), y= c(euro.df$g2,euro.df$g3,euro.df$g4,euro.df$g5)) # nolint
    observations <- c(euro.df$g1,euro.df$g2,euro.df$g3,euro.df$g4,euro.df$g5) # nolint
    euro.balls <- hist(observations,breaks=((0:50)+0.5),col="grey") # nolint
    euro.zusatz  <- hist(c(euro.df$z1,euro.df$z2),breaks=((0:12)+0.5),col="grey") # nolint
  
    df<-data.frame()
    if(n>0){
        for( i in 1:n){

            
            g1 <- sample(euro.df$g1, 1)
            g2 <- getNextLotto(scatter, g1, 1)
            g3 <- getNextLotto(scatter, g2, 1)
            g4 <- getNextLotto(scatter, g3, 1)
            g5 <- getNextLotto(scatter, g4, 1)

            z  <-  sample(1:12, 2)
            d  <- data.frame(g1,g2,g3,g4,g5,z1 = z[1], z2 = z[2])
            df <- rbind(df, d)

        }
    }
    if(m > 0){
        for(i in 1:m){


            g <- matrix(as.numeric(unlist(sample( euro.balls$mids ,5,replace=FALSE,prob=euro.balls$counts))), nrow=1, byrow   = TRUE) # nolint
            z <- matrix(as.numeric(unlist(sample( euro.zusatz$mids ,2,replace=FALSE,prob=euro.zusatz$counts))), nrow=1, byrow   = TRUE) # nolint

            colnames(g) <- paste("g", 1:5, sep="")
            colnames(z) <- paste("z", 1:2, sep="")

            d  <- data.frame(g,z)
            df <- rbind(df, d)

        }
    }
    if(t>0){
        for (i in 1:t){

            dg <-  data.frame(x= euro.balls$mids, y= euro.balls$counts)
            dz <-  data.frame(x= euro.zusatz$mids, y= euro.zusatz$counts)
            g <-  matrix(as.numeric(unlist(dg[order(-dg$y), ]$x[i:(i+4)])), nrow=1, byrow   = TRUE) # nolint
            z <-  matrix(as.numeric(unlist(dz[order(-dz$y), ]$x[i:(i+1)])), nrow=1, byrow   = TRUE) # nolint
            
            colnames(g) <- paste("g", 1:5, sep="")
            colnames(z) <- paste("z", 1:2, sep="")

            d  <- data.frame(g,z)
            df <- rbind(df, d)


        }
    }
    if(r>0){
        for (i in 1:r){
    
            g <-  matrix(sample(1:50 ,5, replace=FALSE), nrow=1, byrow   = TRUE)
            z <-  matrix(sample(1:12 ,2, replace=FALSE), nrow=1, byrow   = TRUE)
            
            colnames(g) <- paste("g", 1:5, sep="")
            colnames(z) <- paste("z", 1:2, sep="")

            d  <- data.frame(g,z)
            df <- rbind(df, d)


        }
    } 
    df

}

 getEuroJackpot(4,2,1.3)


# getEuroJackpot(4,2,1,3)
#   g1 g2 g3 g4 g5 z1 z2
#1  22 29 34 44 48 12  9
#2   1  4 10 25 26  2  4
#3   6 13 19 38 40  5 11
#4   2  3  7 18 20  4 12
#5   5  8 42 49 28  3  7
#6  25 50 26 27 48  3  2
#7  49 19 20 18 35  8  5
#8  31  7 23  8  3 11  4
#9  10 49 32 21 35  1  7
#10 40  5  6 31 33  6 10
