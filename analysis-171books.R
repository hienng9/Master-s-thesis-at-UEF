library(ggplot2)
library(nlme)
library(nlmeU)
library(stats4)
library(MASS)
library(lmfor)
#library(lme4)
#library(lmerTest)
library(zoo)
library(xts)
library("PerformanceAnalytics")
library(stargazer)
library(reshape)
library(dummies)

books<-read.csv('books-6wl.csv')
books$PAR_ID<-as.factor(books$PAR_ID)
books$PRICECEN<-scale(books$PRICE, scale = FALSE)
books$RATINGCEN<-scale(books$RATING, scale = FALSE)
books$PERIOD2<-books$PERIOD^2

books4<-read.csv('books-6wl-na.csv')
books4$PAR_ID<-as.factor(books4$PAR_ID)
books4$PRICECEN<-scale(books4$PRICE, scale = FALSE)
books4$RATINGCEN<-scale(books4$RATING, scale = FALSE)
books4$PERIOD2<-books4$PERIOD^2

                      
##LINEPLOT
linesplot(books$PERIOD, books$EXTSRLOG,group = books$PAR_ID)
par(new=TRUE)
linesplot(books$PERIOD, books$SALESRANKLOG,group = books$PAR_ID, col = 'green')

##SUBSETING
periods0<-subset(books[,-c(1,2,6,7)], PERIOD == 0)
periods12<-subset(books[,-c(1,2,6,7)], PERIOD == c(1,2))
periods2<-subset(books[,-c(1,2,6,7)], PERIOD == 2)
periods3<-subset(books[,-c(1,2,6,7)], PERIOD == 3)
periods4<-subset(books[,-c(1,2,6,7)], PERIOD == 4)
periods5<-subset(books[,-c(1,2,6,7)], PERIOD == 5)

##BOXPLOT
### Melt estsr and salesrank
books2<-melt(books[,c(3,4,11,13)],
             id = c('PAR_ID', 'PERIOD'))
books2$PERIOD<-as.factor(books2$PERIOD)

p1<-ggplot(books3, aes(x=factor(variable), y= value, fill=factor(variable)))+
  geom_boxplot()+facet_grid(.~PERIOD)

## melt extsrlong and salesranklog
books3<-melt(books[,c(3,4,15,16)],
             id = c('PAR_ID', 'PERIOD'))
books3$PERIOD<-factor(books3$PERIOD,
                     levels = c(0,1,2,3,4,5),
                     labels = c('week 1', 'week2','week3',
                                'week4','week5', 'week6'))
 
##Violin plot
p1<-ggplot(books2, aes(x=factor(variable), y= value, fill=factor(variable)))+
  geom_violin()+facet_grid(.~PERIOD)

p1
fun_median <- function(x){
  return(data.frame(y=round(median(x),2),label=round(median(x,na.rm=T),2)))}

p1<-p1+stat_summary(fun.y = median, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data = fun_median, geom="text", vjust=2)
p1<-p1+xlab("Variables")+ylab('Logarithmic value')
p1+theme(legend.position = 'none')
p1

p2<-ggplot(books3, aes(x=factor(variable), y= value))+
  geom_boxplot()+facet_grid(.~PERIOD)
p2<-p2+xlab("Variables")+ylab('Logarithmic value')
p2<-p2+stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
  stat_summary(fun.data = fun_mean, geom="text", vjust=2)
p2

###scatter plot for seqnum and extsrlog
periods0$SEQNUM<-as.factor(periods0$SEQNUM)
seq2<-subset(periods0, SEQNUM==2)
seq3<-subset(periods0, SEQNUM==3)
seq3q<-rbind(seq2, seq3)
ggplot(seq3q, aes(x=EXTSRLOG, y = SALESRANKLOG, color = SEQNUM, shape =SEQNUM))+
  geom_point()+geom_smooth(method = 'lm', fill=NA)#+facet_grid(.~PERIOD)               

## T TEST SALESRANK BETWEEN PERIODS
t.test(periods0$SALESRANKLOG, periods1$SALESRANKLOG, paired=TRUE)


##GROWTH MODEL

## RANDOM INTERCEPT
mod.1<-lme(SALESRANKLOG~PERIOD+PERIOD2, random = ~1|PAR_ID, 
             data = books, na.action = na.omit)

## INCONSTANT VARIANCE
mod.2 <- update(mod.1, weights = varExp(form = ~PERIOD))

mod.2<-lme(SALESRANKLOG~PERIOD, random = ~1|PAR_ID,
           weights = varExp(form = ~PERIOD),
           data = books, na.action=na.omit)

##RANDOM SLOPE
mod.3<-update(mod.2, random = ~1+PERIOD|PAR_ID)

mod.3<-lme(SALESRANKLOG~PERIOD, random = ~1+PERIOD|PAR_ID,
           weights = varExp(form=~PERIOD),
              data=books, na.action = na.omit)

#ADDING EXTENSION ATTRIBUTES
##EXTSRLOG
mod.4<-lme(SALESRANKLOG~PERIOD +PERIOD2+EXTSRLOG, 
              random= ~1+PERIOD|PAR_ID,
              weights = varExp(form=~PERIOD),
              data = books, na.action = na.omit )

##SEQNUM
mod.5<-lme(SALESRANKLOG~PERIOD+PERIOD2+ EXTSRLOG +SEQNUM,
              random= ~1+PERIOD|PAR_ID,
              weights = varExp(form=~PERIOD),
              data = books, na.action = na.omit)

###INTERACTION EXTSRLOG:SEQNUM and EXTSRLOG:PERIOD
mod.6a<-lme(SALESRANKLOG~PERIOD+PERIOD2+EXTSRLOG +SEQNUM 
               +EXTSRLOG:PERIOD,  
               random= ~1+PERIOD|PAR_ID,
              weights = varExp(form=~PERIOD),
              data = books, na.action = na.omit)

mod.6b<-lme(SALESRANKLOG~PERIOD+PERIOD2+EXTSRLOG +SEQNUM 
           +EXTSRLOG:PERIOD+EXTSRLOG:SEQNUM,  
           random= ~1+PERIOD|PAR_ID,
           weights = varExp(form=~PERIOD),
           data = books, na.action = na.omit)

##ML method
mod.6aml<-lme(SALESRANKLOG~PERIOD+PERIOD2+EXTSRLOG +SEQNUM 
            +EXTSRLOG:PERIOD,  
            random= ~1+PERIOD|PAR_ID,
            weights = varExp(form=~PERIOD),method = 'ML',
            data = books, na.action = na.omit)

mod.6bml<-lme(SALESRANKLOG~PERIOD+PERIOD2+EXTSRLOG +SEQNUM 
            +EXTSRLOG:PERIOD+EXTSRLOG:SEQNUM,  
            random= ~1+PERIOD|PAR_ID,
            weights = varExp(form=~PERIOD),method = 'ML',
            data = books, na.action = na.omit)

#ADDING PARENT ATTRIBUTES
## PRICE
mod.7<-lme(SALESRANKLOG~PERIOD+PERIOD2+EXTSRLOG+
                SEQNUM+ EXTSRLOG:PERIOD+EXTSRLOG:SEQNUM +PRICECEN, 
           random= ~1+PERIOD|PAR_ID,
              weights = varExp(form=~PERIOD),
              data = books, na.action = na.omit)
mod.8a<-lme(SALESRANKLOG~PERIOD+PERIOD2+EXTSRLOG + EXTSRLOG:PERIOD+
             SEQNUM +EXTSRLOG:SEQNUM+PRICECEN+VOLLOG, 
           random= ~1+PERIOD|PAR_ID,
           weights = varExp(form=~PERIOD),
           data = books, na.action = na.omit)
mod.8b<-lme(SALESRANKLOG~PERIOD+PERIOD2+EXTSRLOG + EXTSRLOG:PERIOD+
              SEQNUM +EXTSRLOG:SEQNUM+PRICECEN+RATINGCEN+VOLLOG, 
            random= ~1+PERIOD|PAR_ID,
            weights = varExp(form=~PERIOD),
            data = books, na.action = na.omit)
mod.8c<-lme(SALESRANKLOG~PERIOD+PERIOD2+EXTSRLOG +SEQNUM+
              EXTSRLOG:PERIOD +PRICECEN+RATINGCEN*VOLLOG+
              EXTSRLOG:SEQNUM, random= ~1+PERIOD|PAR_ID,
            weights = varExp(form=~PERIOD),
            data = books, na.action = na.omit)
##ML method
mod.8b<-lme(SALESRANKLOG~PERIOD+PERIOD2+EXTSRLOG + EXTSRLOG:PERIOD+
              SEQNUM +EXTSRLOG:SEQNUM+PRICECEN+RATINGCEN+VOLLOG, 
            random= ~1+PERIOD|PAR_ID,
            weights = varExp(form=~PERIOD),
            data = books, na.action = na.omit, method = 'ML')
mod.8c<-lme(SALESRANKLOG~PERIOD+PERIOD2+EXTSRLOG +SEQNUM+
              EXTSRLOG:PERIOD +PRICECEN+RATINGCEN*VOLLOG+
              EXTSRLOG:SEQNUM, random= ~1+PERIOD|PAR_ID,method='ML',
            weights = varExp(form=~PERIOD),
            data = books, na.action = na.omit)
##RATING AND VOLUME
##WITH INTERACTION
mod.8c<-lme(SALESRANKLOG~PERIOD+EXTSRLOG +SEQNUM+
             EXTSRLOG:PERIOD +PRICE2+RATING2*VOLLOG+
             EXTSRLOG:SEQNUM, random= ~1+PERIOD|PAR_ID,
           weights = varExp(form=~PERIOD),
           data = books, na.action = na.omit)
###NO MISSING DATA books4
mod.8a<-lme(SALESRANKLOG~PERIOD+EXTSRLOG +SEQNUM+
             EXTSRLOG:PERIOD +PRICE2+RATING2*VOLLOG+
             EXTSRLOG:SEQNUM, random= ~1+PERIOD|PAR_ID,
           
           data = books, na.action = na.omit)


###CHECK ASSUMPTIONS
###LINEARITY IN EACH VARIABLE
attach(books4)
ggplot(data.frame(VOLLOG,pearson=resid(mod.8, type = 'p')),
       aes(x=VOLLOG,y=pearson)) +
  geom_point() +
  theme_bw()




##MODEL WITH ML METHOD
mod.8ml<-lme(SALESRANKLOG~PERIOD+EXTSRLOG +SEQNUM+
             EXTSRLOG:PERIOD +PRICE2+RATING2*VOLLOG+
             EXTSRLOG:SEQNUM, random= ~1+PERIOD|PAR_ID,
           weights = varExp(form=~PERIOD),method = 'ML',
           data = books, na.action = na.omit)
##FINAL MODEL WITHOUT INTERACTION
mod.8a<-lme(SALESRANKLOG~PERIOD+EXTSRLOG +SEQNUM+
             EXTSRLOG:PERIOD +PRICE2+RATING2*VOLLOG,
             random=~1+PERIOD|PAR_ID,method='ML',
              weights = varExp(form=~PERIOD),
              data = books, na.action = na.omit)

###MODEL WITH SEQNUM AS RANDOM EFFECT==>Nope
books$SEQNUM<-as.factor(books$SEQNUM)
mod.8a<-lme(SALESRANKLOG~PERIOD+EXTSRLOG +
              EXTSRLOG:PERIOD +PRICE2+RATING2*VOLLOG,
            random= list(~1|PAR_ID,~1|SEQNUM),
            weights = varExp(form=~PERIOD),
            data = books, na.action = na.omit)

###MODEL WITH QUADRATIC PERIOD
books$PERIOD2<-books$PERIOD^2
mod.8d<-lme(SALESRANKLOG~PERIOD +PERIOD2+EXTSRLOG +SEQNUM+
             PRICE2+RATING2*VOLLOG+
             EXTSRLOG:SEQNUM, random= ~1+PERIOD|PAR_ID,
           weights = varExp(form=~PERIOD),
           data = books, na.action = na.omit)



##NOT RANDOM SLOPE BUT AUTOCOR

mod.5lme<-lme(SALESRANKLOG~PERIOD+POST_EXT+EXTSRLOG + EXTSRLOG:PERIOD+
                SEQNUM+EXTSRLOG:SEQNUM +PRICE2+
                RATING2*VOLLOG, random= ~1|PAR_ID,
              correlation = corAR1(),
              weights = varExp(form=~PERIOD),
              data = books2, na.action = na.omit)

mod.1lme<-lme(SALESRANKLOG~PERIOD + PERIOD2, random= ~1|PAR_ID,
              correlation = corAR1(),
              weights = varExp(form=~PERIOD),
              data = books2, na.action = na.omit)
###RESIDUAL PLOT
graphics.off()
par("mar")
par(mar=c(1,1,1,1))
par(mfcol = c(1,2))
plot(fitted(mod.8a), resid(mod.8a, type='p'))
mywhiskers(fitted(mod.8a), resid(mod.8a, type ='p'),
           add = TRUE, se = FALSE)
abline(h =0)
plot(fitted(mod.8), resid(mod.8, type = 'p'))
mywhiskers(fitted(mod.8), resid(mod.8, type = 'p'),
           add = TRUE, se = FALSE)
abline(h =0)

##QQPLOT
qqnorm(mod.8, ~ranef(.))
qqnorm(resid(mod.8))
qqnorm(mod.8, ~ resid(., type = "p") | PERIOD, qqline())
qqnorm(mod.8, ~ resid(.), abline = c(0, 1))

books2<-read.csv('books-6wl-na.csv')
books2$PAR_ID<-as.factor(books2$PAR_ID)
books2$PRICE2<-scale(books2$PRICE, scale = FALSE)
books2$RATING2<-scale(books2$RATING, scale = FALSE)
books2$PERIOD2<-books2$PERIOD^2
mod.5lme<-lme(SALESRANKLOG~PERIOD+PERIOD2+EXTSRLOG 
              + EXTSRLOG:PERIOD+
                SEQNUM+EXTSRLOG:SEQNUM +PRICE2+
                RATING2*VOLLOG, random= ~1+PERIOD|PAR_ID,
              weights = varExp(form=~PERIOD),
              data = books3, na.action = na.omit)
##AUGPRED PLOT
mod.6d<-lme(SALESRANKLOG~PERIOD+PERIOD2+EXTSRLOG +SEQNUM 
            +EXTSRLOG:PERIOD+EXTSRLOG:SEQNUM,  
            random= ~1+PERIOD|PAR_ID,
            weights = varExp(form=~PERIOD),
            data = books4, na.action = na.omit)
mod.8d<-lme(SALESRANKLOG~PERIOD+PERIOD2+EXTSRLOG +SEQNUM+
              EXTSRLOG:PERIOD +PRICECEN+RATINGCEN*VOLLOG+
              EXTSRLOG:SEQNUM, random= ~1+PERIOD|PAR_ID,
            weights = varExp(form=~PERIOD),
            data = books4, na.action = na.omit)
aug.Pred<-augPred(mod.6d, primary = ~PERIOD, level=0:1,
                  length.out = 2)
plot(aug.Pred, layout=c(4,4,1),col = 'black', lty = c(1,2),
     key = list(lines=list(lty=c(1,2)),
                text=list(c('Population','Book-specific')),
                columns = 2))

###AUG PRED FOR MODEL WITH RANDOM SLOPE
aug.Pred1<-augPred(mod.2a, primary = ~PERIOD, level=0:1,
                  length.out = 2)
plot(aug.Pred1, layout=c(4,4,1),col = 'black', lty = c(1,2),
     key = list(lines=list(lty=c(1,2)),
                text=list(c('Population','Book-specific')),
                columns = 2))

## CORRELATION CHART
chart.Correlation2 <-
  function (R, histogram = TRUE, method=c("pearson", "kendall", "spearman"), ...)
  { # @author R Development Core Team
    # @author modified by Peter Carl
    # Visualization of a Correlation Matrix. On top the (absolute) value of the
    # correlation plus the result of the cor.test as stars. On botttom, the
    # bivariate scatterplots, with a fitted line
    
    x = checkData(R, method="matrix")
    
    if(missing(method)) method=method[1] #only use one
    
    # Published at http://addictedtor.free.fr/graphiques/sources/source_137.R
    panel.smooth<-function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                            cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) 
    {
      points(x, y, pch = pch, col = col, bg = bg, cex = cex)
      ok <- is.finite(x) & is.finite(y)
    }
    ##Already modified. Need to get the original
    panel.cor <- function(x, y, digits=2, prefix="", use="pairwise.complete.obs", method, cex.cor, ...)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- cor(x, y, use=use, method=method) # MG: remove abs here
      txt <- format(c(r, 0.123456789), digits=digits)[1]
      txt <- paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
      
      test <- cor.test(x,y, method=method)
      # borrowed from printCoefmat
      Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " "))
      # MG: add abs here and also include a 30% buffer for small numbers
      text(0.5, 0.5, txt, cex = cex * (abs(r) + .3) / 1.3)
      text(.8, .8, Signif, cex=cex, col=2)
    }
    f <- function(t) {
      dnorm(t, mean=mean(x), sd=sd.xts(x) )
    }
    hist.panel = function (x, ...) {
      par(new = TRUE)
      hist(x,
           col = "light gray",
           probability = TRUE,
           axes = FALSE,
           main = "",
           breaks = "FD")
      lines(density(x, na.rm=TRUE),
            col = "red",
            lwd = 1)
      #lines(f, col="blue", lwd=1, lty=1) how to add gaussian normal overlay?
      
    }
    # Draw the chart
    if(histogram)
      pairs(x, gap=0, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=hist.panel, method=method, ...)
    else
      pairs(x, gap=0, lower.panel=panel.smooth, upper.panel=panel.cor, method=method, ...) 
  }

books2c<-books[,c(4,5,12,8,14,15,16)]
books2<-books[,c(4,5,12,8,9,13,11)]
chart.Correlation2(books2, method ='pearson',  histogram = TRUE, pch=19)

###LMER FUNCTION
##RANDOM INTERCEPT
mod.1<-lmer(SALESRANKLOG~PERIOD + (1|PAR_ID), data = books)
summary(mod.1)

####Correlation Matrix
vcovb<-vcov(mod.1)
corb<-cov2cor(vcovb)

####Variance-components estimates
VarCorr(mod.1)
(sgm<-sigma(mod.1))
##RANDOM SLOPE
mod.1b<-lmer(SALESRANKLOG~PERIOD + (PERIOD|PAR_ID), data = books)
summary(mod.1)

#EXTENSION ATTRIBUTES
##EXTSRLOG
mod.2<-lmer(SALESRANKLOG~PERIOD+EXTSRLOG+(1|PAR_ID), data = books)
summary(mod.2)

##SEQNUM
mod.3<-lmer(SALESRANKLOG~PERIOD+EXTSRLOG +
              SEQNUM +(1|PAR_ID), data = books)
summary(mod.3)

#PARENT ATTRIBUTES
## PRICE

mod.4<-lmer(SALESRANKLOG~PERIOD+EXTSRLOG +
              SEQNUM+PRICE2+(1|PAR_ID), data = books)
summary(mod.4)
##RATING AND VOLUME

mod.5<-lmer(SALESRANKLOG~PERIOD+EXTSRLOG +
              PRICE2+SEQNUM+
              RATING2*VOLLOG+(1|PAR_ID), data = books)
summary(mod.5)
#INTERACTION
##EXTSRLOG:SEQNUM
mod.6<-lmer(SALESRANKLOG~PERIOD+EXTSRLOG +
              PRICE2+SEQNUM+
              RATING2+VOLLOG+RATING2:VOLLOG +SEQNUM:EXTSRLOG
            +(1|PAR_ID), data = books)
summary(mod.6)
###RANDOM SLOPE
mod.7a<-lmer(SALESRANKLOG~PERIOD+EXTSRLOG +
              PRICE2+SEQNUM+VOLLOG+RATING2+
              RATING2:VOLLOG +SEQNUM:EXTSRLOG
            +(1|PAR_ID), data = books)

mod.7b<-lmer(SALESRANKLOG~PERIOD+EXTSRLOG +
               PRICE2+SEQNUM+VOLLOG+RATING2+
               RATING2:VOLLOG +SEQNUM:EXTSRLOG
             +(1|PAR_ID) + (0+PERIOD|PAR_ID), data = books)


##RESIDUAL PLOT
graphics.off()
par("mar")
par(mar=c(1,1,1,1))
par(mfcol = c(1,2))
plot(fitted(mod.7a), resid(mod.7a, type = 'pearson'))
mywhiskers(fitted(mod.7a), resid(mod.7a, type = 'pearson'),
           add = TRUE, se = FALSE)
abline(h =0)
plot(fitted(mod.7b), resid(mod.7b, type = 'pearson'))
mywhiskers(fitted(mod.7b), resid(mod.7b, type = 'pearson'),
           add = TRUE, se = FALSE)
abline(h =0)
#plot(fitted(model.3b), resid(model.3b, type = 'p'))
#mywhiskers(fitted(model.3b), resid(model.3b, type = 'p'), add = TRUE, se = FALSE)
#abline(h =0)

## ML method
mod.8a<-lmer(SALESRANKLOG~PERIOD+EXTSRLOG +
               PRICE2+SEQNUM+VOLLOG+RATING2+
               RATING2:VOLLOG +SEQNUM:EXTSRLOG
             +(1|PAR_ID) + (0+PERIOD|PAR_ID), data = books)
mod.8b<-lmer(SALESRANKLOG~PERIOD+EXTSRLOG +
               PRICE2+SEQNUM+VOLLOG+RATING2+
               RATING2:VOLLOG +SEQNUM:EXTSRLOG
             +(PERIOD|PAR_ID), data = books)


##remove week 0
books3<-subset(books2, PERIOD!=0)
mod.9b<-lmer(SALESRANKLOG~PERIOD+EXTSRLOG +
               PRICE2+SEQNUM+VOLLOG+RATING2+
               RATING2:VOLLOG +SEQNUM:EXTSRLOG
             +(1|PAR_ID) + (0+PERIOD|PAR_ID), data = books2)



