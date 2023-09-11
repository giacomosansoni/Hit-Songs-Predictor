s10new=subset(Spotify10s,select=-c(uri))
s10new$song<-paste(s10new$track,"-",s10new$artist)
s10new=subset(s10new, select=-c(track,artist,key))


s10new <- s10new %>% relocate(song,.before=danceability)

s10_prediction=s10new[5000:6398,]
s10new=s10new[1:5000,]
s10new$mode= factor(s10new$mode,ordered=F)

s=glm(target~ danceability + energy + loudness + mode + acousticness + instrumentalness + liveness + tempo + valence + duration_ms + sections + chorus_hit + time_signature, family = binomial( link = logit ), data=s10new ) 
summary(s)


#mod = glm( CHD ~ AGE, family = binomial( link = logit ), data=chd )

step( s, direction = "backward" , trace = T) 

snew= glm(formula = target ~ danceability + energy + loudness + mode + 
            acousticness + instrumentalness + liveness + tempo + valence + 
            duration_ms + time_signature, family = binomial(link = logit), 
          data = s10new)
exp( coef( snew ) [ 5 ] )
#hoslem.test( s$y, fitted( snew ), g = 20 )

anova( snew, s, test = "Chisq" )

#salvo snew come miglior combinazione delle covariate di s a dimunuire l'AIC



s10Previsioni = predict(snew, newdata=s10_prediction, type='response')

soglia = 0.5

valori.reali  = s10_prediction$target
valori.predetti = as.numeric( s10Previsioni > soglia )
# 1 se > soglia, 0 se < = soglia
#valori.predetti

tab = table( valori.reali, valori.predetti )

tab

# Sensitivity
sensitivita =  tab [ 2, 2 ] /( tab [ 2, 1 ] + tab [ 2, 2 ] ) 
sensitivita

# Specificity 
specificita = tab[ 1, 1 ] /( tab [ 1, 2 ] + tab [ 1, 1 ] )
specificita

fit2 = s10Previsioni


#media campionaria della prob di sopravvivenza nel campione

soglia_roc  = seq( 0, 1, length.out = 2e2 )
lens = length( soglia_roc )-1
ascissa_roc  = rep( NA, lens )
ordinata_roc = rep( NA, lens )

for ( k in 1 : lens )
{
  soglia = soglia_roc [ k ]
  
  classification = as.numeric( sapply( fit2, function( x ) ifelse( x < soglia, 0, 1 ) ) )
  
  #  ATTENZIONE, voglio sulle righe il vero e sulle colonne il predetto
  # t.misc = table( lw$LOW, classification )
  
  ordinata_roc[ k ] = sum( classification[ which( s10_prediction$target == 1 ) ] == 1 ) /
    length( which( s10_prediction$target == 1 ) )
  
  ascissa_roc[ k ] = sum( classification[ which( s10_prediction$target == 0 ) ] == 1 ) /
    length( which( s10_prediction$target == 0 ) )
  
  # ordinata_roc [ k ]  = t.misc [ 1, 1 ] /( t.misc [ 1, 1 ] + t.misc [ 1, 2 ] )
  #
  # ascissa_roc [ k ]  = t.misc [ 2, 1 ] /( t.misc [ 2, 1 ] + t.misc [ 2, 2 ] )
}


# Visualizziamo la curva ROC.

plot( ascissa_roc, ordinata_roc, type = "l", xlab = "1 - Specificity", ylab = "Sensitivity",
      main = "Curva ROC", lwd = 2, col = 'darkblue', ylim = c( 0, 1 ), xlim = c( 0, 1 ) )
abline( h = c( 0, 1 ), v = c( 0, 1 ), lwd = 1, lty = 2, col = 'red' )
abline( a = 0, b = 1, lty = 2, col = 'black' )

# qual era il nostro punto?
abline( v = 1 - specificita,  h = sensitivita, lty = 3, col = 'blue' )
points( 1 - specificita, sensitivita, pch = 4, lwd = 3, cex = 1.5, col = 'blue' )


PRROC_obj <- roc.curve(scores.class0 = fit2, weights.class0=as.numeric(paste(s10_prediction$target)),
                       curve=TRUE)
x11()
plot(PRROC_obj)

