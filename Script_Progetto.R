################################
### PALMER PENGUINS ANALYSIS ###
################################

library(readr)
library(rgl)
library(car)
library(MASS)
library(dplyr)
library(GGally)
library(Matrix)
library(faraway)
library(BAS)

### iniziamo con l'importazione, la pulizia e la visualizzazione del dataset

PP = penguins

# eliminiamo le covariate che non ci servono
PP = PP[ , -1]
PP = PP[ , -8]

names(PP) <- c("species", "island", "bill_length", "bill_depth", "flipper_length", "mass", "sex")
View(PP)
dim(PP)
str(PP)
summary(PP)

# verifica della completezza del dataset: assenza di caselle vuote
print(sapply(PP,function(x) any(is.na(x))))
PP <- na.omit(PP)

# otteniamo il tipo di dati di ciascuna colonna
print(sapply(PP, typeof))

# visualizziamo graficamente le densità approssimate e le relazioni tra le variabili con la funzione ggpairs
ggpairs(PP[,c("bill_length", "bill_depth", "flipper_length", "mass")], aes(col=as.factor(PP$sex)))
ggpairs(PP[,c("bill_length", "bill_depth", "flipper_length", "mass")], aes(col=as.factor(PP$species)))
ggpairs(PP[,c("bill_length", "bill_depth", "flipper_length", "mass")], aes(col=as.factor(PP$island)))


### costruiamo un modello lineare che spieghi la body mass dei pinguini rispetto 
### alle restanti colonne (covariate), senza considerare, per ora, i predittori categorici

first = lm(mass ~ bill_length + bill_depth + flipper_length, data = PP)
summary(first)

# notiamo che flipper_length è estremamente significativa per spiegare il modello
# Il coefficiente R^2 ed (R^2)_adj sono intorno allo 0.76, valore buono, che ci indica che i predittori
# scelti spiegano bene il modello (la variabile dipendente), la Goodness Of Fit del modello

names(first)

### proseguiamo con la Diagnostica dei dati
std_res = first$residuals/summary(first)$sigma # residui standardizzati

# verifica dell'omoschedasticità, il primo assunto forte dell'OLS
plot(first$fitted.values, std_res, xlab = "Fitted values", ylab = "Std Residuals",
     main = "Std Residuals vs Fitted values", pch = 16)
abline(h = 0, lwd = 2, lty = 2, col = "red") # dal plot possiamo confermare l'omoschedasticità dei residui

# verifica della normalità dei residui, la seconda ipotesi fondamentale da verificare
# eseguiamo un QQplot per verificarla
qqnorm(std_res, ylab = "Raw Residuals", pch = 16)
qqline(std_res) # rispettano l'ipotesi

# test di Shapiro-Wilks per la normalità
shapiro.test(std_res) # p-value del 15%, non rifiuto H_0 (la gaussianità dei residui)

### analizziamo eventuali Leverages Points
X = model.matrix(first)
lev = hat(X)

# Dato un punto h_ii elemento diagonale di H, 
# l'osservazione i-esima è un punto di leva se:
# h_ii > 2*(p)/n

p = first$rank  
n = dim(PP)[1] # = 333

plot(first$fitted.values, lev, ylab = "Leverages", main = "Plot of Leverages", 
     pch = 16, col = 'black' )
abline( h = 2 * p/n, lty = 2, col = 'red' )

watchout_points_lev = lev[ which( lev > 2 * p/n  ) ]
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ] ## identify the rows relative to leverage points

points(first$fitted.values[ watchout_ids_lev ], watchout_points_lev, col = 'red', pch = 16)

lev [ lev >  2 * p/n ]
sum( lev [ lev >  2 * p/n ] )

# costruiamo il modello senza punti leva
second = lm(mass ~ bill_length + bill_depth + flipper_length, PP, subset = ( lev < 0.02402402 ) )
summary(second)

# inoltre, esaminiamo la variazione relativa di hat(beta) dovuta a questi punti influenti.
abs((first$coefficients - second$coefficients) / first$coefficients)
# notiamo come ci siano variazioni per le due covariate apparentemente non significative

### osserviamo eventuali punti influenti tramite residui standardizzati

# un punto è influente se |r_i^{std}| > 2
# È facile notare che i punti influenti in base ai residui standardizzati e ai punti leva sono diversi.
watchout_ids_rstd = which( abs( std_res ) > 2 )
watchout_rstd = std_res[ watchout_ids_rstd ]

# plot Residui standardizzati 
plot(first$fitted.values, std_res, ylab = "Standardized Residuals", main = "Standardized Residuals")
abline( h = c(-2,2), lty = 2, col = 'orange' )
points(first$fitted.values[watchout_ids_rstd], 
       std_res[watchout_ids_rstd], col = 'red', pch = 16 )
points(first$fitted.values[watchout_ids_lev], 
       std_res[watchout_ids_lev], col = 'orange', pch = 16 )
legend('topright', col = c('red','orange'), 
       c('Standardized Residuals', 'Leverages'), pch = rep( 16, 2 ), bty = 'n' )

# costruiamo il modello senza punti influenti
third = lm(mass ~ bill_length + bill_depth + flipper_length, PP, subset = ( abs(std_res) > 2 ) )
summary(third)

# inoltre, esaminiamo la variazione relativa di hat(beta) dovuta a questi punti influenti.
abs((first$coefficients - third$coefficients) / first$coefficients)
# notiamo come l'(R^2)_adj del modello sia bassissimo in confronto a prima. Non eliminiamo i punti
# influenti trovati tramite std_res dalla nostra analisi

# prendiamo una decisione finale su quali punti rimuovere, sfruttando la Cook's distance
# C_i > {4}/{n-p}
Cdist = cooks.distance(first)

watchout_ids_Cdist = which( Cdist > 4/(n-p) ) 
watchout_Cdist = Cdist[ watchout_ids_Cdist ]
# Vengono identificati 11 punti sospetti.

par( mfrow = c( 1, 3 ) )
plot(first$fitted.values, Cdist, pch = 16, xlab = 'Fitted values', 
     ylab = 'Cooks Distance', main = 'Cooks Distance' )
points(first$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ], 
       col = 'green', pch = 16 )
plot(first$fitted.values, std_res, pch = 16, xlab = 'Fitted values', 
     ylab = 'Standardized Residuals', main = 'Standardized Residuals' )
points(first$fitted.values[ watchout_ids_rstd ], std_res[ watchout_ids_rstd ], 
       col = 'pink', pch = 16 )
plot(first$fitted.values, lev, pch = 16, xlab = 'Fitted values', 
     ylab = 'Leverages', main = 'Leverages' )
points(first$fitted.values[ watchout_ids_lev ], lev[ watchout_ids_lev ],
       col = 'orange', pch = 16 )

par( mfrow = c( 1, 1 ) )

# costruiamo il modello senza punti influenti rispetto alla distanza di Cook 
# e confrontiamo il risultato con il modello precedente (sul set di dati completo).
fourth = lm(mass ~ bill_length + bill_depth + flipper_length, PP, subset = ( Cdist <= 4/(n-p) ) )
summary(fourth)

# confrontiamo la variazione dei coefficienti
abs((first$coefficients - fourth$coefficients) / first$coefficients)

# calcoliamo il VIF, un indice di collinearità tra covariate (Variance Inflation Factor)
vif(first) # non sono maggiori di 5, va bene

species = as.factor(species)
sex = as.factor(sex)

### consideriamo ora il modello completo di tutte le covariate, anche quelle categoriche
cat = lm(mass ~ species + island + bill_length + bill_depth + flipper_length + sex, data = PP)
summary(cat) # la categoria islands non sembra essere significativa per il modello completo
# proviamo a rimuoverla (p - value alti)

# eseguiamo una procedura step-wise con AIC (Akaike Information Criterion)
# AIC(model) = -2*log(likelihood) + 2*r

# applichiamo la procedura per scremare il modello e trovare quello con AIC minore, ovvero quello
# che premia maggiormente la goodness of fit
step(cat, direction = "backward" , trace = T)
# il modello che ne risulta è il seguente
cat2 = lm(mass ~ species + bill_length + bill_depth + flipper_length + sex, data = PP)
summary(cat2)

# una rapida osservazione: si noti come il modello finale non comprende islands, come avevamo ipotizzato
# D'altronde, riguardando il plot ggpairs con le suddivisioni di colore per isola, si osserva una netta
# distinzione dell' isola di Biscoe rispetto alle isole Dream e Torgersen. Saremmo dunque portati a pensare
# che il modello non "fitti" adeguatamente.
# Tuttavia, dal dataset si può osservare come l'isola Biscoe sia abitata prevalentemente da pinguini di
# specie Gentoo, quelli più pesanti. Dunque, l'informazione sul peso su Biscoe viene assorbita dall' informazione
# sulla specie (si osservi che l'hat(beta) della dummy speciesGentoo è +1014.627 circa), risultando ridondante
ggpairs(PP[,c("bill_length", "bill_depth", "flipper_length", "mass")], aes(col=as.factor(PP$island)))
vif(cat) # effettivamente il livello della specie Gentoo è alto (circa 17), per cui è collineare ad un'altra
# variabile (l'isola Biscoe)

# Infine, il modello completo cat e il modello finale cat2 presentano un ottimo coefficiente R^2 (0.87 circa)
# Ha senso quindi includere nelle nostre analisi i predittori categorici sex e species
std_res_cat = cat2$residuals/summary(cat2)$sigma # residui standardizzati

# verifica dell'omoschedasticità
plot(cat2$fitted.values, std_res_cat, xlab = "Fitted values", ylab = "Std Residuals",
     main = "Std Residuals vs Fitted values", pch = 16)
abline(h = 0, lwd = 2, lty = 2, col = "red") # dal plot possiamo confermare l'omoschedasticità dei residui

# verifica della normalità dei residui
qqnorm(std_res_cat, ylab = "Raw Residuals", pch = 16)
qqline(std_res_cat) # rispettano l'ipotesi

# test di Shapiro-Wilks per la normalità
shapiro.test(std_res_cat) # p-value del 74%, non rifiuto H_0 (la gaussianità dei residui)

### eseguiamo un'analisi One-way ANOVA per investigare se il peso dei pinguini è influenzato 
### prima dalla specie e poi dal sesso

# iniziamo a farci un'idea descrittiva dei dati per avere indicazioni qualitative sulla presenza di 
# differenze nella risposta a causa dell'appartenenza ad una o all'altra categoria.

# l'analisi della varianza, nota con l'acronimo di ANOVA, è una tecnica statistica che ha come obiettivo il 
# confronto tra le medie di un fenomeno aleatorio fra differenti gruppi di unità statistiche. 
# Tale analisi viene affrontata tramite decomposizione della varianza.

attach(PP)

boxplot( mass ~ species, xlab = 'species', ylab = 'mass',
         main = 'Penguins body mass according to species')
abline( h = mean( mass ) )

# numerosità dei gruppi
tapply( mass, species, length )
tapply( mass, species, mean )

# Sembra che un qualche effetto ci sia, le medie appaiono diverse e 
# sembra vi sia una dominanza stocastica delle distribuzioni dei pesi.

species = as.factor(species)
sex = as.factor(sex)

# Verifichiamo che siano soddisfatte le ipotesi dell'ANOVA: omoschedasticità fra varianze dei gruppi
# e normalità intragruppo

# normalità intragruppo;
n = length(species)
ng = table(species)
treat = levels(species)
g = length(treat)

Ps = tapply( mass, species, function( x ) ( shapiro.test( x )$p ) )
# il p-value della specie Adelie è basso = > non posso accettare l'ipotesi di gaussianità per la specie Adelie

# applico una trasformazione box-cox per invertire questa tendenza

# Trova la lambda ottimale
bc = boxcox(mass ~ species)
lambda_opt = bc$x[which.max(bc$y)] # = 0.465 circa

# Applica la trasformazione
if(lambda_opt == 0) {
  mass_bc = log(mass)
} else {
  mass_bc = (mass^lambda_opt - 1) / lambda_opt
}

# riverifichiamo l'ipotesi di normalità con la mass_bc
Ps = tapply( mass_bc, species, function( x ) ( shapiro.test( x )$p ) )
# p-value alti, accetto l'ipotesi

# omoschedasticità fra i gruppi = varianze dei gruppi omogenee
Var_bc = tapply( mass_bc, species, var )

# Bartlett's test
# H_0: sigma_1 = sigma_2 = ... = sigma_g  vs H_1: H_0^C
# Il test di Bartlett assume che le osservazioni appartenenti ai vari gruppi siano iid da una Normale. 
bartlett.test( mass_bc, species )

# ora che abbiamo verificato che le ipotesi sono soddisfatte possiamo procedere con una One-Way ANOVA (sulla trasformazione)
fit = aov( mass ~ species )
summary( fit )
fit$coefficients # restituisce la media del gruppo di riferimento più i (g-1) tau (le specie)

# Affermiamo quindi che c'è differenza delle medie fra i gruppi.
###### REMINDER: bisogna ricordare che abbiamo eseguito una box-cox, quindi reinterpretare il risultato

detach(PP)

### eseguiamo adesso un'analisi Two-ways ANOVA per capire se c’è interazione tra species e sex nel determinare mass

# visualizziamo i dati

