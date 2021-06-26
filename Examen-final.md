Estado de salud de las personas
================

## Base de Datos

``` r
library(readxl)
Datos <- read_excel("C:/Users/User/Desktop/Angie Universidad/4to semestre/Diseño de experimentos/Office/Base de datos.xlsx")
```

### Tipificación de variables (Estandarización)

``` r
datost <- Datos
datost <- scale(datost, center = T, scale = T)
datost <- as.data.frame(datost)
```

## Normalidad Multivariante

Hipótesis nula H0: Normalidad mutivariante  
Hipótesis alternativa H1: no normalidad multivariante  
Confianza: 95%  
Alfa = 5% = 0.05  
P value &gt; alfa: No se rechaza la H0  
P value &lt; alfa: Se rechaza la H0

``` r
library(MVN)
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## sROC 0.1-2 loaded

``` r
mvn(datost[2:7])
```

    ## $multivariateNormality
    ##              Test          Statistic           p value Result
    ## 1 Mardia Skewness   65.1217455547207  0.18906182273156    YES
    ## 2 Mardia Kurtosis -0.773825751824586 0.439033841704959    YES
    ## 3             MVN               <NA>              <NA>    YES
    ## 
    ## $univariateNormality
    ##           Test                            Variable Statistic   p value
    ## 1 Shapiro-Wilk                Edad                    0.9139  0.0187  
    ## 2 Shapiro-Wilk              Altura(m)                 0.9264  0.0393  
    ## 3 Shapiro-Wilk              Peso(Kg)                  0.9520  0.1914  
    ## 4 Shapiro-Wilk Cantidad de veces que come (al día)    0.7606  <0.001  
    ## 5 Shapiro-Wilk      Horas que duerme (al día)         0.9037  0.0104  
    ## 6 Shapiro-Wilk  Horas que hace ejercicio (al día)     0.8293   2e-04  
    ##   Normality
    ## 1    NO    
    ## 2    NO    
    ## 3    YES   
    ## 4    NO    
    ## 5    NO    
    ## 6    NO    
    ## 
    ## $Descriptives
    ##                                      n          Mean Std.Dev      Median
    ## Edad                                30  1.387779e-16       1 -0.34693628
    ## Altura(m)                           30  1.334731e-15       1 -0.20391099
    ## Peso(Kg)                            30  4.806413e-16       1 -0.08681324
    ## Cantidad de veces que come (al día) 30  2.220446e-16       1 -0.21984843
    ## Horas que duerme (al día)           30 -3.441782e-16       1 -0.11809529
    ## Horas que hace ejercicio (al día)   30  3.698575e-17       1 -0.15743673
    ##                                            Min      Max       25th      75th
    ## Edad                                -1.5714173 2.714266 -0.3469363 0.2653042
    ## Altura(m)                           -1.4403925 2.854754 -0.6594568 0.4794078
    ## Peso(Kg)                            -1.6494516 1.892529 -0.8941764 0.7205499
    ## Cantidad de veces que come (al día) -0.8793937 1.758787 -0.8793937 0.4396969
    ## Horas que duerme (al día)           -1.4466673 2.096191 -0.5609526 0.9890481
    ## Horas que hace ejercicio (al día)   -0.9446204 2.204114 -0.9446204 0.6297469
    ##                                          Skew   Kurtosis
    ## Edad                                0.8263046  0.3814900
    ## Altura(m)                           0.9486739  0.3909938
    ## Peso(Kg)                            0.2177206 -1.2555667
    ## Cantidad de veces que come (al día) 0.5950564 -1.0937333
    ## Horas que duerme (al día)           0.1654222 -1.0898586
    ## Horas que hace ejercicio (al día)   0.5151007 -1.1649010

Ambas pruebas de normalidad multivariante (Kurtosis y Skewness) nos
indican que el AFE es permisible para esta base de datos, pues si
presenta normalidad multivariante por lo tanto la H0 queda rechazada.

## Matriz de correlaciones

Hipótesis nula (H0): Correlacion= 0  
Hipótesis alternativa (H1): Correlacion diferente de 0  
Es necesario rechazar la hipótesis nula para aplicar el AFE.

``` r
library(psych)
corr.test(datost[,2:7])
```

    ## Warning in abbreviate(rownames(r), minlength = minlength): abreviatura utilizada
    ## con caracteres no ASCII

    ## Warning in abbreviate(colnames(r), minlength = minlength): abreviatura utilizada
    ## con caracteres no ASCII

    ## Warning in abbreviate(dimnames(ans)[[2L]], minlength = abbr.colnames):
    ## abreviatura utilizada con caracteres no ASCII

    ## Call:corr.test(x = datost[, 2:7])
    ## Correlation matrix 
    ##                                      Edad Altura(m) Peso(Kg)
    ## Edad                                 1.00      0.30     0.31
    ## Altura(m)                            0.30      1.00     0.87
    ## Peso(Kg)                             0.31      0.87     1.00
    ## Cantidad de veces que come (al día) -0.04      0.19     0.24
    ## Horas que duerme (al día)            0.19     -0.08     0.00
    ## Horas que hace ejercicio (al día)    0.06     -0.16    -0.21
    ##                                     Cantidad de veces que come (al día)
    ## Edad                                                              -0.04
    ## Altura(m)                                                          0.19
    ## Peso(Kg)                                                           0.24
    ## Cantidad de veces que come (al día)                                1.00
    ## Horas que duerme (al día)                                         -0.15
    ## Horas que hace ejercicio (al día)                                  0.07
    ##                                     Horas que duerme (al día)
    ## Edad                                                     0.19
    ## Altura(m)                                               -0.08
    ## Peso(Kg)                                                 0.00
    ## Cantidad de veces que come (al día)                     -0.15
    ## Horas que duerme (al día)                                1.00
    ## Horas que hace ejercicio (al día)                        0.00
    ##                                     Horas que hace ejercicio (al día)
    ## Edad                                                             0.06
    ## Altura(m)                                                       -0.16
    ## Peso(Kg)                                                        -0.21
    ## Cantidad de veces que come (al día)                              0.07
    ## Horas que duerme (al día)                                        0.00
    ## Horas que hace ejercicio (al día)                                1.00
    ## Sample Size 
    ## [1] 30
    ## Probability values (Entries above the diagonal are adjusted for multiple tests.) 
    ##                                     Edad Altura(m) Peso(Kg)
    ## Edad                                0.00      1.00     1.00
    ## Altura(m)                           0.11      0.00     0.00
    ## Peso(Kg)                            0.10      0.00     0.00
    ## Cantidad de veces que come (al día) 0.85      0.31     0.20
    ## Horas que duerme (al día)           0.31      0.67     0.98
    ## Horas que hace ejercicio (al día)   0.75      0.39     0.27
    ##                                     Cantidad de veces que come (al día)
    ## Edad                                                               1.00
    ## Altura(m)                                                          1.00
    ## Peso(Kg)                                                           1.00
    ## Cantidad de veces que come (al día)                                0.00
    ## Horas que duerme (al día)                                          0.44
    ## Horas que hace ejercicio (al día)                                  0.71
    ##                                     Horas que duerme (al día)
    ## Edad                                                     1.00
    ## Altura(m)                                                1.00
    ## Peso(Kg)                                                 1.00
    ## Cantidad de veces que come (al día)                      1.00
    ## Horas que duerme (al día)                                0.00
    ## Horas que hace ejercicio (al día)                        0.98
    ##                                     Horas que hace ejercicio (al día)
    ## Edad                                                                1
    ## Altura(m)                                                           1
    ## Peso(Kg)                                                            1
    ## Cantidad de veces que come (al día)                                 1
    ## Horas que duerme (al día)                                           1
    ## Horas que hace ejercicio (al día)                                   0
    ## 
    ##  To see confidence intervals of the correlations, print with the short=FALSE option

``` r
correlaciones<- corr.test(datost[,2:7])
```

    ## Warning in abbreviate(rownames(r), minlength = minlength): abreviatura utilizada
    ## con caracteres no ASCII

    ## Warning in abbreviate(colnames(r), minlength = minlength): abreviatura utilizada
    ## con caracteres no ASCII

    ## Warning in abbreviate(dimnames(ans)[[2L]], minlength = abbr.colnames):
    ## abreviatura utilizada con caracteres no ASCII

``` r
correlaciones$r
```

    ##                                            Edad   Altura(m)     Peso(Kg)
    ## Edad                                 1.00000000  0.29914587  0.309739608
    ## Altura(m)                            0.29914587  1.00000000  0.868874153
    ## Peso(Kg)                             0.30973961  0.86887415  1.000000000
    ## Cantidad de veces que come (al día) -0.03713107  0.19339511  0.243244663
    ## Horas que duerme (al día)            0.19135289 -0.08056422  0.003712016
    ## Horas que hace ejercicio (al día)    0.05982774 -0.16393062 -0.209255568
    ##                                     Cantidad de veces que come (al día)
    ## Edad                                                        -0.03713107
    ## Altura(m)                                                    0.19339511
    ## Peso(Kg)                                                     0.24324466
    ## Cantidad de veces que come (al día)                          1.00000000
    ## Horas que duerme (al día)                                   -0.14772089
    ## Horas que hace ejercicio (al día)                            0.07161149
    ##                                     Horas que duerme (al día)
    ## Edad                                              0.191352893
    ## Altura(m)                                        -0.080564221
    ## Peso(Kg)                                          0.003712016
    ## Cantidad de veces que come (al día)              -0.147720888
    ## Horas que duerme (al día)                         1.000000000
    ## Horas que hace ejercicio (al día)                 0.004808415
    ##                                     Horas que hace ejercicio (al día)
    ## Edad                                                      0.059827745
    ## Altura(m)                                                -0.163930622
    ## Peso(Kg)                                                 -0.209255568
    ## Cantidad de veces que come (al día)                       0.071611487
    ## Horas que duerme (al día)                                 0.004808415
    ## Horas que hace ejercicio (al día)                         1.000000000

``` r
r<- as.matrix(correlaciones$r)
```

# Indicadores de aplicabilidad del AFE

## Contraste de esfericidad de Bartltett

``` r
dim(datost)
```

    ## [1] 30  7

``` r
cortest.bartlett(r, n=30)
```

    ## $chisq
    ## [1] 46.32353
    ## 
    ## $p.value
    ## [1] 4.722388e-05
    ## 
    ## $df
    ## [1] 15

Como el P value es menor a alfa, se rechaza la H0, por lo tanto las
correlaciones entre cada par de variables es nulo; es decir, sí es
aplicable el AFE.

## Medida de adecuación muestral de Kaiser, Meyer y Oklin (KMO)

Se mantiene en el modelo, si el KMO es igual o mayor a 0,7.  
Se elimina una variable del modelo si el KMO es menor a 0.7.

``` r
KMO(r)
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = r)
    ## Overall MSA =  0.55
    ## MSA for each item = 
    ##                                Edad                           Altura(m) 
    ##                                0.71                                0.55 
    ##                            Peso(Kg) Cantidad de veces que come (al día) 
    ##                                0.54                                0.56 
    ##           Horas que duerme (al día)   Horas que hace ejercicio (al día) 
    ##                                0.36                                0.52

KMO= 0.55, se recomienda conservar la variable edad, y en general no se
recomienda aplicar el AFE a esta base de datos segun el KMO.

# Determinacion del número de factores a extraer

## Método de los componentes principales iterados

``` r
fa.parallel(r, fm="pa", n.obs = 30, ylabel = "Eigenvalues")
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

![](Examen-final_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

Según este método se puede extraer 1 factor que relacione a todas las
variables.

## Método de los componentes principales.

``` r
fa.parallel(r, fm="pc", n.obs = 30, ylabel = "Eigenvalues")
```

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

    ## factor method not specified correctly, minimum residual (unweighted least squares  used
    ## factor method not specified correctly, minimum residual (unweighted least squares  used

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## An ultra-Heywood case was detected. Examine the results carefully

![](Examen-final_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  1

Según este método se puede extraer 1 factor que relacione a todas las
variables.

## Método de la maxima verosimilitud.

``` r
fa.parallel(r, fm="ml", n.obs = 30, ylabel = "Eigenvalues")
```

![](Examen-final_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  1  and the number of components =  0

Según este método se puede extraer 1 factor que relacione a todas las
variables.

## Metodo paralelo con iteraciones.

``` r
library(MASS)
library(paran)
paran(r, iterations = 1000, graph = T)
```

    ## 
    ## Using eigendecomposition of correlation matrix.
    ## Computing: 10%  20%  30%  40%  50%  60%  70%  80%  90%  100%
    ## 
    ## 
    ## Results of Horn's Parallel Analysis for component retention
    ## 1000 iterations, using the mean estimate
    ## 
    ## -------------------------------------------------- 
    ## Component   Adjusted    Unadjusted    Estimated 
    ##             Eigenvalue  Eigenvalue    Bias 
    ## -------------------------------------------------- 
    ## 1           1.138494    2.873099      1.734604
    ## 2           1.108771    1.831513      0.722742
    ## -------------------------------------------------- 
    ## 
    ## Adjusted eigenvalues > 1 indicate dimensions to retain.
    ## (2 components retained)

![](Examen-final_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Según este método se puede extraer 1 factor que relacione a todas las
variables.

# Métodos de extraccion de factores.

## Método de analisis de los componentes principales (ACP)

``` r
acp<- principal(r, nfactors = 1, rotate = "none")
acp
```

    ## Principal Components Analysis
    ## Call: principal(r = r, nfactors = 1, rotate = "none")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                                       PC1      h2   u2 com
    ## Edad                                 0.47 0.21846 0.78   1
    ## Altura(m)                            0.93 0.86356 0.14   1
    ## Peso(Kg)                             0.94 0.89211 0.11   1
    ## Cantidad de veces que come (al día)  0.33 0.10778 0.89   1
    ## Horas que duerme (al día)           -0.03 0.00076 1.00   1
    ## Horas que hace ejercicio (al día)   -0.26 0.06743 0.93   1
    ## 
    ##                 PC1
    ## SS loadings    2.15
    ## Proportion Var 0.36
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 component is sufficient.
    ## 
    ## The root mean square of the residuals (RMSR) is  0.12 
    ## 
    ## Fit based upon off diagonal values = 0.82

Proportion Var 0.36 = 36%  
RMSR = 0.12

Cuanto mayor sea el valor de P var, se dice que el factor extraido tiene
mejor relacion con respecto a las variables; para el valor RMSR ocurre
lo contrario, para un mejor resultado el valor de este item debe ser lo
menor posible.

## Método de los ejes principales o componentes principales iterados (CPI)

``` r
cpi<- fa(r, nfactors = 1, fm = "pa", rotate = "none", n.obs = 30)
cpi
```

    ## Factor Analysis using method =  pa
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "pa")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                                       PA1      h2    u2 com
    ## Edad                                 0.30 0.08907 0.911   1
    ## Altura(m)                            0.91 0.81931 0.181   1
    ## Peso(Kg)                             0.98 0.95843 0.042   1
    ## Cantidad de veces que come (al día)  0.21 0.04308 0.957   1
    ## Horas que duerme (al día)           -0.02 0.00051 0.999   1
    ## Horas que hace ejercicio (al día)   -0.17 0.02817 0.972   1
    ## 
    ##                 PA1
    ## SS loadings    1.94
    ## Proportion Var 0.32
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  1.77 with Chi Square of  46.32
    ## The degrees of freedom for the model are 9  and the objective function was  0.17 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.08 
    ## The df corrected root mean square of the residuals is  0.11 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  6.15  with prob <  0.73 
    ## The total number of observations was  30  with Likelihood Chi Square =  4.44  with prob <  0.88 
    ## 
    ## Tucker Lewis Index of factoring reliability =  1.252
    ## RMSEA index =  0  and the 90 % confidence intervals are  0 0.103
    ## BIC =  -26.17
    ## Fit based upon off diagonal values = 0.91
    ## Measures of factor score adequacy             
    ##                                                    PA1
    ## Correlation of (regression) scores with factors   0.99
    ## Multiple R square of scores with factors          0.97
    ## Minimum correlation of possible factor scores     0.95

Proportion Var 0.32 = 32%  
RMSR = 0.08

## Método de la máxima verosimilitud

``` r
mve<- fa(r, nfactors = 1, fm = "ml", rotate = "none", n.obs = 30)
mve
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = r, nfactors = 1, n.obs = 30, rotate = "none", fm = "ml")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##                                       ML1      h2    u2 com
    ## Edad                                 0.31 9.7e-02 0.903   1
    ## Altura(m)                            0.87 7.6e-01 0.241   1
    ## Peso(Kg)                             1.00 1.0e+00 0.005   1
    ## Cantidad de veces que come (al día)  0.24 5.9e-02 0.941   1
    ## Horas que duerme (al día)            0.00 5.5e-06 1.000   1
    ## Horas que hace ejercicio (al día)   -0.21 4.4e-02 0.956   1
    ## 
    ##                 ML1
    ## SS loadings    1.95
    ## Proportion Var 0.33
    ## 
    ## Mean item complexity =  1
    ## Test of the hypothesis that 1 factor is sufficient.
    ## 
    ## The degrees of freedom for the null model are  15  and the objective function was  1.77 with Chi Square of  46.32
    ## The degrees of freedom for the model are 9  and the objective function was  0.16 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.09 
    ## The df corrected root mean square of the residuals is  0.11 
    ## 
    ## The harmonic number of observations is  30 with the empirical chi square  6.6  with prob <  0.68 
    ## The total number of observations was  30  with Likelihood Chi Square =  4.02  with prob <  0.91 
    ## 
    ## Tucker Lewis Index of factoring reliability =  1.275
    ## RMSEA index =  0  and the 90 % confidence intervals are  0 0.082
    ## BIC =  -26.59
    ## Fit based upon off diagonal values = 0.91
    ## Measures of factor score adequacy             
    ##                                                    ML1
    ## Correlation of (regression) scores with factors   1.00
    ## Multiple R square of scores with factors          1.00
    ## Minimum correlation of possible factor scores     0.99

Proportion Var 0.33 = 33%  
RMSR = 0.09

# Representación gráfica de los factores extraidos

## Método de análisis de los componentes principales (ACP)

``` r
#plot(acp, labels= row.names(r),cex =.7, ylim=c(-.8,.8))
```

## Método de los ejes principales o componentes principales iterados (CPI)

``` r
#plot(cpi, labels = row.names(r), cex =.7, ylim= c(-.8, .8))
```

## Método de la máxima verosimilitud (MVE)

``` r
#plot(mve, labels = row.names(r), cex =1, ylim= c(-.8, .8))
```

# Obtención de las puntuaciones factoriales

## Método de análisis de los componentes principales (ACP)

``` r
acp1<- principal(datost[,2:7], nfactors = 1, rotate = "none", scores= T)
acp1$scores
```

    ##               PC1
    ##  [1,] -1.11311475
    ##  [2,] -0.83997077
    ##  [3,]  0.84527595
    ##  [4,]  0.18917433
    ##  [5,] -0.54231657
    ##  [6,] -0.25481325
    ##  [7,]  1.13203931
    ##  [8,] -0.08455324
    ##  [9,] -0.48843573
    ## [10,] -0.17483716
    ## [11,]  0.72902838
    ## [12,]  1.70730859
    ## [13,] -1.46236319
    ## [14,] -0.60587215
    ## [15,] -0.98946804
    ## [16,] -0.39617198
    ## [17,]  1.12861965
    ## [18,]  0.93678912
    ## [19,] -0.67573787
    ## [20,] -0.10061071
    ## [21,]  1.14974561
    ## [22,]  0.07123697
    ## [23,] -1.10865896
    ## [24,]  2.45564964
    ## [25,] -1.28082930
    ## [26,] -0.85629453
    ## [27,]  0.68411481
    ## [28,] -1.07278857
    ## [29,]  1.39921987
    ## [30,] -0.38136545

``` r
puntuacionesfactoriales_acp<- acp1$scores
puntuacionesfactoriales_acp<- as.data.frame(puntuacionesfactoriales_acp)
```

## Método de los ejes principales o componentes principales iterados(CPI)

``` r
cpi1<- fa(datost[,2:7], nfactors = 1, fm = "pa", rotate = "none", n.obs = 30, scores= "regression")
cpi1$scores
```

    ##               PA1
    ##  [1,] -0.88217415
    ##  [2,] -0.08204645
    ##  [3,]  1.01601652
    ##  [4,]  0.46290397
    ##  [5,] -0.80837172
    ##  [6,]  0.23203611
    ##  [7,]  0.92139042
    ##  [8,]  0.01855904
    ##  [9,] -0.84532697
    ## [10,] -0.03602919
    ## [11,]  0.79479340
    ## [12,]  1.60814627
    ## [13,] -1.64693726
    ## [14,] -0.93976302
    ## [15,] -1.17776923
    ## [16,] -0.84404085
    ## [17,]  1.35146349
    ## [18,]  0.50672560
    ## [19,] -0.76621640
    ## [20,] -0.44779123
    ## [21,]  1.36377817
    ## [22,]  0.25957572
    ## [23,] -0.87208772
    ## [24,]  2.10767556
    ## [25,] -1.30615006
    ## [26,] -1.05669002
    ## [27,]  0.70687604
    ## [28,] -0.40659829
    ## [29,]  1.16664215
    ## [30,] -0.39858990

``` r
puntfact_cpi<- cpi1$scores
puntfact_cpi<- as.data.frame(puntfact_cpi)
```

## Método de la máxima verosimilitud (MVE)

``` r
mve1<- fa(datost[,2:7], nfactors = 1, fm = "ml", rotate = "none", n.obs = 30, scores= "regression")
mve1$scores
```

    ##               ML1
    ##  [1,] -1.01774670
    ##  [2,] -0.19744383
    ##  [3,]  1.15128059
    ##  [4,]  0.52949424
    ##  [5,] -0.61508474
    ##  [6,]  0.21636056
    ##  [7,]  1.14976556
    ##  [8,]  0.01618347
    ##  [9,] -0.81086251
    ## [10,]  0.01390343
    ## [11,]  0.74644313
    ## [12,]  1.68031166
    ## [13,] -1.64461333
    ## [14,] -1.01785796
    ## [15,] -1.22617816
    ## [16,] -0.90655332
    ## [17,]  1.36255608
    ## [18,]  0.64110712
    ## [19,] -0.81064899
    ## [20,] -0.49313996
    ## [21,]  1.36658941
    ## [22,]  0.51942185
    ## [23,] -1.01988874
    ## [24,]  1.91031551
    ## [25,] -1.33403321
    ## [26,] -1.02134074
    ## [27,]  0.55030162
    ## [28,] -0.41022637
    ## [29,]  0.97250291
    ## [30,] -0.30091855

``` r
puntfact_mve<- mve1$scores
puntfact_mve<- as.data.frame(puntfact_mve)
```

# Obtención de los factores extraidos

### ACP

``` r
factor.scores(r, acp, method = "Thurstone")
```

    ## $scores
    ## NULL
    ## 
    ## $weights
    ##                                             PC1
    ## Edad                                 0.21738238
    ## Altura(m)                            0.43220277
    ## Peso(Kg)                             0.43929009
    ## Cantidad de veces que come (al día)  0.15269332
    ## Horas que duerme (al día)           -0.01280704
    ## Horas que hace ejercicio (al día)   -0.12076917
    ## 
    ## $r.scores
    ##     PC1
    ## PC1   1
    ## 
    ## $R2
    ## [1] 1

Z1= 0.217Edad + 0.432Altura + 0.439Peso + 0.152Cantidad de veces que
come -0.012Horas que duerme -0.120Horas que hace ejercicio.

### CPI

``` r
factor.scores(r, cpi, method = "Thurstone")
```

    ## $scores
    ## NULL
    ## 
    ## $weights
    ##                                              PA1
    ## Edad                                -0.020932522
    ## Altura(m)                            0.215813759
    ## Peso(Kg)                             0.816097466
    ## Cantidad de veces que come (al día) -0.037976767
    ## Horas que duerme (al día)           -0.009972658
    ## Horas que hace ejercicio (al día)    0.042335432
    ## 
    ## $r.scores
    ##     PA1
    ## PA1   1
    ## 
    ## $R2
    ## [1] 0.9732907

Z1= -0.020Edad + 0.215Altura + 0.816Peso -0.037Cantidad de veces que
come -0.009Horas que duerme +0.042Horas que hace ejercicio.

### MVE

``` r
factor.scores(r, mve, method = "Thurstone")
```

    ## $scores
    ## NULL
    ## 
    ## $weights
    ##                                               ML1
    ## Edad                                 1.691357e-03
    ## Altura(m)                            1.775402e-02
    ## Peso(Kg)                             9.810134e-01
    ## Cantidad de veces que come (al día)  1.270987e-03
    ## Horas que duerme (al día)            1.157308e-05
    ## Horas que hace ejercicio (al día)   -1.075054e-03
    ## 
    ## $r.scores
    ##     ML1
    ## ML1   1
    ## 
    ## $R2
    ## [1] 0.9950826

Z1= 1.691357e-03Edad + 1.775402e-02Altura + 9.810134e-01Peso +
1.270987e-03Cantidad de veces que come + 1.157308e-05Horas que duerme
-1.075054e-03Horas que hace ejercicio.

# Agregar el factor extraido al dataframe original

``` r
datos_puntuaciones<- c(Datos, puntuacionesfactoriales_acp)
datos_puntuaciones<- as.data.frame(datos_puntuaciones)
```
