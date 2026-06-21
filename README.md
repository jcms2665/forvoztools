# Instalación

## Instalar desde GitHub

Si aún no tienes instalado el paquete `remotes`, ejecútalo una sola vez:

```r
install.packages("remotes")
```

Posteriormente instala la versión más reciente de **forvoztools** desde GitHub:

```r
remotes::install_github("jcms2665/forvoztools")
```

## Cargar el paquete

```r
library(forvoztools)
```

## Ver funciones disponibles

```r
ls("package:forvoztools")
```

## Actualizar a la versión más reciente

Si el paquete ya está instalado y deseas descargar la versión más reciente disponible en GitHub:

```r
remotes::install_github(
  "jcms2665/forvoztools",
  force = TRUE
)
```

## Ejemplos

```r
library(forvoztools)

calculate_Cllr(...)

calibrate_LR(...)

ece_plot(...)

plot_ece_funcional(...)

tippett_plot_pmeh_eer(...)
```
