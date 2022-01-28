
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Visualizador de Fallas de ET

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Visualizador de Fallas de ET es una aplicación web Shiny para el
registro y análisis de fallas que se producen en las salidas de las
estaciones transformadoras de una distribuidora de energía eléctrica.

Los eventos se ingresan en una base de datos (SQLite) para luego ser
analizadas, con el principal objetivo de contabilizar la cantidad de
fallas por salida que se producen en un intervalo de tiempo determinado.
La aplicación permite filtrar por zona geográfica, nivel de tensión y
rango de fechas.

Los resultados se visualizan en un gráfico de barras horizontal y además
puede descargarse un reporte en formato pdf con un resumen de lo
gráficos de mayor interés.

## Instalación

1.  Prerrequisitos

Está aplicación está desarrollada sobre containers Docker, por lo que es
necesario contar con Docker para poder ejecutarla. Los requerimientos y
las instrucciones para su instalación pueden obtenerse en su [sitio web
oficial](https://docs.docker.com/).

2.  Compilar imagen aplicación

Descargar este repositorio y situarse en la carpeta app. Luego compilar
imagen. En este ejemplo se utilizará el nombre epe/visualizadorfallas.

``` bash
cd app
docker build -t epe/visualizadorfallas .
```

3.  Configurar Shinyproxy

La aplicación se ejecuta en el servidor Shinyproxy, que correrá en otra
imagen que será compilada en el punto siguiente. Previo a la
compilación, debe configurarse el servidor Shinyproxy modificando el
archivo application.yml que se encuentra en la carpeta shinyproxy. El
único campo que se debe modificar de forma obligatoria es la ruta
absoluta a la carpeta de la base de datos, en el parámetro
**container-volumes**.

    container-volumes: RUTA-ABSOLUTA-A-MODIFICAR\shinyproxy\db\:/mnt/persistent

Luego, pueden modificarse diversos parámetros, como por ejemplo
credenciales de usuarios, imagen a mostrar, puerto donde se publicará la
interfaz web, etc.

4.  Compilar imagen Shinyproxy

En primer lugar, se debe crear una red para la comunicación de las
imágenes Docker. En este caso se utilizará el nombre epe-fallas-net:

``` bash
docker network create epe-falla-net
docker network ls
```

Luego, se debe compilar la imagen. Se utilizará el nombre
shinyproxy-epe:

``` bash
cd ..
cd shinyproxy
docker build -t shinyproxy-epe .
```

5.  Ejecutar imagen del servidor Shinyproxy

Se deberá ejecutar el siguiente comando. En este caso se utiliza la
etiqueta –restart con el valor unless-stopped, para que se inicie
autimáticamente al iniciar Docker:

``` bash
docker run -d -v /var/run/docker.sock:/var/run/docker.sock --restart unless-stopped --net epe-fallas-net -p 8080:8080 shinyproxy-epe
```

Una vez ejecutado este comando, se puede acceder al servidor desde un
explorador utilizando el puerto seleccionado. Para hacer el login deben
utilizarse las credenciales configuradas en el archivo application.yml
