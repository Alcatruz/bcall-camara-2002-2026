# Siete Cámaras en dos dimensiones

B-Call por período legislativo en la Cámara de Diputados de Chile, de 2002-2006 a 2026-2030 (este último con votaciones hasta el 9 de septiembre de 2026).

** [Leer el análisis →](https://alcatruz.github.io/bcall-camara-2002-2026/)**

El post está en `index.html` y se publica con GitHub Pages.

## Contenido

| Ruta | Qué es |
|---|---|
| `index.html` | El post, con gráficos interactivos (Plotly) |
| `datos/datos.js` | Los datos que lee la página. Se genera con `R/03_indicadores.R` |
| `R/01_base.R` | Arma la base de votos nominales por período |
| `R/02_bcall.R` | Estima B-Call, una corrida por período |
| `R/03_indicadores.R` | Calcula los indicadores del post y arma `datos/datos.js` |
| `R/04_seguridad.R` | Clasifica las votaciones de seguridad por título, valida contra los descriptores de la BCN y estima B-Call por separado |
| `R/05_centro.R` | Repite las comparaciones de `d2` entre diputados a igual distancia del centro de la sala |
| `datos/seguridad.js` | Los datos de la sección de seguridad que lee la página |

## Datos y decisiones

- Votaciones y votos nominales de la API de datos abiertos de la Cámara (opendata.camara.cl). Los registros nominales comienzan en marzo de 2002.
- Solo votaciones de proyectos de ley con número de boletín (18.407).
- A favor 1, en contra −1, abstención 0. Pareos, dispensas y ausencias como dato faltante.
- Paquete [bcall](https://github.com/bcallr/bcall) para R, `bcall_auto()` con distancia Manhattan y `threshold = 0.1`.
- Los períodos se definen por fecha (11 de marzo a 10 de marzo).

Los scripts se corren desde la raíz de un proyecto local que guarda los datos crudos ya bajados de la API (`data/votaciones.rds`, `data/votos_nominales.rds`, `data/diputados.rds`, `data/militancias_x_periodo.rds`). Esos archivos no están en este repositorio.

## Referencia

Toro-Maureira, S., Reutter, J., Valenzuela, L., Alcatruz, D. y Valenzuela, M. (2025). B-Call: integrating ideological position and voting cohesion in legislative behavior. *Frontiers in Political Science, 7*, 1670089. https://doi.org/10.3389/fpos.2025.1670089
