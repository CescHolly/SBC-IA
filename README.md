# SBC-IA

Práctica de Sistema Basado en el Conocimiento (SBC) para la asignatura de IA [Ingeniería Informática, FIB, UPC].

## Descripción

Desarrollo de un SBC para la empresa de catering fictícia RicoRico que necesita un recomendador de menús para los eventos que les encargan la gestión de la comida.

El objetivo del recomendador es ofrecer tres opciones de menús dentro de un rango de precios definido por el usuario con precios diferentes (barato, medio y caro). Además las recomendaciones deben cumplir restricciones alimentícias y deben ser personalizadas según las preferencias que indica el usuario, el tipo de evento, la época del año en el que se realiza, etc.

Para más información, en la documentación se encuentra una explicación mucho más detallada del problema.

## Contenidos

- [Documentación](./Documentacion.owl) : Se describe todo el proceso de desarrollo así como las decisiones y la metodología que se ha seguido.
- [Ontología](./Ontologia.owl) : Fichero elaborado con Protegée que recoge la ontologia usada para el SBC.
- [Recomendador](./RicoRico.clp) : Programa en CLIPS que hace las preguntas al usuario y genera las tres recomendaciones de menús.

## Requisitos de instalación

Para ejecutar el programa solo hace falta tener el CLIPS (en Windows, Linux o MacOS).

## Uso

Se debe cargar el programa RicoRico.clp al entorno CLIPS.

Ejecutar:
```
  (reset)
  (run)
```

Contesta las preguntas y el programa te generará 3 menús (o te informará del motivo si no es posible).

## Autores

(Por órden alfabético de apellido)

- [Théo Fuhrmann](https://github.com/theofuhrmann)

- [Xavier Gordillo](https://github.com/GordilloXavi)

- [Francesc Holly](https://github.com/CescHolly)

## Estado del proyecto

Acabado.
