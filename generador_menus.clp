(defclass Bebida "Guarda la información referente a una bebida."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Indica si las bebidas del menu generado son o no alcoholicas.
    (single-slot BebidaAlcoholica
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Precio del producto.
    (single-slot Precio
        (type FLOAT)
        (create-accessor read-write))
    ;;; Nombre identificador de una instancia de la clase.
    (single-slot Nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass RestriccionAlimentaria "Guarda el nombre de restricciones alimentarias de cumplimiento obligatorio, como pueden ser alérgias, intolerancias o que sea vegetariano. La solución propuesta debe contener sólamente ingredientes que cumplan las restricciones indicadas, que se preguntarán al usuario."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Nombre identificador de una instancia de la clase.
    (single-slot Nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Plato "Guarda la información referente a un plato. Esta es la clase alrededor de la qual se envuelve toda la ontología."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Preferencias que el plato satisface.
    (multislot CumplePreferencias
        (type INSTANCE)
        (create-accessor read-write))
    ;;; La complejidad del plato indica si es apropiado o no servirlo para un número elevado de comensales.
    (single-slot Complejidad
        (type INTEGER)
        (create-accessor read-write))
    ;;; Nombre identificador de una instancia de la clase.
    (single-slot Nombre
        (type STRING)
        (create-accessor read-write))
    ;;; Ingredientes de los que se compone el plato.
    (multislot Ingredientes
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Precio del producto.
    (single-slot Precio
        (type FLOAT)
        (create-accessor read-write))
    ;;; Lista de platos no compatibles con el plato.
    (multislot PlatosIncompatibles
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Indica si el plato es considerado un primero, un segundo, ambos, o un postre.
    (multislot TipoEnMenu
        (type STRING)
        (create-accessor read-write))
    ;;; Descripción general del plato.
    (single-slot InfoGeneral
        (type STRING)
        (create-accessor read-write))
    ;;; Bebidas que acompañan bien al plato.
    (multislot BebidasRecomendadas
        (type INSTANCE)
        (create-accessor read-write))
)

(defclass Menu "Recoge la información de la construcción de un menú hecha por el programa, una instancia representa una posible parte de la solución que se enseña al usuario. Inicialmente no tiene instancias."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Lista de bebidas del menú propuesto: una por plato si así lo ha pedido el usuario o sino sólo una.
    (multislot Bebidas
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Primer plato del menú.
    (single-slot Primero
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Indica el nivel de satisfacción de preferencias del usuario que tiene el menú.
    (single-slot PuntuacionMenu
        (type INTEGER)
        (create-accessor read-write))
    ;;; Precio del producto.
    (single-slot Precio
        (type FLOAT)
        (create-accessor read-write))
    ;;; Plato de postre del menú.
    (single-slot Postre
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Segundo plato del menú.
    (single-slot Segundo
        (type SYMBOL)
        (create-accessor read-write))
)

(defclass DatosEvento "Recoge los datos proporcionados por el usuario sobre el evento que se quiere planificar."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Temporada de la fecha en la que ocurre el evento.
    (single-slot TemporadaActual
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Lista de restricciones alimentarias que el usuario especifica para el evento.
    (multislot Restricciones
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Numero de personas que atenderán al evento.
    (single-slot NumeroComensales
        (type INTEGER)
        (create-accessor read-write))
    ;;; Lista de preferencias alimentarias que el usuario especifica para el evento.
    (multislot Preferencias
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Indica si las bebidas del menu generado son o no alcoholicas.
    (single-slot BebidaAlcoholica
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Lista de ingredientes que no pueden estar presentes en los menús propuestos para el evento.
    (multislot IngredientesProhibidos
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Máximo precio que el usuario está dispuesto a pagar por un menú para el evento.
    (single-slot PrecioMax
        (type FLOAT)
        (create-accessor read-write))
    ;;; Indica si se trata de un evento familiar o congreso.
    (multislot TipoEvento
        (type STRING)
        (create-accessor read-write))
    ;;; Indica si se quiere tener una bebida para cada uno de los platos, o solo una para todo el menú del evento.
    (single-slot BebidaPorPlatos
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Mínimo precio que el usuario está dispuesto a pagar por un menú para el evento.
    (single-slot PrecioMin
        (type FLOAT)
        (create-accessor read-write))
)

(defclass Ingrediente "Guarda la información referente a un ingrediente."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Lista de temporadas donde el producto está disponible.
    (multislot Disponibilidad
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Nombre identificador de una instancia de la clase.
    (single-slot Nombre
        (type STRING)
        (create-accessor read-write))
    ;;; Restricciones alimentarias que cumple el ingrediente.
    (multislot CumpleRestricciones
        (type INSTANCE)
        (create-accessor read-write))
)

(defclass Temporada "Guarda el nombre de una temporada concreta. Hay un conjunto de instancias iniciales que cubren todas las temporadas en las que funciona el restaurante."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Nombre identificador de una instancia de la clase.
    (single-slot Nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Preferencia "Guarda nombres de preferencias alimentarias. Los menús generados intentarán satisfacer (no obligadamente) el mayor número de éstas. Las instancias de esta clase estan definidas inicialmente y se pregunta al usuario que escoja un subconjunto de éstas para personalizar los menús."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Nombre identificador de una instancia de la clase.
    (single-slot Nombre
        (type STRING)
        (create-accessor read-write))
)

(definstances instances
    ([salsa_de_soja] of Ingrediente
         (Disponibilidad  [primavera] [verano] [invierno] [otoño])
         (Nombre  "Salsa de soja")
         (CumpleRestricciones  [vegetariano] [vegano] [intolerancia_gluten] [intolerancia_lactosa])
    )

    ([atún] of Ingrediente
         (Disponibilidad  [invierno] [verano] [primavera] [otoño])
         (Nombre  "Atún")
         (CumpleRestricciones  [intolerancia_gluten] [intolerancia_lactosa])
    )

    ([Agua_con_gas] of Bebida
         (BebidaAlcoholica  "false")
         (Precio  1.60)
         (Nombre  "Agua con gas")
    )

    ([fresa] of Ingrediente
         (Disponibilidad  [verano] [primavera])
         (Nombre  "Fresas")
         (CumpleRestricciones  [vegano] [intolerancia_gluten] [intolerancia_lactosa] [vegetariano])
    )

    ([Revuelto_de_espinacas_y_langostinos] of Plato
         (CumplePreferencias  [sibarita] [moderno])
         (Complejidad  25)
         (Nombre  "Revuelto de espinacas y langostinos")
         (Ingredientes  [aceite_de_oliva] [ajo] [langostino] [guindilla] [queso] [sal] [huevo] [espinaca])
         (Precio  13.50)
         (TipoEnMenu  "primero")
         (InfoGeneral  "Plato caliente de verdura y pescado")
         (BebidasRecomendadas  [Agua] [Cava_Anna_de_Codorniu] [Copa_de_vino_rosado_Los_Frailes] [Copa_de_vino_tinto_Vivanco])
    )

    ([huevo] of Ingrediente
         (Disponibilidad  [verano] [invierno] [primavera] [otoño])
         (Nombre  "Huevo")
         (CumpleRestricciones  [intolerancia_gluten] [vegetariano] [intolerancia_lactosa])
    )

    ([col] of Ingrediente
         (Disponibilidad  [primavera] [verano] [invierno] [otoño])
         (Nombre  "Col")
         (CumpleRestricciones  [vegano] [intolerancia_lactosa] [intolerancia_gluten] [vegetariano])
    )

    ([Macarrones_con_chorizo] of Plato
         (CumplePreferencias  [clásico] [regional])
         (Complejidad  30)
         (Nombre  "Macarrones con chorizo")
         (Ingredientes  [huevo] [pasta] [sal] [tomate] [agua] [aceite_de_oliva] [queso] [chorizo])
         (Precio  10.50)
         (TipoEnMenu  "primero")
         (InfoGeneral  "Plato caliente de pasta y carne")
         (BebidasRecomendadas  [Agua] [Refresco_Nestea] [Copa_de_sangría] [Cerveza_Estrella_Damm] [Copa_de_vino_tinto_Fuenteseca] [Refresco_Aquarius])
    )

    ([vinagre] of Ingrediente
         (Disponibilidad  [invierno] [verano] [primavera] [otoño])
         (Nombre  "Vinagre")
    )

    ([invierno] of Temporada
         (Nombre  "Invierno")
    )

    ([granada] of Ingrediente
         (Disponibilidad  [invierno])
         (Nombre  "Granada")
         (CumpleRestricciones  [vegano] [vegetariano] [intolerancia_lactosa] [intolerancia_gluten])
    )

    ([ajo] of Ingrediente
         (Disponibilidad  [otoño] [invierno] [verano] [primavera])
         (Nombre  "Ajo")
         (CumpleRestricciones  [vegetariano] [intolerancia_gluten] [vegano] [intolerancia_lactosa])
    )

    ([vegetariano] of RestriccionAlimentaria
         (Nombre  "Vegetariano/a")
    )

    ([pera] of Ingrediente
         (Disponibilidad  [verano] [primavera] [otoño] [invierno])
         (Nombre  "Pera")
         (CumpleRestricciones  [vegano] [intolerancia_gluten] [vegetariano] [intolerancia_lactosa])
    )

    ([pimienta] of Ingrediente
         (Disponibilidad  [invierno] [otoño] [primavera] [verano])
         (Nombre  "Pimienta")
         (CumpleRestricciones  [intolerancia_lactosa] [intolerancia_gluten] [vegano] [vegetariano])
    )

    ([Cava_Anna_de_Codorniu] of Bebida
         (BebidaAlcoholica  "true")
         (Precio  19.00)
         (Nombre  "Cava Anna de Codorniu")
    )

    ([Pasta_al_pesto] of Plato
         (CumplePreferencias  [clásico])
         (Complejidad  25)
         (Nombre  "Pasta al pesto")
         (Ingredientes  [sal] [frutos_secos] [pasta] [perejil] [aceite_de_oliva] [albahaca] [ajo])
         (Precio  8.75)
         (TipoEnMenu  "primero")
         (InfoGeneral  "Plato vegano caliente de pasta")
         (BebidasRecomendadas  [Agua] [Copa_de_sangría] [Agua_con_gas] [Copa_de_vino_rosado_Los_Frailes])
    )

    ([pimentón_dulce] of Ingrediente
         (Disponibilidad  [otoño] [verano] [invierno] [primavera])
         (Nombre  "Pimentón dulce")
         (CumpleRestricciones  [vegetariano] [intolerancia_gluten] [intolerancia_lactosa] [vegano])
    )

    ([primavera] of Temporada
         (Nombre  "Primavera")
    )

    ([Tacos_de_hummus_con_aguacate] of Plato
         (CumplePreferencias  [moderno] [sibarita])
         (Complejidad  15)
         (Nombre  "Tacos de hummus con aguacate")
         (Ingredientes  [tortillas] [lechuga] [pepino] [aguacate] [aceite_de_oliva] [hummus] [sal])
         (Precio  10.75)
         (TipoEnMenu  "segundo")
         (InfoGeneral  "Plato frío vegano")
         (BebidasRecomendadas  [Copa_de_vino_blanco_Penedès] [Zumo_natural] [Copa_de_vino_tinto_Fuenteseca] [Agua])
    )

    ([Cerveza_Moritz] of Bebida
         (BebidaAlcoholica  "true")
         (Precio  3.50)
         (Nombre  "Cerveza Moritz")
    )

    ([Risotto_de_setas] of Plato
         (CumplePreferencias  [moderno] [sibarita])
         (Complejidad  60)
         (Nombre  "Risotto de setas")
         (Ingredientes  [sal] [cebolla] [seta] [caldo_de_verduras] [ajo] [pimienta] [vino_blanco] [arroz])
         (Precio  14.00)
         (PlatosIncompatibles  [Risotto_de_setas])
         (TipoEnMenu  "primero_segundo")
         (InfoGeneral  "Plato vegano caliente de pasta")
         (BebidasRecomendadas  [Agua] [Copa_de_sangría] [Agua_con_gas] [Copa_de_vino_blanco_Empordà])
    )

    ([melón] of Ingrediente
         (Disponibilidad  [otoño] [primavera] [verano])
         (Nombre  "Melón")
         (CumpleRestricciones  [intolerancia_lactosa] [intolerancia_gluten] [vegano] [vegetariano])
    )

    ([chocolate] of Ingrediente
         (Disponibilidad  [primavera] [otoño] [invierno] [verano])
         (Nombre  "Chocolate")
         (CumpleRestricciones  [vegetariano] [intolerancia_gluten])
    )

    ([pasta] of Ingrediente
         (Disponibilidad  [primavera] [verano] [otoño] [invierno])
         (Nombre  "Pasta")
         (CumpleRestricciones  [vegano] [vegetariano] [intolerancia_lactosa])
    )

    ([Entrecot_a_la_plancha_con_salteado_de_verduras] of Plato
         (CumplePreferencias  [sibarita] [clásico])
         (Complejidad  55)
         (Nombre  "Entrecot a la plancha con salteado de verduras")
         (Ingredientes  [calabacín] [sal] [ajo] [cebolla] [aceite_de_oliva] [pimiento_verde] [mantequilla] [patata] [pimiento_rojo] [tomate] [ternera])
         (Precio  23.50)
         (TipoEnMenu  "segundo")
         (InfoGeneral  "Plato caliente de carne")
         (BebidasRecomendadas  [Agua_con_gas] [Copa_de_vino_tinto_Vivanco] [Agua] [Cava_Anna_de_Codorniu])
    )

    ([seta] of Ingrediente
         (Disponibilidad  [otoño])
         (Nombre  "Seta")
         (CumpleRestricciones  [intolerancia_gluten] [intolerancia_lactosa] [vegano] [vegetariano])
    )

    ([aguacate] of Ingrediente
         (Disponibilidad  [otoño] [verano] [invierno] [primavera])
         (Nombre  "Aguacate")
         (CumpleRestricciones  [vegano] [intolerancia_lactosa] [vegetariano] [intolerancia_gluten])
    )

    ([perejil] of Ingrediente
         (Disponibilidad  [otoño] [verano] [invierno] [primavera])
         (Nombre  "Perejil")
         (CumpleRestricciones  [vegano] [intolerancia_gluten] [intolerancia_lactosa] [vegetariano])
    )

    ([canela] of Ingrediente
         (Disponibilidad  [invierno] [verano] [otoño] [primavera])
         (Nombre  "Canela")
         (CumpleRestricciones  [vegano] [intolerancia_gluten] [intolerancia_lactosa] [vegetariano])
    )

    ([otoño] of Temporada
         (Nombre  "Otoño")
    )

    ([jamón] of Ingrediente
         (Disponibilidad  [invierno] [otoño] [primavera] [verano])
         (Nombre  "Jamón")
         (CumpleRestricciones  [intolerancia_gluten] [intolerancia_lactosa])
    )

    ([Mariscada_de_calamares_y_langostinos] of Plato
         (CumplePreferencias  [sibarita] [clásico])
         (Complejidad  40)
         (Nombre  "Mariscada de calamares y langostinos")
         (Ingredientes  [perejil] [romero] [aceite_de_oliva] [sal] [cebolla] [langostino] [vino_blanco] [calamar])
         (Precio  25.00)
         (TipoEnMenu  "segundo")
         (InfoGeneral  "Plato frío de pescado")
         (BebidasRecomendadas  [Champagne_Renard-Barnier] [Agua] [Cava_Anna_de_Codorniu] [Copa_de_vino_tinto_Vivanco])
    )

    ([Poke_bowl_vegano] of Plato
         (CumplePreferencias  [moderno])
         (Complejidad  35)
         (Nombre  "Poke bowl vegano")
         (Ingredientes  [soja_texturizada] [tomate] [arroz] [aguacate] [salsa_de_soja] [cebolla])
         (Precio  10.00)
         (PlatosIncompatibles  [Poke_bowl_vegano])
         (TipoEnMenu  "primero_segundo")
         (InfoGeneral  "Plato vegano frío de arroz y verdura")
         (BebidasRecomendadas  [Copa_de_vino_rosado_Los_Frailes] [Zumo_natural] [Agua] [Agua_con_gas])
    )

    ([frutos_secos] of Ingrediente
         (Disponibilidad  [otoño] [verano] [invierno] [primavera])
         (Nombre  "Frutos secos")
         (CumpleRestricciones  [vegetariano] [intolerancia_gluten] [intolerancia_lactosa] [vegano])
    )

    ([Copa_de_vino_blanco_Empordà] of Bebida
         (BebidaAlcoholica  "true")
         (Precio  4.50)
         (Nombre  "Copa de vino blanco D.O. Empordà")
    )

    ([Hamburguesa_vegana_de_lentejas] of Plato
         (CumplePreferencias  [moderno])
         (Complejidad  50)
         (Nombre  "Hamburguesa vegana de lentejas")
         (Ingredientes  [arroz] [lentejas] [tomate] [cebolla] [sal] [frutos_secos] [patata] [ajo])
         (Precio  12.50)
         (PlatosIncompatibles  [Hamburguesa_vegana_de_lentejas])
         (TipoEnMenu  "primero_segundo")
         (InfoGeneral  "Plato caliente vegano")
         (BebidasRecomendadas  [Agua_con_gas] [Refresco_Fanta] [Refresco_Coca_Cola] [Refresco_Nestea] [Cerveza_Estrella_Damm] [Agua])
    )

    ([Salmón_al_horno_con_patata] of Plato
         (CumplePreferencias  [sibarita] [moderno])
         (Complejidad  45)
         (Nombre  "Salmón_al_horno_con_patata" "Salmón al horno con patata")
         (Ingredientes  [sal] [cebolla] [salmón] [patata] [ajo] [aceite_de_oliva])
         (Precio  13.00)
         (TipoEnMenu  "segundo")
         (InfoGeneral  "Plato caliente de pescado")
         (BebidasRecomendadas  [Copa_de_vino_rosado_Los_Frailes] [Copa_de_vino_blanco_Empordà] [Agua] [Cava_Anna_de_Codorniu])
    )

    ([calamar] of Ingrediente
         (Disponibilidad  [invierno] [primavera] [otoño])
         (Nombre  "Calamar")
         (CumpleRestricciones  [intolerancia_lactosa] [intolerancia_gluten])
    )

    ([pollo] of Ingrediente
         (Disponibilidad  [invierno] [otoño] [primavera] [verano])
         (Nombre  "Pollo")
         (CumpleRestricciones  [intolerancia_gluten] [intolerancia_lactosa])
    )

    ([uva] of Ingrediente
         (Disponibilidad  [invierno] [primavera] [otoño] [verano])
         (Nombre  "Uvas")
         (CumpleRestricciones  [intolerancia_lactosa] [intolerancia_gluten] [vegetariano] [vegano])
    )

    ([Berenjenas_rellenas_gratinadas] of Plato
         (CumplePreferencias  [moderno] [regional])
         (Complejidad  60)
         (Nombre  "Berenjenas rellenas gratinadas")
         (Ingredientes  [atún] [berenjena] [queso] [calabacín] [aceite_de_oliva] [sal] [cebolla])
         (Precio  14.00)
         (TipoEnMenu  "segundo")
         (InfoGeneral  "Plato caliente de pescado y verdura")
         (BebidasRecomendadas  [Copa_de_vino_rosado_Los_Frailes] [Copa_de_vino_blanco_Penedès] [Agua] [Copa_de_sangría])
    )

    ([guindilla] of Ingrediente
         (Disponibilidad  [primavera] [invierno] [verano] [otoño])
         (Nombre  "Guindilla")
         (CumpleRestricciones  [intolerancia_gluten] [vegano] [vegetariano] [intolerancia_lactosa])
    )

    ([judía] of Ingrediente
         (Disponibilidad  [primavera] [invierno] [verano] [otoño])
         (Nombre  "Judía")
         (CumpleRestricciones  [intolerancia_lactosa] [vegano] [vegetariano] [intolerancia_gluten])
    )

    ([plátano] of Ingrediente
         (Disponibilidad  [verano] [invierno] [otoño] [primavera])
         (Nombre  "Plátano")
         (CumpleRestricciones  [vegano] [intolerancia_lactosa] [vegetariano] [intolerancia_gluten])
    )

    ([albahaca] of Ingrediente
         (Disponibilidad  [primavera] [otoño] [verano] [invierno])
         (Nombre  "Albahaca")
         (CumpleRestricciones  [intolerancia_lactosa] [intolerancia_gluten] [vegetariano] [vegano])
    )

    ([cereza] of Ingrediente
         (Disponibilidad  [verano])
         (Nombre  "Cerezas")
         (CumpleRestricciones  [vegano] [vegetariano] [intolerancia_lactosa] [intolerancia_gluten])
    )

    ([vino_blanco] of Ingrediente
         (Disponibilidad  [otoño] [invierno] [verano] [primavera])
         (Nombre  "Vino blanco")
         (CumpleRestricciones  [vegetariano] [intolerancia_lactosa] [vegano] [intolerancia_gluten])
    )

    ([almidón] of Ingrediente
         (Disponibilidad  [invierno] [primavera] [otoño] [verano])
         (Nombre  "Almidón")
         (CumpleRestricciones  [vegano] [vegetariano] [intolerancia_lactosa])
    )

    ([pimiento_rojo] of Ingrediente
         (Disponibilidad  [primavera] [otoño] [verano] [invierno])
         (Nombre  "Pimiento rojo")
         (CumpleRestricciones  [intolerancia_gluten] [intolerancia_lactosa] [vegetariano] [vegano])
    )

    ([Cerveza_Estrella_Galicia_1906] of Bebida
         (BebidaAlcoholica  "true")
         (Precio  3.00)
         (Nombre  "Cerveza Estrella Galicia 1906")
    )

    ([Refresco_Fanta] of Bebida
         (BebidaAlcoholica  "false")
         (Precio  2.20)
         (Nombre  "Refresco Fanta")
    )

    ([salmón] of Ingrediente
         (Disponibilidad  [primavera] [invierno] [verano] [otoño])
         (Nombre  "Salmón")
         (CumpleRestricciones  [intolerancia_lactosa] [intolerancia_gluten])
    )

    ([leche] of Ingrediente
         (Disponibilidad  [otoño] [verano] [invierno] [primavera])
         (Nombre  "Leche")
         (CumpleRestricciones  [vegetariano] [intolerancia_gluten])
    )

    ([Macedonia] of Plato
         (Complejidad  15)
         (Nombre  "Macedonia")
         (Ingredientes  [uva] [pera] [granada] [manzana] [naranja] [plátano])
         (Precio  8.00)
         (TipoEnMenu  "postre")
         (InfoGeneral  "Macedonia de frutas de temporada")
    )

    ([Zumo_natural] of Bebida
         (BebidaAlcoholica  "false")
         (Precio  3.00)
         (Nombre  "Zumo natural")
    )

    ([zanahoria] of Ingrediente
         (Disponibilidad  [otoño] [invierno] [verano] [primavera])
         (Nombre  "Zanahoria")
         (CumpleRestricciones  [vegano] [intolerancia_gluten] [vegetariano] [intolerancia_lactosa])
    )

    ([ñoquis] of Ingrediente
         (Disponibilidad  [otoño] [verano] [invierno] [primavera])
         (Nombre  "Ñoquis")
         (CumpleRestricciones  [vegetariano] [intolerancia_lactosa] [intolerancia_gluten] [vegano])
    )

    ([cacao] of Ingrediente
         (Disponibilidad  [verano] [otoño] [primavera] [invierno])
         (Nombre  "Cacao")
         (CumpleRestricciones  [vegetariano] [intolerancia_gluten] [vegano] [intolerancia_lactosa])
    )

    ([arroz] of Ingrediente
         (Disponibilidad  [verano] [otoño] [primavera] [invierno])
         (Nombre  "Arroz")
         (CumpleRestricciones  [intolerancia_lactosa] [intolerancia_gluten] [vegano] [vegetariano])
    )

    ([Espaguetis_con_mozzarella_albahaca_y_tomate] of Plato
         (CumplePreferencias  [clásico])
         (Complejidad  20)
         (Nombre  "Espaguetis con mozzarella albahaca y tomate")
         (Ingredientes  [orégano] [queso] [aceite_de_oliva] [sal] [albahaca] [pasta] [tomate])
         (Precio  11.25)
         (TipoEnMenu  "primero")
         (InfoGeneral  "Plato vegetariano frío de pasta")
         (BebidasRecomendadas  [Agua] [Refresco_Aquarius] [Cerveza_Estrella_Galicia_1906])
    )

    ([Melón] of Plato
         (CumplePreferencias  [clásico])
         (Complejidad  4)
         (Nombre  "Melón")
         (Ingredientes  [melón])
         (Precio  3.00)
         (TipoEnMenu  "postre")
         (InfoGeneral  "Fruta de temporada")
    )

    ([agua] of Ingrediente
         (Disponibilidad  [invierno] [verano] [primavera] [otoño])
         (Nombre  "Agua")
    )

    ([Cerveza_Estrella_Damm] of Bebida
         (BebidaAlcoholica  "true")
         (Precio  2.50)
         (Nombre  "Cerveza Estrella Damm")
    )

    ([Albóndigas_con_champiñones] of Plato
         (CumplePreferencias  [clásico] [regional])
         (Complejidad  75)
         (Nombre  "Albóndigas con champiñones")
         (Ingredientes  [aceite_de_oliva] [agua] [ternera] [cebolla] [sal] [huevo] [seta] [harina] [ajo] [perejil])
         (Precio  12.00)
         (TipoEnMenu  "segundo")
         (InfoGeneral  "Plato caliente de carne")
         (BebidasRecomendadas  [Agua] [Cerveza_Moritz] [Agua_con_gas] [Copa_de_vino_tinto_Fuenteseca])
    )

    ([Champagne_Renard-Barnier] of Bebida
         (BebidaAlcoholica  "true")
         (Precio  27.00)
         (Nombre  "Champagne Renard-Barnier")
    )

    ([Copa_de_vino_tinto_Fuenteseca] of Bebida
         (BebidaAlcoholica  "true")
         (Precio  3.00)
         (Nombre  "Copa de vino tinto Fuenteseca")
    )

    ([langostino] of Ingrediente
         (Disponibilidad  [invierno] [verano] [primavera] [otoño])
         (Nombre  "Langostino")
    )

    ([sibarita] of Preferencia
         (Nombre  "Estilo sibarita")
    )

    ([espinaca] of Ingrediente
         (Disponibilidad  [invierno] [verano] [otoño] [primavera])
         (Nombre  "Espinacas")
         (CumpleRestricciones  [vegano] [intolerancia_gluten] [intolerancia_lactosa] [vegetariano])
    )

    ([pimiento_verde] of Ingrediente
         (Disponibilidad  [invierno] [verano] [otoño] [primavera])
         (Nombre  "Pimiento verde")
    )

    ([Refresco_Coca_Cola] of Bebida
         (BebidaAlcoholica  "false")
         (Precio  2.20)
         (Nombre  "Refresco Coca Cola")
    )

    ([chorizo] of Ingrediente
         (Disponibilidad  [otoño] [invierno] [verano] [primavera])
         (Nombre  "Chorizo")
         (CumpleRestricciones  [intolerancia_gluten] [intolerancia_lactosa])
    )

    ([orégano] of Ingrediente
         (Disponibilidad  [verano] [invierno] [primavera] [otoño])
         (Nombre  "Orégano")
         (CumpleRestricciones  [intolerancia_gluten] [intolerancia_lactosa] [vegano] [vegetariano])
    )

    ([berenjena] of Ingrediente
         (Disponibilidad  [invierno] [otoño])
         (Nombre  "Berenjena")
         (CumpleRestricciones  [vegetariano] [vegano] [intolerancia_gluten] [intolerancia_lactosa])
    )

    ([Melón_con_jamón] of Plato
         (CumplePreferencias  [regional])
         (Complejidad  5)
         (Nombre  "Melón con jamón")
         (Ingredientes  [melón] [jamón])
         (Precio  6.50)
         (TipoEnMenu  "primero")
         (InfoGeneral  "Plato frío de fruta y jamón")
         (BebidasRecomendadas  [Zumo_natural] [Agua_con_gas] [Agua] [Copa_de_vino_blanco_Penedès])
    )

    ([Ñoquis_con_kale_y_frutos_secos] of Plato
         (CumplePreferencias  [moderno] [regional])
         (Complejidad  18)
         (Nombre  "Ñoquis con kale y frutos secos")
         (Ingredientes  [sal] [frutos_secos] [ñoquis] [col] [ajo] [aceite_de_oliva])
         (Precio  11.50)
         (PlatosIncompatibles  [Ñoquis_con_kale_y_frutos_secos])
         (TipoEnMenu  "primero_segundo")
         (InfoGeneral  "Plato caliente vegano")
         (BebidasRecomendadas  [Cerveza_Moritz] [Agua_con_gas] [Copa_de_sangría] [Agua] [Copa_de_vino_blanco_Penedès])
    )

    ([ternera] of Ingrediente
         (Disponibilidad  [invierno] [primavera] [otoño] [verano])
         (Nombre  "Ternera")
         (CumpleRestricciones  [intolerancia_lactosa] [intolerancia_gluten])
    )

    ([tomate] of Ingrediente
         (Disponibilidad  [otoño] [verano] [invierno] [primavera])
         (Nombre  "Tomate")
    )

    ([Refresco_Aquarius] of Bebida
         (BebidaAlcoholica  "false")
         (Precio  2.20)
         (Nombre  "Refresco Aquarius")
    )

    ([vegano] of RestriccionAlimentaria
         (Nombre  "Vegano/a")
    )

    ([Butifarra_con_judías] of Plato
         (CumplePreferencias  [regional] [clásico])
         (Complejidad  40)
         (Nombre  "Butifarra con judías")
         (Ingredientes  [aceite_de_oliva] [sal] [judía] [ajo] [perejil])
         (Precio  11.50)
         (TipoEnMenu  "segundo")
         (InfoGeneral  "Plato caliente de carne")
         (BebidasRecomendadas  [Copa_de_vino_tinto_Vivanco] [Refresco_Nestea] [Cerveza_Estrella_Damm] [Agua] [Agua_con_gas])
    )

    ([caldo_de_verduras] of Ingrediente
         (Disponibilidad  [invierno] [primavera] [otoño] [verano])
         (Nombre  "Caldo de verduras")
         (CumpleRestricciones  [intolerancia_gluten] [vegano] [vegetariano] [intolerancia_lactosa])
    )

    ([Cerezas] of Plato
         (CumplePreferencias  [clásico])
         (Complejidad  3)
         (Nombre  "Cerezas")
         (Ingredientes  [cereza])
         (Precio  4.00)
         (TipoEnMenu  "postre")
         (InfoGeneral  "Fruta de temporada")
    )

    ([intolerancia_lactosa] of RestriccionAlimentaria
         (Nombre  "Intolerancia a la lactosa")
    )

    ([Refresco_Nestea] of Bebida
         (BebidaAlcoholica  "false")
         (Precio  2.20)
         (Nombre  "Refresco Nestea")
    )

    ([Brownie] of Plato
         (Complejidad  35)
         (Nombre  "Brownie")
         (Ingredientes  [chocolate] [harina] [azúcar] [huevo] [frutos_secos] [mantequilla] [sal])
         (Precio  8.00)
         (TipoEnMenu  "postre")
         (InfoGeneral  "Brownie casero con nueces y chocolate")
    )

    ([Ensalada_griega] of Plato
         (CumplePreferencias  [moderno])
         (Complejidad  32)
         (Nombre  "Ensalada griega")
         (Ingredientes  [aceituna] [atún] [vinagre] [queso] [romero] [aceite_de_oliva] [pimiento_verde] [orégano] [pepino] [pasta] [pimiento_rojo] [sal] [cebolla])
         (Precio  7.75)
         (TipoEnMenu  "primero")
         (InfoGeneral  "Plato frio de verdura y pasta")
         (BebidasRecomendadas  [Copa_de_vino_tinto_Vivanco] [Copa_de_sangría] [Agua] [Copa_de_vino_blanco_Penedès])
    )

    ([manzana] of Ingrediente
         (Disponibilidad  [invierno] [otoño] [primavera] [verano])
         (Nombre  "Manzana")
         (CumpleRestricciones  [intolerancia_gluten] [vegetariano] [vegano] [intolerancia_lactosa])
    )

    ([harina] of Ingrediente
         (Disponibilidad  [verano] [invierno] [otoño] [primavera])
         (Nombre  "Harina")
         (CumpleRestricciones  [vegano] [intolerancia_lactosa] [vegetariano])
    )

    ([clásico] of Preferencia
         (Nombre  "Estilo clásico")
    )

    ([Copa_de_sangría] of Bebida
         (BebidaAlcoholica  "true")
         (Precio  4.00)
         (Nombre  "Copa de Sangría")
    )

    ([sal] of Ingrediente
         (Disponibilidad  [verano] [invierno] [otoño] [primavera])
         (Nombre  "Sal")
    )

    ([lechuga] of Ingrediente
         (Disponibilidad  [otoño] [verano] [invierno] [primavera])
         (Nombre  "Lechuga")
         (CumpleRestricciones  [vegetariano] [intolerancia_gluten] [intolerancia_lactosa] [vegano])
    )

    ([tortillas] of Ingrediente
         (Disponibilidad  [otoño] [verano] [invierno] [primavera])
         (Nombre  "Tortillas de tacos")
         (CumpleRestricciones  [vegetariano] [intolerancia_lactosa] [vegano])
    )

    ([queso] of Ingrediente
         (Disponibilidad  [primavera] [invierno] [verano] [otoño])
         (Nombre  "Queso")
         (CumpleRestricciones  [vegetariano] [intolerancia_gluten])
    )

    ([pepino] of Ingrediente
         (Disponibilidad  [primavera] [otoño] [invierno] [verano])
         (Nombre  "Pepino")
         (CumpleRestricciones  [intolerancia_gluten] [vegano] [vegetariano] [intolerancia_lactosa])
    )

    ([naranja] of Ingrediente
         (Disponibilidad  [primavera] [otoño] [invierno] [verano])
         (Nombre  "Naranja")
         (CumpleRestricciones  [intolerancia_lactosa] [intolerancia_gluten] [vegetariano] [vegano])
    )

    ([moderno] of Preferencia
         (Nombre  "Estilo moderno")
    )

    ([Canelones_de_pollo] of Plato
         (CumplePreferencias  [regional] [clásico])
         (Complejidad  75)
         (Nombre  "Canelones de pollo")
         (Ingredientes  [queso] [pollo] [harina] [ajo] [cebolla] [sal] [pasta] [leche] [mantequilla] [aceite_de_oliva] [pimentón_dulce] [tomate])
         (Precio  14.50)
         (PlatosIncompatibles  [Canelones_de_pollo])
         (TipoEnMenu  "primero_segundo")
         (InfoGeneral  "Plato caliente de pasta y carne")
         (BebidasRecomendadas  [Copa_de_sangría] [Copa_de_vino_tinto_Fuenteseca] [Agua] [Cerveza_Moritz] [Copa_de_vino_rosado_Los_Frailes])
    )

    ([romero] of Ingrediente
         (Disponibilidad  [verano] [invierno] [primavera] [otoño])
         (Nombre  "Romero")
    )

    ([intolerancia_gluten] of RestriccionAlimentaria
         (Nombre  "Intolerancia al gluten")
    )

    ([Batido_de_fresa_y_plátano] of Plato
         (CumplePreferencias  [clásico])
         (Complejidad  6)
         (Nombre  "Batido de fresa y plátano")
         (Ingredientes  [plátano] [leche] [fresa])
         (Precio  7.00)
         (TipoEnMenu  "postre")
         (InfoGeneral  "Batido fresco de fresa y plátano")
    )

    ([Copa_de_vino_tinto_Vivanco] of Bebida
         (BebidaAlcoholica  "true")
         (Precio  4.00)
         (Nombre  "Copa de vino tinto Vivanco")
    )

    ([mantequilla] of Ingrediente
         (Disponibilidad  [verano] [otoño] [primavera] [invierno])
         (Nombre  "Mantequilla")
         (CumpleRestricciones  [intolerancia_gluten] [vegetariano])
    )

    ([Tiramisú] of Plato
         (Complejidad  35)
         (Nombre  "Tiramisú")
         (Ingredientes  [almidón] [cacao] [café] [sal] [harina] [azúcar] [huevo] [queso])
         (Precio  8.00)
         (TipoEnMenu  "postre")
         (InfoGeneral  "Tiramisú casero")
    )

    ([calabacín] of Ingrediente
         (Disponibilidad  [verano] [invierno] [otoño] [primavera])
         (Nombre  "Calabacín")
         (CumpleRestricciones  [vegano] [intolerancia_gluten] [intolerancia_lactosa] [vegetariano])
    )

    ([lentejas] of Ingrediente
         (Disponibilidad  [verano] [invierno] [otoño] [primavera])
         (Nombre  "Lentejas")
         (CumpleRestricciones  [vegano] [intolerancia_gluten] [intolerancia_lactosa] [vegetariano])
    )

    ([azúcar] of Ingrediente
         (Disponibilidad  [invierno] [otoño] [primavera] [verano])
         (Nombre  "Azúcar")
         (CumpleRestricciones  [intolerancia_gluten] [vegano] [intolerancia_lactosa] [vegetariano])
    )

    ([Ensalada_de_queso_de_cabra_y_nueces] of Plato
         (CumplePreferencias  [moderno])
         (Complejidad  20)
         (Nombre  "Ensalada de queso de cabra y nueces")
         (Ingredientes  [vinagre] [lechuga] [frutos_secos] [queso] [aceite_de_oliva] [cebolla] [sal] [zanahoria])
         (Precio  8.50)
         (TipoEnMenu  "primero")
         (InfoGeneral  "Plato vegetariano frío de verdura y queso")
         (BebidasRecomendadas  [Copa_de_vino_tinto_Vivanco] [Copa_de_vino_blanco_Empordà] [Agua])
    )

    ([café] of Ingrediente
         (Disponibilidad  [invierno] [primavera] [otoño] [verano])
         (Nombre  "Café")
         (CumpleRestricciones  [vegano] [vegetariano] [intolerancia_lactosa] [intolerancia_gluten])
    )

    ([aceite_de_oliva] of Ingrediente
         (Disponibilidad  [primavera] [verano] [invierno] [otoño])
         (Nombre  "Aceite de oliva virgen extra")
         (CumpleRestricciones  [vegano] [vegetariano] [intolerancia_gluten] [intolerancia_lactosa])
    )

    ([Flan_de_huevo] of Plato
         (Complejidad  35)
         (Nombre  "Flan de huevo")
         (Ingredientes  [canela] [leche] [azúcar] [huevo])
         (Precio  8.00)
         (TipoEnMenu  "postre")
         (InfoGeneral  "Flan casero de huevo con canela")
    )

    ([Agua] of Bebida
         (BebidaAlcoholica  "false")
         (Precio  1.50)
         (Nombre  "Agua")
    )

    ([cebolla] of Ingrediente
         (Disponibilidad  [verano] [otoño] [primavera] [invierno])
         (Nombre  "Cebolla")
         (CumpleRestricciones  [intolerancia_lactosa] [intolerancia_gluten] [vegano] [vegetariano])
    )

    ([Copa_de_vino_rosado_Los_Frailes] of Bebida
         (BebidaAlcoholica  "true")
         (Precio  3.50)
         (Nombre  "Copa de vino rosado Los Frailes")
    )

    ([aceituna] of Ingrediente
         (Disponibilidad  [otoño] [verano] [invierno] [primavera])
         (Nombre  "Aceitunas")
         (CumpleRestricciones  [vegetariano] [intolerancia_lactosa] [intolerancia_gluten] [vegano])
    )

    ([regional] of Preferencia
         (Nombre  "Estilo regional")
    )

    ([soja_texturizada] of Ingrediente
         (Disponibilidad  [primavera] [otoño] [invierno] [verano])
         (Nombre  "Soja texturizada")
         (CumpleRestricciones  [vegano] [intolerancia_lactosa] [vegetariano] [intolerancia_gluten])
    )

    ([Copa_de_vino_blanco_Penedès] of Bebida
         (BebidaAlcoholica  "true")
         (Precio  3.50)
         (Nombre  "Copa de vino blanco D.O. Penedès")
    )

    ([hummus] of Ingrediente
         (Disponibilidad  [primavera] [verano] [invierno] [otoño])
         (Nombre  "Hummus")
         (CumpleRestricciones  [intolerancia_lactosa] [vegano] [vegetariano] [intolerancia_gluten])
    )

    ([Arroz_a_la_cubana] of Plato
         (CumplePreferencias  [regional] [clásico])
         (Complejidad  45)
         (Nombre  "Arroz a la cubana")
         (Ingredientes  [tomate] [aceite_de_oliva] [arroz] [sal] [ajo] [agua] [plátano] [huevo])
         (Precio  11.25)
         (TipoEnMenu  "primero")
         (InfoGeneral  "Plato caliente de arroz con huevo y platano")
         (BebidasRecomendadas  [Cerveza_Moritz] [Cerveza_Estrella_Damm] [Agua] [Copa_de_vino_tinto_Fuenteseca] [Cerveza_Estrella_Galicia_1906])
    )

    ([patata] of Ingrediente
         (Disponibilidad  [invierno] [otoño] [primavera] [verano])
         (Nombre  "Patata")
         (CumpleRestricciones  [vegetariano] [intolerancia_gluten] [vegano] [intolerancia_lactosa])
    )

    ([verano] of Temporada
         (Nombre  "Verano")
    )

)
