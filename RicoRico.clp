
; #########################################
; ############### ONTOLOGIA ###############
; #########################################

(defclass Bebida "Guarda la informacion referente a una bebida."
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

(defclass RestriccionAlimentaria "Guarda el nombre de restricciones alimentarias de cumplimiento obligatorio, como pueden ser alergias, intolerancias o que sea vegetariano. La solucion propuesta debe contener solamente ingredientes que cumplan las restricciones indicadas, que se preguntaran al usuario."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Nombre identificador de una instancia de la clase.
    (single-slot Nombre
        (type STRING)
        (create-accessor read-write))
)

(defclass Plato "Guarda la informacion referente a un plato. Esta es la clase alrededor de la qual se envuelve toda la ontologia."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Preferencias que el plato satisface.
    (multislot CumplePreferencias
        (type INSTANCE)
        (create-accessor read-write))
    ;;; La complejidad del plato indica si es apropiado o no servirlo para un numero elevado de comensales.
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
    ;;; Descripcion general del plato.
    (single-slot InfoGeneral
        (type STRING)
        (create-accessor read-write))
    ;;; Bebidas que acompañan bien al plato.
    (multislot BebidasRecomendadas
        (type INSTANCE)
        (create-accessor read-write))
)

(defclass Menu "Recoge la informacion de la construccion de un menu hecha por el programa, una instancia representa una posible parte de la solucion que se enseña al usuario. Inicialmente no tiene instancias."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Lista de bebidas del menu propuesto: una por plato si asi lo ha pedido el usuario o sino solo una.
    (multislot Bebidas
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Primer plato del menu.
    (single-slot Primero
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Indica el nivel de satisfaccion de preferencias del usuario que tiene el menu.
    (single-slot PuntuacionMenu
        (type INTEGER)
        (create-accessor read-write))
    ;;; Precio del producto.
    (single-slot Precio
        (type FLOAT)
        (create-accessor read-write))
    ;;; Plato de postre del menu.
    (single-slot Postre
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Segundo plato del menu.
    (single-slot Segundo
        (type INSTANCE)
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
    ;;; Numero de personas que atenderan al evento.
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
    ;;; Lista de ingredientes que no pueden estar presentes en los menus propuestos para el evento.
    (multislot IngredientesProhibidos
        (type INSTANCE)
        (create-accessor read-write))
    ;;; Maximo precio que el usuario esta dispuesto a pagar por un menu para el evento.
    (single-slot PrecioMax
        (type FLOAT)
        (create-accessor read-write))
    ;;; Indica si se trata de un evento familiar o congreso.
    (single-slot TipoEvento
        (type STRING)
        (create-accessor read-write))
    ;;; Indica si se quiere tener una bebida para cada uno de los platos, o solo una para todo el menu del evento.
    (single-slot BebidaPorPlatos
        (type SYMBOL)
        (create-accessor read-write))
    ;;; Minimo precio que el usuario esta dispuesto a pagar por un menu para el evento.
    (single-slot PrecioMin
        (type FLOAT)
        (create-accessor read-write))
)

(defclass Ingrediente "Guarda la informacion referente a un ingrediente."
    (is-a USER)
    (role concrete)
    (pattern-match reactive)
    ;;; Lista de temporadas donde el producto esta disponible.
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

(defclass Preferencia "Guarda nombres de preferencias alimentarias. Los menus generados intentaran satisfacer (no obligadamente) el mayor numero de estas. Las instancias de esta clase estan definidas inicialmente y se pregunta al usuario que escoja un subconjunto de estas para personalizar los menus."
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

    ([atun] of Ingrediente
         (Disponibilidad  [invierno] [verano] [primavera] [otoño])
         (Nombre  "Atun")
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
         (CumplePreferencias  [clasico] [regional])
         (Complejidad  30)
         (Nombre  "Macarrones con chorizo")
         (Ingredientes  [huevo] [pasta] [sal] [tomate] [agua] [aceite_de_oliva] [queso] [chorizo])
         (Precio  10.50)
         (TipoEnMenu  "primero")
         (InfoGeneral  "Plato caliente de pasta y carne")
         (BebidasRecomendadas  [Agua] [Refresco_Nestea] [Copa_de_sangria] [Cerveza_Estrella_Damm] [Copa_de_vino_tinto_Fuenteseca] [Refresco_Aquarius])
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
         (CumplePreferencias  [clasico])
         (Complejidad  25)
         (Nombre  "Pasta al pesto")
         (Ingredientes  [sal] [frutos_secos] [pasta] [perejil] [aceite_de_oliva] [albahaca] [ajo])
         (Precio  8.75)
         (TipoEnMenu  "primero")
         (InfoGeneral  "Plato vegano caliente de pasta")
         (BebidasRecomendadas  [Agua] [Copa_de_sangria] [Agua_con_gas] [Copa_de_vino_rosado_Los_Frailes])
    )

    ([pimenton_dulce] of Ingrediente
         (Disponibilidad  [otoño] [verano] [invierno] [primavera])
         (Nombre  "Pimenton dulce")
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
         (InfoGeneral  "Plato frio vegano")
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
         (BebidasRecomendadas  [Agua] [Copa_de_sangria] [Agua_con_gas] [Copa_de_vino_blanco_Empordà])
    )

    ([melon] of Ingrediente
         (Disponibilidad  [otoño] [primavera] [verano])
         (Nombre  "Melon")
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
         (CumplePreferencias  [sibarita] [clasico])
         (Complejidad  55)
         (Nombre  "Entrecot a la plancha con salteado de verduras")
         (Ingredientes  [calabacin] [sal] [ajo] [cebolla] [aceite_de_oliva] [pimiento_verde] [mantequilla] [patata] [pimiento_rojo] [tomate] [ternera])
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

    ([jamon] of Ingrediente
         (Disponibilidad  [invierno] [otoño] [primavera] [verano])
         (Nombre  "Jamon")
         (CumpleRestricciones  [intolerancia_gluten] [intolerancia_lactosa])
    )

    ([Mariscada_de_calamares_y_langostinos] of Plato
         (CumplePreferencias  [sibarita] [clasico])
         (Complejidad  40)
         (Nombre  "Mariscada de calamares y langostinos")
         (Ingredientes  [perejil] [romero] [aceite_de_oliva] [sal] [cebolla] [langostino] [vino_blanco] [calamar])
         (Precio  25.00)
         (TipoEnMenu  "segundo")
         (InfoGeneral  "Plato frio de pescado")
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
         (InfoGeneral  "Plato vegano frio de arroz y verdura")
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

    ([Salmon_al_horno_con_patata] of Plato
         (CumplePreferencias  [sibarita] [moderno])
         (Complejidad  45)
         (Nombre  "Salmon al horno con patata")
         (Ingredientes  [sal] [cebolla] [salmon] [patata] [ajo] [aceite_de_oliva])
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
         (Ingredientes  [atun] [berenjena] [queso] [calabacin] [aceite_de_oliva] [sal] [cebolla])
         (Precio  14.00)
         (TipoEnMenu  "segundo")
         (InfoGeneral  "Plato caliente de pescado y verdura")
         (BebidasRecomendadas  [Copa_de_vino_rosado_Los_Frailes] [Copa_de_vino_blanco_Penedès] [Agua] [Copa_de_sangria])
    )

    ([guindilla] of Ingrediente
         (Disponibilidad  [primavera] [invierno] [verano] [otoño])
         (Nombre  "Guindilla")
         (CumpleRestricciones  [intolerancia_gluten] [vegano] [vegetariano] [intolerancia_lactosa])
    )

    ([judia] of Ingrediente
         (Disponibilidad  [primavera] [invierno] [verano] [otoño])
         (Nombre  "Judia")
         (CumpleRestricciones  [intolerancia_lactosa] [vegano] [vegetariano] [intolerancia_gluten])
    )

    ([platano] of Ingrediente
         (Disponibilidad  [verano] [invierno] [otoño] [primavera])
         (Nombre  "Platano")
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

    ([almidon] of Ingrediente
         (Disponibilidad  [invierno] [primavera] [otoño] [verano])
         (Nombre  "Almidon")
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

    ([salmon] of Ingrediente
         (Disponibilidad  [primavera] [invierno] [verano] [otoño])
         (Nombre  "Salmon")
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
         (Ingredientes  [uva] [pera] [granada] [manzana] [naranja] [platano])
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
         (CumplePreferencias  [clasico])
         (Complejidad  20)
         (Nombre  "Espaguetis con mozzarella albahaca y tomate")
         (Ingredientes  [oregano] [queso] [aceite_de_oliva] [sal] [albahaca] [pasta] [tomate])
         (Precio  11.25)
         (TipoEnMenu  "primero")
         (InfoGeneral  "Plato vegetariano frio de pasta")
         (BebidasRecomendadas  [Agua] [Refresco_Aquarius] [Cerveza_Estrella_Galicia_1906])
    )

    ([Melon] of Plato
         (CumplePreferencias  [clasico])
         (Complejidad  4)
         (Nombre  "Melon")
         (Ingredientes  [melon])
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

    ([Albondigas_con_champiñones] of Plato
         (CumplePreferencias  [clasico] [regional])
         (Complejidad  75)
         (Nombre  "Albondigas con champiñones")
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

    ([oregano] of Ingrediente
         (Disponibilidad  [verano] [invierno] [primavera] [otoño])
         (Nombre  "Oregano")
         (CumpleRestricciones  [intolerancia_gluten] [intolerancia_lactosa] [vegano] [vegetariano])
    )

    ([berenjena] of Ingrediente
         (Disponibilidad  [invierno] [otoño])
         (Nombre  "Berenjena")
         (CumpleRestricciones  [vegetariano] [vegano] [intolerancia_gluten] [intolerancia_lactosa])
    )

    ([Melon_con_jamon] of Plato
         (CumplePreferencias  [regional])
         (Complejidad  5)
         (Nombre  "Melon con jamon")
         (Ingredientes  [melon] [jamon])
         (Precio  6.50)
         (TipoEnMenu  "primero")
         (InfoGeneral  "Plato frio de fruta y jamon")
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
         (BebidasRecomendadas  [Cerveza_Moritz] [Agua_con_gas] [Copa_de_sangria] [Agua] [Copa_de_vino_blanco_Penedès])
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

    ([Butifarra_con_judias] of Plato
         (CumplePreferencias  [regional] [clasico])
         (Complejidad  40)
         (Nombre  "Butifarra con judias")
         (Ingredientes  [aceite_de_oliva] [sal] [judia] [ajo] [perejil])
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
         (CumplePreferencias  [clasico])
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
         (Ingredientes  [chocolate] [harina] [azucar] [huevo] [frutos_secos] [mantequilla] [sal])
         (Precio  8.00)
         (TipoEnMenu  "postre")
         (InfoGeneral  "Brownie casero con nueces y chocolate")
    )

    ([Ensalada_griega] of Plato
         (CumplePreferencias  [moderno])
         (Complejidad  32)
         (Nombre  "Ensalada griega")
         (Ingredientes  [aceituna] [atun] [vinagre] [queso] [romero] [aceite_de_oliva] [pimiento_verde] [oregano] [pepino] [pasta] [pimiento_rojo] [sal] [cebolla])
         (Precio  7.75)
         (TipoEnMenu  "primero")
         (InfoGeneral  "Plato frio de verdura y pasta")
         (BebidasRecomendadas  [Copa_de_vino_tinto_Vivanco] [Copa_de_sangria] [Agua] [Copa_de_vino_blanco_Penedès])
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

    ([clasico] of Preferencia
         (Nombre  "Estilo clasico")
    )

    ([Copa_de_sangria] of Bebida
         (BebidaAlcoholica  "true")
         (Precio  4.00)
         (Nombre  "Copa de Sangria")
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
         (CumplePreferencias  [regional] [clasico])
         (Complejidad  75)
         (Nombre  "Canelones de pollo")
         (Ingredientes  [queso] [pollo] [harina] [ajo] [cebolla] [sal] [pasta] [leche] [mantequilla] [aceite_de_oliva] [pimenton_dulce] [tomate])
         (Precio  14.50)
         (PlatosIncompatibles  [Canelones_de_pollo])
         (TipoEnMenu  "primero_segundo")
         (InfoGeneral  "Plato caliente de pasta y carne")
         (BebidasRecomendadas  [Copa_de_sangria] [Copa_de_vino_tinto_Fuenteseca] [Agua] [Cerveza_Moritz] [Copa_de_vino_rosado_Los_Frailes])
    )

    ([romero] of Ingrediente
         (Disponibilidad  [verano] [invierno] [primavera] [otoño])
         (Nombre  "Romero")
    )

    ([intolerancia_gluten] of RestriccionAlimentaria
         (Nombre  "Intolerancia al gluten")
    )

    ([Batido_de_fresa_y_platano] of Plato
         (CumplePreferencias  [clasico])
         (Complejidad  6)
         (Nombre  "Batido de fresa y platano")
         (Ingredientes  [platano] [leche] [fresa])
         (Precio  7.00)
         (TipoEnMenu  "postre")
         (InfoGeneral  "Batido fresco de fresa y platano")
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

    ([Tiramisu] of Plato
         (Complejidad  35)
         (Nombre  "Tiramisu")
         (Ingredientes  [almidon] [cacao] [cafe] [sal] [harina] [azucar] [huevo] [queso])
         (Precio  8.00)
         (TipoEnMenu  "postre")
         (InfoGeneral  "Tiramisu casero")
    )

    ([calabacin] of Ingrediente
         (Disponibilidad  [verano] [invierno] [otoño] [primavera])
         (Nombre  "Calabacin")
         (CumpleRestricciones  [vegano] [intolerancia_gluten] [intolerancia_lactosa] [vegetariano])
    )

    ([lentejas] of Ingrediente
         (Disponibilidad  [verano] [invierno] [otoño] [primavera])
         (Nombre  "Lentejas")
         (CumpleRestricciones  [vegano] [intolerancia_gluten] [intolerancia_lactosa] [vegetariano])
    )

    ([azucar] of Ingrediente
         (Disponibilidad  [invierno] [otoño] [primavera] [verano])
         (Nombre  "Azucar")
         (CumpleRestricciones  [intolerancia_gluten] [vegano] [intolerancia_lactosa] [vegetariano])
    )

    ([Ensalada_de_queso_de_cabra_y_nueces] of Plato
         (CumplePreferencias  [moderno])
         (Complejidad  20)
         (Nombre  "Ensalada de queso de cabra y nueces")
         (Ingredientes  [vinagre] [lechuga] [frutos_secos] [queso] [aceite_de_oliva] [cebolla] [sal] [zanahoria])
         (Precio  8.50)
         (TipoEnMenu  "primero")
         (InfoGeneral  "Plato vegetariano frio de verdura y queso")
         (BebidasRecomendadas  [Copa_de_vino_tinto_Vivanco] [Copa_de_vino_blanco_Empordà] [Agua])
    )

    ([cafe] of Ingrediente
         (Disponibilidad  [invierno] [primavera] [otoño] [verano])
         (Nombre  "Cafe")
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
         (Ingredientes  [canela] [leche] [azucar] [huevo])
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
         (CumplePreferencias  [regional] [clasico])
         (Complejidad  45)
         (Nombre  "Arroz a la cubana")
         (Ingredientes  [tomate] [aceite_de_oliva] [arroz] [sal] [ajo] [agua] [platano] [huevo])
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

; #########################################
; ######## DECLARACION DE MODULOS #########
; #########################################


(defmodule MAIN (export ?ALL))

(defmodule recogida_datos_evento
  (import MAIN ?ALL)

	(export ?ALL)
)

; main

(defrule MAIN::initialRule "Regla inicial"
	(declare (salience 10))
	=>
  	(printout t"         Personalizacion de menu con RicoRico         " crlf)
	(printout t"----------------------------------------------------------" crlf)
  	(printout t crlf)
	(printout t"Estimado cliente, a continuacion se le formularan una serie de preguntas para poder recomendarle 3 menus diferentes." crlf)
	(printout t crlf)
  (focus recogida_datos_evento)
)

; #########################################
; ########## RECOGIDA DE DATOS ############
; #########################################

; funcion para hacer una pregunta numerica
(deffunction MAIN::pregunta_numerica (?pregunta ?primera ?ultima)
  ; se saca la pregunta por pantalla
  (bind ?linea (format nil "%s" ?pregunta))
  (printout t ?linea crlf)
  ; se guarda la respuesta del usuario en la variable ?respuesta
  (bind ?respuesta (read))
  ; mientras la respuesta no este dentro del rango acceptado o no se responda
  ; con un integer, se vuelve a pedir una respuesta
  (while (or (not (integerp ?respuesta)) (not(and(>= ?respuesta ?primera)(<= ?respuesta ?ultima)))) do
    (bind ?linea (format nil "%s (%d - %d):" "Por favor, responda con un valor dentro del rango de respuestas " ?primera ?ultima))
    (printout t ?linea crlf)
    (bind ?respuesta (read))
  )
  ?respuesta
)

; funcion para hacer una pregunta numerica con una unica opcion
(deffunction MAIN::pregunta_single_choice (?pregunta $?llista_elem)
  (bind ?linea (format nil "%s" ?pregunta))
  (printout t ?linea crlf)
  (progn$ (?elem ?llista_elem)
    (bind ?linea (format nil " %d. %s" ?elem-index ?elem))
    (printout t ?linea crlf)
  )
  (format t "%s" "Escribe el indice de la respuesta: ")
  (bind ?respuesta (read))
  (while (or (not (integerp ?respuesta)) (or (< ?respuesta 1)(> ?respuesta (length$ ?llista_elem)))) do
    (bind ?linea (format nil "%s (%d - %d):" "Por favor, responda con un valor dentro del rango de respuestas " 1 (length$ ?llista_elem)))
    (printout t ?linea crlf)
    (bind ?respuesta (read))
  )
  ?respuesta
)

; Hace una pregunta de si/no. Devuelve TRUE si la respuesta es si o s, otherwise devuelve FALSE
(deffunction MAIN::pregunta_si_no (?pregunta)
  (bind ?linea (format nil "%s %s" ?pregunta " (si/no)"))
  (printout t ?linea crlf)
  (bind ?respuesta (read))
  (if (or (eq ?respuesta si) (eq ?respuesta s))
    then TRUE
    else FALSE)
)

(deffunction MAIN::pregunta_multi_choice (?pregunta $?lista_elem)
  (bind ?linea (format nil "%s" ?pregunta))
  (printout t ?linea crlf)
  (progn$ (?elem ?lista_elem)
    (bind ?linea (format nil " %d. %s" ?elem-index ?elem))
    (printout t ?linea crlf)
  )
  (printout t "" crlf)
  (printout t " 0. Ninguna de las opciones anteriores" crlf)
  (printout t "" crlf)
  (bind ?linea (format nil "%s" "Escribe los indices de tus respuestas separados por un espacio: "))
  (printout t ?linea crlf)
  (bind ?entrada (readline))
  (bind ?indices_respuesta (str-explode ?entrada))
  (bind $?resultado (create$))
  (progn$ (?indice ?indices_respuesta)
    (if (= ?indice 0) then
      (bind $?resultado (create$))
      (return ?resultado)
    )
    (if (and (integerp ?indice) (and (> ?indice 0) (<= ?indice (length$ ?lista_elem))))
      then (if (not (member$ ?indice ?resultado))
        then (bind ?resultado (insert$ ?resultado (+ (length$ ?resultado) 1) ?indice))
      )
    )
  )
  ?resultado
)

; #########################################
; ###### PREGUNTAS PARA EL CLIENTE ########
; #########################################

(deftemplate MAIN::datos_evento ;
	(slot tipo_evento (type STRING)) ;
	(slot numero_comensales (type INTEGER)) ;
	(slot precio_min (type INTEGER)) ;
  (slot precio_max (type INTEGER)) ;
  (multislot restricciones (type INSTANCE)) ;
  (multislot ingredientes_prohibidos (type INSTANCE)) ;
  (multislot preferencias (type INSTANCE)) ;
  (slot temporada_actual (type INSTANCE)) ;
  (slot bebida_alcoholica (type SYMBOL)(default FALSE)) ;
  (slot bebida_por_platos (type SYMBOL)(default FALSE)) ;
)

; - Se trata de un evento familiar o un congreso?
;   1. Familiar
;   2. Congreso
(deffacts recogida_datos_evento::preparacion "Establece hechos para poder recoger los datos del evento"
  (numero_comensales ask)
  (precio_min ask)
  (precio_max ask)
  (restricciones_alimentarias ask)
  (alergias_alimentarias ask)
  (preferencias_alimentarias ask)
  (bebida_por_platos ask)
  (bebida_alcoholica ask)
  (temporada ask)
)

(defrule recogida_datos_evento::familiar_congreso "Indica si el evento es familiar o un congreso"
  (not (datos_evento))
  =>
  (bind ?respuesta (pregunta_single_choice "Se trata de un evento familiar o un congreso?" "Familiar" "Congreso"))
  (if (= ?respuesta 1) then (bind ?tipo_evento "familiar"))
  (if (= ?respuesta 2) then (bind ?tipo_evento "congreso"))
  (assert (datos_evento (tipo_evento ?tipo_evento)))
)

; - Cuantos comensales habran en el evento?
;   #
(defrule recogida_datos_evento::numero_comensales "Indica el numero de comensales presentes en el evento"
  ?fact <- (numero_comensales ask)
  ?datos_evento <- (datos_evento)
  =>
  (bind ?respuesta (pregunta_numerica "¿Cuantos comensales habran en el evento?" 1 500))
  (retract ?fact)
  (modify ?datos_evento (numero_comensales ?respuesta))
)

; - Indique su presupuesto minimo y maximo (separado por un espacio):

(defrule recogida_datos_evento::precio_min "Indica precio mínimo del menú"
  ?fact <- (precio_min ask)
  ?datos_evento <- (datos_evento)
  =>
  (bind ?respuesta (pregunta_numerica "Cuál será el precio mínimo del menú?" 1 250))
  (retract ?fact)
  (modify ?datos_evento (precio_min ?respuesta))
)

(defrule recogida_datos_evento::precio_max "Indica precio máximo del menú"
  ?fact <- (precio_max ask)
  ?datos_evento <- (datos_evento)
  =>
  (bind ?respuesta (pregunta_numerica "Cuál será el precio máximo del menú?" 1 250))
  (retract ?fact)
  (modify ?datos_evento (precio_max ?respuesta))
)

; - Qué restricciones alimentarias hay?
;   1. Vegeteriano
;   2. Vegano
;   3. Intolerante a la lactosa
;   4. Intolerancia al gluten
(defrule recogida_datos_evento::restricciones_alimentarias "Indica que restricciones alimentarias hay"
  ?fact <- (restricciones_alimentarias ask)
  ?datos_evento <- (datos_evento)
  =>
  (bind ?restricciones (find-all-instances ((?inst RestriccionAlimentaria)) TRUE ))
  (bind $?posibles_respuestas (create$ ))
  (loop-for-count (?i 1 (length$ $?restricciones)) do
    (bind ?i_restriccion (nth$ ?i ?restricciones))
    (bind ?i_respuesta (send ?i_restriccion get-Nombre))
    (bind $?posibles_respuestas (insert$ $?posibles_respuestas (+ (length$ $?posibles_respuestas) 1) ?i_respuesta))
  )

  (bind ?choice (pregunta_multi_choice "Qué restricciones alimentarias hay?" $?posibles_respuestas))

  (bind $?respuesta (create$))
  (loop-for-count (?i 1 (length$ ?choice)) do
    (bind ?index (nth$ ?i ?choice))
    (bind ?i_choice (nth$ ?index ?restricciones))
    (printout t ?i_choice crlf)
    (bind $?respuesta (insert$ $?respuesta (+ (length$ $?respuesta) 1) ?i_choice))
  )
  (retract ?fact)
  (modify ?datos_evento (restricciones $?respuesta))
)

; - ¿Entre los miembros del grupo hay alguien que tenga alergia o deteste algun ingrediente concreto?
;   1. Si
;   2. No
;   [Solo en caso afirmativo de la anterior pregunta]
; - Marque, de entre todos los ingredientes de nuestros platos, quales no pueden aparecer en el menu.
;   [Lista de ingredientes].
(defrule recogida_datos_evento::alergias_alimentarias "Indica si deben prohibirse alimentos"
  ?fact <- (alergias_alimentarias ask)
  ?datos_evento <- (datos_evento)
  =>
  (bind ?si_no (pregunta_si_no "¿Entre los miembros del grupo hay alguien que tenga alergia o deteste algun ingrediente concreto?"))
  (bind $?respuesta (create$ ))
  (if (eq ?si_no TRUE) then
    (bind ?ingredientes (find-all-instances ((?inst Ingrediente)) TRUE))
    (bind $?respuestas (create$ ))
    (loop-for-count (?i 1 (length$ $?ingredientes)) do
      (bind ?i_ingrediente (nth$ ?i ?ingredientes))
      (bind ?i_nom (send ?i_ingrediente get-Nombre))
      (bind $?respuestas(insert$ $?respuestas (+ (length$ $?respuestas) 1) ?i_nom))
    )

    (bind ?escogido (pregunta_multi_choice "Selecciona los ingredientes que no toleras: " $?respuestas))
    (loop-for-count (?i 1 (length$ ?escogido)) do
      (bind ?curr-index (nth$ ?i ?escogido))
      (bind ?curr-ingredientes (nth$ ?curr-index ?ingredientes))
      (bind $?respuesta(insert$ $?respuesta (+ (length$ $?respuesta) 1) ?curr-ingredientes))
    )
  )
  (retract ?fact)
  (modify ?datos_evento (ingredientes_prohibidos $?respuesta))
)

; - Alguna preferencia respecto al tipo de platos? (elegir solo 1 opción??)
;   1. Clasicos
;   2. Modernos
;   3. Regionales
;   4. Sibaritas
(defrule recogida_datos_evento::preferencias_alimentarias "Indica la preferencia del estilo de plato"
  ?fact <- (preferencias_alimentarias ask)
  ?datos_evento <- (datos_evento)
  =>
  (bind ?preferencias (find-all-instances ((?inst Preferencia)) TRUE ))
  (bind $?posibles_respuestas (create$ ))
  (loop-for-count (?i 1 (length$ $?preferencias)) do
    (bind ?i_preferencia (nth$ ?i ?preferencias))
    (bind ?i_respuesta (send ?i_preferencia get-Nombre))
    (bind $?posibles_respuestas (insert$ $?posibles_respuestas (+ (length$ $?posibles_respuestas) 1) ?i_respuesta))
  )
  (bind ?choice (pregunta_multi_choice "Alguna preferencia respecto al tipo de platos?" $?posibles_respuestas))
  (bind $?respuesta (create$ ))
  (loop-for-count (?i 1 (length$ ?choice)) do
    (bind ?index (nth$ ?i ?choice))
    (bind ?i_choice (nth$ ?index ?preferencias))
    (bind $?respuesta(insert$ $?respuesta (+ (length$ $?respuesta) 1) ?i_choice))
  )
  (retract ?fact)
  (modify ?datos_evento (preferencias $?respuesta))
)

; - Quieres una bebida por cada plato? (s/n)
(defrule recogida_datos_evento::bebida_por_plato "Indica si el menú debe considerar una bebida para cada plato o una comuna para todos"
  ?fact <- (bebida_por_platos ask)
  ?datos_evento <- (datos_evento)
  =>
  (bind ?respuesta (pregunta_si_no "Desea que el menú incorpore una bebida individual para cada plato?"))
  (retract ?fact)
  (modify ?datos_evento (bebida_por_platos ?respuesta))
)

;(defrule recogida_datos_evento::bebida_cada_plato "Indica si quieres una bebida por casa plato (s/n)"
;  (bind ?)
;)

; - Quieres bebidas alcoholicas? (s/n)
(defrule recogida_datos_evento::bebida_alcoholica "Indica si el menú puede tener o no bebidas alcoholicas"
  ?fact <- (bebida_alcoholica ask)
  ?datos_evento <- (datos_evento)
  =>
  (bind ?respuesta (pregunta_si_no "El menú propuesto puede contener bebidas alcoholicas?"))
  (retract ?fact)
  (modify ?datos_evento (bebida_alcoholica ?respuesta))
)

; - ¿Para que epoca desea hacer la reserva?
;   1. Invierno
;   2. Primavera
;   3. Verano
;   4. Otoño
(defrule recogida_datos_evento::temporada_actual "Indica la temporada para la cual se hace la reserva"
  ?fact <- (temporada ask)
  ?datos_evento <- (datos_evento)
  =>
  (bind ?temporadas (find-all-instances ((?inst Temporada)) TRUE ))
  (bind $?posibles_respuestas (create$ ))
  (loop-for-count (?i 1 (length$ $?temporadas)) do
    (bind ?i_temporada (nth$ ?i ?temporadas))
    (bind ?i_respuesta (send ?i_temporada get-Nombre))
    (bind $?posibles_respuestas (insert$ $?posibles_respuestas (+ (length$ $?posibles_respuestas) 1) ?i_respuesta))
  )
  (bind ?choice (pregunta_single_choice "Para qué temporada del año desea hacer la reserva en Rico Rico?" $?posibles_respuestas))
  (bind ?respuesta (nth$ ?choice ?temporadas))
  (retract ?fact)
  (modify ?datos_evento (preferencias ?respuesta))
)




; #########################################
; ###### MESSAGE HANDLERS DE CLASES #######
; #########################################

; Funcion para escribir por pantalla la recomendacion de un menu concreto
(defmessage-handler Menu print primary ()
  ; Nombre de los platos
  (bind ?nombre_primero (send ?self:Primero get-Nombre))
  (bind ?nombre_segundo (send ?self:Segundo get-Nombre))
  (bind ?nombre_postre  (send ?self:Postre  get-Nombre))

  ; Nombre de las bebidas
  (bind $?bebidas ?self:Bebidas)
  (bind $?nombre_bebidas (create$))
  (loop-for-count (?i 1 (length$ $?bebidas)) do
    (bind ?i_bebida (nth$ ?i ?bebidas))
    (bind ?i_nombre (send ?i_bebida get-Nombre))
    (bind $?nombre_bebidas (insert$ $?nombre_bebidas (+ (length$ $?nombre_bebidas) 1) ?i_nombre))
  )

  (printout t "-------------------------------------------------------------------------" crlf)
  (printout t "|                            MENU RECOMENDADO                           |" crlf)
  (printout t "-------------------------------------------------------------------------" crlf)
  (printout t "-------------------------------------------------------------------------" crlf)
  (printout t "|                           --    PLATOS    --                          |" crlf)
  (bind ?line (format nil "Primer plato:    %s" ?nombre_primero))
  (format t "|    %-63s    |%n" ?line)
  (bind ?line (format nil "Segundo plato:   %s" ?nombre_segundo))
  (format t "|    %-63s    |%n" ?line)
  (bind ?line (format nil "Postres:         %s" ?nombre_postre))
  (format t "|    %-63s    |%n" ?line)
  (printout t "-------------------------------------------------------------------------" crlf)
  (printout t "|                           --    BEBIDAS   --                          |" crlf)

  (if (= (length$ $?nombre_bebidas) 1) then
    (bind ?nombre_bebida_unica (nth$ 1 ?nombre_bebidas))
    (bind ?line (format nil "Bebida unica:    %s" ?nombre_bebida_unica))
    (format t "|    %-63s    |%n" ?line)
  else
    (bind ?nombre_bebida_primero (nth$ 1 ?nombre_bebidas))
    (bind ?line (format nil "Bebida del primero: %s" ?nombre_bebida_primero))
    (format t "|    %-63s    |%n" ?line)
    (bind ?nombre_bebida_segundo (nth$ 2 ?nombre_bebidas))
    (bind ?line (format nil "Bebida del segundo: %s" ?nombre_bebida_segundo))
    (format t "|    %-63s    |%n" ?line)
  )
  (printout t "-------------------------------------------------------------------------" crlf)
  (bind ?line (format nil "              Precio total del menu: %.2f euros" ?self:Precio))
  (format t "|    %-63s    |%n" ?line)
  (printout t "-------------------------------------------------------------------------" crlf)
)
