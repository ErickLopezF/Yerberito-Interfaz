:- use_module(library(pce)).

% Definición de plantas y sus propiedades
:- dynamic planta_medicinal/1, propiedad_real/2, enfermedad/2, receta/2, iman/2, nombre_cientifico/2, elementos_contiene/2, efecto/2, medicamento/2.

% Plantas medicinales
planta_medicinal(cuajilote).
planta_medicinal(cuasia).
planta_medicinal(damiana).
planta_medicinal(pulsatilla).
planta_medicinal(quebracho).
planta_medicinal(quina).
planta_medicinal(regaliz).
planta_medicinal(pirul).
planta_medicinal(pinguica).
planta_medicinal(pericon).
planta_medicinal(prodigiosa).
planta_medicinal(diente_de_leon).
planta_medicinal(manzanilla).
planta_medicinal(abrojo).
planta_medicinal(acacia).
planta_medicinal(acanto).
planta_medicinal(aceitilla).
planta_medicinal(achicoria).
planta_medicinal(fenogreco).
planta_medicinal(genciana).
planta_medicinal(eucalipto).

% Nombres científicos de las plantas
nombre_cientifico(cuajilote, 'Parmentiera aculeata').
nombre_cientifico(cuasia, 'Quassia amara').
nombre_cientifico(damiana, 'Turnera diffusa').
nombre_cientifico(pulsatilla, 'Anemone pulsatilla').
nombre_cientifico(quebracho, 'Aspidosperma quebracho-blanco').
nombre_cientifico(quina, 'Cinchona officinalis').
nombre_cientifico(regaliz, 'Glycyrrhiza glabra').

% Elementos contenidos en las plantas
elementos_contiene(cuajilote, [fibra, antioxidantes]).
elementos_contiene(cuasia, [quassina]).
elementos_contiene(damiana, [flavonoides, taninos]).
elementos_contiene(pulsatilla, [febrifuga, sudorifica, expectorante, calmante]).
elementos_contiene(quebracho, [agua, corteza]).
elementos_contiene(quina, [quinina]).
elementos_contiene(regaliz, [orozus, antiinflamatorio]).

% Propiedades reales de las plantas
propiedad_real(cuajilote, 'Es un remedio natural para el estrenimiento y la anasarca').
propiedad_real(cuasia, 'Es conocido por su uso en el tratamiento de la diabetes y la artritis').
propiedad_real(damiana, 'Ayuda a aumentar la libido y mejorar el estado de animo en casos de depresion').
propiedad_real(pulsatilla, 'Se usa para problemas menstruales y oculares').
propiedad_real(quebracho, 'Se utiliza en el tratamiento de afecciones respiratorias como el asma y la bronquitis').
propiedad_real(quina, 'Es efectivo contra la malaria y estimula el apetito').
propiedad_real(regaliz, 'Alivia las ulceras estomacales y la bronquitis').
propiedad_real(pirul, 'Tiene propiedades antiinflamatorias, desinfectantes y alivia el dolor dental').
propiedad_real(pinguica, 'Actua como diuretico, expectorante y anticatarral').
propiedad_real(pericon, 'Estimula el apetito, regula la menstruacion y alivia el dolor de estomago').
propiedad_real(prodigiosa, 'Se usa para tratar disenteria, diarreas y problemas hepaticos como la cirrosis e ictericia').
propiedad_real(diente_de_leon, 'Funciona como laxante, diuretico, colagogo, digestivo y antiinflamatorio').
propiedad_real(manzanilla, 'Es curativa, antiinflamatoria, digestiva y sedante').
propiedad_real(abrojo, 'Es diuretico y antiinflamatorio, especialmente util para infecciones de pecho y ojos').
propiedad_real(acacia, 'Alivia el dolor de garganta, la tos y la bronquitis').
propiedad_real(acanto, 'Se usa para tratar picaduras de arana, hemorroides y quemaduras').
propiedad_real(aceitilla, 'Ayuda a aliviar el cansancio intelectual y la depresion nerviosa').
propiedad_real(achicoria, 'Actua como diuretico, depurativo y mejora la digestion').
propiedad_real(fenogreco, 'Tiene propiedades laxantes, estimulantes del apetito y expectorantes').
propiedad_real(genciana, 'Estimula el apetito y combate la anemia, escorbuto y debilidad').
propiedad_real(eucalipto, 'Es desinfectante, expectorante y antiinflamatorio, alivia la tos y el asma').

% Efectos de las plantas
efecto(cuajilote, 'Antioxidante').
efecto(cuajilote, 'Fibra natural').
efecto(cuasia, 'Antidiabetico').
efecto(cuasia, 'Antiartritico').
efecto(damiana, 'Aumento de libido').
efecto(damiana, 'Mejora del estado de animo').
efecto(pulsatilla, 'Calmante').
efecto(pulsatilla, 'Expectorante').
efecto(quebracho, 'Expectorante').
efecto(quebracho, 'Antiasmatico').
efecto(quina, 'Antimalarico').
efecto(quina, 'Estimulante del apetito').
efecto(regaliz, 'Antiulceroso').
efecto(regaliz, 'Antiinflamatorio').
efecto(pirul, 'Antibacteriano').
efecto(pirul, 'Alivio del dolor dental').
efecto(pinguica, 'Diuretico').
efecto(pinguica, 'Expectorante').
efecto(pericon, 'Digestivo').
efecto(pericon, 'Regulador menstrual').
efecto(prodigiosa, 'Hepatoprotector').
efecto(prodigiosa, 'Antiinflamatorio').
efecto(diente_de_leon, 'Depurativo').
efecto(diente_de_leon, 'Digestivo').
efecto(manzanilla, 'Sedante').
efecto(manzanilla, 'Digestivo').
efecto(abrojo, 'Antiinflamatorio').
efecto(abrojo, 'Diuretico').
efecto(acacia, 'Expectorante').
efecto(acacia, 'Alivio del dolor de garganta').
efecto(acanto, 'Cicatrizante').
efecto(acanto, 'Desintoxicante').
efecto(aceitilla, 'Antidepresivo').
efecto(aceitilla, 'Estimulante mental').
efecto(achicoria, 'Digestivo').
efecto(achicoria, 'Depurativo').
efecto(fenogreco, 'Estimulante del apetito').
efecto(fenogreco, 'Laxante').
efecto(genciana, 'Tonico amargo').
efecto(genciana, 'Estimulante del apetito').
efecto(eucalipto, 'Antiviral').
efecto(eucalipto, 'Expectorante').

% Enfermedades que curan
enfermedad(cuajilote, 'estrenimiento').
enfermedad(cuajilote, 'anasarca').
enfermedad(cuasia, 'diabetes').
enfermedad(cuasia, 'artritis').
enfermedad(damiana, 'libido').
enfermedad(damiana, 'depresion').
enfermedad(pulsatilla, 'menstruacion').
enfermedad(pulsatilla, 'ojos').
enfermedad(quebracho, 'asma').
enfermedad(quebracho, 'bronquitis').
enfermedad(quina, 'malaria').
enfermedad(quina, 'apetito').
enfermedad(regaliz, 'ulceras').
enfermedad(regaliz, 'bronquitis').
enfermedad(pirul, 'infecciones_urinarias').
enfermedad(pirul, 'gonorrea').
enfermedad(pirul, 'antiinflamatorio').
enfermedad(pirul, 'dolor_dental').
enfermedad(pirul, 'desinfectante').
enfermedad(pinguica, 'diuretico').
enfermedad(pinguica, 'nefritis').
enfermedad(pinguica, 'expectorante').
enfermedad(pinguica, 'catarro').
enfermedad(pinguica, 'infecciones_urinarias').
enfermedad(pinguica, 'antiseptico_urinario').
enfermedad(pericon, 'apetito').
enfermedad(pericon, 'menstrual').
enfermedad(pericon, 'dolor_estomago').
enfermedad(pericon, 'parto').
enfermedad(pericon, 'antiinflamatorio').
enfermedad(pericon, 'parasitos').
enfermedad(prodigiosa, 'disenteria').
enfermedad(prodigiosa, 'diarrea').
enfermedad(prodigiosa, 'cirrosis').
enfermedad(prodigiosa, 'ictericia').
enfermedad(prodigiosa, 'colicos').
enfermedad(prodigiosa, 'antiinflamatorio').
enfermedad(prodigiosa, 'dolor_menstrual').
enfermedad(diente_de_leon, 'laxante').
enfermedad(diente_de_leon, 'diuretico').
enfermedad(diente_de_leon, 'colagogo').
enfermedad(diente_de_leon, 'digestivo').
enfermedad(diente_de_leon, 'antiinflamatorio').
enfermedad(manzanilla, 'curativo').
enfermedad(manzanilla, 'antiinflamatorio').
enfermedad(manzanilla, 'digestivo').
enfermedad(manzanilla, 'sedante').
enfermedad(abrojo, 'diuretico').
enfermedad(abrojo, 'infecciones_pecho').
enfermedad(abrojo, 'antiinflamatorio_ojos').
enfermedad(abrojo, 'antiinflamatorio_higado').
enfermedad(abrojo, 'circulacion').
enfermedad(acacia, 'dolor_garganta').
enfermedad(acacia, 'tos').
enfermedad(acacia, 'bronquitis').
enfermedad(acacia, 'antiinflamatorio').
enfermedad(acanto, 'picaduras_arana').
enfermedad(acanto, 'desintoxicante').
enfermedad(acanto, 'hemorroides').
enfermedad(acanto, 'quemaduras').
enfermedad(acanto, 'dispepsia').
enfermedad(acanto, 'disenteria').
enfermedad(aceitilla, 'cansancio').
enfermedad(aceitilla, 'depresion_nerviosa').
enfermedad(aceitilla, 'antiinflamatorio').
enfermedad(achicoria, 'diuretico').
enfermedad(achicoria, 'depurativo').
enfermedad(achicoria, 'digestivo').
enfermedad(achicoria, 'colicos').
enfermedad(achicoria, 'estrenimiento').
enfermedad(achicoria, 'rinones').
enfermedad(fenogreco, 'laxante').
enfermedad(fenogreco, 'apetito').
enfermedad(fenogreco, 'antiinflamatorio').
enfermedad(fenogreco, 'expectorante').
enfermedad(genciana, 'apetito').
enfermedad(genciana, 'tonico_amargo').
enfermedad(genciana, 'escorbuto').
enfermedad(genciana, 'anemia').
enfermedad(genciana, 'clorosis').
enfermedad(genciana, 'debilidad').
enfermedad(genciana, 'escrofulosis').
enfermedad(genciana, 'leucorrea').
enfermedad(genciana, 'pecas').
enfermedad(genciana, 'digestivo').
enfermedad(eucalipto, 'desinfectante').
enfermedad(eucalipto, 'expectorante').
enfermedad(eucalipto, 'antiinflamatorio').
enfermedad(eucalipto, 'tos').
enfermedad(eucalipto, 'asma').
enfermedad(eucalipto, 'bronquitis').
enfermedad(eucalipto, 'mal_aliento').
enfermedad(eucalipto, 'tisis').
enfermedad(eucalipto, 'gripe').
enfermedad(eucalipto, 'catarros').
enfermedad(eucalipto, 'pulmonia').

% Recetas de cada planta
receta(cuajilote, 'Hervir la pulpa del fruto en agua y beber dos veces al dia').
receta(cuasia, 'Preparar una infusion con la corteza y beber una taza al dia').
receta(damiana, 'Hervir las hojas en agua y beber como te').
receta(pulsatilla, 'Hervir la planta en agua y aplicar como compresa').
receta(quebracho, 'Decoccion de la corteza y tomar una taza al dia').
receta(quina, 'Infusion de la corteza y beber antes de las comidas').
receta(regaliz, 'Decoccion de la raiz y tomar una taza al dia').
receta(pirul, 'Hervir frutos en agua y usar como enjuague bucal').
receta(pinguica, 'Infusion de hojas y beber una taza al dia').
receta(pericon, 'Hervir planta en agua y beber como te').
receta(prodigiosa, 'Infusion de hojas y beber una taza al dia').
receta(diente_de_leon, 'Infusion de hojas y beber una taza al dia').
receta(manzanilla, 'Infusion de flores y beber una taza al dia').
receta(abrojo, 'Decoccion de hojas y beber una taza al dia').
receta(acacia, 'Infusion de corteza y beber una taza al dia').
receta(acanto, 'Cataplasma de hojas y aplicar en la zona afectada').
receta(aceitilla, 'Infusion de flores y beber una taza al dia').
receta(achicoria, 'Infusion de raiz y beber una taza al dia').
receta(fenogreco, 'Infusion de semillas y beber una taza al dia').
receta(genciana, 'Infusion de raiz y beber una taza al dia').
receta(eucalipto, 'Infusion de hojas y beber una taza al dia').

% Definición de imágenes de las plantas con las rutas actualizadas
iman(cuajilote, 'C:\\prolog\\proyecto\\imagenes\\Cuajilote.jpg').
iman(cuasia, 'C:\\prolog\\proyecto\\imagenes\\Cuasia.jpg').
iman(damiana, 'C:\\prolog\\proyecto\\imagenes\\Damiana.jpg').
iman(pulsatilla, 'C:\\prolog\\proyecto\\imagenes\\Pulsatilla.jpg').
iman(quebracho, 'C:\\prolog\\proyecto\\imagenes\\Quebracho.jpg').
iman(quina, 'C:\\prolog\\proyecto\\imagenes\\Quina.jpg').
iman(regaliz, 'C:\\prolog\\proyecto\\imagenes\\Regaliz.jpg').
iman(pirul, 'C:\\prolog\\proyecto\\imagenes\\pirul.jpg').
iman(pinguica, 'C:\\prolog\\proyecto\\imagenes\\pinguica.jpg').
iman(pericon, 'C:\\prolog\\proyecto\\imagenes\\pericon.jpg').
iman(prodigiosa, 'C:\\prolog\\proyecto\\imagenes\\prodigiosa.jpg').
iman(diente_de_leon, 'C:\\prolog\\proyecto\\imagenes\\diente_de_leon.jpg').
iman(manzanilla, 'C:\\prolog\\proyecto\\imagenes\\manzanilla.jpg').
iman(abrojo, 'C:\\prolog\\proyecto\\imagenes\\abrojo.jpg').
iman(acacia, 'C:\\prolog\\proyecto\\imagenes\\acacia.jpg').
iman(acanto, 'C:\\prolog\\proyecto\\imagenes\\acanto.jpg').
iman(aceitilla, 'C:\\prolog\\proyecto\\imagenes\\aceitilla.jpg').
iman(achicoria, 'C:\\prolog\\proyecto\\imagenes\\achicoria.jpg').
iman(fenogreco, 'C:\\prolog\\proyecto\\imagenes\\fenogreco.jpg').
iman(genciana, 'C:\\prolog\\proyecto\\imagenes\\genciana.jpg').
iman(eucalipto, 'C:\\prolog\\proyecto\\imagenes\\eucalipto.jpg').

% Medicamentos producidos por las plantas
medicamento(digital, 'Digitalina (Tonico cardiaco)').
medicamento(opio, 'Morfina').
medicamento(opio, 'Codeina').
medicamento(ipeca, 'Emetina (Para disenteria, hemoptisis y hepatitis amebiana)').
medicamento(nuez_vomica, 'Estricnina').
medicamento(eleboro_blanco, 'Veratrina (Hipertensor)').
medicamento(colchico, 'Colchicina (Para la gota)').
medicamento(belladona, 'Atropina (Uso en oftalmologia)').
medicamento(quina, 'Quinina').
medicamento(cacao, 'Teobromina (De accion diuretica)').
medicamento(retama, 'Esparteina (Tonico cardiaco)').
medicamento(coca, 'Cocaina').
medicamento(peyote, 'Mescalina y 30 alcaloides mas').
medicamento(efedra, 'Efedrina (En oftalmologia)').
medicamento(barbasco, 'Hormonas').
medicamento(nenufar_amarillo, 'Tenerina (Nuevo antibiotico)').
medicamento(talna, 'Diosponina').
medicamento(artemisa, 'Tuyona').
medicamento(semilla_de_yute, 'Olitorisida (Glucosido)').
medicamento(tolocoche, 'Ololiuqui (LSD)').
medicamento(eucalipto, 'Eucaliptol').
medicamento(rosal, 'Vitamina C').
medicamento(rosal, 'Quercitrina').

% Hechos
hierba(anis_estrella).
hierba(menta).
hierba(arnica).
hierba(salvia).
hierba(tila).
hierba(eucalipto).
hierba(yerbabuena).
hierba(manzanilla).
hierba(cola_de_caballo).
hierba(romero).
hierba(toronjil).
hierba(sanguinaria).
hierba(linaza).
hierba(hamamelis).
hierba(zarzaparrilla).
hierba(boldo).
hierba(diente_de_leon).
hierba(azahar).
hierba(malva).
hierba(marrubio).
hierba(rosal).

% Relaciones de tratamiento
tratar(anis_estrella, digestion).
tratar(menta, digestion).
tratar(arnica, antiinflamatorio).
tratar(salvia, infecciones).
tratar(tila, ansiedad).
tratar(eucalipto, respiratorio).
tratar(yerbabuena, digestion).
tratar(manzanilla, digestion).
tratar(cola_de_caballo, diuretico).
tratar(romero, dolor_muscular).
tratar(toronjil, nervios).
tratar(sanguinaria, respiratorio).
tratar(linaza, digestion).
tratar(hamamelis, piel).
tratar(zarzaparrilla, purificar_sangre).
tratar(boldo, higado).
tratar(diente_de_leon, digestion).
tratar(azahar, relajante).
tratar(malva, tos).
tratar(marrubio, tos).
tratar(rosal, piel).

% Relaciones de fortalecimiento
fortalecer(anis_estrella, sistema_digestivo).
fortalecer(menta, sistema_digestivo).
fortalecer(arnica, sistema_circulatorio).
fortalecer(salvia, sistema_inmunologico).
fortalecer(tila, sistema_nervioso).
fortalecer(eucalipto, sistema_respiratorio).
fortalecer(yerbabuena, sistema_digestivo).
fortalecer(manzanilla, sistema_digestivo).
fortalecer(cola_de_caballo, sistema_urinario).
fortalecer(romero, sistema_muscular).
fortalecer(toronjil, sistema_nervioso).
fortalecer(sanguinaria, sistema_respiratorio).
fortalecer(linaza, sistema_digestivo).
fortalecer(hamamelis, sistema_piel).
fortalecer(zarzaparrilla, sistema_circulatorio).
fortalecer(boldo, sistema_hepatico).
fortalecer(diente_de_leon, sistema_digestivo).
fortalecer(azahar, sistema_nervioso).
fortalecer(malva, sistema_respiratorio).
fortalecer(marrubio, sistema_respiratorio).
fortalecer(rosal, sistema_piel).

% Mostrar informacion de una planta especifica
mostrar_info_planta(Hierba) :-
    new(D, dialog(string('Informacion de %s', Hierba))),
    send(D, append, label(tratamientos_label, 'Tratamientos:')),
    forall(tratar(Hierba, Tratamiento), (
        send(D, append, label(plant_label, string('- %s', Tratamiento)))
    )),
    send(D, append, label(fortalecimientos_label, 'Fortalece:')),
    forall(fortalecer(Hierba, Fortalecimiento), (
        send(D, append, label(plant_label, string('- %s', Fortalecimiento)))
    )),
    send(D, open).

% Funcion para mostrar un mini botiquin con botones
mini_botiquin :-
    % Crear dialogo para el mini botiquin
    new(D, dialog('Mini Botiquin')),
    send(D, append, label(botiquin_label, 'Plantas Medicinales:')),

    % Anadir botones para cada planta medicinal
    forall(hierba(Hierba), (
        send(D, append, button(Hierba, message(@prolog, mostrar_info_planta, Hierba)))
    )),
    
    % Mostrar el dialogo del mini botiquin
    send(D, open).

% Mostrar imagen
mostrar(V, D, Menu) :-
    new(I, image(V)),
    new(B, bitmap(I)),
    new(F2, figure),
    send(F2, display, B),
    new(D1, device),
    send(D1, display, F2),
    send(D, display, D1, point(10, 390)).  % Colocar la imagen en la parte inferior izquierda

% Dialogo para preguntar el nombre de una planta
:- pce_global(@name_prompter, make_name_prompter).

make_name_prompter(P) :-
    new(P, dialog),
    send(P, kind, transient),
    send(P, append, label(prompt)),
    send(P, append, new(TI, text_item(name, '', message(P?ok_member, execute)))),
    send(P, append, button(ok, message(P, return, TI?selection))),
    send(P, append, button(cancel, message(P, return, @nil))).

ask_name(Prompt, Label, Name) :-
    send(@name_prompter?prompt_member, selection, Prompt),
    send(@name_prompter, append, label(name_label, Label)),
    send(@name_prompter, open_centered),
    get(@name_prompter, confirm, Name).

% Funcion para mostrar botones de seleccion de plantas
show_plant_selection :-
    new(D, dialog('Seleccion de plantas')),
    send(D, append, label(selection_label, 'Seleccione una planta:')),
    forall(planta_medicinal(Planta),
           send(D, append, button(Planta, message(@prolog, pp, Planta)))),
    send(D, open).

% Funcion para mostrar indice de plantas y enfermedades
show_index :-
    new(D, dialog('Indice de Plantas Medicinales')),
    send(D, append, label(index_label, 'Plantas y lo que curan:')),
    forall(planta_medicinal(Planta),
           (   enfermedades_planta(Planta, Enfermedades),
               atomics_to_string(Enfermedades, ', ', EnfStr),
               send(D, append, new(TI, text_item(Planta, EnfStr))),
               send(TI, editable, @off)
           )),
    send(D, open).

% Funcion para buscar plantas por enfermedad
buscar_enfermedad :-
    new(D, dialog('Buscar por Enfermedad')),
    send(D, append, new(E, text_item(enfermedad))),
    send(D, append, button(buscar, message(@prolog, buscar_por_enfermedad, E?selection))),
    send(D, open).

buscar_por_enfermedad(Enfermedad) :-
    new(D, dialog('Resultados de Busqueda')),
    send(D, append, label(title, string('Plantas que curan: %s', Enfermedad))),
    forall((planta_medicinal(Planta), enfermedad(Planta, Enfermedad)), (
        send(D, append, label(nombre, string('%s', Planta)))
    )),
    send(D, open).

% Funcion para buscar por efecto
buscar_por_efecto :-
    new(D, dialog('Buscar por Efecto')),
    send(D, append, new(E, text_item(efecto))),
    send(D, append, button(buscar, message(@prolog, buscar_plantas_por_efecto, E?selection))),
    send(D, open).

buscar_plantas_por_efecto(Efecto) :-
    new(D, dialog('Resultados de Busqueda')),
    send(D, append, label(title, string('Plantas con efecto: %s', Efecto))),
    forall((planta_medicinal(Planta), efecto(Planta, Efecto)), (
        send(D, append, label(nombre, string('%s', Planta)))
    )),
    send(D, open).

% Menu interactivo de plantas que producen medicamentos
show_medicine_producing_plants :-
    new(D, dialog('Plantas que producen medicamentos')),
    send(D, append, label(index_label, 'Seleccione una planta:')),
    forall(medicamento(Planta, _),
           send(D, append, button(Planta, message(@prolog, show_medicines, Planta)))),
    send(D, open).

% Mostrar medicamentos de una planta especifica
show_medicines(Planta) :-
    new(D, dialog(string('Medicamentos de %s', Planta))),
    send(D, append, label(title, string('Medicamentos producidos por %s:', Planta))),
    forall(medicamento(Planta, Medicamento),
           send(D, append, label(medicine, string('- %s', Medicamento)))),
    send(D, open).

% Indice de Efectos
show_effect_index :-
    new(D, dialog('Indice de Efectos de Plantas Medicinales')),
    send(D, append, label(index_label, 'Plantas y sus efectos:')),
    forall(planta_medicinal(Planta),
           (   efectos_planta(Planta, Efectos),
               atomics_to_string(Efectos, ', ', EfectosStr),
               send(D, append, new(TI, text_item(Planta, EfectosStr))),
               send(TI, editable, @off)
           )),
    send(D, open).

efectos_planta(Planta, Efectos) :-
    findall(Efecto, efecto(Planta, Efecto), Efectos).

% Integracion de nuevas funciones en el menu principal
start :-
    new(D, dialog('Busqueda de plantas medicinales')),
    send(D, size, size(660, 600)),
    send(D, append, new(Menu, menu_bar)),
    send(Menu, append, new(BuscarPlanta, popup(buscarplanta))),
    send_list(BuscarPlanta, append,
               [menu_item(buscar, message(@prolog, show_plant_selection)),
               menu_item(indice, message(@prolog, show_index)),
               menu_item(buscar_enfermedad, message(@prolog, buscar_enfermedad)),
               menu_item(mini_botiquin, message(@prolog, mini_botiquin)),
               menu_item(buscar_efecto, message(@prolog, buscar_por_efecto)),
               menu_item(indice_efectos, message(@prolog, show_effect_index)),
               menu_item(medicamentos, message(@prolog, show_medicine_producing_plants))]),
    mostrar('C:\\prolog\\proyecto\\imagenes\\plantas.jpg', D, Menu),
    send(D, open).

% Mostrar informacion de la planta seleccionada
pp(Planta) :-
    planta_medicinal(Planta),
    new(D, dialog(Planta)),
    send(D, size, size(660, 600)),
    send(D, append, new(Menu, menu_bar)),
    send(D, display, text(Planta, center, bold), point(320, 5)),

    send(D, display, text('Propiedades:', left, normal), point(10, 50)),
    propiedades_planta(Planta, Propiedades),
    mostrar_lista(Propiedades, D, 70),

    send(D, display, text('Enfermedades que cura:', left, normal), point(10, 150)),
    enfermedades_planta(Planta, Enfermedades),
    mostrar_lista(Enfermedades, D, 170),

    send(D, display, text('Efectos:', left, normal), point(10, 250)),
    efectos_planta(Planta, Efectos),
    mostrar_lista(Efectos, D, 270),

    (receta(Planta, Receta) ->
        send(D, display, text('Receta:', left, normal), point(10, 350)),
        send(D, display, text(Receta, left, normal), point(10, 370))
    ; true),

    (iman(Planta, Foto),
     exists_file(Foto) ->  % Asegúrate de que la ruta del archivo exista
        mostrar(Foto, D, Menu)
    ; true),

    send(D, open, point(200, 200)).

% Mostrar lista de propiedades y enfermedades
mostrar_lista([], _, _).
mostrar_lista([H|T], D, Y) :-
    send(D, display, text(H, left, normal), point(10, Y)),
    Y1 is Y + 20,
    mostrar_lista(T, D, Y1).

propiedades_planta(Planta, Propiedades) :-
    findall(Propiedad, propiedad_real(Planta, Propiedad), Propiedades).

enfermedades_planta(Planta, Enfermedades) :-
    findall(Enfermedad, enfermedad(Planta, Enfermedad), Enfermedades).

efectos_planta(Planta, Efectos) :-
    findall(Efecto, efecto(Planta, Efecto), Efectos).

:- start.
