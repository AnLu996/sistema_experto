:- dynamic respuesta/1.
:- consult('base_conocimientos.pl').

agregar_respuestas([]).
agregar_respuestas([R|Rs]) :-
    assertz(respuesta(R)),
    agregar_respuestas(Rs).

iniciar :-
    encontrar_diagnostico,
    halt.

encontrar_diagnostico :-
    findall(R, respuesta(R), Respuestas),
    posible_diagnostico(E, Respuestas),
    recomendacion(E, Reco),
    format('~w|~w', [E, Reco]),
    !.

encontrar_diagnostico :-
    write('ninguno|No se pudo determinar un estado claro. Considera buscar ayuda profesional.').
