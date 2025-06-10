% motor_inferencias.pl

:- dynamic respuesta/1.
:- consult('base_conocimientos.pl').

% Agregar respuestas desde Python
agregar_respuestas([]).
agregar_respuestas([R|Rs]) :-
    assertz(respuesta(R)),
    agregar_respuestas(Rs).

% Diagnóstico e impresión
iniciar :-
    encontrar_diagnostico,
    halt.

encontrar_diagnostico :-
    findall(R, respuesta(R), Respuestas),
    posible_diagnostico(E, Respuestas),
    recomendacion(E, R),
    format('~w|~w', [E, R]),
    !.

encontrar_diagnostico :-
    write('ninguno|No se pudo determinar un estado claro. Considera buscar ayuda profesional.').
