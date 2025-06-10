
% motor_inferencias.pl
:- dynamic respuesta/1.
:- consult('base_conocimientos.pl').

iniciar :-
    hacer_preguntas,
    encontrar_diagnostico,
    limpiar.

hacer_preguntas :-
    sintoma(_, Pregunta),
    format("~w (s/n): ", [Pregunta]),
    read(Resp),
    procesar_respuesta(Pregunta, Resp),
    fail.
hacer_preguntas.

procesar_respuesta(Pregunta, s) :- assert(respuesta(Pregunta)).
procesar_respuesta(_, _).

encontrar_diagnostico :-
    diagnostico(Estado),
    recomendacion(Estado, Reco),
    format("Diagnóstico: ~w~n", [Estado]),
    format("Sugerencia: ~w~n", [Reco]),
    !.
encontrar_diagnostico :-
    write("No se pudo determinar un estado claro. Considera buscar ayuda profesional.").

limpiar :- retractall(respuesta(_)).
