% base_conocimientos.pl

% Emociones y sus síntomas (en forma de preguntas)
conocimiento(ansiedad, [
    "Te cuesta dormir ultimamente?",
    "Te sientes inquieto o acelerado?",
    "Te cuesta concentrarte en tareas sencillas?"
]).

conocimiento(tristeza, [
    "Te sientes sin ganas de hacer cosas?",
    "Lloras con frecuencia sin razon clara?",
    "Sientes que nada te entusiasma ultimamente?"
]).

conocimiento(estres, [
    "Sientes mucha presion por tus responsabilidades?",
    "Tienes dolores fisicos sin causa medica clara?",
    "Te sientes al limite todo el tiempo?"
]).

conocimiento(depresion, [
    "Has perdido el interes por actividades que antes disfrutabas?",
    "Te sientes sin valor o con culpa constante?",
    "Tienes pensamientos negativos recurrentes?"
]).

conocimiento(agotamiento, [
    "Te sientes constantemente cansado a pesar de descansar?",
    "Sientes que no puedes más con tus responsabilidades?",
    "Te cuesta encontrar motivación incluso para lo básico?"
]).

% conocimiento(soledad, [
%     "Sientes que no tienes con quién hablar realmente?",
%     "Te sientes desconectado incluso rodeado de personas?",
%     "Pasas mucho tiempo solo y no por decisión propia?"
% ]).
% 
% conocimiento(frustracion, [
%     "Sientes que tus esfuerzos no rinden frutos?",
%     "Te enojas con facilidad por cosas pequeñas?",
%     "Sientes que siempre chocas contra una pared?"
% ]).
% 
% conocimiento(miedo, [
%     "Te preocupa constantemente que algo malo suceda?",
%     "Evitas situaciones por temor al resultado?",
%     "Tu corazon se acelera sin motivo claro?"
% ]).
% 
% conocimiento(aburrimiento, [
%     "Sientes que nada de lo que haces te estimula?",
%     "Te cuesta encontrar algo que te motive o interese?",
%     "El tiempo parece pasar muy lento ultimamente?"
% ]).
% 
% conocimiento(culpa, [
%     "Te sientes mal por algo que hiciste o dijiste?",
%     "No puedes dejar de pensar en un error pasado?",
%     "Sientes que lastimaste a alguien aunque no fuera tu intencion?"
% ]).
% 
% conocimiento(verguenza, [
%     "Te preocupa lo que otros piensen de ti constantemente?",
%     "Evitas hablar o actuar por miedo a hacer el ridiculo?",
%     "Te sientes juzgado incluso sin razon aparente?"
% ]).

% Recomendaciones por emocion
recomendacion(ansiedad, "Puede que estes con ansiedad leve. Trata de hablar con alguien de confianza o caminar al aire libre.").
recomendacion(tristeza, "Podrias estar pasando por tristeza. Intenta escribir como te sientes o hablar con alguien cercano.").
recomendacion(estres, "Podrias tener estres acumulado. Prueba ejercicios de respiracion o pausas activas.").
recomendacion(depresion, "Tal vez estes experimentando signos de depresion leve. Busca apoyo emocional y considera consultar a un profesional.").
recomendacion(agotamiento, "El agotamiento emocional es serio. Tomate un tiempo para descansar y desconectarte si puedes, y no dudes en pedir ayuda.").

% recomendacion(soledad, "Sentirse solo es más común de lo que parece. Busca reconectar con alguien cercano o únete a una actividad grupal.").
% recomendacion(frustracion, "La frustración puede aliviarse reformulando tus metas o tomando pausas. Hablarlo con alguien puede darte otra perspectiva.").
% recomendacion(miedo, "Reconocer tus miedos es el primer paso. Respira hondo y analiza si ese temor es real o imaginado.").
% recomendacion(aburrimiento, "Intenta probar algo nuevo o cambiar tu rutina diaria. A veces un pequeño cambio despierta motivación.").
% recomendacion(culpa, "Habla con la persona involucrada si puedes, y perdónate a ti mismo. Todos cometemos errores.").
% recomendacion(verguenza, "Recuerda que todos cometemos errores. Practicar la autoaceptación puede ayudarte a soltar esa carga.").

% Motor de inferencia
posible_diagnostico(Emocion, SintomasUsuario) :-
    conocimiento(Emocion, SintomasRequeridos),
    subset(SintomasRequeridos, SintomasUsuario).

% Verifica que todos los síntomas requeridos estén presentes
subset([], _).
subset([H|T], Lista) :-
    member(H, Lista),
    subset(T, Lista).
