
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Predicados provistos para mostrar juegos y tableros por pantalla %%%%

% mostrarJuego(+Juego).

mostrarJuego(juego(Tablero, RF, RC)) :-
  nl,
  format("    |  "), mostrarFila(RC),
  format("----+--"), mostrarSep(RC),
  mostrarTableroRF(Tablero, RF).

mostrarX(X) :- var(X), format("__  ").
mostrarX(X) :- nonvar(X), not(number(X)), format(" ~p  ", [X]).
mostrarX(X) :- nonvar(X), number(X), X < 10, format(" ~p  ", [X]).
mostrarX(X) :- nonvar(X), number(X), X >= 10, format("~p  ", [X]).

mostrarFila([]) :- nl.
mostrarFila([X|Xs]) :- mostrarX(X), mostrarFila(Xs).

mostrarSep([]) :- nl.
mostrarSep([_|Xs]) :- format("----"), mostrarSep(Xs).

mostrarTableroRF([], []) :- nl.
mostrarTableroRF([F|Fs], [R|Rs]) :-
  mostrarX(R), format("|  "), mostrarFila(F), mostrarTableroRF(Fs, Rs).

% mostrarTablero(+Tablero).

mostrarTablero([]) :- nl.
mostrarTablero([F|Fs]) :- mostrarFila(F), mostrarTablero(Fs).

