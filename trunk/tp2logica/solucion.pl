
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult(['mostrar', 'tests']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Ejercicios %%%%

%% Ejercicio 1

%dimension(+Tablero, -N, -M).
dimension(Tablero, N, M):- length(Tablero, N), nth0(0, Tablero, L), length(L, M).
% posicion(+Tablero, -I, -J).
posicion(Tablero, I, J):- dimension(Tablero, N, M), between(0, N, I), between(0, M, J).


%% Ejercicio 2

% subtablero(+Tablero, +I, +J, -Subtablero).


%% Ejercicio 3

% transponer(+Tablero, -Transpuesto).


%% Ejercicio 4

% asignarPeso(+Silueta, +Peso, -SiluetaConPeso).


%% Ejercicio 5

% ubicarSilueta(+Silueta, -I, -J, -Tablero).


%% Ejercicio 6

% ubicarPieza(+NombrePieza, +Peso, +DiccionarioPiezas, -I, -J, -Tablero).


%% Ejercicio 7
% solucionValida(-Juego).


%% Ejercicio 8
% resolver(+DiccionarioPiezas, +PiezasDisponibles, -Juego).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
