
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult(['mostrar', 'tests']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Ejercicios %%%%

%% Ejercicio 1

%dimension(+Tablero, -N, -M).
dimension(T,N,M):- nonvar(T), dimensionNonvar(T,N,M).
dimension(T,N,M):- var(T), dimensionVar(T,N,M).
dimensionNonvar(T, N, M):- length(T, N), nth0(0, T, L), length(L, M).
dimensionVar(T, N, M):- length(T, N), repetir(N,M,X), maplist(length,T,X).
%dimension(T, N, M):- length(T, N) , N1 is N-1, forall( between(0,N1,X), (nth0(X,T,L), length(L, M)) ).
%dimension(T, N, M):- length(T, N), nth0(0,T,L), length(L,M) ,forall( member(X,T), length(X,M) ).
%dimension(T, N, M):- length(T, N), nth0(0,T,L), length(L,M), checkCols(T,1,N,L).

repetir(0,N,[]).
repetir(I,N,[N|M]):- I > 0, I1 is I-1, repetir(I1,N,M).

% posicion(+Tablero, -I, -J).
posicion(T, I, J):- dimension(T, N, M), N1 is N-1, M1 is M-1, between(0, N1, I), between(0, M1, J).


%% Ejercicio 2

% subtablero(+Tablero, +I, +J, -Subtablero).
%subtablero(T, I, J, S):- dimension(T,N,M), NewN is N-I, NewM is M-J, N1 is N-1, M1 is M-1 ,dimension(S, NewN, NewM), 
%		forall(between(I,N1,F),forall(between(J,M1,C),(
%					FI is F-I,
%					CJ is C-J,
%					nth0(T,F,X1),
%					nth0(X1,C,X2),
%			       		nth0(S,FI,Y1),
%					nth0(Y1,CJ,X2)))).


% subtablero(+Tablero, +I, +J, -Subtablero).
subtablero(T, I, J, L):- dimension(T, N, M), I >= N, L = [].
subtablero(T, I, J, L):- dimension(T, N, M), I < N, I2 is I+1, subtablero(T, I2, J, L2), nth0(I, T, Fila),
					sacarN(J, Fila, Col), L = [Col | L2].

% sacarN(+N, +L, -L2)
sacarN(0, L1, L1).
sacarN(N, [L|L1], L2):- N1 is N-1, sacarN(N1, L1, L2).

%% Ejercicio 3

% transponer(+Tablero, -Transpuesto).
transponer(T, Trans):- transponerAux(T, Trans, 0).


transponerAux(T,Trans,I):- dimension(T, N, M), I>=M, Trans=[].
transponerAux(T,Trans,I):- dimension(T, N, M), I< M, I1 is I+1, transponerAux(T, Rec, I1), getCol(T, I, Col),
				Trans = [Col|Rec].
		
%getCol(T,C,L):- dimension(T,N,M), C < M, getColAux(T,C,0,L).
getCol(T,C,L):- getColAux(T,C,0,L).

getColAux(T,C,I,L):- dimension(T,N,M), I>=N, L=[].
getColAux(T,C,I,L):- dimension(T,N,M), I< N, I1 is I+1, getColAux(T, C, I1, L1), nth0(I, T, F), nth0(C, F, R), L = [R|L1].

%% Ejercicio 4

% asignarPeso(+Silueta, +Peso, -SiluetaConPeso).
%asignarPeso(S, P, SP):- %dimension(S,F,C),F1 is F-1, C1 is C-1, 
%			forall(
%				(between(0,1,I), between(0,1,J), elemento(S,I,J,E)), 
%						nonvar(E) -> elemento(SP,I,J,P) ; true 
%				).

% asignarPeso(+Silueta, +Peso, -SiluetaConPeso).
asignarPeso(S, P, SP):- asignarAux(S,0,P,SP).

asignarAux(S,I,P,SP):- dimension(S,N,M), I>=N, SP=[].
asignarAux(S,I,P,SP):- dimension(S,N,M), I< N, IR is I+1, asignarAux(S, IR, P, LR), nth0(I, S, F), reformado(F,P,C),
				SP=[C|LR].

reformado([],P,C):- C = [].
reformado([L|LS],P,[C|CS]):- reformado(LS,P,CS), ((nonvar(L), C = P )| var(L)).

elemento(S,I,J,E):- nth0(I,S,R), nth0(J,R,E).


%% Ejercicio 5

%ubicarSilueta(+Silueta, -I, -J, -Tablero).
ubicarSilueta(S, I, J, T):- posicion(T,I,J), subtablero(T,I,J,ST), entraSiluetaArriba(S,ST).

%rellenarSilueta(S, N, M, SR):- dimension(S, N1, M1), N >= N1, M >= M1.
%rellenarSilueta(S, N, M, SR):- dimension(S, N1, M1), N < N1, M < M1, dimension(SR,N,M) .

entraSiluetaArriba(S,T):- length(S,LS), length(T,LT), LS =< LT, L is LT-LS, recortar(T,L,TR), maplist(entraFila,S,TR).
%entraSiluetaArriba(S,T):- length(S,LS), length(T,LT), LS = LT, maplist(entraFila,S,T).

%entraFila(R,N):-maplist(entraCelda,R,N).
entraFila(S,T):- length(S,LS), length(T,LT), LS =< LT, L is LT-LS, recortar(T,L,TR), maplist(entraCelda,S,TR).
%entraFila(S,T):- length(S,LS), length(T,LT), LS = LT, maplist(entraCelda,S,T).

%recortar(+List,+N,?List)
recortar(L,N,R):- reverse(L,RL), sacarN(N,RL,RLR), reverse(RLR,R).

entraCelda(X,Y):-nonvar(X) -> var(Y) ; true.




%% Ejercicio 6

% ubicarPieza(+NombrePieza, +Peso, +DiccionarioPiezas, -I, -J, -Tablero).


%% Ejercicio 7
% solucionValida(-Juego).


%% Ejercicio 8
% resolver(+DiccionarioPiezas, +PiezasDisponibles, -Juego).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%