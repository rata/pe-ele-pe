
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% Tests %%%%

tablero1([
  [_, _, _, _],
  [_, o, _, _],
  [_, _, o, _],
  [_, _, _, _]
]).

silueta1([
  [x, x],
  [_, x]
]).

algunasPiezas([
  orientacion(palito, [[x, x, x, x]]),
  orientacion(palito, [[x], [x], [x], [x]]),
  orientacion(ele, [[x, _], [x, _], [x, x]]),
  orientacion(ele, [[_, _, x], [x, x, x]]),
  orientacion(ele, [[x, x], [_, x], [_, x]]),
  orientacion(ele, [[x, x, x], [x, _, _]]),
  orientacion(ese, [[_, x, x], [x, x, _]]),
  orientacion(ese, [[x, _], [x, x], [_, x]])
]).

tetrominos([
  orientacion(palito, [[x, x, x, x]]),
  orientacion(palito, [[x], [x], [x], [x]]),
  orientacion(ele, [[x, _], [x, _], [x, x]]),
  orientacion(ele, [[_, _, x], [x, x, x]]),
  orientacion(ele, [[x, x], [_, x], [_, x]]),
  orientacion(ele, [[x, x, x], [x, _, _]]),
  orientacion(jota, [[x, x], [x, _], [x, _]]),
  orientacion(jota, [[x, _, _], [x, x, x]]),
  orientacion(jota, [[_, x], [_, x], [x, x]]),
  orientacion(jota, [[x, x, x], [_, _, x]]),
  orientacion(ese, [[_, x, x], [x, x, _]]),
  orientacion(ese, [[x, _], [x, x], [_, x]]),
  orientacion(zeta, [[x, x, _], [_, x, x]]),
  orientacion(zeta, [[_, x], [x, x], [x, _]]),
  orientacion(cuadrado, [[x, x], [x, x]]),
  orientacion(te, [[x, x, x], [_, x, _]]),
  orientacion(te, [[x, _], [x, x], [x, _]]),
  orientacion(te, [[_, x, _], [x, x, x]]),
  orientacion(te, [[_, x], [x, x], [_, x]])
]).

tablero2([
  [_, _, _, _],
  [_, 1, 2, _],
  [_, 3, 4, _],
  [_, _, 5, _]
]).

juego1(juego([
  [_, _, _, _],
  [_, _, _, _],
  [_, _, _, _],
  [_, _, _, _]
], [_, 15, _, _], [_, _, 14, _])).

% juego1(Juego), algunasPiezas(DiccionarioPiezas), PiezasDisponibles = [pieza(palito, 4), pieza(ele, 7), pieza(ele, 3), pieza(ese, 2)], resolver(DiccionarioPiezas, PiezasDisponibles, Juego), mostrarJuego(Juego).

juego2(juego([
  [1, _, _, 7],
  [_, _, _, _],
  [0, _, _, _],
  [_, _, 5, _]
], [_, _, _, _], [4, 6, 11, 16])).

% juego2(Juego), algunasPiezas(DiccionarioPiezas), PiezasDisponibles = [pieza(ese, 2), pieza(ele, 3), pieza(ese, 1)], resolver(DiccionarioPiezas, PiezasDisponibles, Juego), mostrarJuego(Juego).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

