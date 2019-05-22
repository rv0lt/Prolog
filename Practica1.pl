
alumno_prode('Parra','Garcia','Alejandro Carmelo','Y16I028').
alumno_prode('Vladimirov','Stoyanov','Petko','Y16I019').
alumno_prode('Revuelta','Martinez','Alvaro','Y16I009').

%###########################
%###Predicados auxiliares###
%###########################


%comprueba si dados dos numeros(X e Y) el primero es menor o igual que el segundo
menor_igual(0,_).
menor_igual(s(X),s(Y)):-
	menor_igual(X,Y).

%devuelve yes si se pasa uno de los colores que pueden tener una pieza:r (Rojo),a(azul),v(verde) o am(amarillo)
color(r). %rojo
color(a). %azul
color(v). %verde
color(am).%amarillo

%Este predicado saca el primer color de la primera lista tras ello llama a esta color para ver si este esta en la segunda lista. Despues de eto se llama a si mismo con el resto de la lista de Colores1
color_esta_incluido([],_).
color_esta_incluido([C1|Colores1],Colores2):-
	esta_color(C1,Colores2),
	color_esta_incluido(Colores1,Colores2).

%metodo que comprueba si un color C1 esta en la lista del segundo argumento.
esta_color(C1,[C2|_]):-
	C1=C2.
esta_color(C1,[C2|Colores]):-
	C1\=C2,
	esta_color(C1,Colores).

%------------------------------------------------------------
%############################
%###Predicados principales###
%############################

%caso base de la torre en la que solo hay una pieza
%mientras que pieza cumpla las condiciones de ser una pieza la torre al tener solo una pieza es imposible que se incumplan sus requisitos (que la pieza superior sea menor o igual que la inferior)
esTorre([pieza(_,_,_,C)]):-
	color(C).
	%esPieza(pieza(X,Y,Z,C)).
%Comprobamos que la primera pieza es una pieza. La segunda pieza no hace falta comprobarla pues en la siguiente iteracion se comprobara o sino se comprobara en el caso base)
%Despues se comprueba que la pieza mas elevada tiene unas dimensiones menores o iguales que la siguiente pieza con piezas_menor_igual
%Y por ultimo hacemos la recursividad con la lista de piezas eliminando la primera
esTorre([pieza(X0,_,Z0,C0),pieza(X1,Y1,Z1,C1)|Lista]):-
	color(C0),
	menor_igual(X0,X1),
	menor_igual(Z0,Z1),
	esTorre([pieza(X1,Y1,Z1,C1)|Lista]).


%alturaTorre esta dividia tambien en alturaTorreCont para evitar comprobar en cada iteracion si la lista es una torre. AlturaTorre llama a esTorre para coprobar si es una torre e inmediatamente despues llama a alturaTorreCont para comprobar la altura

alturaTorre([pieza(X0,Y0,Z0,C0)],A):-
	esTorre([pieza(X0,Y0,Z0,C0)]),
	alturaTorreCont([pieza(X0,Y0,Z0,C0)],A).

alturaTorre([pieza(X0,Y0,Z0,C0),pieza(X1,Y1,Z1,C1)|Lista],A):-
	esTorre([pieza(X0,Y0,Z0,C0),pieza(X1,Y1,Z1,C1)|Lista]),
	alturaTorreCont([pieza(X0,Y0,Z0,C0),pieza(X1,Y1,Z1,C1)|Lista],A).

%AlturaTorreCont va decrementando la altura de la primera pieza a la vez que la altura general, cuando la primera pieza tiene altura 0 la elimina de la lista y procede con la siguiente pieza hasta que la lista esta vacia y ahora si al acabarse la altura de la ultima pieza la altura general es 0 se cumple.
alturaTorreCont([pieza(_,0,_,_)],0).

alturaTorreCont([pieza(_,0,_,_),pieza(X1,Y1,Z1,C1)|Lista],A):-
	alturaTorreCont([pieza(X1,Y1,Z1,C1)|Lista],A).

alturaTorreCont([pieza(X0,s(Y0),Z0,C0)|Lista],s(A)):-
	alturaTorreCont([pieza(X0,Y0,Z0,C0)|Lista],A).


%coloresTorre usa el mismo funcionamiento que alturaTorre hacemos una llamada a esTorre y despues llamamos a coloresTorreCont que se encarga de ir comprobando si los colores de la pieza son los mismos que los de la lista de colores
coloresTorre([pieza(X0,Y0,Z0,C0)],Colores):-
	esTorre([pieza(X0,Y0,Z0,C0)]),
	coloresTorreCont([pieza(X0,Y0,Z0,C0)],Colores).

coloresTorre([pieza(X0,Y0,Z0,C0),pieza(X1,Y1,Z1,C1)|Lista],Colores):-
	esTorre([pieza(X0,Y0,Z0,C0),pieza(X1,Y1,Z1,C1)|Lista]),
	coloresTorreCont([pieza(X0,Y0,Z0,C0),pieza(X1,Y1,Z1,C1)|Lista],Colores).

%ColoresTorreCont se encarga de ir comprobando si los colores de la pieza son los mismos que los de la lista de colores
%caso base con la lista de piezas vacía, que devuelve yes si la lista de colores tambien está vacía
coloresTorreCont([],[]).

%Saca el color de la primer pieza de la lista y lo compara con el primer color de la lista de colores 
coloresTorreCont([pieza(_,_,_,C0)|Lista],[C1|Colores]):-
	C0=C1,
	coloresTorreCont(Lista,Colores).


%Este Predicado saca los colores de ambas construcciones en las listas Colores1 y 2
%tras esto llamamos al metodo color_esta_incluido.
coloresIncluidos(Construccion1,Construccion2):-
	coloresTorre(Construccion1,Colores1),
	coloresTorre(Construccion2,Colores2),
	color_esta_incluido(Colores1,Colores2).

%-------------------------------------------------------------------
%Segunda Parte
%-------------------------------------------------------------------
%###########################
%###Predicados auxiliares###
%###########################
%Predicado que cuenta el numero de clavos de una Fila y comprueba si es par
esFilaPar(Fila):-
	numeroClavos(Fila,N),
	par(N).

%Este predicado cuenta el numero de clavos de una fila ignorando las b
%Saca el primer elemento de la Fila y si es una b se hace llamada recusiva sin ese elemento
%en otro caso, saca el primer color, hace llamada a color para comprobar que es un color valido, suma un numero de Peano
%luego una llamada recursiva hasta llegar al caso base de tener una lista vacía
numeroClavos([],0).
numeroClavos([b|Fila],N):-
	numeroClavos(Fila,N).
numeroClavos([C|Fila],s(N)):-
	color(C),
	numeroClavos(Fila,N).

%Predicado que comprueba si un numero es par (se asume que el 0 no es par pues no pueden darnos una matriz con una fila vacia)
par(s(s(0))).
par(s(s(N))):-
	par(N).

%comprueba si un numero N1 es menor estricto que N2
menor_estricto(0,s(_)).
menor_estricto(s(N1),s(N2)):-
	menor_estricto(N1,N2).

%Comprueba que una Fila de piezas sea fila. Para ello quita las b y saca el color de cada fila
%si el color(C) es un color valido, hace llamada recursiva a esFila hasta llegar al caso base.
esFila([]).
esFila([b|Fila]):-
	esFila(Fila).
esFila([C|Fila]):-
	color(C),
	esFila(Fila).
%---------------------------------------------------

%############################
%###Predicados principales###
%############################

%esEdificioPar saca la primera fila de la matriz con ella llama a esFilaPar, este predicado llama a numeroClavos y despues comprueba si el numero de clavos es par llamando a par/1. El predicado numeroClavos/2 lo que comprueba es si el numero de clavos (sin contar las b) es igual al segundo parametro, aunque en nuestro caso lo usamos para contar el numero de clavos de una fila para despues poder llamar a par. (Consideramos que el numero 0 no es par pues no tiene sentido tener una fila vacia)

%Caso base en el que solo hay una fila. Comprueba si esa fila cumple la condicion de tener un numero par de clavos sin contar las b 
esEdificioPar([Fila]):-
	esFilaPar(Fila).
%Predicado que comprueba fila a fila si el numero de clavos es par, llamando a otro predicado (esFilaPar)
esEdificioPar([Fila|Lista]):-
	esFilaPar(Fila),
	esEdificioPar(Lista).


%Sacamos las dos primeras filas de la lista(las dos superiores de la piramide), contamos el numero de clavos usando numeroClavos tras esto comprobamos que los clavos de la fila superior es menor estricto de la fila inferior. Tras esto llamamos a esEdificioPiramide con la construccion sin la fila superior

%caso base de esEdificioPiramide con solo una fila
%La condición de Pirmide se cumplirá en este caso si la fila cumple las condiciones para ser Fila
esEdificioPiramide([Fila]):-
	esFila(Fila).

%Comprueba si una construccion es una piramide
esEdificioPiramide([Fila1,Fila2|Lista]):-
	numeroClavos(Fila1,N1),
	numeroClavos(Fila2,N2),
	menor_estricto(N1,N2),
	esEdificioPiramide([Fila2|Lista]).
