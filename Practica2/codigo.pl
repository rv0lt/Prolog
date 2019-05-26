alumno_prode('Parra','Garcia','Alejandro Carmelo','Y16I028').
alumno_prode('Vladimirov','Stoyanov','Petko','Y16I019').
alumno_prode('Revuelta','Martinez','Alvaro','Y16I009').

%menor(A,B,Comp,M):-
%	functor(Comp,X,0),
%        ( compare(X, A, B) -> M=A ; M=B ).

menor(A,B,Comp,M):-
	functor(X,Comp,2),
	arg(1,X,A),
	arg(2,X,B),
	(call(X) -> M=A; M=B ).

%Caso base en el que si A o B son variables libres es true,
%pues "Una variable libre es igual a cualquier otro termino"
menor_o_igual(A,B):-
	var(A);                  
	var(B).
%Usamos functor/3 para obtener el termino y la ariedad tanto de A como de B
%Comparamos el termino de A y de B, Si el de A es menor que el de B ya se cumple
%la condicion y por tanto es TRUE. Por el contrario si no es menor miramos si
%son iguales, si es asi tenemos que comparar la ariedad. Si la ariedad de A es
%menor que la ariedad de B se cumple que A es menor_o_igual de B. Si la ariedad de A
%no es menor que el de B, comparamos si es igual. Si ambas ariedades son iguales
%llamamos a menor_o_igual_aux/2.
%En el caso que:
%A es menor que B devolvemos TRUE.
%A es igual a B, y la ariedad de A es menor que la de B, devolvemos TRUE.
%A es igual a B, y la ariedad de A y la de B son iguales, llamamos a menor_o_igual_aux/2.
menor_o_igual(A,B):-
	functor(A,NA,La),
	functor(B,NB,Lb),
	(
	    NA @< NB; %al ser un or si NA @< NB no evalua el resto
	    (
		NA == NB, %si son iguales evaluo la airedad
		(
		    La < Lb; %al ser un or si La < Lb no evalua el resto
		    (
			La =:= Lb, %si la ariedad es la misma llamo 
			menor_o_igual_aux(A,B) %a menor_o_igual_aux/2.
		    )
		)
	    )
	).

%Predicado auxiliar al cual solo se le llama si dos terminos son iguales y con la misma ariedad
%Este predicado va elemento por elemento comparandolos. Si el de A es menor que el de B ya cumple
%la condicion(segun lo especificado en el enunciado). si son iguales compruebo el siguiente termino,
%En cualquier otro caso es false

%CASO BASE en el que la ariedad de A es 0,por lo que ambos terminos son iguales.
%Esto es asi pues a este predicado solo se le llama cuando el termino de A y B son iguales y con la
%misma ariedad, por lo que si la ariedad es 0 son identicos.
menor_o_igual_aux(A,_):-
	functor(A,_,XA),
	XA == 0.

%En el caso de que la ariedad no sea 0, se obtiene el primer termino de A y de B, que son los que se
%han de comparar. Llamamos a menor_o_igual para compararlos, devuelve true cuando A es menor o igual a B
%y entonces si A distinto de B, significa que A es menor que B por lo que es true.
menor_o_igual_aux(A,B):-
	arg(1,A,ElemA),
	arg(1,B,ElemB),
	menor_o_igual(ElemA,ElemB),
	\+soy_igual(ElemA,ElemB).

%Caso en el que el termino 1 de A y de B son identicos, creamos nuevos predicados de ariedad 1 menor que
%el de A y de B, son los mismos terminos salvo el primero(que ya sabemos que es identico).
%Tras esto llamamos a menor_o_igual_aux/2 con los nuevos predicados.
%Ejem. A=p(a,b,c) B=p(a,b,d), comparamos el primer termino, 'a' es igual a 'a'. Genero nuevos predicados,
%NuevaA=p(b,c), NuevaB=p(b,d) y hacemos la recursividad.
menor_o_igual_aux(A,B):-
	arg(1,A,ElemA),                   %Obtenemos el primer termino de A
	arg(1,B,ElemB),	                  %Obtenemos el primer termino de B
	soy_igual(ElemA,ElemB),           %Comparamos si son iguales los terminos.
	functor(A,MA,XA),                 %Obtenemos el termino y la ariedad de A
	functor(B,MB,XB),                 %Obtenemos el termino y la ariedad de B
	Xnuevo is XA-1,                   %Nueva ariedad para los nuevos terminos
	functor(NuevaA,MA,Xnuevo),        %Creamos NuevaA con el termino de A y la nueva ariedad
	functor(NuevaB,MB,Xnuevo),        %Creamos NuevaA con el termino de A y la nueva ariedad
	reducir_predicado(A,NuevaA,1,XA), %Llamada a reducir_predicado con A, NuevaA, un cont, y un Tamanno
	reducir_predicado(B,NuevaB,1,XB),!, %Llamada a reducir_predicado con B, NuevaB, un cont, y un Tamanno
	menor_o_igual_aux(NuevaA,NuevaB). %Llamada recursiva con los nuevos predicados.


%Predicado auxiliar que reduce en 1 la ariedad de P1 eliminando el primer termino.
%Ejem. P1=p(1,2,3,4) -> P2=p(2,3,4).
reducir_predicado(P1,P2,Cont,Tam):-
	Cont =\= Tam,
	Cont2 is Cont+1,
	arg(Cont2,P1,M),
	arg(Cont,P2,M),
	reducir_predicado(P1,P2,Cont2,Tam);
	true.
		
	
soy_igual(A,B):-
	var(A);
	var(B);
	A == B.
%PARTE 3

listas_hojas([],[]).
listas_hojas([H|L],[tree(H,void,void)|HOJAS]):-
	lista_hojas(L,HOJAS).

hojas_arbol([],[]).
hojas_arbol([tree(H,void,void)|[]],tree(H,void,void)).
hojas_arbol([tree(H1,void,void),tree(H2,void,void)|HOJAS],ARBOL):-
	hojas_arbol(HOJAS,tree(P,R1,R2)),
	menor(H1,H2,<,M),
	menor(M,P,<,M2),
	ARBOL = tree(M2,tree(M,tree(H1,void,void),tree(H2,void,void)),tree(P,R1,R2)).
	
	