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
%llamamos a menor_o_igual_aux/3.
%En el caso que:
%A es menor que B devolvemos TRUE.
%A es igual a B, y la ariedad de A es menor que la de B, devolvemos TRUE.
%A es igual a B, y la ariedad de A y la de B son iguales, llamamos a menor_o_igual_aux/2.
menor_o_igual(A,B):-
	functor(A,NA,_),
	functor(B,NB,_),
	NA @< NB. %NA @< NB no hace falta evaluar el resto.
menor_o_igual(A,B):-
	functor(A,NA,La),
	functor(B,NB,Lb),
	NA == NB, %si son iguales evaluo la airedad
	(
	    La < Lb; %al ser un or si La < Lb no evalua el resto
	    (
		La =:= Lb, %si la ariedad es la misma llamo
		(
		    menor_o_igual_aux(A,B,1) %a menor_o_igual_aux/2.
		)
	    )
	).

%Predicado auxiliar al cual solo se le llama si dos terminos son iguales y con la misma ariedad
%Este predicado va elemento por elemento comparandolos. Si el de A es menor que el de B ya cumple
%la condicion(segun lo especificado en el enunciado). si son iguales compruebo el siguiente termino,
%En cualquier otro caso es false

%Si la ariedad de A es 0 significa que ya hemos evaluado todos los terminos y son iguales por tanto A es igual a B y TRUE
menor_o_igual_aux(A,_,N):-
	functor(A,_,La),
	N1 is N-1,
	La =:= N1.
%Sacamos el termino N de A y de B, si nos iguales hago recursividad avanzando N para comparar el siguiente termmino.
menor_o_igual_aux(A,B,N):-
	arg(N,A,ElemA),
	arg(N,B,ElemB),
	soy_igual(ElemA,ElemB),!,
	N1 is N+1,
	menor_o_igual_aux(A,B,N1).
%En este caso sabemos que el termino N de A y B no es igual, por lo que si es menor devolvemos true yno hace falta comparar mas.
menor_o_igual_aux(A,B,N):-
	arg(N,A,ElemA),
	arg(N,B,ElemB),
	menor_o_igual(ElemA,ElemB).



%Predicado auxiliar que se usa para evaluar si dos terminos son iguales
%Dos terminos son iguales si alguno de ellos es variable libre.
%Dos terminos son iguales los nombres de ambos, tienen la misma ariedad y sus argumentos son identicos.
soy_igual(A,B):-
	var(A);
	var(B).
%Miramos que los nombres y la ariedad de A y B sean iguales, si es asi llamo a soy/igual_aux/3 que avalua cada argumento.
soy_igual(A,B):-
	functor(A,NA,La),
	functor(B,NB,Lb),
	NA == NB,
	La =:= Lb,
	soy_igual_aux(A,B,1).
%caso base en el que ya se han comparado todos los argumentos, por lo tanto son iguales
soy_igual_aux(A,_,N):-
	functor(A,_,La),
	N1 is N-1,
	La =:= N1.
%Sacamos el argumento N de A y B y miramos si son iguales llamando a soy_igual/2
%si son iguales avanzamos N y hacemos recursividad
soy_igual_aux(A,B,N):-
	arg(N,A,ElemA),
	arg(N,B,ElemB),
	soy_igual(ElemA,ElemB),
	N1 is N+1,
	soy_igual_aux(A,B,N1).
%PARTE 3

listas_hojas([],[]).
listas_hojas([H|L],[tree(H,void,void)|HOJAS]):-
	lista_hojas(L,HOJAS).


hojas_arbol([tree(P,I,D)|[]],_,tree(P,I,D)).
hojas_arbol([tree(P1,I1,D1),tree(P2,I2,D2)|HOJAS],Comp,ARBOL):-
	menor(P1,P2,Comp,M),
	(
	    hojas_arbol_aux(tree(M,tree(P1,I1,D1),tree(P2,I2,D2)),HOJAS,Comp,ARBOL);
	    ARBOL = tree(M,tree(P1,I1,D1),tree(P2,I2,D2))
	).

hojas_arbol_aux(tree(P1,I1,D1),[tree(P2,I2,D2)|[]],Comp,ARBOL):-
	menor(P1,P2,Comp,M),
	ARBOL = tree(M,tree(P1,I1,D1),tree(P2,I2,D2)).
hojas_arbol_aux(tree(P0,I0,D0),[tree(P1,I1,D1),tree(P2,I2,D2)|HOJAS],Comp,ARBOL):-
	menor(P1,P2,Comp,M),
	menor(P0,M,Comp,M0),
	(
	    
	    hojas_arbol_aux(tree(M0,tree(P0,I0,D0),tree(M,tree(P1,I1,D1),tree(P2,I2,D2))),HOJAS,Comp,ARBOL);
	    ARBOL = tree(M0,tree(P0,I0,D0),tree(M,tree(P1,I1,D1),tree(P2,I2,D2)))
	).





	
%par(X):- 0 is X mod 2.



%hojas_arbol(HOJAS,Comp,ARBOL):-
%	length(HOJAS,N),
%	(par(N) -> hojas_arbol_aux_par(HOJAS,Comp,ARBOL) ; hojas_arbol_aux_impar(HOJAS,Comp,ARBOL)),
%	 ARBOL = ARBOL.

%hojas_arbol_aux_par([tree(P1,I1,D1)|tree(P2,I2,D2)],Comp,ARBOL):-
%	menor(P1,P2,Comp,M),
%	ARBOL = tree(M,tree(P,I1,D1),tree(P2,I2,D2)).
%
%
%hojas_arbol_aux_par([ [] | tree(H1,I1,D1),tree(H2,I2,D2)],Comp,ARBOL):-
%	menor(H1,H2,Comp,M),
%	ARBOL = tree(M,tree(H1,I1,D1),tree(H2,I2,D2)).
%hojas_arbol_aux_par([HOJAS|[tree(H1,I1,D1)|tree(H2,I2,D2)]],Comp,ARBOL):-
%	hojas_arbol_aux_par(HOJAS,Comp,tree(P,R1,R2)),
%	menor(H1,H2,Comp,M),
%	menor(M,P,Comp,M2),
%	ARBOL = tree(M2,tree(P,R1,R2),tree(M,tree(H1,I1,D1),tree(H2,I2,D2))).
%
%hojas_arbol_aux_impar([HOJAS],Comp,ARBOL):-
%	
%	hojas_arbol_aux_par(HOJAS,Comp,tree(P,R1,R2)),
%	menor(H,P,Comp,M),
%	ARBOL = tree(M,tree(P,R1,R2),tree(H,void,void)).






	 	
%hojas_arbol_aux(tree(P,I,D),Comp,tree(P2,I2,D2)):-
	
%hay_mas_hojas([_,_|[_]]).
%constuir_rama(ARBOLz,tree(P2,I2,D2),Comp):-
%	ARBOLz = tree(P,I,D),
%	menor(P,P2,Comp,M),
%	ARBOLz is tree(M,tree(P,I,D),tree(P2,I2,D2)).


%hojas_arbol([tree(H1,void,void),tree(H2,void,void)|HOJAS],Comp,ARBOL):-
%	(
%	   % hojas_arbol(HOJAS,Comp,tree(P,R1,R2)),
%	    menor(H1,H2,Comp,M),
%	    %menor(M,P,Comp,M2),
%	    constuir_rama(H1,H2,M,ARBOL_AUX),
%	    
%	    hojas_arbol(HOJAS,Comp,tree(P,R1,R2)), 
%	    menor(M,P,Comp,M2),
%	    
%	    ARBOL = tree(M2,ARBOL_AUX,tree(P,R1,R2))
%	);
%	(
%	    hojas_arbol(HOJAS,[]),
%	    menor(H1,H2,Comp,M),
%	    ARBOL=tree(M,tree(H1,void,void),tree(H2,void,void))
%	).
	
