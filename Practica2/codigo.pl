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





lista_hojas([],[]).
lista_hojas([H|L],[tree(H,void,void)|HOJAS]):-
  lista_hojas(L,HOJAS).


%Si esta vacio la lisa de hojas no hay arbol.
hojas_arbol([],_,_).
%Si solo hay una hoja, esa es el arbol
hojas_arbol([X],_,X).
%Si hay mas hojas se llama a hojas_arbol_aux/4 para que genere el arbol
hojas_arbol(Lista,Comp,Arbol):-
	hojas_arbol_aux(Lista,[],Comp,Arbol).

%El primer termino es una list con las hojas que quedan por fusionarse, una vez se fususionan se pasan a la segunda lista, cuando la primera lista esta vacia se pasan todos los elementos de la segunda a la primera y se vacia la segunda. Si queda 1 elemento en la primera lista este se pone al final de la segunda y despues se pasa la segunda lista a la primera y se vacia la segunda.
%El proceso se repite hasta que solo quede un elemento en la primera, que significa que ya esta el arbol creado


%Si solo queda 1 hoja se devuelve esa como el arbol
hojas_arbol_aux([X],[],_,X).
%Si la primera lista esta vacia se vuelca el contenido de la segunda en la primera y se vacia la segunda. Y se vuelve a llamar a hojas_arbol_aux/4
hojas_arbol_aux([],Lista,Comp,Arbol):-
	hojas_arbol_aux(Lista,[],Comp,Arbol).
%Si solo queda 1 elemento en la primera lista se añade al final de la segunda. Posteriormente la segunda lista se vuelca en la primera y se vacia la segunda. Y se vuelve a llamar a hojas_arbol_aux/4
hojas_arbol_aux([X],Lista,Comp,Arbol):-
	insertar(Lista,X,Lista1),
	hojas_arbol_aux(Lista1,[],Comp,Arbol).
%si hay mas elementos se sacan los dos priemos y se construye un arbol con ellos. Despues se añaden al final de la  segunda lista. Se vuelve a llamr a hojas_arbol_aux/4, pero en la primera lista ya no estan los dos primeros elementos.
hojas_arbol_aux([tree(E_A,H1_A,H2_A),tree(E_B,H1_B,H2_B)|Hojas],Lista,Comp,Arbol):-
	menor(E_A,E_B,Comp,M),
	insertar(Lista,tree(M,tree(E_A,H1_A,H2_A),tree(E_B,H1_B,H2_B)),Lista1),
	hojas_arbol_aux(Hojas,Lista1,Comp,Arbol).
	

%nserta un Item al final de una lista y lo devuelve en SOlucion.
insertar(Lista,Item,Solucion):-
	length(Lista,X),
	X =:= 0,
	Solucion = [Item].
insertar([E|Lista],Item,[E|Solucion]):-
	insertar(Lista,Item,Solucion).



ordenacion(Arbol,Comp,Orden):-
	ordenacion_aux(Arbol,Comp,[],Aux),
	Orden = Aux.
ordenacion_aux(tree(E,I,D),Comp,Orden,Aux):-
	insertar(Orden,E,Nueva_Lista),
	reflotar(tree(E,I,D),Comp,X) ->
	ordenacion_aux(X,Comp,Nueva_Lista,Aux);
	Aux = Nueva_Lista.


%ordenacion_aux(tree(E,tree(E1,void,void),tree(E2,void,void))),_,Orden,Aux):-
%	insertar(Orden,E,Aux),

%ordenacion_aux(tree(E,I,D),_,Orden,Aux):-
%	insertar(Orden,E,Aux).

		    
%ordenacion(tree(E_padre(tree(E_padre,void,void),tree(E_2,void,void))),Comp,Orden):-
	
	
%ordenacion(tree(E_padre,(tree(E_padre,I1,D1),tree(_,_,_))),_,_):-
%	ordenacion(tree(E_padre,I1,D1)).

reflotar(tree(_,void,void),_,X):-
	X=void.
reflotar(tree(E,tree(E,void,void),tree(E2,I2,D2)),_,X):-
	X = tree(E2,I2,D2).
reflotar(tree(E,tree(E1,I1,D1),tree(E,void,void)),_,X):-
	X = tree(E1,I1,D1).
%reflotar(tree(E,tree(E1,void,void),tree(E2,void,void)),_,X):-
%	E == E1 -> X=tree(E2,void,void); X=tree(E1,void,void).

reflotar(tree(E,tree(E1,I1,D1),tree(E2,I2,D2)),Comp,X):-
	(E == E1 ->
	 (reflotar(tree(E1,I1,D1),Comp,tree(X1Elem,XI1,XD1)),
	  menor(E2,X1Elem,Comp,M),
	  X=tree(M,tree(X1Elem,XI1,XD1),tree(E2,I2,D2))
	  );
	    (reflotar(tree(E2,I2,D2),Comp,tree(X2Elem,XI2,XD2)),
	     menor(E1,X2Elem,Comp,M),
	     X=tree(M,tree(E2,I2,D2),tree(X2Elem,XI2,XD2)) 
	  )
	).
	%X=X1.







