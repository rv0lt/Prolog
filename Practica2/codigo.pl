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
	
menor_o_igual(A,B):-
	var(A);
	var(B).
menor_o_igual(A,B):-
	functor(A,NA,La),
	functor(B,NB,Lb),
	NA @< NB
	(NA @< NB;
	    soy_igual(NA,NB),La =< Lb);
	menor_o_igual_aux(A,B).

menor_o_igual_aux(A,B):-
	functor(A,MA,XA),
	functor(B,MB,XB),
	XA =:= XB,
	(XA =:= 0;
	(
	    arg(1,A,ElemA),
	    arg(1,B,ElemB),
	    menor_o_igual(ElemA,ElemB),
	    soy_igual(ElemA,ElemB),
	    Xnuevo is XA-1,
	    functor(NuevaA,MA,Xnuevo),
	    functor(NuevaB,MB,Xnuevo),
	    reducir_predicado(A,NuevaA,1,XA),
	    reducir_predicado(B,NuevaB,1,XB),
	    menor_o_igual_aux(NuevaA,NuevaB))).


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
