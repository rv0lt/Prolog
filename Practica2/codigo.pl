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
	
	
