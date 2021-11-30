estafeta(e1).
estafeta(e2).
estafeta(e3).

%meio_de_transporte(volume máximo em kg, v média em km/h)
max_bicicleta(5, 10).
max_moto(20, 35).
max_carro(100, 25).
%fazer_função_max

cliente(c1).
cliente(c2).
cliente(c3).


%adicionar local, distância, hora em vez de data(?)

%encomenda(nr, cliente, preço, prazo)
%entrega(nr, estafeta, cliente, meio_de_transporte, volume, date_de_entrega,classificação)

g([encomenda(1, c1, 15, 18/11/2021), encomenda(2, c1, 25, 16/11/2021), encomenda(3, c2, 26, 18/11/2021)], [entrega(1, e1, c1, bicicleta, 4, 18/11/2021,1), entrega(2, e2, c1, moto, 6, 16/11/2021,2), entrega(3, e1, c2, moto, 7, 18/11/2021,4)]).
g1([ entrega(1, e1, c1, bicicleta, 4, 18/11/2021,1),
	 entrega(2, e2, c1, moto, 6, 16/11/2021,2), 
	 entrega(3, e1, c2, moto, 7, 18/11/2021,4)]).


%entrega_estafeta______________peso_volume_maybe_____________
%classificação_nota

%penalização_por_não_cumprir_entregas

%1

%2
identificar_estafetas([], _, []).
identificar_estafetas([entrega(_,E,C,_,_,_)|T], C, [E|S]):- 
	estafeta(E), cliente(C),
	identificar_estafetas(T, C, S), not(member(E,S)), !.

identificar_estafetas([_|T], C, S):- identificar_estafetas(T, C, S).


%3
clientes_servidos(E, S):- g(_, L), clientes_servidos(E, L, S).

clientes_servidos(_, [], []).
clientes_servidos(E, [entrega(_,E,X,_,_,_)|T], [X|S]):- 
	estafeta(E), cliente(X),
	clientes_servidos(E, T, S),
	not(member(X,S)), !.
clientes_servidos(E, [_|T], S):- clientes_servidos(E, T, S).

%4
valor_faturado(D/M/A, S):- g(L, _), valor_faturado(D/M/A, L, S).

valor_faturado(_, [], 0).
valor_faturado(D/M/A, [encomenda(_,_,P,D/M/A)|T], S):-
	valor_faturado(D/M/A, T, S1),
	S is S1 + P, !.
valor_faturado(D/M/A, [_|T], S):- valor_faturado(D/M/A, T, S).

%5

%6
%-- calcular a classificação média de satisfação de cliente para um determinado estafeta;
%-- avaliacao_estafeta(Lista de entregas,cliente,estafeta avaliar,Resultado acumulado)

avaliacao_estafeta(List,E,Result) :- avaliacao_estafeta(List,E,0,0,Result).


avaliacao_estafeta([],_,R,C,Result) :- Result is div(R,C).
avaliacao_estafeta([(entrega(_,E,_,_,_,_,Cls))|T],E,R,C,Res) :-
			R1 is Cls + R,
			C1 is C + 1,
			avaliacao_estafeta(T,E,R1,C1,Res).
avaliacao_estafeta([_|T],E,R,Cs,Res) :- avaliacao_estafeta(T,E,R,Cs,Res).
			

%7
%adicionar intervalo de tempo
%!
%!
%!
%!
%!
%!
%!
%!
%!






p_transportes(B, M, C):- g(L), p_transporte(bicicleta, L, B), p_transporte(moto, L, M), p_transporte(carro, L, C). 

p_transporte(_, [], 0).
p_transporte(V, [entrega(_,_,_,V,_,_)|T], S):- 
	p_transporte(V, T, S1),
	S is S1 + 1, !.
p_transporte(V, [_|T], S):- p_transporte(V, T, S).

%8

%9

%10
peso_carregado(E, D/M/A, S):- g(_, L), peso_carregado(E, D/M/A, L, S).

peso_carregado(_, _, [], 0).
peso_carregado(E, D/M/A, [entrega(_,E,_,_,N,D/M/A)|T], S):- 
	peso_carregado(E,D/M/A,T,S1), S is S1 + N, !.
peso_carregado(E, D/M/A, [_|T], S):- peso_carregado(E, D/M/A, T, S).

