estafeta(n1).
estafeta(n2).
estafeta(n3).

%meio_de_transporte(volume máximo em kg, v média em km/h)
max_bicicleta(5, 10).
max_moto(20, 35).
max_carro(100, 25).
%fazer_função_max

cliente(c1).
cliente(c2).
cliente(c3).


%encomenda
%adicionar dia
%prazo_de_entrega, preço

g([entrega(1, n1, c1, bicicleta, 4), entrega(2, n2, c1, moto, 6), entrega(2, n1, c2, moto, 6)]).

%entrega_estafeta______________peso_volume_maybe_____________
%classificação_nota

%penalização_por_não_cumprir_entregas

%1

%2
identificar_estafetas([], _, []).
identificar_estafetas([entrega(_,E,C,_,_)|T], C, [E|S]):- 
	estafeta(E), cliente(C),
	identificar_estafetas(T, C, S), not(member(E,S)), !.

identificar_estafetas([_|T], C, S):- identificar_estafetas(T, C, S).


%3

clientes_servidos(E, S):- g(L), clientes_servidos(E, L, S).

clientes_servidos(_, [], []).
clientes_servidos(E, [entrega(_,E,X,_,_)|T], [X|S]):- 
	estafeta(E), cliente(X),
	clientes_servidos(E, T, S),
	not(member(X,S)).
clientes_servidos(E, [_|T], S):- clientes_servidos(E, T, S).

%4

%5

%6

%7
%adicionar intervalo de tempo
p_transportes(B, M, C):- g(L), p_transporte(bicicleta, L, B), p_transporte(moto, L, M), p_transporte(carro, L, C). 

p_transporte(_, [], 0).
p_transporte(V, [entrega(_,_,_,V,_)|T], S):- 
	p_transporte(V, T, S1),
	S is S1 + 1, !.
p_transporte(V, [_|T], S):- p_transporte(V, T, S).

%8

%9

%10
%adicionar dia
peso_carregado(E, S):- g(L), peso_carregado(E, L, S).

peso_carregado(_, [], 0).
peso_carregado(E, [entrega(_,E,_,_,N)|T], S):- 
	peso_carregado(E,T,S1), S is S1 + N, !.
peso_carregado(E, [_|T], S):- peso_carregado(E,T,S).

