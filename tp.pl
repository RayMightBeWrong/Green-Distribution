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

entrega(1, estafeta(n1), cliente(n1), bicicleta, 4). %prazo_de_entrega
entrega(2, estafeta(n1), cliente(n2), moto, 6).

g([entrega(1, n1, c1, bicicleta, 4), entrega(2, n2, c1, moto, 6), entrega(2, n1, c2, moto, 6)]).

%entrega_estafeta______________peso_volume_maybe_____________
%classificação_nota

%penalização_por_não_cumprir_entregas
%adicionar_preço_por_prazo_e_transporte

%1

%2
identificar_estafetas([], _, []).
identificar_estafetas([entrega(_,E,C,_,_)|T], C, [E|S]):- 
	estafeta(E), cliente(C),
	identificar_estafetas(T, C, S), not(member(E,S)), !.

identificar_estafetas([X|T], C, S):- identificar_estafetas(T, C, S).


%3

clientes_servidos(E, S):- g(L), clientes_servidos(E, L, S).

clientes_servidos(E, [], []).
clientes_servidos(E, [entrega(_,E,X,_,_)|T], [X|S]):- 
	estafeta(E), cliente(X),
	clientes_servidos(E, T, S),
	not(member(X,S)).
clientes_servidos(E, [X|T], S):- clientes_servidos(E, T, S).


%adicionar dia
