:- include('mapa.pl').
:- include('registos.pl').
:- include('aux.pl').

%encomenda(nr, cliente, preço, prazo)
%entrega(nr, estafeta, cliente, meio_de_transporte, volume, data_de_entrega, classificação)

%adicionar estimativa de tempo ao mapa?
%dizer o tempo máximo de entrega fica como prazo de entrega
%estado_entregue_ou_não
%penalização_por_não_cumprir_entregas
g([encomenda(1, c1, 15, 18/11/2021), encomenda(2, c1, 25, 16/11/2021), encomenda(3, c2, 26, 18/11/2021)], [entrega(1, e1, c1, bicicleta, 4, 18/11/2021,1), entrega(2, e1, c1, moto, 6, 16/11/2021,2), entrega(3, e2, c2, bicicleta, 7, 18/11/2021,4), entrega(3, e2, c2, moto, 7, 18/11/2021,4)]).

g1([entrega(1, e1, c1, bicicleta, 4, 18/11/2021,1),
	entrega(2, e2, c1, moto, 6, 16/11/2021,2), 
	entrega(3, e1, c2, moto, 7, 18/11/2021,4)]).

%extras:
%quando gastou um cliente na GreenDistribution
%...


%1
mais_ecologico(S):- g(_, L), mais_ecologico(L, [], S).

mais_ecologico([entrega(_,E,_,V,_,_,_)|T], [], S):- 
	add_veiculo_tuple(E/0/0/0, V, R), !, 
	mais_ecologico(T, [R], S).
mais_ecologico([], LE, S):- select_mais_ecologico(LE, S).

mais_ecologico([entrega(_,E,_,V,_,_,_)|T], LE, S):- 
	estafeta_pertence(E, R1, LE), 
	add_veiculo_tuple(R1, V, R2),
	replace_estafeta(R2, LE, NEWLE),
	mais_ecologico(T, NEWLE, S).

mais_ecologico([entrega(_,E,_,V,_,_,_)|T], LE, S):- 
	not(estafeta_pertence(E, _, LE)), 
	add_veiculo_tuple(E/0/0/0, V, R),
	append([R], LE, NEWLE),
	mais_ecologico(T, NEWLE, S).

estafeta_pertence(E, E/B/M/C, [E/B/M/C|_]).
estafeta_pertence(E, R, [Y/_/_/_|L]):- E \= Y, estafeta_pertence(E, R, L).

replace_estafeta(E/B/M/C, [E/_/_/_|L], [E/B/M/C|L]). 
replace_estafeta(E/B1/M1/C1, [Y/B2/M2/C2|L1], [Y/B2/M2/C2|L2]):- 
	E \= Y, 
	replace_estafeta(E/B1/M1/C1, L1, L2).


add_veiculo_tuple(E/B/M/C, bicicleta, E/NEWB/M/C):- NEWB is B + 1.
add_veiculo_tuple(E/B/M/C, moto, E/B/NEWM/C):- NEWM is M + 1.
add_veiculo_tuple(E/B/M/C, carro, E/B/M/NEWC):- NEWC is C + 1.

%select_mais_ecologico por fazer obviamente
select_mais_ecologico([E/_/_/_], E):- !.
select_mais_ecologico([E1/B1/M1/C1, _/B2/_/_|T], Best):-
	B1 > B2, !,
	select_mais_ecologico([E1/B1/M1/C1|T], Best).
select_mais_ecologico([E1/B1/M1/C1, _/B2/_/_|T], Best):-
	B1 == B2,
	write("i'm here"),
	select_mais_ecologico([E1/B1/M1/C1|T], Best).
select_mais_ecologico([_|T], Best):- select_mais_ecologico(T, Best).

%2
identificar_estafetas([], _, []).
identificar_estafetas([entrega(_,E,C,_,_,_,_)|T], C, [E|S]):- 
	estafeta(E), cliente(C),
	identificar_estafetas(T, C, S), not(member(E,S)), !.

identificar_estafetas([_|T], C, S):- identificar_estafetas(T, C, S).


%3
clientes_servidos(E, S):- g(_, L), clientes_servidos(E, L, S).

clientes_servidos(_, [], []).
clientes_servidos(E, [entrega(_,E,X,_,_,_,_)|T], [X|S]):- 
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

%falta caso nulo
%!
%!
%!
%!
%!

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

p_transportes(B, M, C):- g(L), p_transporte(bicicleta, L, B), p_transporte(moto, L, M), p_transporte(carro, L, C). 

p_transporte(_, [], 0).
p_transporte(V, [entrega(_,_,_,V,_,_,_)|T], S):- 
	p_transporte(V, T, S1),
	S is S1 + 1, !.
p_transporte(V, [_|T], S):- p_transporte(V, T, S).


%8


%9


%10
peso_carregadoOG(E, D/M/A, S):- g(_, L), peso_carregadoOG(E, D/M/A, L, S).

peso_carregadoOG(_, _, [], 0).
peso_carregadoOG(E, D/M/A, [entrega(_,E,_,_,N,D/M/A,_)|T], S):- 
	peso_carregadoOG(E,D/M/A,T,S1), S is S1 + N, !.
peso_carregadoOG(E, D/M/A, [_|T], S):- peso_carregadoOG(E, D/M/A, T, S).


peso_carregado(E, D/M/A, S):- lista_de_entregas(L), peso_carregado(E, D/M/A, L, S).

peso_carregado(_, _, [], 0).
peso_carregado(E, D/M/A, [registo(ARG1,ARG2,ARG3)|T], S):- 
	same_estafeta(E, ARG1), same_data_entrega(D/M/A, ARG3), get_preco(P, ARG2),
	peso_carregado(E,D/M/A,T,S1), S is S1 + P, !.
peso_carregado(E, D/M/A, [_|T], S):- peso_carregado(E, D/M/A, T, S).
