%funções chamam-se isget, porque:
%servem para verificar se o elemento é o mesmo (is) e
%servem para ir buscar o elemento (get)

isget_estafeta(E, entrega(E,_)).
isget_veiculo(V, entrega(_,transporte(V))).

isget_cliente(C, encomenda(C,_,_,_,_)). 
isget_local(L, encomenda(_,L,_,_,_)).
isget_peso(P, encomenda(_,_,P,_,_)).
isget_preco(P, encomenda(_,_,_,P,_)).
isget_nota(N, encomenda(_,_,_,_,N)).

isget_hora_limite(H/M, prazo(H/M,_,_,_)).
isget_data_limite(D/M/A, prazo(_,D/M/A,_,_)).
isget_hora_entrega(0, prazo(_,_, 0,_)).
isget_hora_entrega(H/M, prazo(_,_, H/M ,_)).
isget_data_entrega(0, prazo(_, _, _, 0)).
isget_data_entrega(D/M/A, prazo(_, _, _, D/M/A)).


entrega_in_time(Hi, Di, Hf, Df, prazo(_, _, HE, DE)):-
	is_older_than(Hi, Di, HE, DE), is_older_than(HE, DE, Hf, Df).

is_older_than(_, _/_/A1, _/_, _/_/A2):- A1 < A2, !.
is_older_than(_, _/M1/A1, _, _/M2/A2):- 
	A1 =:= A2, M1 < M2, !. 
is_older_than(_, D1/M1/A1, _, D2/M2/A2):- 
	A1 =:= A2, M1 =:= M2, D1 < D2, !. 
is_older_than(H1/_, D1/M1/A1, H2/_, D2/M2/A2):- 
	A1 =:= A2, M1 =:= M2, D1 =:= D2, H1 < H2, !. 
is_older_than(H1/MIN1, D1/M1/A1, H2/MIN2, D2/M2/A2):- 
	A1 =:= A2, M1 =:= M2, D1 =:= D2, H1 =:= H2, MIN1 =< MIN2, !.



% (meio de transporte, peso máximo, velocidade máxima, velocidade que perde por cada Kg que carrega)
props_transporte(bicicleta, 5, 10, 0.7).
props_transporte(moto, 20, 35, 0.5).
props_transporte(carro, 100, 25, 0.1).


%
createListAresta([_], [], 0).
createListAresta([A,B|T1], [aresta(A, B, D2)|T2], NewD):- 
	createListAresta([B|T1], T2, D),
	adjacente(A, B, D2),
	NewD is D + D2.

%
distHeuristica(F, S):- mapa(L, _), distHeuristica(F, L, S).

distHeuristica(_, [], []).
distHeuristica(F, [nodo(F)|T1], [nodo(F, 0)|T2]):- distHeuristica(F, T1, T2).
distHeuristica(F, [nodo(A)|T1], [nodo(A, H)|T2]):- 
	circuitoBFS(F, A, transporte(tmp), 0, circuito(_, _, _, _, S)), 
	createListAresta(S, _, H),
	distHeuristica(F, T1, T2).

%
distHeuristica(F, nodo(A), nodo(A, H)):- 
        circuitoBFS(F, A, _, _, circuito(_, _, S)), 
        createListAresta(S, _, H), !.


%
timeHeuristica(F, S):- mapa(L, _), timeHeuristica(F, L, S).

timeHeuristica(_, [], _, _, []).
timeHeuristica(F, [nodo(F)|T1], T, P, [nodo(F, 0)|T2]):- timeHeuristica(F, T1, T, P, T2).
timeHeuristica(F, [nodo(A)|T1], T, P, [nodo(A, H)|T2]):- 
	circuitoBFS(F, A, T, P, S), 
	avaliarCircuito(S, _, H),
	timeHeuristica(F, T1, T, P, T2).

%
timeHeuristica(F, nodo(A), T, P, nodo(A, H)):- 
	circuitoBFS(F, A, T, P, S), 
        avaliarCircuito(S, _, H), !.


%
avaliarCircuito(circuito(Transporte, P, L), D, T):-
	createListAresta(L, _, D), calculaVelocidade(Transporte, P, V), calculaTempo(D, V, T).

%
calculaVelocidade(T, P, V):- props_transporte(T, _, VMax, LOSS), V is (VMax - P * LOSS).

%
calculaTempo(D, V, T):- T is (D * 60) / V.


%
cmpCircuitos(C1, C2, dist, S):- 
	avaliarCircuito(C1, D1, _), avaliarCircuito(C2, D2, _),
	D1 =< D2, S is C1.
cmpCircuitos(C1, C2, dist, S):- 
	avaliarCircuito(C1, D1, _), avaliarCircuito(C2, D2, _),
	D1 > D2, S is C2.

%cmpCircuitos(C1, C2, time, S):- 
%	avaliarCircuito(C1, _, T1), avaliarCircuito(C2, _, T2),
%	T1 =< T2, S is C1.
%cmpCircuitos(C1, C2, time, S):- 
%	avaliarCircuito(C1, D1, _), avaliarCircuito(C2, D2, _),
%	D1 > D2, S is T2.
