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

isget_transporte_circuito(circuito(T, _, _), T).


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
listaEstafetas(E):- findall(estafeta(A,B,C), (estafeta(A,B,C)), E).

%
reverseBack([], []).
reverseBack(L, S):- 
	length(L, N), RM_INDEX is N - 1,
	rmIndexFromList(RM_INDEX, L, L2),
	reverse(L2, L3), append(L, L3, S).

%
getIndexFromList(0, [S|_], S).
getIndexFromList(N, [_|T], S):- N > 0, NewN is N - 1, getIndexFromList(NewN, T, S).

findIndexFromList(X, [X|_], 0).
findIndexFromList(X, [_|T], NEW_N):- findIndexFromList(X, T, N), NEW_N is N + 1.

modIndexFromList(0, X, [_|T], [X|T]).
modIndexFromList(N, X, [A|T], [A|S]):- N > 0, NewN is N - 1, modIndexFromList(NewN, X, T, S).

rmIndexFromList(0, [_|S], S).
rmIndexFromList(N, [X|T1], [X|T2]):- N > 0, NewN is N - 1, rmIndexFromList(NewN, T1, T2).

%
createListAresta([_], []).
createListAresta([A,B|T1], [aresta(A, B)|T2]):- 
	createListAresta([B|T1], T2),
	adjacente(A, B, _, _).

createListAresta([_], [], 0).
createListAresta([A,B|T1], [aresta(A, B, D2)|T2], NewD):- 
	createListAresta([B|T1], T2, D),
	adjacente(A, B, D2, _),
	NewD is D + D2.

createListAresta([_], [], _, 0, 0).
createListAresta([A,B|T1], [aresta(A, B, D2, C2)|T2], V, NewD, NewTime):- 
	createListAresta([B|T1], T2, V, D, Time),
	adjacente(A, B, D2, C2), calculaTempo(D2, V, C2, Time2),
	NewD is D + D2, NewTime is Time + Time2.

%
allElemsInList([], _).
allElemsInList([A|T1], L):- member(A, L), allElemsInList(T1, L).

%
getHeuristicaOfNodo(nodo(A), [nodo(A, H)|_], H).
getHeuristicaOfNodo(A, [_|T], H):- getHeuristicaOfNodo(A, T, H).


%
distHeuristica(F, S):- mapa(L, _), distHeuristica(F, L, S).

distHeuristica(_, [], []).
distHeuristica(F, [nodo(F)|T1], [nodo(F, 0)|T2]):- distHeuristica(F, T1, T2).
distHeuristica(F, [nodo(A)|T1], [nodo(A, H)|T2]):- 
	circuitoBFS(F, A, transporte(tmp), 0, circuito(_, _, S)), 
	createListAresta(S, _, H),
	distHeuristica(F, T1, T2).


%
timeHeuristica(F, T, P, S):- mapa(L, _), timeHeuristica(F, L, T, P, S).

timeHeuristica(_, [], _, _, []).
timeHeuristica(F, [nodo(F)|T1], Transporte, P, [nodo(F, 0)|T2]):- 
	timeHeuristica(F, T1, Transporte, P, T2).
timeHeuristica(F, [nodo(A)|T1], Transporte, P, [nodo(A, H)|T2]):- 
	circuitoBFS(F, A, transporte(Transporte), P, circuito(_,_,S)), 
	calculaVelocidade(Transporte, P, V),
	createListAresta(S, _, V, _, H),
	timeHeuristica(F, T1, Transporte, P, T2).


%
avaliarCircuito(DEST, circuito(TRANSPORTE, P, L), D, T):-
	avaliarCircuitoXEntregas(circuito(TRANSPORTE, [encomenda(DEST, P)], L), D, T).


avaliarCircuitoXEntregas(circuito(_, _, [_]), 0, 0).
avaliarCircuitoXEntregas(circuito(TRANSPORTE, ENCOMENDAS, [A,B|T]), DIST_TOTAL, TIME_TOTAL):-
	adjacente(A, B, DIST, COEF),
	getInfoFromEncomendas(ENCOMENDAS, CITIES, _),
	member(A, CITIES),
	getIndexFromList(INDEX, CITIES, A), rmIndexFromList(INDEX, ENCOMENDAS, NEW_ENCOMENDAS),
	getInfoFromEncomendas(NEW_ENCOMENDAS, _, PESO),
	calculaVelocidade(TRANSPORTE, PESO, VELOCIDADE),
	calculaTempo(DIST, VELOCIDADE, COEF, TIME),
	avaliarCircuitoXEntregas(circuito(TRANSPORTE, NEW_ENCOMENDAS, [B|T]), DIST2, TIME2),
	DIST_TOTAL is DIST + DIST2,
	TIME_TOTAL is TIME + TIME2.
avaliarCircuitoXEntregas(circuito(TRANSPORTE, ENCOMENDAS, [A,B|T]), DIST_TOTAL, TIME_TOTAL):-
	adjacente(A, B, DIST, COEF),
	getInfoFromEncomendas(ENCOMENDAS, _, PESO),
	calculaVelocidade(TRANSPORTE, PESO, VELOCIDADE),
	calculaTempo(DIST, VELOCIDADE, COEF, TIME),
	avaliarCircuitoXEntregas(circuito(TRANSPORTE, ENCOMENDAS, [B|T]), DIST2, TIME2),
	DIST_TOTAL is DIST + DIST2,
	TIME_TOTAL is TIME + TIME2.


%
getInfoFromEncomendas([], [], 0).
getInfoFromEncomendas([encomenda(LOCAL, PESO)|T1], [LOCAL|T2], NEWPESO):-
        getInfoFromEncomendas(T1, T2, PESO2),
        NEWPESO is PESO + PESO2.


%
calculaVelocidade(T, P, V):- props_transporte(T, _, VMax, LOSS), V is (VMax - P * LOSS).

%
calculaTempo(D, V, C, T):- T is (D * 60) / (V * C).


%
cmpCircuitos(DEST, C1, C2, dist, C1):- 
	avaliarCircuito(DEST, C1, D1, _), avaliarCircuito(DEST, C2, D2, _),
	D1 < D2.
cmpCircuitos(DEST, C1, C2, dist, C2):- 
	avaliarCircuito(DEST, C1, D1, _), avaliarCircuito(DEST, C2, D2, _),
	D1 > D2.
cmpCircuitos(DEST, C1, C2, dist, C1):- 
	avaliarCircuito(DEST, C1, D1, _), avaliarCircuito(DEST, C2, D2, _),
	D1 =:= D2,
	isget_transporte_circuito(C1, bicicleta).
cmpCircuitos(DEST, C1, C2, dist, C1):- 
	avaliarCircuito(DEST, C1, D1, _), avaliarCircuito(DEST, C2, D2, _),
	D1 =:= D2,
	isget_transporte_circuito(C1, moto), isget_transporte_circuito(C2, carro).
cmpCircuitos(DEST, C1, C2, dist, C2):- 
	avaliarCircuito(DEST, C1, D1, _), avaliarCircuito(DEST, C2, D2, _),
	D1 =:= D2.


cmpCircuitos(DEST, C1, C2, time, TMax, C1):- 
	avaliarCircuito(DEST, C1, _, T1), avaliarCircuito(DEST, C2, _, T2),
	T1 < TMax, T2 >= TMax.
cmpCircuitos(DEST, C1, C2, time, TMax, C2):- 
	avaliarCircuito(DEST, C1, _, T1), avaliarCircuito(DEST, C2, _, T2),
	T2 < TMax, T1 >= TMax.
cmpCircuitos(_, C1, C2, time, _, C1):- 
	isget_transporte_circuito(C1, bicicleta), not(isget_transporte_circuito(C2, bicicleta)).
cmpCircuitos(_, C1, C2, time, _, C2):- 
	isget_transporte_circuito(C2, bicicleta), not(isget_transporte_circuito(C1, bicicleta)).
cmpCircuitos(DEST, C1, C2, time, _, C1):- 
	avaliarCircuito(DEST, C1, _, T1), avaliarCircuito(DEST, C2, _, T2),
	T1 < T2.
cmpCircuitos(DEST, C1, C2, time, _, C2):- 
	avaliarCircuito(DEST, C1, _, T1), avaliarCircuito(DEST, C2, _, T2),
	T1 >= T2.
