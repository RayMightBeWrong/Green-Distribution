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


% verifica se uma entrega foi feita a tempo
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


% cria uma lista com todos os estafetas no sistema
listaEstafetas(E):- findall(estafeta(A,B,C), (estafeta(A,B,C)), E).


% filtra todos os estafetas elegíveis de transportar uma encomenda com determinado peso
estafetasPossiveis(_, [], _, [], []).
estafetasPossiveis(N, [estafeta(A, Transporte, C)|T1], P, [estafeta(A, Transporte, C)|T2], [N|T3]):-
        props_transporte(Transporte, PMax, _, _),
        P =< PMax,
        NewN is N + 1,
        estafetasPossiveis(NewN, T1, P, T2, T3).
estafetasPossiveis(N, [_|T1], P, T2, T3):-
        NewN is N + 1,
        estafetasPossiveis(NewN, T1, P, T2, T3).


% cria uma lista com pontos de partida de todos os estafetas (no formato nodo(ponto_de_partida))
partidasToNodos(S):- listaEstafetas(L), partidasToNodos(L, S).

partidasToNodos([], []).
partidasToNodos([estafeta(_, _, P)|T1], [nodo(P)|T2]):- partidasToNodos(T1, T2).


% anexa o reverso da lista à própria lista sem duplicar o último elemento
reverseBack([], []).
reverseBack(L, S):- 
	length(L, N), RM_INDEX is N - 1,
	rmIndexFromList(RM_INDEX, L, L2),
	reverse(L2, L3), append(L, L3, S).


% seleciona um elemento da lista, funciona como remove também
select(X, [X|T], T).
select(X, [Y|T1], [Y|T2]):- select(X, T1, T2).


% devolve o elemento da lista num determinado índice
getIndexFromList(0, [S|_], S).
getIndexFromList(N, [_|T], S):- N > 0, NewN is N - 1, getIndexFromList(NewN, T, S).


% devolve o índice de um determinado elemento da lista
findIndexFromList(X, [X|_], 0).
findIndexFromList(X, [_|T], NEW_N):- findIndexFromList(X, T, N), NEW_N is N + 1.


% modifica o elemento da lista num determinado índice
modIndexFromList(0, X, [_|T], [X|T]).
modIndexFromList(N, X, [A|T], [A|S]):- N > 0, NewN is N - 1, modIndexFromList(NewN, X, T, S).


% remove o elemento da lista num determinado índice
rmIndexFromList(0, [_|S], S).
rmIndexFromList(N, [X|T1], [X|T2]):- N > 0, NewN is N - 1, rmIndexFromList(NewN, T1, T2).


% recebe uma lista de pontos no mapa e cria uma lista com as arestas de um trajeto
% algumas variantes do predicado podem devolver a distância percorrida e/ou o tempo demorado a percorrer
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


% verifica se todos os elementos de uma lista estão noutra lista
allElemsInList([], _).
allElemsInList([A|T1], L):- member(A, L), allElemsInList(T1, L).


% devolve o valor da heurística calculada para um certo nodo
getHeuristicaOfNodo(nodo(A), [nodo(A, H)|_], H).
getHeuristicaOfNodo(A, [_|T], H):- getHeuristicaOfNodo(A, T, H).


% calcula a heurística da distância
distHeuristica(F, S):- mapa(L, _), distHeuristica(F, L, S).

distHeuristica(_, [], []).
distHeuristica(F, [nodo(F)|T1], [nodo(F, 0)|T2]):- distHeuristica(F, T1, T2).
distHeuristica(F, [nodo(A)|T1], [nodo(A, H)|T2]):- 
	circuitoBFS(F, A, transporte(tmp), 0, circuito(_, _, S)), 
	createListAresta(S, _, H),
	distHeuristica(F, T1, T2).


% calcula a heurística do tempo
timeHeuristica(F, T, P, S):- mapa(L, _), timeHeuristica(F, L, T, P, S).

timeHeuristica(_, [], _, _, []).
timeHeuristica(F, [nodo(F)|T1], Transporte, P, [nodo(F, 0)|T2]):- 
	timeHeuristica(F, T1, Transporte, P, T2).
timeHeuristica(F, [nodo(A)|T1], Transporte, P, [nodo(A, H)|T2]):- 
	circuitoBFS(F, A, transporte(Transporte), P, circuito(_,_,S)), 
	calculaVelocidade(Transporte, P, V),
	createListAresta(S, _, V, _, H),
	timeHeuristica(F, T1, Transporte, P, T2).


% devolve a informação de uma lista de encomendas
getInfoFromEncomendas([], [], 0).
getInfoFromEncomendas([encomenda(LOCAL, PESO)|T1], [LOCAL|T2], NEWPESO):-
        getInfoFromEncomendas(T1, T2, PESO2),
        NEWPESO is PESO + PESO2.


% seleciona o melhor circuito de uma lista de circuitos
melhorCircuitoFromList(_, _, [S], S).
melhorCircuitoFromList(DEST, dist, [A,B|T1], S):-
        cmpCircuitos(DEST, A, B, dist, BEST),
        melhorCircuitoFromList(DEST, dist, [BEST|T1], S).

melhorCircuitoFromList(_, _, _, [S], S).
melhorCircuitoFromList(DEST, time, TMax, [A,B|T1], S):-
        cmpCircuitos(DEST, A, B, time, TMax, BEST),
        melhorCircuitoFromList(DEST, time, TMax, [BEST|T1], S).


% calcula a velocidade média de um certo transporte a carregar um certo peso
calculaVelocidade(T, P, V):- props_transporte(T, _, VMax, LOSS), V is (VMax - P * LOSS).


% calcula o tempo (em minutos) que é demorado a percorrer uma certa aresta
calculaTempo(D, V, C, T):- T is (D * 60) / (V * C).
