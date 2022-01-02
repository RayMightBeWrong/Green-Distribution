% (meio de transporte, peso máximo, velocidade máxima, velocidade que perde por cada Kg que carrega)
props_transporte(bicicleta, 5, 10, 0.7).
props_transporte(moto, 20, 35, 0.5).
props_transporte(carro, 100, 25, 0.1).



createListAresta([_], [], 0).
createListAresta([A,B|T1], [aresta(A, B, D2)|T2], NewD):- 
		createListAresta([B|T1], T2, D),
		adjacente(A, B, D2),
		NewD is D + D2.


distHeuristica(F, S):- mapa(L, _), distHeuristica(F, L, S).

distHeuristica(_, [], []).
distHeuristica(F, [nodo(F)|T1], [nodo(F, 0)|T2]):- distHeuristica(F, T1, T2).
distHeuristica(F, [nodo(A)|T1], [nodo(A, H)|T2]):- 
                        circuitoBFS(F, A, transporte(tmp), 0, circuito(_, _, _, _, S)), 
                        createListAresta(S, _, H),
                        distHeuristica(F, T1, T2).
distHeuristica(F, nodo(A), nodo(A, H)):- 
        circuitoBFS(F, A, transporte(tmp), 0, circuito(_, _, _, _, S)), 
        createListAresta(S, _, H), !.



timeHeuristica(F, S):- mapa(L, _), timeHeuristica(F, L, S).

timeHeuristica(_, [], []).
timeHeuristica(F, [nodo(F)|T1], [nodo(F, 0)|T2]):- timeHeuristica(F, T1, T2).
timeHeuristica(F, [nodo(A)|T1], [nodo(A, H)|T2]):- 
                        circuitoBFS(F, A, transporte(tmp), 0, circuito(_, _, _, _, S)), 
			write("H: "), write(H), write('\n'),
                        createListAresta(S, _, _, H),
                        timeHeuristica(F, T1, T2).

timeHeuristica(F, nodo(A), nodo(A, H)):- 
        circuitoBFS(F, A, transporte(tmp), 0, circuito(_, _, _, _, S)), 
        createListAresta(S, _, _, H), !.



avaliarCircuito(circuito(transporte(M), P, L), D, 0):-
	createListAresta(L, S, D).


calculaVelocidade(T, P, V):- props_transporte(T, _, VMax, LOSS), V is (VMax - P * LOSS).

calculaTempo(D, V, T):- T is (D * 60) / V.
