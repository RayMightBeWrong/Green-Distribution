:- include('mapa.pl').
:- include('helper.pl').
:- include('registos.pl').

%
createCircuito(TRANSPORTE, PESO, TRACK, circuito(TRANSPORTE, PESO, TRACK)).


gera_todos(A, PESO, S):- 
	listaEstafetas(E), estafetasPossiveis(0, E, PESO, ELEGIVEIS, _),
	gera_todos(A, PESO, ELEGIVEIS, [], S).

gera_todos(_, _, [], S, S).
gera_todos(A, PESO, [estafeta(_,TRANSPORTE,CITY)|T1], T2, S):-
	findall(RESULT, (circuitoDFS_noReverse(A, CITY, TRANSPORTE, PESO, circuito(_,_,TRACK1)),
			 circuitoDFS_noReverse(CITY, A, TRANSPORTE, 0, circuito(_,_,TRACK2)),
			 rmIndexFromList(0, TRACK2, NEW_TRACK2),
			 append(TRACK1, NEW_TRACK2, THE_TRACK),
			 createCircuito(TRANSPORTE, PESO, THE_TRACK, RESULT),
			 write('RESULT: '), write(RESULT), write('\n')), RESULTS),
	append(RESULTS, T2, NEW_RESULTS),
	gera_todos(A, PESO, T1, NEW_RESULTS, S).


% A e B -> nodos de partida e de chegada, respetivamente
% T -> transporte
% P -> peso da encomenda

% depth first search
circuitoDFS(A, B, T, P, circuito(T, P, S)):- circuitoDFS(A, [B], TRACK), reverseBack(TRACK, S).

circuitoDFS(A, [A|T], [A|T]).
circuitoDFS(A, [B|T], S):- adjacente(X, B, _, _), not(member(X, [B|T])),
	circuitoDFS(A, [X,B|T], S).

circuitoDFS_noReverse(A, B, T, P, circuito(T, P, S)):- circuitoDFS(A, [B], S).


% breadth first search 
circuitoBFS(A, B, T, P, circuito(T, P, S)):- circuitoBFS(B, [[A]], TRACK), reverseBack(TRACK, S).

circuitoBFS(F, [[F|T]|_], S):- reverse([F|T], S). 

circuitoBFS(F, [EstadosA|Outros], S):-
	EstadosA = [Atual|_],
	findall([X|EstadosA],
		(F \== Atual, adjacente(Atual, X, _, _), not(member(X, EstadosA))), Novos),
	append(Outros, Novos, Todos),
	circuitoBFS(F, Todos, S).


circuitoBFS_noReverse(A, B, T, P, circuito(T, P, S)):- circuitoBFS(B, [[A]], S).


% encomenda(local, peso)
xEntregas(A, TRANSPORTE, ENCOMENDAS, circuito(TRANSPORTE, ENCOMENDAS, S)):- 
	getInfoFromEncomendas(ENCOMENDAS, CITIES, _), 
	xEntregasTrack(A, CITIES, TRACK1),
	length(TRACK1, N), INDEX is N - 1,
	getIndexFromList(INDEX, TRACK1, ELEM), rmIndexFromList(INDEX, TRACK1, NEW_TRACK1),
	circuitoBFS_noReverse(ELEM, A, TRANSPORTE, 0, circuito(_,_,TRACK2)),
	append(NEW_TRACK1, TRACK2, S).

xEntregasTrack(A, L_PONTOS, S):- xEntregasTrack(L_PONTOS, L_PONTOS, [[A]], S).

xEntregasTrack([F], L_PONTOS, [[F|T]|_], S):- allElemsInList(L_PONTOS, [F|T]), reverse([F|T], S).

xEntregasTrack(PONTOS, L_PONTOS, [EstadosA|Outros], S):-
	EstadosA = [Atual|_],
	member(Atual, PONTOS), select(Atual, PONTOS, NEW_PONTOS),
	findall([X|EstadosA],
		(adjacente(Atual, X, _, _), not(member(X, EstadosA))), Novos),
	append(Outros, Novos, Todos),
	xEntregasTrack(NEW_PONTOS, L_PONTOS, Todos, S).

xEntregasTrack(PONTOS, L_PONTOS, [EstadosA|Outros], S):-
	EstadosA = [Atual|_],
	findall([X|EstadosA],
		(adjacente(Atual, X, _, _), not(member(X, EstadosA))), Novos),
	append(Outros, Novos, Todos),
	xEntregasTrack(PONTOS, L_PONTOS, Todos, S).

% iterative deepening search
circuitoIDS(A, B, T, P, circuito(T, P, S)):- loopIDS(1, A, [B], TRACK), reverseBack(TRACK, S).

% o número de pontos no mapa
loop_size(16).
	
loopIDS(N, _, [_], []):- loop_size(N), !.
loopIDS(N, _, S, S):- loop_size(N), length(S, LEN), LEN > 1.
loopIDS(N, A, [B], S):- loop_size(SIZE), N < SIZE, 
	circuitoIDS(A, [B], N, S), length(S, LEN), LEN > 0.
loopIDS(N, A, [B], S):- loop_size(SIZE), N < SIZE,	
	NewN is N + 1, 
	circuitoIDS(A, [B], N, R),
	length(R, 0),
	loopIDS(NewN, A, [B], S).


circuitoIDS(A, [A|T], _, [A|T]).
circuitoIDS(_, _, 0, []).
circuitoIDS(A, [B|T], N, S):- A \= B, adjacente(X, B, _, _), not(member(X, [B|T])),
	NewN is N - 1, NewN >= 0, 
	circuitoIDS(A, [X,B|T], NewN, S).

%
selectBestStarts(_, [], _, INDEXES, S, INDEXES, S).
selectBestStarts(H, [_|T1], [INDEX0|T2], [], L, INDEXES, S):- selectBestStarts(H, T1, T2, [INDEX0], L, INDEXES, S).

selectBestStarts(H, [A|T1], [INDEX|T2], _, [B|_], INDEXES, S):-
	getHeuristicaOfNodo(A, H, HA), getHeuristicaOfNodo(B, H, HB), 
	HA < HB, 
	selectBestStarts(H, T1, T2, [INDEX], [A], INDEXES, S).
selectBestStarts(H, [A|T1], [INDEXA|T2], [INDEXB|T3], [B|T4], INDEXES, S):-
	getHeuristicaOfNodo(A, H, HA), getHeuristicaOfNodo(B, H, HB), 
	HA =:= HB,
	append([B|T4], [A], L1), append([INDEXB|T3], [INDEXA], L2),
	selectBestStarts(H, T1, T2, L2, L1, INDEXES, S).
selectBestStarts(H, [_|T1], [_|T2], [INDEXB|T3], [B|T4], INDEXES, S):-
	selectBestStarts(H, T1, T2, [INDEXB|T3], [B|T4], INDEXES, S).


buildCircuitosFromEstafetas(_, [], _, _, S, S).
buildCircuitosFromEstafetas(L, [INDEX|T1], [TRACK/_|T2], P, T3, S):-
	getIndexFromList(INDEX, L, estafeta(_, Transporte, _)),
	buildCircuitosFromEstafetas(L, T1, T2, P, [circuito(Transporte, P, TRACK)|T3], S).


%
maisRapido(A, P, greedy, circuito(TRANSPORTE, P, S)):- 
	listaEstafetas(E), estafetasPossiveis(0, E, P, E2, INDEXES),
	distHeuristica(A, H), !, partidasToNodos(E2, PS),
	getIndexFromList(0, PS, FIRST),
	selectBestStarts(H, PS, INDEXES, [], [FIRST], NEWINDEXES, STARTS),
	buildGreedy(A, H, STARTS, [], RESULTS),
	buildCircuitosFromEstafetas(E, NEWINDEXES, RESULTS, P, [], TRACKS),
	melhorCircuitoFromList(A, dist, TRACKS, circuito(TRANSPORTE, P, TRACK)), 
	reverseBack(TRACK, S), !.

maisRapido(A, P, a_estrela, circuito(TRANSPORTE, P, S)):- 
	listaEstafetas(E), estafetasPossiveis(0, E, P, E2, INDEXES),
	distHeuristica(A, H), !, partidasToNodos(E2, PS),
	getIndexFromList(0, PS, FIRST),
	selectBestStarts(H, PS, INDEXES, [], [FIRST], NEWINDEXES, STARTS),
	buildAEstrela(A, H, STARTS, [], RESULTS),
	buildCircuitosFromEstafetas(E, NEWINDEXES, RESULTS, P, [], TRACKS),
	melhorCircuitoFromList(A, dist, TRACKS, circuito(TRANSPORTE, P, TRACK)), 
	reverseBack(TRACK, S), !.

buildGreedy(_, _, [], S, S).
buildGreedy(A, H, [B|T1], T2, S):- 
	getHeuristicaOfNodo(B, H, RH),
	greedy(A, B, H, RH, R), buildGreedy(A, H, T1, [R|T2], S).


buildAEstrela(_, _, [], S, S).
buildAEstrela(A, H, [B|T1], T2, S):- 
	getHeuristicaOfNodo(B, H, RH),
	aEstrela(A, B, H, RH, R), buildAEstrela(A, H, T1, [R|T2], S).


%
maisEcologico(A, P, TMax, greedy, circuito(TRANSPORTE, P, S)):- 
	listaEstafetas(E), estafetasPossiveis(0, E, P, E2, _),
	timeHeuristica(A, bicicleta, P, HBIKE),
	timeHeuristica(A, moto, P, HMOTO),
	timeHeuristica(A, carro, P, HCARRO), !,
	buildGreedyTime(A, P, HBIKE, HMOTO, HCARRO, E2, [], RESULTS),
	melhorCircuitoFromList(A, time, TMax, RESULTS, circuito(TRANSPORTE, P, TRACK)),
	reverseBack(TRACK, S), !.

maisEcologico(A, P, TMax, a_estrela, circuito(TRANSPORTE, P, S)):- 
	listaEstafetas(E), estafetasPossiveis(0, E, P, E2, _),
	timeHeuristica(A, bicicleta, P, HBIKE),
	timeHeuristica(A, moto, P, HMOTO),
	timeHeuristica(A, carro, P, HCARRO), !,
	buildAEstrelaTime(A, P, HBIKE, HMOTO, HCARRO, E2, [], RESULTS),
	melhorCircuitoFromList(A, time, TMax, RESULTS, circuito(TRANSPORTE, P, TRACK)),
	reverseBack(TRACK, S), !.

buildGreedyTime(_, _, _, _, _, [], S, S).
buildGreedyTime(A, P, HBIKE, HMOTO, HCARRO, [estafeta(_,bicicleta,CITY)|T1], T2, S):- 
	getHeuristicaOfNodo(nodo(CITY), HBIKE, RH),
	greedy(A, nodo(CITY), HBIKE, RH, TRACK/_),
	buildGreedyTime(A, P, HBIKE, HMOTO, HCARRO, T1, [circuito(bicicleta, P, TRACK)|T2], S). 
buildGreedyTime(A, P, HBIKE, HMOTO, HCARRO, [estafeta(_,moto,CITY)|T1], T2, S):- 
	getHeuristicaOfNodo(nodo(CITY), HMOTO, RH),
	greedy(A, nodo(CITY), HMOTO, RH, TRACK/_),
	buildGreedyTime(A, P, HBIKE, HMOTO, HCARRO, T1, [circuito(moto, P, TRACK)|T2], S). 
buildGreedyTime(A, P, HBIKE, HMOTO, HCARRO, [estafeta(_,carro,CITY)|T1], T2, S):- 
	getHeuristicaOfNodo(nodo(CITY), HCARRO, RH),
	greedy(A, nodo(CITY), HCARRO, RH, TRACK/_),
	buildGreedyTime(A, P, HBIKE, HMOTO, HCARRO, T1, [circuito(carro, P, TRACK)|T2], S). 


buildAEstrelaTime(_, _, _, _, _, [], S, S).
buildAEstrelaTime(A, P, HBIKE, HMOTO, HCARRO, [estafeta(_,bicicleta,CITY)|T1], T2, S):- 
	getHeuristicaOfNodo(nodo(CITY), HBIKE, RH),
	calculaVelocidade(bicicleta, P, V),
	aEstrelaTime(A, nodo(CITY), HBIKE, RH, V, TRACK/_),
	buildAEstrelaTime(A, P, HBIKE, HMOTO, HCARRO, T1, [circuito(bicicleta, P, TRACK)|T2], S). 
buildAEstrelaTime(A, P, HBIKE, HMOTO, HCARRO, [estafeta(_,moto,CITY)|T1], T2, S):- 
	getHeuristicaOfNodo(nodo(CITY), HMOTO, RH),
	calculaVelocidade(moto, P, V),
	aEstrelaTime(A, nodo(CITY), HMOTO, RH, V, TRACK/_),
	buildAEstrelaTime(A, P, HBIKE, HMOTO, HCARRO, T1, [circuito(moto, P, TRACK)|T2], S). 
buildAEstrelaTime(A, P, HBIKE, HMOTO, HCARRO, [estafeta(_,carro,CITY)|T1], T2, S):- 
	getHeuristicaOfNodo(nodo(CITY), HCARRO, RH),
	calculaVelocidade(carro, P, V),
	aEstrelaTime(A, nodo(CITY), HCARRO, RH, V, TRACK/_),
	buildAEstrelaTime(A, P, HBIKE, HMOTO, HCARRO, T1, [circuito(carro, P, TRACK)|T2], S). 


%
greedy(A, nodo(B), H, RH, S/Total):-
	greedy(A, H, [[B]/0/RH], R/Total/_),
	reverse(R, S).

greedy(F, _, L, Best):-
	select_greedy(L, Best),
	Best = [F|_]/_/_.

greedy(F, H, L, S):-
	select_greedy(L, Best),
	select(Best, L, NewL),
	expand_adj(H, Best, ExpL),
	append(NewL, ExpL, New2L),
	greedy(F, H, New2L, S).

select_greedy([Best], Best):- !.
select_greedy([P/C/H1, _/_/H2|T], Best):-
	H1 =< H2, !,
	select_greedy([P/C/H1|T], Best).
select_greedy([_|T], Best):- select_greedy(T, Best).


%
aEstrela(A, nodo(B), H, RH, S/Total):-
	aEstrela(A, H, [[B]/0/RH], R/Total/_),
	reverse(R, S).
aEstrela(F, _, L, Best):-
	select_aEstrela(L, Best),
	Best = [F|_]/_/_.
aEstrela(F, H, L, S):-
	select_aEstrela(L, Best),
	select(Best, L, NewL),
	expand_adj(H, Best, ExpL),
	append(NewL, ExpL, New2L),
	aEstrela(F, H, New2L, S).


aEstrelaTime(A, nodo(B), H, RH, V, S/Total):-
	aEstrelaTime(A, H, V, [[B]/0/RH], R/Total/_),
	reverse(R, S).
aEstrelaTime(F, _, _, L, Best):-
	select_aEstrela(L, Best),
	Best = [F|_]/_/_.
aEstrelaTime(F, H, V, L, S):-
	select_aEstrela(L, Best),
	select(Best, L, NewL),
	expand_adj(H, V, Best, ExpL),
	append(NewL, ExpL, New2L),
	aEstrelaTime(F, H, V, New2L, S).

select_aEstrela([Best], Best):- !.
select_aEstrela([P/C1/H1, _/C2/H2|T], Best):-
	C1 + H1 =< C2 + H2, !,
	select_aEstrela([P/C1/H1|T], Best).
select_aEstrela([_|T], Best):- select_aEstrela(T, Best).


expand_adj(H, Best, ExpL):-
	findall(R, (adj2(H, Best, R)), ExpL).

expand_adj(H, V, Best, ExpL):-
	findall(R, (adj2(H, V, Best, R)), ExpL).

adj2(H, [A|T]/C/_, [B,A|T]/NewC/RH):-
	adjacente(A, B, D, _),
	not(member(B, T)),
	getHeuristicaOfNodo(nodo(B), H, RH),
	NewC is C + D.

adj2(H, V, [A|T]/C/_, [B,A|T]/NewC/RH):-
	adjacente(A, B, D, COEF),
	not(member(B, T)),
	getHeuristicaOfNodo(nodo(B), H, RH),
	calculaTempo(D, V, COEF, TIME),
	NewC is C + TIME.


%
circuitosMaisEntregas(RESULT) :- 
	findall(Cir,registo(_,_,circuito(Cir),_),S), !,
	maisEntregas(S, [], TRACKS, [], VALORES),
	selectBest(RESULT, TRACKS, VALORES), !.


maisEntregas([], TRACKS, TRACKS, VALORES, VALORES).
maisEntregas([TRACK|T1], T2, TRACKS, T3, VALORES):- 
	member(TRACK, T2),
	findIndexFromList(TRACK, T2, INDEX),
	getIndexFromList(INDEX, T3, OLD_NR), NEW_NR is OLD_NR + 1,
	modIndexFromList(INDEX, NEW_NR, T3, NEW_T3),
	maisEntregas(T1, T2, TRACKS, NEW_T3, VALORES).
maisEntregas([TRACK|T1], T2, TRACKS, T3, VALORES):-
	not(member(TRACK, T2)),
	maisEntregas(T1, [TRACK|T2], TRACKS, [1|T3], VALORES).


selectBest(S, [S], _).
selectBest(S, [A,_|T1], [NR_A,NR_B|T2]):- 
	NR_A > NR_B, selectBest(S, [A|T1], [NR_A|T2]).
selectBest(S, [_,B|T1], [NR_A,NR_B|T2]):- 
	NR_A =< NR_B, selectBest(S, [B|T1], [NR_B|T2]).

%
circuitosMaisPeso(RESULT) :- 
	findall((Cir,PESO), (registo(ARG1,encomenda(A2,A3,PESO,A4,A5),circuito(Cir),ARG4),
				not(excecao(registo(ARG1, encomenda(A2,A3,PESO,A4,A5), circuito(Cir), ARG4)))),S), !,
	maisPeso(S, [], TRACKS, [], VALORES),
	selectBest(RESULT, TRACKS, VALORES), !.

maisPeso([], TRACKS, TRACKS, VALORES, VALORES).
maisPeso([(TRACK,PESO)|T1], T2, TRACKS, T3, VALORES):- 
	member(TRACK, T2),
	findIndexFromList(TRACK, T2, INDEX),
	getIndexFromList(INDEX, T3, OLD_NR), NEW_NR is OLD_NR + PESO,
	modIndexFromList(INDEX, NEW_NR, T3, NEW_T3),
	maisPeso(T1, T2, TRACKS, NEW_T3, VALORES).
maisPeso([(TRACK,PESO)|T1], T2, TRACKS, T3, VALORES):-
	not(member(TRACK, T2)),
	maisPeso(T1, [TRACK|T2], TRACKS, [PESO|T3], VALORES).


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


%
cmpMultiplasEntregas(A, ENCOMENDAS, TRANSPORTE, DIST_ONE, TIME_ONE, DIST_MULTIPLAS, TIME_MULTIPLAS):-
	getInfoFromEncomendas(ENCOMENDAS, _, PESO_TOTAL),
	props_transporte(TRANSPORTE, PMAX, _, _), PMAX >= PESO_TOTAL,
	xEntregas(A, TRANSPORTE, ENCOMENDAS, TRACK_MULTIPLAS),
	avaliarCircuitoXEntregas(TRACK_MULTIPLAS, DIST_MULTIPLAS, TIME_MULTIPLAS),
	buildMultiplasEntregas(A, TRANSPORTE, ENCOMENDAS, DIST_ONE, TIME_ONE),
	write("TIME ENTREGAS SIMULTÂNEAS: "), write(TIME_MULTIPLAS), write('\n'),
	write("DIST ENTREGAS SIMULTÂNEAS: "), write(DIST_MULTIPLAS), write('\n'),
	write("TIME ENTREGAS SEQUENCIAIS: "), write(TIME_ONE), write('\n'),
	write("DIST ENTREGAS SEQUENCIAIS: "), write(DIST_ONE), write('\n\n'), !.

buildMultiplasEntregas(_, _, [], 0, 0).
buildMultiplasEntregas(A, TRANSPORTE, [encomenda(B,PESO)|T1], NEWDIST, NEWTIME):-
	circuitoBFS(A, B, TRANSPORTE, PESO, TRACK),
	avaliarCircuito(B, TRACK, DIST, TIME),
	buildMultiplasEntregas(A, TRANSPORTE, T1, DIST2, TIME2),
	NEWDIST is DIST + DIST2, NEWTIME is TIME + TIME2.

%
avaliarCircuito(DEST, circuito(TRANSPORTE, P, L), D, T):-
        avaliarCircuitoXEntregas(circuito(TRANSPORTE, [encomenda(DEST, P)], L), D, T).


avaliarCircuitoXEntregas(circuito(_, _, [_]), 0, 0).
avaliarCircuitoXEntregas(circuito(TRANSPORTE, ENCOMENDAS, [A,B|T]), DIST_TOTAL, TIME_TOTAL):-
        adjacente(A, B, DIST, COEF),
        getInfoFromEncomendas(ENCOMENDAS, CITIES, _),
        member(A, CITIES),
        findIndexFromList(A, CITIES, INDEX), rmIndexFromList(INDEX, ENCOMENDAS, NEW_ENCOMENDAS),
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
