:- include('mapa.pl').
:- include('helper.pl').

%
%gera_todos(A, B, T, P, L):- findall(S, gera_circuito(A, B, T, P, S), L).


% A e B -> nodos de partida e de chegada, respetivamente
% T -> transporte
% P -> peso da encomenda

% depth first search
circuitoDFS(A, B, T, P, circuito(A, B, T, P, S)):- circuitoDFS(A, [B], S).

circuitoDFS(A, [A|T], [A|T]).
circuitoDFS(A, [B|T], S):- adjacente(X, B, _, _), not(member(X, [B|T])),
			%write("Atual: "), write(B), write('\n'),
			%write("Próximo: "), write(X), write('\n'),
			%write("Estado: "), write([B|T]), write('\n'),
			%write("Próximo Estado: "), write([X,B|T]), write('\n'), write('\n'),
			circuitoDFS(A, [X,B|T], S).
			
% breadth first search 
circuitoBFS(A, B, T, P, circuito(A, B, T, P, S)):- circuitoBFS(B, [[A]], S).

circuitoBFS(F, [[F|T]|_], S):- reverse([F|T], S). %write("final: "), write(S), write("\n").

circuitoBFS(F, [EstadosA|Outros], S):-
	EstadosA = [Atual|_],
	findall([X|EstadosA],
		(F \== Atual, adjacente(Atual, X, _, _), not(member(X, EstadosA))), Novos),
	append(Outros, Novos, Todos),
	%write("Nodo atual: "), write(Atual), write("\n"),
	%write("EstadosA: "), write(EstadosA), write("\n"),
	%write("Novos: "), write(Novos), write("\n"),
	%write("Outros: "), write(Outros), write("\n"),
	%write("estado: "), write(Todos), write("\n"), write("\n"),
	circuitoBFS(F, Todos, S).


% iterative deepening search
circuitoIDS(A, B, T, P, circuito(A, B, T, P, S)):- loopIDS(1, A, [B], S).

loop_size(5).
	
loopIDS(N, _, [_], []):- loop_size(N), !.
loopIDS(N, _, S, S):- loop_size(N), length(S, LEN), LEN > 1.
loopIDS(N, A, [B], S):- loop_size(SIZE), N < SIZE, 
		%write('N LOOP: '), write(N), write('\n'), 
		circuitoIDS(A, [B], N, S), length(S, LEN), LEN > 0.
loopIDS(N, A, [B], S):- loop_size(SIZE), N < SIZE,
		%write('N LOOP: '), write(N), write('\n'), 
		NewN is N + 1, 
		%write('NewN LOOP: '), write(NewN), write('\n'), write('\n'), 
		circuitoIDS(A, [B], N, R),
		length(R, 0),
		loopIDS(NewN, A, [B], S).


circuitoIDS(A, [A|T], _, [A|T]).
circuitoIDS(_, _, 0, []).
circuitoIDS(A, [B|T], N, S):- A \= B, adjacente(X, B, _, _), not(member(X, [B|T])),		
			NewN is N - 1, NewN >= 0, 
			%write("NewN: "), write(NewN), write('\n'), 
			%write("Atual: "), write(B), write('\n'), 
			%write("Estado: "), write([B|T]), write('\n'), 
			%write("Próximo: "), write(X), write('\n'), 
			%write("Próximo Estado: "), write([X,B|T]), write('\n'), write('\n'), 
			circuitoIDS(A, [X,B|T], NewN, S).


distHeuristica(F, S):- mapa(L, _), distHeuristica(F, L, S).

distHeuristica(_, [], []).
distHeuristica(F, [nodo(F)|T1], [nodo(F, 0)|T2]):- distHeuristica(F, T1, T2).
distHeuristica(F, [nodo(A)|T1], [nodo(A, D)|T2]):- 
			circuitoBFS(F, A, transporte(tmp), 0, circuito(_, _, _, _, S)), 
			createListAresta(S, _, D, _),
			distHeuristica(F, T1, T2).

distHeuristica(F, nodo(A), nodo(A, H)):- 
	circuitoBFS(F, A, transporte(tmp), 0, circuito(_, _, _, _, S)), 
	createListAresta(S, _, H, _), !.


select(X, [X|T], T).
select(X, [Y|T1], [Y|T2]):- select(X, T1, T2).

%mudar o custo maybe
greedy(A, B, S/Total):-
	distHeuristica(A, nodo(B), nodo(B, D)),
	greedy(A, [[B]/0/D], R/Total/_),
	reverse(R, S).

greedy(F, L, Best):-
	%write('L GREEDY: '), write(L), write('\n'),
	select_greedy(L, Best),
	%write('Best GREEDY: '), write(Best), write('\n'),
	Best = [F|_]/_/_.

greedy(F, L, S):-
	select_greedy(L, Best),
	%write('L: '), write(L), write('\n'),
	%write('Best: '), write(Best), write('\n'),
	select(Best, L, NewL),
	%write('NewL: '), write(NewL), write('\n'),
	expand_adj(F, Best, ExpL),
	%write('ExpL: '), write(ExpL), write('\n'), write('\n'),
	append(NewL, ExpL, New2L),
	greedy(F, New2L, S).


select_greedy([Best], Best):- !.
select_greedy([P/C/D1, _/_/D2|T], Best):-
	D1 =< D2, !,
	select_greedy([P/C/D1|T], Best).
select_greedy([_|T], Best):- select_greedy(T, Best).

expand_adj(A, Best, ExpL):-
	findall(R, (adj2(A, Best, R)), ExpL).

adj2(F, [A|T]/C/_, [B,A|T]/NewC/H):-
	adjacente(A, B, D, _),
	%write('D: '), write(D), write('\n'),
	not(member(B, T)),
	distHeuristica(F, nodo(B), nodo(B, H)),
	NewC is C + D.




a*(A, B, S/Total):-
	distHeuristica(A, nodo(B), nodo(B, D)),
	a*(A, [[B]/0/D], R/Total/_),
	reverse(R, S).

a*(F, L, Best):-
	select_a*(L, Best),
	Best = [F|_]/_/_.

a*(F, L, S):-
	select_a*(L, Best),
	%write('L: '), write(L), write('\n'),
	%write('Best: '), write(Best), write('\n'),
	select(Best, L, NewL),
	%write('NewL: '), write(NewL), write('\n'),
	expand_adj(F, Best, ExpL),
	%write('ExpL: '), write(ExpL), write('\n'), write('\n'),
	append(NewL, ExpL, New2L),
	a*(F, New2L, S).


select_a*([Best], Best):- !.
select_a*([P/C1/D1, _/C2/D2|T], Best):-
	C1 + D1 =< C2 + D2, !,
	select_a*([P/C1/D1|T], Best).
select_a*([_|T], Best):- select_a*(T, Best).
