:- include('mapa.pl').
:- include('helper.pl').

%
%gera_todos(A, B, T, P, L):- findall(S, gera_circuito(A, B, T, P, S), L).


% A e B -> nodos de partida e de chegada, respetivamente
% T -> transporte
% P -> peso da encomenda

circuitoDFS(A, B, T, P, circuito(A, B, T, P, S)):- circuitoDFS(A, [B], S).

circuitoDFS(A, [A|T], [A|T]).
circuitoDFS(A, [B|T], S):- adjacente(X, B, _, _), not(member(X, [B|T])),
				circuitoDFS(A, [X,B|T], S).
			

circuitoBFS(A, B, T, P, circuito(A, B, T, P, S)):- circuitoBFS(B, [[A]], S).

circuitoBFS(F, [[F|T]|_], S):- reverse([F|T], S). %write("final: "), write(S), write("\n").

circuitoBFS(B, [EstadosA|Outros], S):-
	EstadosA = [Atual|_],
	findall([X|EstadosA],
		(B \== Atual, adjacente(Atual, X, _, _), not(member(X, EstadosA))), Novos),
	append(Outros, Novos, Todos),
	%write("Nodo atual: "), write(Atual), write("\n"),
	%write("EstadosA: "), write(EstadosA), write("\n"),
	%write("Novos: "), write(Novos), write("\n"),
	%write("Outros: "), write(Outros), write("\n"),
	%write("estado: "), write(Todos), write("\n"), write("\n"),
	circuitoBFS(B, Todos, S).


%getArestaFromNovos -> appendArestasToListOfLists
createListAresta([_], []).
createListAresta([A,B|T1], [aresta(A, B, D, C)|T2]):- adjacente(A, B, D, C), createListAresta([B|T1], T2).



%solve(S):- bfs(jarros(0,0), jarros(_,4), S).

%bfs(EstadoI, EstadoF, S):- bfs_M(EstadoF, [[EstadoI]], S).

%bfs_M(EstadoF, [[EstadoF|T]|_], S):- reverse([EstadoF|T], S).

%bfs_M(EstadoF, [EstadosA|Outros], S):-
%	EstadosA = [Act|_],
%	findall([EstadoX|EstadosA],
%		(EstadoF \== Act, transicao(Act, Move, EstadoX), \+member(EstadoX, EstadosA)), Novos),
%	append(Outros, Novos, Todos),
%	bfs_M(EstadoF, Todos, S).

