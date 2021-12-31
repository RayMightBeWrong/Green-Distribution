:- include('mapa.pl').
:- include('helper.pl').

%
%gera_todos(A, B, T, P, L):- findall(S, gera_circuito(A, B, T, P, S), L).


% A e B -> nodos de partida e de chegada, respetivamente
% T -> transporte
% P -> peso da encomenda
circuitoDFS(A, B, T, P, circuito(A, B, T, P, S)):- circuitoDFS(A, [B], [], S).

circuitoDFS(A, [A|_], S, S).
circuitoDFS(A, [B|T], L, S):- adjacente(X, B, D, C), not(member(X, [B|T])),
				circuitoDFS(A, [X,B|T], [aresta(X, B, D, C)|L], S).

