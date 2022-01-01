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
				write("Atual: "), write(B), write('\n'),
				write("Próximo: "), write(X), write('\n'),
				write("Estado: "), write([B|T]), write('\n'),
				write("Próximo Estado: "), write([X,B|T]), write('\n'), write('\n'),
				circuitoDFS(A, [X,B|T], S).
			
% breadth first search 
circuitoBFS(A, B, T, P, circuito(A, B, T, P, S)):- circuitoBFS(B, [[A]], S).

circuitoBFS(F, [[F|T]|_], S):- reverse([F|T], S). %write("final: "), write(S), write("\n").

circuitoBFS(F, [EstadosA|Outros], S):-
	EstadosA = [Atual|_],
	findall([X|EstadosA],
		(F \== Atual, adjacente(Atual, X, _, _), not(member(X, EstadosA))), Novos),
	append(Outros, Novos, Todos),
	write("Nodo atual: "), write(Atual), write("\n"),
	write("EstadosA: "), write(EstadosA), write("\n"),
	write("Novos: "), write(Novos), write("\n"),
	write("Outros: "), write(Outros), write("\n"),
	write("estado: "), write(Todos), write("\n"), write("\n"),
	circuitoBFS(F, Todos, S).


% iterative deepening search
% mudar T
%circuitoIDS(A, B, T, P, circuito(A, B, T, P, S)):- circuitoIDS(A, [B], T, S).

%circuitoIDS(A, [A|T], _, [A|T]).
%circuitoIDS(_, _, 0, []).
%circuitoIDS(A, [B|T], N, S):- A \= B, adjacente(X, B, _, _), not(member(X, [B|T])),		
%			NewN is N - 1, NewN >= 0, 
%			write("NewN: "), write(NewN), write('\n'), 
%			write("Atual: "), write(B), write('\n'), 
%			write("Estado: "), write([B|T]), write('\n'), 
%			write("Próximo: "), write(X), write('\n'), 
%			write("Próximo Estado: "), write([X,B|T]), write('\n'), write('\n'), 
%			circuitoIDS(A, [X,B|T], NewN, S).

rmFromList(_, [], []).
rmFromList(A, [A|T1], T2):- rmFromList(A, T1, T2).
rmFromList(A, [B|T1], [B|T2]):- rmFromList(A, T1, T2).


tmp(A, B, T, P, circuito(A, B, T, P, S)):- tmp(A, B, [], [B], 1, 1, S).

tmp(F, I, T1, [A|T2], N1, N2, S):- NewN is N1 - 1, NewN >= 0,
			findall(X, (adjacente(X, A, _, _), not(member(X, T2))), L), 
			append(L, T2, NewL),
			tmp(F, I, [A|T1], NewL, NewN, N2, S).


%loop(10).
%loop(N):- N < 10, write('N: '), write(N), write('\n'), NewN is N + 1, loop(NewN).




circuitoIDS(A, B, T, P, circuito(A, B, T, P, S)):- loop(1, A, [B], S).

loop_size(5).
	
loop(N, _, [_], []):- loop_size(N), !.
loop(N, _, S, S):- loop_size(N), length(L, LEN), LEN > 1.
loop(N, A, [B], S):- loop_size(SIZE), N < SIZE, 
		write('N LOOP: '), write(N), write('\n'), 
		NewN is N + 1, 
		write('NewN LOOP: '), write(NewN), write('\n'), write('\n'), 
		write('deu bem\n'),
		circuitoIDS(A, [B], N, S), length(S, LEN), LEN > 0.
loop(N, A, [B], S):- loop_size(SIZE), N < SIZE,
		write('N LOOP: '), write(N), write('\n'), 
		NewN is N + 1, 
		write('NewN LOOP: '), write(NewN), write('\n'), write('\n'), 
		circuitoIDS(A, [B], N, R),
		length(R, 0),
		write('deu mal\n'),
		loop(NewN, A, [B], S).


circuitoIDS(A, [A|T], _, [A|T]):- write('atão\n').
circuitoIDS(_, _, 0, []).
circuitoIDS(A, [B|T], N, S):- A \= B, adjacente(X, B, _, _), not(member(X, [B|T])),		
			NewN is N - 1, NewN >= 0, 
			write("NewN: "), write(NewN), write('\n'), 
			write("Atual: "), write(B), write('\n'), 
			write("Estado: "), write([B|T]), write('\n'), 
			write("Próximo: "), write(X), write('\n'), 
			write("Próximo Estado: "), write([X,B|T]), write('\n'), write('\n'), 
			circuitoIDS(A, [X,B|T], NewN, S).

