:- include('mapa.pl').

gera_caminho(A, B, S):- gera_caminhoL(A, [B], S).

gera_caminhoL(A, [A|T], [A|T]).
gera_caminhoL(A, [B|T], S):- adjacente(X, B, _, _), not(member(X, [B|T])),
				gera_caminhoL(A, [X,B|T], S).



gera_todos(A, B, L):- findall(S, gera_caminho(A, B, S), L).

