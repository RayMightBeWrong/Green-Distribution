:- include('mapa.pl').
:- include('registos.pl').
:- include('aux.pl').


%adicionar estimativa de tempo ao mapa?
%dizer o tempo máximo de entrega fica como prazo de entrega
%penalização_por_não_cumprir_entregas

%extras:
%quando gastou um cliente na GreenDistribution
%calcular a média de satisfação de um cliente

% Total de gastos de um cliente
custo_total(C,T) :- lista_de_entregas(L), custo_total(L,C,T).
custo_total([],_,0).
custo_total([registo(_,ARG2,_)| T],C,Total) :- 
	isget_cliente(C,ARG2),
	custo_total(T,C,Acc),
	isget_preco(P,ARG2),
	Total is Acc + P.
custo_total([_|T],C,Total) :- custo_total(T,C,Total).



%Média de satisfação de um cliente

media_satisfacao(C,S) :- lista_de_entregas(L) , media_satisfacao(C,Total,Soma,L), Total > 0, S is Soma / Total,!.
media_satisfacao(_,0).
media_satisfacao(_,0,0,[]).
media_satisfacao(C,T,S,[registo(_,ARG2,_)|R]):- 
	isget_cliente(C,ARG2),
	media_satisfacao(C,Total,Soma,R),
	isget_nota(Class,ARG2),
	Class >= 0,
	T is Total+1,
	S is Soma+Class.
media_satisfacao(C,T,S,[_|R]) :- media_satisfacao(C,T,S,R).
%1 --- done
%calcular quem mais vezes a bicicleta, a média do coeficiente green é o fator de desempate
mais_ecologico(S):- lista_de_entregas(L), mais_ecologico(L, [], S).

mais_ecologico([registo(ARG1,_,_)|T], [], S):- 
	isget_veiculo(V, ARG1), isget_coef(COEF, ARG1), isget_estafeta(E, ARG1),
	add_veiculo_tuple(E/0/0/0, V/COEF, R), !, 
	mais_ecologico(T, [R], S).
mais_ecologico([], LE, S):- select_mais_ecologico(LE, S).

mais_ecologico([registo(ARG1,_,_)|T], LE, S):- 
	isget_estafeta(E, ARG1), isget_coef(COEF, ARG1), isget_veiculo(V, ARG1), 
	estafeta_pertence(E, R1, LE), 
	add_veiculo_tuple(R1, V/COEF, R2),
	replace_estafeta(R2, LE, NEWLE),
	mais_ecologico(T, NEWLE, S).

mais_ecologico([registo(ARG1,_,_)|T], LE, S):- 
	isget_veiculo(V, ARG1), isget_coef(COEF, ARG1), isget_estafeta(E, ARG1),
	not(estafeta_pertence(E, _, LE)), 
	add_veiculo_tuple(E/0/0/0, V/COEF, R),
	append([R], LE, NEWLE),
	mais_ecologico(T, NEWLE, S).

estafeta_pertence(E, E/B/C/N, [E/B/C/N|_]).
estafeta_pertence(E, R, [Y/_/_/_|L]):- E \= Y, estafeta_pertence(E, R, L).

replace_estafeta(E/B/C/N, [E/_/_/_|L], [E/B/C/N|L]). 
replace_estafeta(E/B1/C1/N1, [Y/B2/C2/N2|L1], [Y/B2/C2/N2|L2]):- 
	E \= Y, 
	replace_estafeta(E/B1/C1/N1, L1, L2).

add_veiculo_tuple(E/B/C/N, bicicleta/_, E/NEWB/C/NEWN):- NEWB is B + 1, NEWN is N + 1.
add_veiculo_tuple(E/B/C/N, moto/COEF, E/B/NEWC/NEWN):- NEWC is C + COEF, NEWN is N + 1.
add_veiculo_tuple(E/B/C/N, carro/COEF, E/B/NEWC/NEWN):- NEWC is C + COEF, NEWN is N + 1.

select_mais_ecologico([E/_/_/_], E):- !.
select_mais_ecologico([E1/B1/C1/N1, _/B2/_/_|T], Best):-
	B1 > B2, !,
	select_mais_ecologico([E1/B1/C1/N1|T], Best).
select_mais_ecologico([E1/B1/C1/N1, _/B2/C2/N2|T], Best):-
	B1 =:= B2, C1 / N1 < C2 / N2, !,
	select_mais_ecologico([E1/B1/C1/N1|T], Best).
select_mais_ecologico([E1/B1/C1/N1, _/B2/C2/N2|T], Best):-
	B1 =:= B2, C1 / N1 =:= C2 / N2, N1 >= N2, !,
	select_mais_ecologico([E1/B1/C1/N1|T], Best).
select_mais_ecologico([_|T], Best):- select_mais_ecologico(T, Best).


%2 --- done
identificar_estafetas([], _, []).
identificar_estafetas([registo(ARG1,ARG2,_)|T], C, [E|S]):- 
	isget_estafeta(E, ARG1), isget_cliente(C, ARG2),
	identificar_estafetas(T, C, S), not(member(E,S)), !.

identificar_estafetas([_|T], C, S):- identificar_estafetas(T, C, S).

%3 --- done
clientes_servidos(E, S):- lista_de_entregas(L), clientes_servidos(E, L, S).

clientes_servidos(_, [], []).
clientes_servidos(E, [registo(ARG1,ARG2,_)|T], [C|S]):- 
	isget_estafeta(E, ARG1), isget_cliente(C, ARG2),
	clientes_servidos(E, T, S),
	not(member(C,S)), !.
clientes_servidos(E, [_|T], S):- clientes_servidos(E, T, S).

%4 --- done
%assumimos que a data de pagamento é a mesma da entrega
valor_faturado(D/M/A, S):- lista_de_entregas(L), valor_faturado(D/M/A, L, S).

valor_faturado(_, [], 0).
valor_faturado(D/M/A, [registo(_,ARG2,ARG3)|T], S):-
	isget_data_entrega(D/M/A, ARG3), isget_preco(P, ARG2),
	valor_faturado(D/M/A, T, S1),
	S is S1 + P, !.
valor_faturado(D/M/A, [_|T], S):- valor_faturado(D/M/A, T, S).


%5 --- done
mais_pzona(N, S):- lista_de_entregas(L), build_mais_pzona(L, [], R1), insert_sort(R1, R2), get_Nelements(N, R2, S).

build_mais_pzona([], L, L).
build_mais_pzona([registo(_,ARG2,_)|T], [], S):-
	isget_local(X, ARG2), build_mais_pzona(T, [X/1], S).
build_mais_pzona([registo(_,ARG2,_)|T], L, S):-
	isget_local(X, ARG2), 
	local_pertence(X, X/N, L), N1 is N + 1,
	replace_local(X/N1, L, NEWL),
        build_mais_pzona(T, NEWL, S).
build_mais_pzona([registo(_,ARG2,_)|T], L, S):-
	isget_local(X, ARG2), 
	not(local_pertence(X, _, L)),
        append([X/1], L, NEWL),
        build_mais_pzona(T, NEWL, S).

local_pertence(X, X/SUM, [X/SUM|_]).
local_pertence(X, R, [Y/_|T]):- X \= Y, local_pertence(X, R, T).

replace_local(L/N, [L/_|T], [L/N|T]).
replace_local(L1/N1, [L2/N2|T1], [L2/N2|T2]):-
        L1 \= L2,
        replace_local(L1/N1, T1, T2).

insert_sort(L, S):- isort(L, [], S).
isort([], A, A).
isort([X/N|T], A, S):- insert(X/N, A, NEWA), isort(T, NEWA, S).

insert(X/N1, [Y/N2|T], [Y/N2|NEWT]):- N1 < N2, insert(X/N1, T, NEWT).
insert(X/N1, [Y/N2|T], [X/N1,Y/N2|T]):- N1 >= N2.
insert(X/N, [], [X/N]).

get_Nelements(0, _, []).
get_Nelements(_, [], []).
get_Nelements(N, [X/_|T1], [X|T2]):- NEWN is N - 1, get_Nelements(NEWN, T1, T2).


%6 --- done
avaliacao_estafeta(E, S):- lista_de_entregas(L), avaliacao_estafeta(L, E, 0, 0, S).

avaliacao_estafeta([], _, 0, 0, 0).
avaliacao_estafeta([], _, R, N, S):- S is R / N.
avaliacao_estafeta([registo(ARG1,ARG2,_)|T], E, R, N, S) :-
	isget_estafeta(E, ARG1), isget_nota(C, ARG2),
	NEWR is C + R, NEWN is N + 1, !,
	avaliacao_estafeta(T, E, NEWR, NEWN, S).
avaliacao_estafeta([_|T], E, R, N, S):- avaliacao_estafeta(T, E, R, N, S).


%7 --- done
p_transportes(B, M, C, Hi, Di, Hf, Df):- lista_de_entregas(L), p_transporte([B,M,C], [0,0,0], L, Hi, Di, Hf, Df).

p_transporte(L, L, [], _, _, _, _).
p_transporte(S, L, [registo(ARG1,_,ARG3)|T], Hi, Di, Hf, Df):- 
	entrega_in_time(Hi, Di, Hf, Df, ARG3),
	isget_veiculo(V, ARG1),
	modify_list(V, L, NEWL),
	p_transporte(S, NEWL, T, Hi, Di, Hf, Df).
p_transporte(S, L, [_|T], Hi, Di, Hf, Df):-
	p_transporte(S, L, T, Hi, Di, Hf, Df).

modify_list(bicicleta, [B,M,C], [NEWB,M,C]):- NEWB is B + 1.
modify_list(moto, [B,M,C], [B,NEWM,C]):- NEWM is M + 1.
modify_list(carro, [B,M,C], [B,M,NEWC]):- NEWC is C + 1.


%8 --- done
entregas_tempo(Hi,Di,Hf, Df, N):- lista_de_entregas(L), entregas_tempo(Hi,Di,Hf, Df, L, N), !.
entregas_tempo(_,_,_, _, [], 0).
entregas_tempo(Hi,Di, Hf,Df, [registo(_,_,ARG3)|T], G):- 
	entrega_in_time(Hi, Di, Hf, Df, ARG3),
	entregas_tempo(Hi,Di, Hf,Df, T, N),
	G is N + 1.
entregas_tempo(Hi,Di, Hf,Df, [_|T], G) :- entregas_tempo(Hi,Di, Hf,Df, T, G).


%9 --- done

enc_entregue_naoentregue(Hi,Di,Hf,Df,E, N) :- lista_de_entregas(L) , enc_entregue_naoentregue(Hi,Di,Hf,Df,L,E,N) , !.

enc_entregue_naoentregue(Hi,Di,Hf,Df,[], 0, 0).
enc_entregue_naoentregue(Hi,Di,Hf,Df,[registo(_,_,ARG3)|T], E, N):-
	isget_data_entrega(D, ARG3), D =:= 0,
	isget_data_limite(Dl,ARG3),
	isget_hora_limite(Hl,ARG3),
	entrega_in_time(Hi,Di,Hf,Df,prazo(_,_,Hl,Dl)),
	enc_entregue_naoentregue(Hi,Di,Hf,Df,T, E, G), N is G+1.
enc_entregue_naoentregue(Hi,Di,Hf,Df,[registo(_,_,ARG3)|T], E, N) :- 
	entrega_in_time(Hi,Di,Hf,Df,ARG3),
	enc_entregue_naoentregue(Hi,Di,Hf,Df,T, G, N),
	E is G + 1.
enc_entregue_naoentregue(Hi,Di,Hf,Df,[_|T], E, N) :- enc_entregue_naoentregue(Hi,Di,Hf,Df,T, E, N).


%10 --- done
peso_carregado(E, D/M/A, S):- lista_de_entregas(L), peso_carregado(E, D/M/A, L, S).

peso_carregado(_, _, [], 0).
peso_carregado(E, D/M/A, [registo(ARG1,ARG2,ARG3)|T], S):- 
	isget_estafeta(E, ARG1), isget_data_entrega(D/M/A, ARG3), isget_peso(P, ARG2),
	peso_carregado(E,D/M/A,T,S1), S is S1 + P, !.
peso_carregado(E, D/M/A, [_|T], S):- peso_carregado(E, D/M/A, T, S).
