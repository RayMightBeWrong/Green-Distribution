props_transporte(bicicleta, 5, 10).
props_transporte(moto, 20, 35).
props_transporte(carro, 100, 25).

%funções chamam-se isget, porque:
%servem para verificar se o elemento é o mesmo (is) e
%servem para ir buscar o elemento (get)

isget_estafeta(E, entrega(E,_)).
isget_veiculo(V, entrega(_,transporte(V,_))).
isget_coef(C, entrega(_,transporte(_,C))).

isget_cliente(C, encomenda(C,_,_,_,_)). 
isget_local(L, encomenda(_,L,_,_,_)).
isget_peso(P, encomenda(_,_,P,_,_)).
isget_preco(P, encomenda(_,_,_,P,_)).

isget_data_entrega(0, prazo(_, _, _, 0)).
isget_data_entrega(D/M/A, prazo(_, _, _, D/M/A)).


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
