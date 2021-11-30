props_transporte(bicicleta, 5, 10).
props_transporte(moto, 20, 35).
props_transporte(carro, 100, 25).

same_estafeta(E, entrega(E,_)).
same_data_entrega(D/M/A, prazo(_, _, _, D/M/A)).

get_preco(P, encomenda(_,_,_,P,_)).
