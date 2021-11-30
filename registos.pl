estafeta(e1).
estafeta(e2).
estafeta(e3).
estafeta(e4).
estafeta(e5).

%remover clientes?
cliente(c1).
cliente(c2).
cliente(c3).

%registo(entrega(estafeta, meio_de_transporte), encomenda(cliente, local, volume, preço, classificação), prazo(hora_limite, data_limite, hora_de_entrega, data_de_entrega))
%registo(A, B, C)

lista_de_entregas([registo(entrega(e1, bicicleta), encomenda(c1, evergrande, 4, 15, 4), prazo(13/55, 18/11/2021, 13/55, 17/11/2021)),
		   registo(entrega(e1, moto), encomenda(c1, evergrande, 6, 25, 4), prazo(15/0, 16/11/2021, 14/2, 16/11/2021)),
	   	   registo(entrega(e2, carro), encomenda(c1, evergrande, 20, 26, 3), prazo(16/9, 16/11/2021, 16/8, 16/11/2021))]).

