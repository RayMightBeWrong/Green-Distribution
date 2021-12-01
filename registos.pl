estafeta(e1).
estafeta(e2).
estafeta(e3).
estafeta(e4).
estafeta(e5).

%registo(entrega(estafeta, transporte(veiculo, coeficiente_green)), encomenda(cliente, local, volume, preço, classificação), prazo(hora_limite, data_limite, hora_de_entrega, data_de_entrega))
%registo(A, B, C)

%coeficiente green é sempre 0 para bicicletas
%varia entre 5 e 10 para motos e carros

lista_de_entregas([registo(entrega(e1, transporte(bicicleta, 0)), encomenda(c1, evergrande, 4, 15, 3), prazo(13/55, 18/11/2021, 13/55, 17/11/2021)),
		   registo(entrega(e1, transporte(moto, 7)), encomenda(c2, oldale, 6, 25, 4), prazo(15/0, 16/11/2021, 14/2, 16/11/2021)),
	   	   registo(entrega(e2, transporte(carro, 6)), encomenda(c1, evergrande, 20, 26, 3), prazo(16/9, 16/11/2021, 16/8, 16/11/2021))]).


%ser consistente ao fazer o caso prático com os veículos que os estafetas usam
