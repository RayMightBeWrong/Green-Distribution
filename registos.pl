estafeta(brendan).
estafeta(may).
estafeta(wally).
estafeta(travis).
estafeta(iris).

%não importante na fase1
partida(brendan, littleroot).
partida(may, littleroot).
partida(wally, verdanturf).
partida(travis, pacifidlog).
partida(iris, mossdeep).

%registo(entrega(estafeta, transporte(veiculo, coeficiente_green)), encomenda(cliente, local, volume, preço, classificação), prazo(hora_limite, data_limite, hora_de_entrega, data_de_entrega))
%registo(A, B, C)

%coeficiente green é sempre 0 para bicicletas
%varia entre 5 e 10 para motos e carros

lista_de_entregas([registo(entrega(may, transporte(bicicleta, 0)), encomenda(c1, slateport, 4, 20, 5), prazo(10/45, 15/11/2021, 10/15, 15/11/2021)), %may
		   registo(entrega(may, transporte(bicicleta, 0)), encomenda(c2, petalburg, 3, 25, 5), prazo(15/45, 15/11/2021, 14/30, 15/11/2021)),
		   registo(entrega(may, transporte(bicicleta, 0)), encomenda(c3, rustboro, 3, 22, 5), prazo(19/45, 15/11/2021, 18/45, 15/11/2021)),
		   registo(entrega(may, transporte(bicicleta, 0)), encomenda(c4, petalburg, 5, 20, 5), prazo(10/45, 16/11/2021, 10/10, 16/11/2021)),
		   registo(entrega(may, transporte(bicicleta, 0)), encomenda(c5, oldale, 3, 22, 5), prazo(16/45, 16/11/2021, 12/30, 16/11/2021)),
		   registo(entrega(may, transporte(bicicleta, 0)), encomenda(c6, dewford, 3, 25, 5), prazo(19/45, 16/11/2021, 17/50, 16/11/2021)),
		   registo(entrega(may, transporte(bicicleta, 0)), encomenda(c1, slateport, 4, 20, 5), prazo(10/45, 17/11/2021, 10/15, 17/11/2021)),
		   registo(entrega(may, transporte(bicicleta, 0)), encomenda(c2, petalburg, 3, 25, 5), prazo(15/45, 17/11/2021, 14/30, 17/11/2021)),
		   registo(entrega(may, transporte(bicicleta, 0)), encomenda(c3, rustboro, 3, 22, 5), prazo(19/45, 17/11/2021, 18/45, 17/11/2021)),
		   registo(entrega(may, transporte(bicicleta, 0)), encomenda(c4, petalburg, 5, 20, 5), prazo(10/45, 18/11/2021, 10/10, 18/11/2021)),
		   registo(entrega(may, transporte(bicicleta, 0)), encomenda(c5, oldale, 3, 22, 5), prazo(16/45, 18/11/2021, 12/30, 18/11/2021)),
		   registo(entrega(may, transporte(bicicleta, 0)), encomenda(c6, dewford, 3, 25, 5), prazo(19/45, 18/11/2021, 17/50, 18/11/2021)),

		   registo(entrega(travis, transporte(carro, 8)), encomenda(c9, fallarbor, 80, 60, -1), prazo(20/45, 15/11/2021, 0, 0)), %travis
		   registo(entrega(travis, transporte(carro, 8)), encomenda(c8, mauville, 70, 50, 1), prazo(17/45, 16/11/2021, 19/30, 16/11/2021)),
		   registo(entrega(travis, transporte(carro, 8)), encomenda(c10, sootopolis, 65, 55, 1), prazo(17/45, 17/11/2021, 18/30, 17/11/2021)),
		   registo(entrega(travis, transporte(carro, 8)), encomenda(c11, lilycove, 75, 30, 3), prazo(18/45, 18/11/2021, 18/30, 18/11/2021)),

		   registo(entrega(brendan, transporte(moto, 6)), encomenda(c12, rustboro, 10, 25, 4), prazo(11/30, 15/11/2021, 10/10, 15/11/2021)), %brendan
		   registo(entrega(brendan, transporte(moto, 6)), encomenda(c13, verdanturf, 12, 35, 4), prazo(16/30, 15/11/2021, 15/11, 15/11/2021)),
		   registo(entrega(brendan, transporte(moto, 6)), encomenda(c14, pacifidlog, 15, 50, 5), prazo(21/30, 15/11/2021, 20/30, 15/11/2021)),
		   registo(entrega(brendan, transporte(moto, 6)), encomenda(c12, rustboro, 10, 25, 4), prazo(11/30, 16/11/2021, 10/10, 16/11/2021)),
		   registo(entrega(brendan, transporte(moto, 6)), encomenda(c13, verdanturf, 12, 35, 4), prazo(16/30, 16/11/2021, 15/25, 16/11/2021)),
		   registo(entrega(brendan, transporte(moto, 6)), encomenda(c14, pacifidlog, 15, 50, 5), prazo(21/30, 16/11/2021, 20/36, 16/11/2021)),
		   registo(entrega(brendan, transporte(moto, 6)), encomenda(c12, rustboro, 10, 25, 4), prazo(11/30, 17/11/2021, 10/12, 17/11/2021)),
		   registo(entrega(brendan, transporte(moto, 6)), encomenda(c13, verdanturf, 12, 35, 4), prazo(16/30, 17/11/2021, 15/27, 17/11/2021)),
		   registo(entrega(brendan, transporte(moto, 6)), encomenda(c14, pacifidlog, 15, 50, 5), prazo(21/30, 17/11/2021, 20/22, 17/11/2021)),
		   registo(entrega(brendan, transporte(moto, 6)), encomenda(c12, rustboro, 10, 25, 4), prazo(11/30, 18/11/2021, 10/15, 18/11/2021)),
		   registo(entrega(brendan, transporte(moto, 6)), encomenda(c13, verdanturf, 12, 35, 4), prazo(16/30, 18/11/2021, 15/13, 18/11/2021)),
		   registo(entrega(brendan, transporte(moto, 6)), encomenda(c14, pacifidlog, 15, 50, 5), prazo(21/30, 18/11/2021, 20/14, 18/11/2021)),

		   registo(entrega(iris, transporte(bicicleta, 0)), encomenda(c14, lilycove, 4, 25, 4), prazo(14/15, 15/11/2021, 14/14, 15/11/2021)), %iris
		   registo(entrega(iris, transporte(bicicleta, 0)), encomenda(c15, sootopolis, 2, 20, 5), prazo(19/45, 15/11/2021, 19/44, 15/11/2021)),
		   registo(entrega(iris, transporte(carro, 9)), encomenda(c16, fortree, 60, 45, 4), prazo(10/15, 16/11/2021, 10/14, 16/11/2021)),
		   registo(entrega(iris, transporte(carro, 9)), encomenda(c17, sootopolis, 70, 35, 2), prazo(16/45, 16/11/2021, 16/55, 16/11/2021)),
		   registo(entrega(iris, transporte(carro, 9)), encomenda(c18, evergrande, 99, 80, 5), prazo(20/15, 16/11/2021, 20/14, 16/11/2021)),
		   registo(entrega(iris, transporte(bicicleta, 0)), encomenda(c14, lilycove, 4, 25, 4), prazo(14/15, 17/11/2021, 14/14, 17/11/2021)),
		   registo(entrega(iris, transporte(bicicleta, 0)), encomenda(c15, sootopolis, 2, 20, 5), prazo(19/15, 17/11/2021, 19/14, 17/11/2021)),
		   registo(entrega(iris, transporte(carro, 9)), encomenda(c16, fortree, 60, 50, 4), prazo(10/15, 18/11/2021, 10/14, 18/11/2021)),
		   registo(entrega(iris, transporte(carro, 9)), encomenda(c17, sootopolis, 70, 40, 2), prazo(16/45, 18/11/2021, 16/46, 18/11/2021)),
		   registo(entrega(iris, transporte(carro, 9)), encomenda(c18, evergrande, 99, 80, 5), prazo(20/15, 18/11/2021, 20/14, 18/11/2021)),

		   registo(entrega(wally, transporte(carro, 6)), encomenda(c15, sootopolis, 88, 80, 4), prazo(20/45, 15/11/2021, 20/22, 15/11/2021)), %wally
		   registo(entrega(wally, transporte(moto, 7)), encomenda(c19, fallarbor, 15, 35, 3), prazo(11/30, 16/11/2021, 10/26, 16/11/2021)),
		   registo(entrega(wally, transporte(moto, 7)), encomenda(c20, littleroot, 10, 50, 4), prazo(16/15, 16/11/2021, 14/28, 16/11/2021)),
		   registo(entrega(wally, transporte(moto, 7)), encomenda(c21, mauville, 18, 60, 4), prazo(20/45, 16/11/2021, 18/27, 16/11/2021)),
		   registo(entrega(wally, transporte(carro, 6)), encomenda(c15, sootopolis, 88, 80, 4), prazo(20/45, 17/11/2021, 20/12, 17/11/2021)),
		   registo(entrega(wally, transporte(moto, 7)), encomenda(c19, fallarbor, 6, 35, 3), prazo(11/45, 18/11/2021, 10/13, 18/11/2021)),
		   registo(entrega(wally, transporte(moto, 7)), encomenda(c20, littleroot, 6, 50, 4), prazo(16/15, 18/11/2021, 14/29, 18/11/2021)),
		   registo(entrega(wally, transporte(moto, 7)), encomenda(c21, mauville, 18, 60, 4), prazo(20/45, 18/11/2021, 18/37, 18/11/2021))]).
