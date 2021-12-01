mapa([nodo(dewford), nodo(petalburg), nodo(oldale), nodo(littleroot), nodo(slateport), nodo(rustboro), nodo(verdanturf), nodo(fallarbor),
      nodo(lavaridge), nodo(mauville), nodo(fortree), nodo(lilycove), nodo(mossdeep), nodo(sootopolis), nodo(pacifidlog), nodo(evergrande)],
	[aresta(dewford, petalburg, 30, 0.8), aresta(petalburg, oldale, 5, 1), aresta(oldale, littleroot, 5, 1), aresta(petalburg, rustboro, 20, 0.9), aresta(rustboro, verdanturf, 15, 1),
	aresta(oldale, slateport, 15, 0.8), aresta(verdanturf, mauville, 10, 1), aresta(slateport, mauville, 25, 0.9), aresta(mauville, lavaridge, 10, 0.7), aresta(lavaridge, fallarbor, 15, 0.6),        
	aresta(rustboro, fallarbor, 35, 0.8), aresta(fallarbor, mauville, 40, 0.9), aresta(mauville, fortree, 40, 0.9), aresta(fortree, lilycove, 50, 0.9), aresta(lilycove, mossdeep, 15, 0.9),        
	aresta(mossdeep, sootopolis, 20, 0.8), aresta(sootopolis, evergrande, 20, 0.7), aresta(slateport, pacifidlog, 40, 0.5), aresta(mossdeep, evergrande, 30, 0.9), aresta(pacifidlog, evergrande, 45, 0.8)]).

%aresta(nodoA, nodoB, distancia(km), coeficiente_de_facilidade_de_travessia)
%coeficiente de facilidade de travessia em cada aresta varia entre 0.5 e 1, sendo 1 um caminho fácil e 0.5 difícil

adjacente(A, B, D, C):- mapa(_, L), member(aresta(A,B,D,C), L).
adjacente(A, B, D, C):- mapa(_, L), member(aresta(B,A,D,C), L).


