mapa([nodo(dewford), nodo(petalburg), nodo(oldale), nodo(littleroot), nodo(slateport), nodo(rustboro), nodo(verdanturf), nodo(fallarbor),
      nodo(lavaridge), nodo(mauville), nodo(fortree), nodo(lilycove), nodo(mossdeep), nodo(sootopolis), nodo(pacifidlog), nodo(evergrande)],
	[aresta(dewford, petalburg, 12, 0.95), aresta(petalburg, oldale, 3, 1.1), aresta(oldale, littleroot, 3, 1.1), aresta(petalburg, rustboro, 10, 0.95), aresta(rustboro, verdanturf, 7, 1),
	aresta(oldale, slateport, 8, 0.9), aresta(verdanturf, mauville, 6, 1.2), aresta(slateport, mauville, 12, 0.95), aresta(mauville, lavaridge, 6, 0.85), aresta(lavaridge, fallarbor, 8, 0.85),        
	aresta(rustboro, fallarbor, 18, 0.9), aresta(fallarbor, mauville, 20, 0.95), aresta(mauville, fortree, 20, 0.95), aresta(fortree, lilycove, 15, 0.95), aresta(lilycove, mossdeep, 8, 0.95),        
	aresta(mossdeep, sootopolis, 10, 0.9), aresta(sootopolis, evergrande, 10, 0.8), aresta(slateport, pacifidlog, 16, 0.8), aresta(mossdeep, evergrande, 15, 0.95), aresta(pacifidlog, evergrande, 22, 0.85)]).

% aresta(nodoA, nodoB, distancia(km), coefiecente de travessia)

% coeficiente de travessia varia entre 0.8 e 1.2 e influencia a velocidade média
% sendo que valores menores que 1 diminuem a velocidade e maiores que 1 aumentam a velocidade

adjacente(A, B, D, C):- mapa(_, L), member(aresta(A,B,D,C), L).
adjacente(A, B, D, C):- mapa(_, L), member(aresta(B,A,D,C), L).


