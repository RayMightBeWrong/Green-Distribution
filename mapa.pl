mapa([nodo(dewford), nodo(petalburg), nodo(oldale), nodo(littleroot), nodo(slateport), nodo(rustboro), nodo(verdanturf), nodo(fallarbor),
      nodo(lavaridge), nodo(mauville), nodo(fortree), nodo(lilycove), nodo(mossdeep), nodo(sootopolis), nodo(pacifidlog), nodo(evergrande)],
	[aresta(dewford, petalburg, 30), aresta(petalburg, oldale, 5), aresta(oldale, littleroot, 5), aresta(petalburg, rustboro, 20), aresta(rustboro, verdanturf, 15),
	aresta(oldale, slateport, 15), aresta(verdanturf, mauville, 10), aresta(slateport, mauville, 25), aresta(mauville, lavaridge, 10), aresta(lavaridge, fallarbor, 20),        
	aresta(rustboro, fallarbor, 35), aresta(fallarbor, mauville, 40), aresta(mauville, fortree, 40), aresta(fortree, lilycove, 50), aresta(lilycove, mossdeep, 15),        
	aresta(mossdeep, sootopolis, 20), aresta(sootopolis, evergrande, 20), aresta(slateport, pacifidlog, 40), aresta(mossdeep, evergrande, 30), aresta(pacifidlog, evergrande, 45)]).


adjacente(A, B, C):- mapa(_, L), member(aresta(A,B,C), L).
adjacente(A, B, C):- mapa(_, L), member(aresta(B,A,C), L).

