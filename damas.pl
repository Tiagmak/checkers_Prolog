:-use_module(library(lists)).                                               
%%%%%%%%%%%%%%%%%%%%%%%  REPRESENTACAO DO ESTADO DO TABULEIRO  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                                                                          
% p - jogador 1 com peças pretas
% b - jogador 2 com peças brancas
% '-' - posições não jogáveis
% * - onde posso jogar
% '1' - dama preta
% '2' - dama branca


tabuleiro(	
	[[-,p,-,p,-,p,-,p],
	 [p,-,p,-,p,-,p,-],
	 [-,p,-,p,-,p,-,p],
	 [*,-,*,-,*,-,*,-],
	 [-,*,-,*,-,*,-,*],
	 [b,-,b,-,b,-,b,-],
	 [-,b,-,b,-,b,-,b],
	 [b,-,b,-,b,-,b,-]]).


tabuleiro_Pontos(   
	[[-,4,-,4,-,4,-,4],
	 [4,-,3,-,3,-,3,-],
	 [-,3,-,2,-,2,-,4],
	 [4,-,2,-,1,-,3,-],
	 [-,3,-,1,-,2,-,4],
	 [4,-,2,-,2,-,3,-],
	 [-,3,-,3,-,3,-,4],
	 [4,-,4,-,4,-,4,-]]).
                       
                        
udtq([1,2,3,4,5,6,7,8]).










%%%%%%%%%%%%%%%%%%%%%%  PREDICADOS CENTRAIS DO JOGO  %%%%%%%%%%%%%%%%%%%%%%%%%%

inicio:-
	tabuleiro(Tab), nl,
	write('                JOGO DAS DAMAS'), nl,nl,nl,
	write('Regras do jogo'),nl,nl,nl,
	write('As Regras são.....'),nl,nl,nl,
	write('                  Selecione o modo de jogo'),nl,nl,
	write('1 - Jogador Vs Jogador           2 - Computador Vs Jogador'),nl, nl,nl,
	assert(modo(1)),
	assert(modo(2)),
	ler_modo(Modo),
	nl,nl,nl,
	define_jogador(Tab,Modo,1).

%passa a jogada para o jogador certo
define_jogador(Tab,2,_):-
    verifica_vencedor(Tab,1),
    imprime(Tab),
    write("Ganhou o computador!!!!"),!.

define_jogador(Tab,1,_):-
    verifica_vencedor(Tab,1),
    imprime(Tab),    
    write("Ganhou o Jogador2!!!!"),!.

define_jogador(Tab,_,_):-
    verifica_vencedor(Tab,2),
    imprime(Tab),    
    write("Ganhou o jogador1!!!!"),!.

define_jogador(Tab,1,1):-
	vez_Jogador1(Tab,1),!.

define_jogador(Tab,1,2):-
	vez_Jogador2(Tab,1),!.

define_jogador(Tab,2,1):-
	vez_Jogador1(Tab,2),!.

define_jogador(Tab,2,2):-
	vez_computador(Tab,2),!.



vez_Jogador1(Tab,Modo):-
	write("                   É a vez do jogador 1"),nl,nl,
	imprime(Tab),nl,
	info_nr_pecas(Tab),
	construir_array_P(Tab,1,Tab, X),
	writeln('Que peça queres jogar?'),
	ver_Primeiro(X),nl,
	write('[Linha, coluna] '), nl,
	ler_peca(Input),   	                           
	ver_opcoes_P(Input,X,1, InputPeca),
	ver_tipo_peca(Tab,Input,1,TP),
	mudarTabuleiro(Tab, Input, InputPeca, TP, TabNovo),
	retractall(pecas(_)),
	retractall(jogaveis(_)),
	nl,nl,nl,nl,nl,
	define_jogador(TabNovo, Modo, 2).


vez_Jogador2(Tab, Modo):-
	write("                   É a vez do jogador 2"),nl,nl,
	imprime(Tab),
	info_nr_pecas(Tab),
	construir_array_B(Tab,1,Tab, X),
	writeln('Que peça queres jogar?'),
	ver_Primeiro(X),nl,
	write('[Linha, coluna]'), nl,
	ler_peca(Input),
	ver_opcoes_B(Input,X,8, InputPeca),
	ver_tipo_peca(Tab,Input,1,TP),	
	mudarTabuleiro(Tab, Input, InputPeca,TP, TabNovo),
	retractall(pecas(_)),
	retractall(jogaveis(_)),
	nl,nl,nl,nl,nl,
	define_jogador(TabNovo, Modo, 1).



%executa a jogada do computador
vez_computador(Tab,Modo):-
	dificuldade(X),
	alphaBeta([2,play,Tab], -200, 200, X, [_,_,Tab2], _),
	define_jogador(Tab2, Modo, 1).











%%%%%%%%%%%%%%%%%%%%%%  PREDICADOS IMPORTANTES AO LONGO DO JOGO %%%%%%%%%%%%%%%%%%%%%%%%%%
%imprime o tabuleiro do jogo
imprime(K):-
	write('   1 2 3 4 5 6 7 8'),
	nl,
	udtq(Y),
	imprime_linhas(K,Y).

imprime_linhas([],[]):- !.
imprime_linhas([X|L1], [Y|L2]):-
	write(Y),
	write(' '),
	write(X), nl,
	imprime_linhas(L1,L2).

%le a peça que o jogador quer jogar
ler_peca(Input):-
	read(Input),
	pecas(Input), !.
ler_peca(Input):-
	writeln("Peça inválida!"),
	ler_peca(Input).

%le o modo do jogo, se é jogador contra jogador ou jogador contra o computador
ler_modo(Modo):-
	read(Modo),
	tipo_modo(Modo),
	!.

ler_modo(Modo):-
	write('Modo de jogo inválido!'), nl,
	ler_modo(Modo).

ler_modo(Modo):-
	read(Modo),
	modo(Modo),
	!.

tipo_modo(Modo):-
	modo(Modo),
	Modo == 2,
	define_dificuldade,
	!.

tipo_modo(Modo):-
	modo(Modo), !.



%da a escolher ao jogador a dificuldade do jogo
%isto sera a profundidade do alpha-beta
define_dificuldade:-
	write("Selecione a dificuldade do computador"), nl,
	write("1 - Fácil            2 - Médio            3 - Difícil"), nl,
	read(X),
	ler_dificuldade(X),
	nl, nl, nl, nl, nl.

ler_dificuldade(1):-
	assert(dificuldade(2)),
	!.
ler_dificuldade(2):-
	assert(dificuldade(4)),
	!.
ler_dificuldade(3):-
	assert(dificuldade(7)),
	!.
ler_dificuldade(_):-
	write("opção invalida"), nl,nl,nl,nl,
	define_dificuldade.	


%ver_tipo_peca(Tabuleiro, Peça de Input,linha atual, Tipo de peça em determinada posiçao)
ver_tipo_peca([],_,_,_):-!.
ver_tipo_peca([Z|_], [X,Y], L, TP):-
	L==X,
	ver_tipo_peca2(Z,Y,1,TP), ! .

ver_tipo_peca([_|Zs], Peca, L, TP):-
	L1 is L +1,
	ver_tipo_peca(Zs,Peca,L1,TP).


%ver_tipo_peca2(Linha do Tabuleiro, Coluna do Input,coluna atual, Tipo de peça em determinada posiçao)
ver_tipo_peca2([],_,_,_):-!.
ver_tipo_peca2([Z|_], Y, C, Z):-
	C==Y.

ver_tipo_peca2([_|Zs], Y, C, TP):-
	C1 is C+1,
	ver_tipo_peca2(Zs,Y,C1,TP).



info_nr_pecas(Tab):-
	contador_tipo_peca(Tab,p,0,T),
	contador_tipo_peca(Tab,1,0,T1),
	contador_tipo_peca(Tab,b,0,T2),
	contador_tipo_peca(Tab,2,0,T3),

	write('Nr de Peças Pretas: '),
	writeln(T),
	write('Nr de Damas Pretas: '),
	writeln(T1),nl,
	write('Nr de Peças Brancas: '),
	writeln(T2),
	write('Nr de Damas Brancas: '),
	writeln(T3),nl.


%conta o numero de peças de determinado tipo
%contador_tipo_peca(Tabuleiro, tipo da peça a contar,contador atual, nr de peças final)
contador_tipo_peca([],_,Atual,Atual):-!.
contador_tipo_peca([Z|Zs],TP,Atual,N):-
	contador_tipo_peca2(Z,TP,0,N2),
	Atual1 is N2+Atual,
	contador_tipo_peca(Zs,TP,Atual1,N).

%contador_tipo_peca2(linha do tabuleiro, tipo da peça a contar, nr de peças)
contador_tipo_peca2([],_,Atual,Atual):-!.
contador_tipo_peca2([Z|Zs],TP,Atual,N):-
	Z==TP,
	Atual1 is Atual+1,
	contador_tipo_peca2(Zs,TP,Atual1,N),!.

contador_tipo_peca2([_|Zs],TP,Atual,N):-
	contador_tipo_peca2(Zs,TP,Atual,N).


%verifica quem ganha
verifica_vencedor(Tab, 1):-
	contador_tipo_peca(Tab,p,0,0),
	contador_tipo_peca(Tab,1,0,0),!.

verifica_vencedor(Tab, 2):-
	contador_tipo_peca(Tab,b,0,0),
	contador_tipo_peca(Tab,2,0,0),!.


%%%%%%%%%%%%%%%%%%%%%%  CONSTRUÇÃO DA LISTA DE JOGO %%%%%%%%%%%%%%%%%%%%%%%%%%

%% -------------- PRETAS ---  P ----------------> 


%Esta função vai buscar cada uma das linhas do tabuleiro e manda-a para a função que avalia peça a peça
%Começa na linha 1
%construir_array_P(tabuleiro, nr da linha do tabuleiro, tabuleiro fixo, saída).
%constroi a lista das peças que se podem mexer e para onde
construir_array_P([], _, _,[]):- !.

construir_array_P([X|L1],Y, Tab, [L|T]):-
	procurar_Pecas_P(X,Y,1, Tab,L),
	Y1 is Y+1,
	construir_array_P(L1,Y1,Tab, T).


%vamos fazer varias maneiras para quando apenas tera uma opção e duas opçoes
%procurar_Pecas_P(Listadelinha, nrdaLinha,nrdacoluna, tabuleiro, saída do Lista de jogo )
procurar_Pecas_P([],_,_,_,[]).

procurar_Pecas_P([p|L1],Nl, Nc, Tab, [[[Nl,Nc]|T1]|T]):-     %t1 sao as opçoes e T é o resto da lista
	N1 is Nc+1,
	Nc \== 1,
	Nc \== 8,
	builtT1_P(Nl,Nc,Tab,T1),
	procurar_Pecas_P(L1,Nl,N1, Tab,T), !.

procurar_Pecas_P([p|L1],Nl, Nc, Tab, [[[Nl,Nc],T1]|T]):- 
	N1 is Nc+1,
	builtT1_P(Nl,Nc,Tab,T1),
	procurar_Pecas_P(L1,Nl,N1, Tab,T), !.


procurar_Pecas_P([1|L1],Nl, Nc, Tab, [[[Nl,Nc]|T1]|T]):- 
	N1 is Nc+1,
	Nc \== 1,
	Nc \== 8,
	builtT1_Cima_P(Nl,Nc,Tab, T2),
	builtT1_P(Nl,Nc,Tab,T0),
	append(T2,T0,T1), 
	procurar_Pecas_P(L1,Nl,N1, Tab,T), !.

procurar_Pecas_P([1|L1],Nl, Nc, Tab, [[[Nl,Nc]|T1]|T]):- 
	N1 is Nc+1,
	builtT1_Cima_P(Nl,Nc,Tab, T2),
	builtT1_P(Nl,Nc,Tab,T0),
	T1 = [T2, T0],
	procurar_Pecas_P(L1,Nl,N1, Tab,T), !.

procurar_Pecas_P([1|L1],Nl, Nc, Tab, [[[Nl,Nc]|T1]|T]):- 
	N1 is Nc+1,
	Nc \== 1,
	Nc \== 8,
	builtT1_Cima_P(Nl,Nc,Tab, T1),
	procurar_Pecas_P(L1,Nl,N1, Tab,T), !.


procurar_Pecas_P([1|L1],Nl, Nc, Tab, [[[Nl,Nc],T1]|T]):- 
	N1 is Nc+1,
	builtT1_Cima_P(Nl,Nc,Tab, T1),
	procurar_Pecas_P(L1,Nl,N1, Tab,T), !.

%só para baixo
procurar_Pecas_P([1|L1],Nl, Nc, Tab, [[[Nl,Nc]|T1]|T]):- 
	N1 is Nc+1,
	Nc \== 1,
	Nc \== 8,
	builtT1_P(Nl,Nc,Tab, T1),
	procurar_Pecas_P(L1,Nl,N1, Tab,T), !.

%só para baixo
procurar_Pecas_P([1|L1],Nl, Nc, Tab, [[[Nl,Nc],T1]|T]):- 
	N1 is Nc+1,
	builtT1_P(Nl,Nc,Tab, T1),
	procurar_Pecas_P(L1,Nl,N1, Tab,T), !.


procurar_Pecas_P([_|L1],Nl, Nc, Tab, T):-
	N1 is Nc+1,
	procurar_Pecas_P(L1,Nl,N1, Tab,T).







%builtT1_P(nr da linha, nr da coluna, tabuleiro, saída que são as posiçoẽs jogaveis)
%Esta função verifica quais as jogadas possiveis para cima.
%builtT1_Cima_P(1,_,_,[]):-!.

builtT1_Cima_P(L,1,Tab,T):-
	L1 is L-1,
	T = [L1,2],
	procurarT(Tab,L1,2),
	writeln("este caso" + L),
	!.


%vou ver se dá para comer
builtT1_Cima_P(L,1,Tab,T):-
	L1 is L-1,
	procurarT_Cima_Dir_P(Tab,L1,2, Tab, L1, 2),
	L2 is L1 -1,
	T = [L2,3],
	!.


builtT1_Cima_P(L,8,Tab,T):-
	L1 is L-1,
	T = [L1,7],
	procurarT(Tab,L1,7),!.

%vou ver se dá para comer
builtT1_Cima_P(L,8,Tab,T):-
	L1 is L-1,
	procurarT_Cima_Esq_P(Tab,L1,7, Tab, L1, 7),
	L2 is L1-1,
	T = [L2,6],
	!.


builtT1_Cima_P(Y, N, Tab, T):-
	aux_builtT1_Cima_Esq_P(Y, N, Tab, T1),
	aux_builtT2_Cima_Dir_P(Y,N, Tab, T2),
	T = [T1,T2],!.

builtT1_Cima_P(Y, N, Tab, T):-
	aux_builtT1_Cima_Esq_P(Y, N, Tab, T1),
	T = [T1], !.

builtT1_Cima_P(Y, N, Tab, T):-
	aux_builtT2_Cima_Dir_P(Y, N, Tab, T2),
	T = [T2], !.


%aux_builtT1_cima_esq_P (nr da linha, nr da coluna, Tabuleiro, lista de saída)
aux_builtT1_Cima_Esq_P(Y,N, Tab, T):-
	Y1 is Y-1,
	N2 is N-1,
	T = [Y1,N2],
	procurarT(Tab, Y1, N2),!.

%vai ver se dá para comer
aux_builtT1_Cima_Esq_P(Y,N, Tab, T):-
	Y1 is Y-1,
	N2 is N-1,
	procurarT_Cima_Esq_P(Tab, Y1, N2, Tab, Y1, N2),
	Y2 is Y1 -1,
	N3 is N2 -1,
	T = [Y2,N3], !.




%aux_builtT2_cima_Dir_P (nr da linha, nr da coluna, Tabuleiro, lista de saída)
aux_builtT2_Cima_Dir_P(Y,N, Tab, T):-
	Y1 is Y-1,
	N1 is N+1,
	T = [Y1,N1],
	procurarT(Tab, Y1, N1),!.

%vai ver se dá para comer
aux_builtT2_Cima_Dir_P(Y,N, Tab, T):-
	Y1 is Y-1,
	N2 is N+1,
	procurarT_Cima_Dir_P(Tab, Y1, N2, Tab, Y1, N2),
	Y2 is Y1 -1,
	N3 is N2 +1,
	T = [Y2,N3], !.






%builtT1_P(nr da linha, nr da coluna, tabuleiro, saída que são as posiçoẽs jogaveis)
%builtT1_P(8,_, _,[]):-!.
builtT1_P(L,1, Tab, T):-
	L1 is L+1,
	T = [L1,2],
	procurarT(Tab, L1, 2),
	!.
%esta parte vai ver se dá para comer a peça
builtT1_P(L,1, Tab, T):-
	L1 is L+1,
	procurarT_Baixo_Dir_P(Tab, L1, 2, Tab, L1, 2),           %vai verificar se a pessoa é inimiga e se a seguir tem espaço livre (tableiro, linha da peça, coluna da peça, tabuleiro, linha da peça, coluna da peça)
	L2 is L1 +1,
	T = [L2,3],
	!.

builtT1_P(L,8, Tab, T):-
	L1 is L+1,
	T = [L1,7],
	procurarT(Tab, L1, 7),
	!.

builtT1_P(L,8, Tab, T):-
	L1 is L+1,
	procurarT_Baixo_Esq_P(Tab, L1, 7, Tab, L1, 7),
	L2 is L1 +1,
	T = [L2,6],
	!.


builtT1_P(Y, N, Tab, T):-
	aux_built_Baixo_Esq_P(Y, N, Tab, T1),
	aux_built_Baixo_Dir_P(Y,N, Tab, T2),

	T = [T1,T2], !.

builtT1_P(Y, N, Tab, T):-
	aux_built_Baixo_Esq_P(Y, N, Tab, T1),
	T = [T1], !.

builtT1_P(Y, N, Tab, T):-
	aux_built_Baixo_Dir_P(Y, N, Tab, T2),
	T = [T2], !.



%aux_built_Baixo_Esq_P (nr da linha, nr da coluna, Tabuleiro, lista de saída)
aux_built_Baixo_Esq_P(Y,N, Tab, T):-
	Y1 is Y+1,
	N2 is N-1,
	T = [Y1,N2],
	procurarT(Tab, Y1, N2), !.

aux_built_Baixo_Esq_P(Y,N, Tab, T):-
	Y1 is Y+1,
	N2 is N-1,
	procurarT_Baixo_Esq_P(Tab, Y1, N2, Tab, Y1, N2),
	Y2 is Y1 +1,
	N3 is N2 -1,
	T = [Y2,N3], !.


%aux_built_Baixo_Dir_P(nr da linha, nr da coluna, Tabuleiro, lista de saída)
aux_built_Baixo_Dir_P(Y,N, Tab, T):-
	Y1 is Y+1,
	N1 is N+1,
	T = [Y1,N1],
	procurarT(Tab, Y1, N1), !.

aux_built_Baixo_Dir_P(Y,N, Tab, T):-
	Y1 is Y+1,
	N2 is N+1,
	procurarT_Baixo_Dir_P(Tab, Y1, N2, Tab, Y1, N2),
	Y2 is Y1 +1,
	N3 is N2 +1,
	T = [Y2,N3], !.



%encontrou a linha da peça que quer comer, vai ver as colunas , para poder comer para o lado direito e em cima
%procurarT_Baixo_Dir_P(tabuleiro, numero da linha a ver, numero da coluna a ver, tabuleiro, linha a ver, coluna a ver)
procurarT_Cima_Dir_P([Z|_], 1, Y, Tab, Linha, Coluna):-
	procurarTColunas_Cima_Dir_P(Z, Y, Tab, Linha, Coluna), !.

procurarT_Cima_Dir_P([_|Zs], X, Y, Tab, Linha, Coluna):-
	X1 is X-1,
	procurarT_Cima_Dir_P(Zs, X1, Y, Tab, Linha, Coluna).

%ja temos a linha, agora vamos ver se na coluna certa é uma peça inimiga para poder comer para o lado direito e em cima
%procurarTColunas_Cima_Dir_P(linha das colunas, posicao da coluna a ver, tabuleiro, linha da peça, coluna da peça)
procurarTColunas_Cima_Dir_P([Z|_], 1, Tab, Linha, Coluna):-
	(Z == 'b' ; Z == 2 ), 
	L1 is Linha -1,
	C1 is Coluna +1,
	procurarT(Tab, L1, C1), !.  %encontramos uma peça inimiga, vamos verificar se a peça a seguir esta livre

procurarTColunas_Cima_Dir_P([_|Zs], Y, Tab, Linha, Coluna):-
	Y1 is Y-1,
	procurarTColunas_Baixo_Dir_P(Zs, Y1, Tab, Linha, Coluna).



procurarT_Cima_Esq_P([Z|_], 1, Y, Tab, Linha, Coluna):-
	procurarTColunas_Cima_Esq_P(Z, Y, Tab, Linha, Coluna), !.

procurarT_Cima_Esq_P([_|Zs], X, Y, Tab, Linha, Coluna):-
	X1 is X-1,
	procurarT_Cima_Esq_P(Zs, X1, Y, Tab, Linha, Coluna).


%ja temos a linha, agora vamos ver se na coluna certa é uma peça inimiga para poder comer para o lado esquerdo e em cima
%procurarTColunas_Cima_Esq_P(linha das colunas, posicao da coluna a ver, tabuleiro, linha da peça, coluna da peça)
procurarTColunas_Cima_Esq_P([Z|_], 1, Tab, Linha, Coluna):-
	(Z == 'b' ; Z == 2 ), 
	L1 is Linha -1,
	C1 is Coluna -1,
	procurarT(Tab, L1, C1), !.  %encontramos uma peça inimiga, vamos verificar se a peça a seguir esta livre

procurarTColunas_Cima_Esq_P([_|Zs], Y, Tab, Linha, Coluna):-
	Y1 is Y-1,
	procurarTColunas_Cima_Esq_P(Zs, Y1, Tab, Linha, Coluna).




%encontrou a linha da peça que quer comer, vai ver as colunas , para poder comer para o lado direito e em baixo
%procurarT_Baixo_Dir_P(tabuleiro, numero da linha a ver, numero da coluna a ver, tabuleiro, linha a ver, coluna a ver)
procurarT_Baixo_Dir_P([Z|_], 1, Y, Tab, Linha, Coluna):-
	procurarTColunas_Baixo_Dir_P(Z, Y, Tab, Linha, Coluna), !.

procurarT_Baixo_Dir_P([_|Zs], X, Y, Tab, Linha, Coluna):-
	X1 is X-1,
	procurarT_Baixo_Dir_P(Zs, X1, Y, Tab, Linha, Coluna).

%ja temos a linha, agora vamos ver se na coluna certa é uma peça inimiga para poder comer para o lado direito e em baixo
%procurarTColunas_Baixo_Dir_P(linha das colunas, posicao da coluna a ver, tabuleiro, linha da peça, coluna da peça)
procurarTColunas_Baixo_Dir_P([Z|_], 1, Tab, Linha, Coluna):-
	(Z == 'b' ; Z == 2 ), 
	L1 is Linha +1,
	C1 is Coluna +1,
	procurarT(Tab, L1, C1), !.  %encontramos uma peça inimiga, vamos verificar se a peça a seguir esta livre

procurarTColunas_Baixo_Dir_P([_|Zs], Y, Tab, Linha, Coluna):-
	Y1 is Y-1,
	procurarTColunas_Baixo_Dir_P(Zs, Y1, Tab, Linha, Coluna).




%encontrou a linha da peça que quer comer, vai ver as colunas , para poder comer para o lado esquerdo e em baixo
%procurarT_Baixo_Esq_P(tabuleiro, numero da linha a ver, numero da coluna a ver, tabuleiro, linha a ver, coluna a ver)
procurarT_Baixo_Esq_P([Z|_], 1, Y, Tab, Linha, Coluna):-
	procurarTColunas_Baixo_Esq_P(Z, Y, Tab, Linha, Coluna), !.

procurarT_Baixo_Esq_P([_|Zs], X, Y, Tab, Linha, Coluna):-
	X1 is X-1,
	procurarT_Baixo_Esq_P(Zs, X1, Y, Tab, Linha, Coluna).



%ja temos a linha, agora vamos ver se na coluna certa é uma peça inimiga para poder comer para o lado esquerdo e em baixo
%procurarTColunas_Baixo_Esq_P(linha das colunas, posicao da coluna a ver, tabuleiro, linha da peça, coluna da peça)
procurarTColunas_Baixo_Esq_P([Z|_], 1, Tab, Linha, Coluna):-
	(Z == 'b' ; Z == 2 ), 
	L1 is Linha +1,
	C1 is Coluna -1,
	procurarT(Tab, L1, C1), !.

procurarTColunas_Baixo_Esq_P([_|Zs], Y, Tab, Linha, Coluna):-
	Y1 is Y-1,
	procurarTColunas_Baixo_Esq_P(Zs, Y1, Tab, Linha, Coluna).

%------------------------------------------------------------------------------------------------------------------------------------------







%%%%ESTA FUNÇÃO É COMUM TANTO A PEÇAS BRANCAS COMO A PEÇAS PRETAS

%vai procurar a linha que se quer
%procurarT_P(Tabuleiro, nr da linha, nr da coluna)
procurarT([Z|_], 1, Y):-
	procurarTColunas(Z, Y), !.

procurarT([_|Zs], X, Y):-
	X1 is X-1,
	procurarT(Zs, X1, Y).


%vai procurar a coluna que se quer e verifica se esta livre
%procurarT_PColuna(Tabuleiro, nr da coluna)
procurarTColunas([Z|_], 1):-
	Z =='*', !.

procurarTColunas([_|Zs], Y):-
	Y1 is Y-1,
	procurarTColunas(Zs, Y1).








%% -------------- BRANCAS ---  B ----------------> 
%construir_array_B(tabuleiro, nr da linha do tabuleiro, tabuleiro fixo, saída).
%constroi a lista das peças que se podem mexer e para onde
construir_array_B(Tab,1,Tab, X):-
	reverse(Tab,Tab2),
	construir_array_B1(Tab2,1,Tab2,X).

construir_array_B1([], _, _,[]):- !.

construir_array_B1([X|L1],Y, Tab, [L|T]):-
	procurar_Pecas_B(X,Y,1, Tab,L),
	Y1 is Y+1,
	construir_array_B1(L1,Y1,Tab, T).


%vamos fazer varias maneiras para quando apenas tera uma opção e duas opçoes
%procurar_Pecas_P(Listadelinha, nrdaLinha,nrdacoluna, tabuleiro, saída do Lista de jogo )
procurar_Pecas_B([],_,_,_,[]).

procurar_Pecas_B([b|L1],Nl, Nc, Tab, [[[Nl1,Nc]|T1]|T]):-     %t1 sao as opçoes e T é o resto da lista
	N1 is Nc+1,
	Nc \== 1,
	Nc \== 8,
	Nl1 is 9 - Nl,
	builtT1_B(Nl,Nc,Tab,T1),
	procurar_Pecas_B(L1,Nl,N1, Tab,T), !.

procurar_Pecas_B([b|L1],Nl, Nc, Tab, [[[Nl1,Nc],T1]|T]):- 
	N1 is Nc+1,
	Nl1 is 9 - Nl,
	builtT1_B(Nl,Nc,Tab,T1),
	procurar_Pecas_B(L1,Nl,N1, Tab,T), !.


procurar_Pecas_B([2|L1],Nl, Nc, Tab, [[[Nl1,Nc]|T1]|T]):- 
	N1 is Nc+1,
	Nc \== 1,
	Nc \== 8,
	Nl1 is 9 - Nl,
	builtT1_Cima_B(Nl,Nc,Tab, T2),
	builtT1_B(Nl,Nc,Tab,T0),
	append(T2,T0,T1), 
	procurar_Pecas_B(L1,Nl,N1, Tab,T), !.

procurar_Pecas_B([2|L1],Nl, Nc, Tab, [[[Nl1,Nc]|T1]|T]):- 
	N1 is Nc+1,
	Nl1 is 9 - Nl,
	builtT1_Cima_B(Nl,Nc,Tab, T2),
	builtT1_B(Nl,Nc,Tab,T0),
	T1 = [T2, T0],
	%append(T21,T01,T1), 
	procurar_Pecas_B(L1,Nl,N1, Tab,T), !.

procurar_Pecas_B([2|L1],Nl, Nc, Tab, [[[Nl1,Nc]|T1]|T]):- 
	N1 is Nc+1,
	Nl1 is 9 - Nl,
	Nc \== 1,
	Nc \== 8,
	builtT1_Cima_B(Nl,Nc,Tab, T1),
	procurar_Pecas_B(L1,Nl,N1, Tab,T), !.


procurar_Pecas_B([2|L1],Nl, Nc, Tab, [[[Nl1,Nc],T1]|T]):- 
	N1 is Nc+1,
	Nl1 is 9 - Nl,
	builtT1_Cima_B(Nl,Nc,Tab, T1),
	procurar_Pecas_B(L1,Nl,N1, Tab,T), !.

%só para baixo
procurar_Pecas_B([2|L1],Nl, Nc, Tab, [[[Nl1,Nc]|T1]|T]):- 
	N1 is Nc+1,
	Nl1 is 9 - Nl,
	Nc \== 1,
	Nc \== 8,
	builtT1_B(Nl,Nc,Tab, T1),
	procurar_Pecas_B(L1,Nl,N1, Tab,T), !.

%só para baixo
procurar_Pecas_B([2|L1],Nl, Nc, Tab, [[[Nl1,Nc],T1]|T]):- 
	N1 is Nc+1,
	Nl1 is 9 - Nl,
	builtT1_B(Nl,Nc,Tab, T1),
	procurar_Pecas_B(L1,Nl,N1, Tab,T), !.


procurar_Pecas_B([_|L1],Nl, Nc, Tab, T):-
	N1 is Nc+1,
	procurar_Pecas_B(L1,Nl,N1, Tab,T).







%builtT1_B(nr da linha, nr da coluna, tabuleiro, saída que são as posiçoẽs jogaveis)
%Esta função verifica quais as jogadas possiveis para cima.
%builtT1_Cima_B(1,_,_,[]):-!.
builtT1_Cima_B(L,1,Tab,T):-
	L1 is L-1,
	Nl1 is 9 - L1,
	T = [Nl1,2],
	procurarT(Tab,L1,2),
	!.


%vou ver se dá para comer
builtT1_Cima_B(L,1,Tab,T):-
	L1 is L-1,
	procurarT_Cima_Dir_B(Tab,L1,2, Tab, L1, 2),
	L2 is L1 -1,
	Nl1 is 9 - L2,
	T = [Nl1,3],
	!.


builtT1_Cima_B(L,8,Tab,T):-
	L1 is L-1,
	Nl1 is 9 - L1,
	T = [Nl1,7],
	procurarT(Tab,L1,7),!.

%vou ver se dá para comer
builtT1_Cima_B(L,8,Tab,T):-
	L1 is L-1,
	procurarT_Cima_Esq_B(Tab,L1,7, Tab, L1, 7),
	L2 is L1-1,
	Nl1 is 9 - L2,
	T = [Nl1,6],
	!.


builtT1_Cima_B(Y, N, Tab, T):-
	aux_builtT1_Cima_Esq_B(Y, N, Tab, T1),
	aux_builtT2_Cima_Dir_B(Y,N, Tab, T2),
	T = [T1,T2],!.

builtT1_Cima_B(Y, N, Tab, T):-
	aux_builtT1_Cima_Esq_B(Y, N, Tab, T1),
	T = [T1], !.

builtT1_Cima_B(Y, N, Tab, T):-
	aux_builtT2_Cima_Dir_B(Y, N, Tab, T2),
	T = [T2], !.


%aux_builtT1_cima_Esq_B (nr da linha, nr da coluna, Tabuleiro, lista de saída)
aux_builtT1_Cima_Esq_B(Y,N, Tab, T):-
	Y1 is Y-1,
	N2 is N-1,
	Nl1 is 9 - Y1,
	T = [Nl1,N2],
	procurarT(Tab, Y1, N2),!.

%vai ver se dá para comer
aux_builtT1_Cima_Esq_B(Y,N, Tab, T):-
	Y1 is Y-1,
	N2 is N-1,
	procurarT_Cima_Esq_B(Tab, Y1, N2, Tab, Y1, N2),
	Y2 is Y1 -1,
	N3 is N2 -1,
	Nl1 is 9 - Y2,
	T = [Nl1,N3], !.




%aux_builtT2_cima_Dir_B (nr da linha, nr da coluna, Tabuleiro, lista de saída)
aux_builtT2_Cima_Dir_B(Y,N, Tab, T):-
	Y1 is Y-1,
	N1 is N+1,
	Nl1 is 9 - Y1,
	T = [Nl1,N1],
	procurarT(Tab, Y1, N1),!.

%vai ver se dá para comer
aux_builtT2_Cima_Dir_B(Y,N, Tab, T):-
	Y1 is Y-1,
	N2 is N+1,
	procurarT_Cima_Dir_B(Tab, Y1, N2, Tab, Y1, N2),
	Y2 is Y1 -1,
	N3 is N2 +1,
	Nl1 is 9 - Y2,
	T = [Nl1,N3], !.






%builtT1_B(nr da linha, nr da coluna, tabuleiro, saída que são as posiçoẽs jogaveis)
%builtT1_B(8,_, _,[]):-!.
builtT1_B(L,1, Tab, T):-
	L1 is L+1,
	Nl1 is 9 - L1,
	T = [Nl1,2],
	procurarT(Tab, L1, 2),
	!.
%esta parte vai ver se dá para comer a peça
builtT1_B(L,1, Tab, T):-
	L1 is L+1,
	procurarT_Baixo_Dir_B(Tab, L1, 2, Tab, L1, 2),           %vai verificar se a pessoa é inimiga e se a seguir tem espaço livre (tableiro, linha da peça, coluna da peça, tabuleiro, linha da peça, coluna da peça)
	L2 is L1 +1,
	Nl1 is 9 - L2,
	T = [Nl1,3],
	!.

builtT1_B(L,8, Tab, T):-
	L1 is L+1,
	Nl1 is 9 - L1,
	T = [Nl1,7],
	procurarT(Tab, L1, 7),
	!.

builtT1_B(L,8, Tab, T):-
	L1 is L+1,
	procurarT_Baixo_Esq_B(Tab, L1, 7, Tab, L1, 7),
	L2 is L1 +1,
	Nl1 is 9 - L2,
	T = [Nl1,6],
	!.


builtT1_B(Y, N, Tab, T):-
	aux_built_Baixo_Esq_B(Y, N, Tab, T1),
	aux_built_Baixo_Dir_B(Y,N, Tab, T2),

	T = [T1,T2], !.

builtT1_B(Y, N, Tab, T):-
	aux_built_Baixo_Esq_B(Y, N, Tab, T1),
	T = [T1], !.

builtT1_B(Y, N, Tab, T):-
	aux_built_Baixo_Dir_B(Y, N, Tab, T2),
	T = [T2], !.



%aux_built_Baixo_Esq_B (nr da linha, nr da coluna, Tabuleiro, lista de saída)
aux_built_Baixo_Esq_B(Y,N, Tab, T):-
	Y1 is Y+1,
	Nl1 is 9 - Y1,
	N2 is N-1,
	T = [Nl1,N2],
	procurarT(Tab, Y1, N2), !.

aux_built_Baixo_Esq_B(Y,N, Tab, T):-
	Y1 is Y+1,
	N2 is N-1,
	procurarT_Baixo_Esq_B(Tab, Y1, N2, Tab, Y1, N2),
	Y2 is Y1 +1,
	N3 is N2 -1,
	Nl1 is 9 - Y2,
	T = [Nl1,N3], !.


%aux_built_Baixo_Dir_B(nr da linha, nr da coluna, Tabuleiro, lista de saída)
aux_built_Baixo_Dir_B(Y,N, Tab, T):-
	Y1 is Y+1,
	N1 is N+1,
	Nl1 is 9 - Y1,
	T = [Nl1,N1],
	procurarT(Tab, Y1, N1), !.

aux_built_Baixo_Dir_B(Y,N, Tab, T):-
	Y1 is Y+1,
	N2 is N+1,
	procurarT_Baixo_Dir_B(Tab, Y1, N2, Tab, Y1, N2),
	Y2 is Y1 +1,
	N3 is N2 +1,
	Nl1 is 9 - Y2,
	T = [Nl1,N3], !.



%encontrou a linha da peça que quer comer, vai ver as colunas , para poder comer para o lado direito e em cima
%procurarT_Baixo_Dir_B(tabuleiro, numero da linha a ver, numero da coluna a ver, tabuleiro, linha a ver, coluna a ver)
procurarT_Cima_Dir_B([Z|_], 1, Y, Tab, Linha, Coluna):-
	procurarTColunas_Cima_Dir_B(Z, Y, Tab, Linha, Coluna), !.

procurarT_Cima_Dir_B([_|Zs], X, Y, Tab, Linha, Coluna):-
	X1 is X-1,
	procurarT_Cima_Dir_B(Zs, X1, Y, Tab, Linha, Coluna).

%ja temos a linha, agora vamos ver se na coluna certa é uma peça inimiga para poder comer para o lado direito e em cima
%procurarTColunas_Cima_Dir_B(linha das colunas, posicao da coluna a ver, tabuleiro, linha da peça, coluna da peça)
procurarTColunas_Cima_Dir_B([Z|_], 1, Tab, Linha, Coluna):-
	(Z == 'p' ; Z == 1 ), 
	L1 is Linha -1,
	C1 is Coluna +1,
	procurarT(Tab, L1, C1), !.  %encontramos uma peça inimiga, vamos verificar se a peça a seguir esta livre

procurarTColunas_Cima_Dir_B([_|Zs], Y, Tab, Linha, Coluna):-
	Y1 is Y-1,
	procurarTColunas_Baixo_Dir_B(Zs, Y1, Tab, Linha, Coluna).



procurarT_Cima_Esq_B([Z|_], 1, Y, Tab, Linha, Coluna):-
	procurarTColunas_Cima_Esq_B(Z, Y, Tab, Linha, Coluna), !.

procurarT_Cima_Esq_B([_|Zs], X, Y, Tab, Linha, Coluna):-
	X1 is X-1,
	procurarT_Cima_Esq_B(Zs, X1, Y, Tab, Linha, Coluna).


%ja temos a linha, agora vamos ver se na coluna certa é uma peça inimiga para poder comer para o lado esquerdo e em cima
%procurarTColunas_Cima_Esq_B(linha das colunas, posicao da coluna a ver, tabuleiro, linha da peça, coluna da peça)
procurarTColunas_Cima_Esq_B([Z|_], 1, Tab, Linha, Coluna):-
	(Z == 'p' ; Z == 1 ), 
	L1 is Linha -1,
	C1 is Coluna -1,
	procurarT(Tab, L1, C1), !.  %encontramos uma peça inimiga, vamos verificar se a peça a seguir esta livre

procurarTColunas_Cima_Esq_B([_|Zs], Y, Tab, Linha, Coluna):-
	Y1 is Y-1,
	procurarTColunas_Cima_Esq_B(Zs, Y1, Tab, Linha, Coluna).




%encontrou a linha da peça que quer comer, vai ver as colunas , para poder comer para o lado direito e em baixo
%procurarT_Baixo_Dir_B(tabuleiro, numero da linha a ver, numero da coluna a ver, tabuleiro, linha a ver, coluna a ver)
procurarT_Baixo_Dir_B([Z|_], 1, Y, Tab, Linha, Coluna):-
	procurarTColunas_Baixo_Dir_B(Z, Y, Tab, Linha, Coluna), !.

procurarT_Baixo_Dir_B([_|Zs], X, Y, Tab, Linha, Coluna):-
	X1 is X-1,
	procurarT_Baixo_Dir_B(Zs, X1, Y, Tab, Linha, Coluna).

%ja temos a linha, agora vamos ver se na coluna certa é uma peça inimiga para poder comer para o lado direito e em baixo
%procurarTColunas_Baixo_Dir_B(linha das colunas, posicao da coluna a ver, tabuleiro, linha da peça, coluna da peça)
procurarTColunas_Baixo_Dir_B([Z|_], 1, Tab, Linha, Coluna):-
	(Z == 'p' ; Z == 1 ), 
	L1 is Linha +1,
	C1 is Coluna +1,
	procurarT(Tab, L1, C1), !.  %encontramos uma peça inimiga, vamos verificar se a peça a seguir esta livre

procurarTColunas_Baixo_Dir_B([_|Zs], Y, Tab, Linha, Coluna):-
	Y1 is Y-1,
	procurarTColunas_Baixo_Dir_B(Zs, Y1, Tab, Linha, Coluna).




%encontrou a linha da peça que quer comer, vai ver as colunas , para poder comer para o lado esquerdo e em baixo
%procurarT_Baixo_Esq_B(tabuleiro, numero da linha a ver, numero da coluna a ver, tabuleiro, linha a ver, coluna a ver)
procurarT_Baixo_Esq_B([Z|_], 1, Y, Tab, Linha, Coluna):-
	procurarTColunas_Baixo_Esq_B(Z, Y, Tab, Linha, Coluna), !.

procurarT_Baixo_Esq_B([_|Zs], X, Y, Tab, Linha, Coluna):-
	X1 is X-1,
	procurarT_Baixo_Esq_B(Zs, X1, Y, Tab, Linha, Coluna).



%ja temos a linha, agora vamos ver se na coluna certa é uma peça inimiga para poder comer para o lado esquerdo e em baixo
%procurarTColunas_Baixo_Esq_B(linha das colunas, posicao da coluna a ver, tabuleiro, linha da peça, coluna da peça)
procurarTColunas_Baixo_Esq_B([Z|_], 1, Tab, Linha, Coluna):-
	(Z == 'p' ; Z == 1 ), 
	L1 is Linha +1,
	C1 is Coluna -1,
	procurarT(Tab, L1, C1), !.

procurarTColunas_Baixo_Esq_B([_|Zs], Y, Tab, Linha, Coluna):-
	Y1 is Y-1,
	procurarTColunas_Baixo_Esq_B(Zs, Y1, Tab, Linha, Coluna).


%_________________________________________________________________________________________________________________________________________________












%%%%%%%%%%%%%%%%%%%%%%  INDICAÇÃO DE PEÇA A MUDAR %%%%%%%%%%%%%%%%%%%%%%%%%%

%ver_Primeiro(Lista de jogo)
ver_Primeiro([]):-!.

ver_Primeiro([Z|Zs]):-
	\+length(Z,0),
	ver_Primeiro2(Z),
	ver_Primeiro(Zs), !.

ver_Primeiro([_|Zs]):-
	ver_Primeiro(Zs).



%o que recebe é linha ----->[[caso 1], [caso 2], [caso3],...]
%ver_Primeiro2(recebe uma sublista do Lista de jogo que corresponde a uma linha)
ver_Primeiro2([]):-!.

ver_Primeiro2([Z|Zs]):-
	ver_Primeiro3(Z),
	ver_Primeiro2(Zs).



%compara os casos
%ver_Primeiro3(Recebe o Lista relativo a cada peça)
ver_Primeiro3([]):-!.
ver_Primeiro3([Z|_]):-
	writeln(Z),     %util
	assert(pecas(Z)).













%%%%%%%%%%%%%%%%%%%%%%  INDICAÇÃO DE PARA ONDE MUDAR A PEÇA ESCOLHIDA %%%%%%%%%%%%%%%%%%%%%%%%%%


%ver_opçoes(Peça que se quer jogar, Lista de jogo,contador de linha, Posiçao jogavel escolhida)
%ver_opcoes(_,[], _):-!.
%L de linha, C de coluna
ver_opcoes_P([X,Y],[_|Zs], L,InputPeca):-
	X \== L,
	L1 is L + 1,
	ver_opcoes_P([X,Y], Zs, L1,InputPeca), !.

ver_opcoes_P([X,Y], [Z|_], L, InputPeca):-
	X == L, 
	ver_opcoes2([X,Y], Z, InputPeca).



ver_opcoes_B([X,Y],[_|Zs], L,InputPeca):-
	X \== L,
	L1 is L - 1,
	ver_opcoes_B([X,Y], Zs, L1,InputPeca), !.

ver_opcoes_B([X,Y], [Z|_], L, InputPeca):-
	X == L, 
	ver_opcoes2([X,Y], Z, InputPeca).




%ver_opçoes2(Peça que se quer jogar, recebe uma sublista da Lista de jogo que corresponde a uma linha)
%ver_opcoes2(_,[]):-!.
ver_opcoes2(L,[Z|_],InputPeca):-
	ver_caso(L, Z),       %vai ser se a posiçãoque escolhemos é o priemiro caso
	ver_opcoes3(Z,InputPeca), !.   

ver_opcoes2(L, [_|Zs],InputPeca):-
	ver_opcoes2(L, Zs,InputPeca).


%nos casos temos sempre primeiro a cabeça e depois as opçoes, aqui vemos se a cabeça é igual ao que queremos
%Compara a peça que se quer jogar com a cabeça do caso que se recebe
%ver_caso(Peça que se quer jogar, o caso a tratar )
ver_caso(L, [Z|_]):-
	L == Z.


%nesta função sabemos estamos no caso certo, agora vemos se tem 1 ou 2 opçoes
%ver_opcoes3(Lista ue contem a peça e as opçoes)
ver_opcoes3([_| Zs],InputPeca):-                      %neste caso vemos que tem 1 opção
	length(Zs, 1),
	caso1_Opcao(Zs,InputPeca),!.

ver_opcoes3([_| Zs],InputPeca):-                %neste caso vemos que tem 2 opçoes,
	caso_mais_Opcoes(Zs,InputPeca),!.


%caso1_Opcao(array com 1 posição jogavel para determinada peça)
caso1_Opcao([Z|_],Z):-
	nl.


%nesta função vemos as 2 opçoes que temos, mostramos ao jogador e damos assert delas
%caso_mais_Opcoes(array de posiçoes jogaveis para determinada peça)
caso_mais_Opcoes([],InputPeca):-
	write('Escolha o par onde queres jogar: '), nl,
	ler_Input_Peca_jogaveis(InputPeca).

caso_mais_Opcoes([X|Xs],InputPeca):-
	assert(jogaveis(X)),
	write(X),
	nl,
	caso_mais_Opcoes(Xs,InputPeca).


%Le a peça escolhida pelo jogador
ler_Input_Peca_jogaveis(InputPeca):-
	read(InputPeca),
	jogaveis(InputPeca).

ler_Input_Peca_jogaveis(InputPeca):-
	write('Peça inválida!'),
	nl,
	ler_Input_Peca_jogaveis(InputPeca).







%%%%%%%%%%%%%%%%%%%%%%  MODIFICAR TABULEIRO %%%%%%%%%%%%%%%%%%%%%%%%%%

%modifica o tabuleiro depois de uma jogada
%mudarTabuleiro(tabuleiro, posição jogada, posição para onde a mudou, linha atual, Tipo de peça,  tabuleiro novo)
mudarTabuleiro(Tab,[X,Y],[W,Z],TP,TabNovo3):-  % comer para cima esq
	C2 is X-W,
	C2 == 2,
	Z<Y,

	X1 is X-1,
	Y1 is Y-1,
	mudarTabuleiro_posicao_para_asterisco(Tab,[X1,Y1],1,TabNovo),	
	mudarTabuleiro_posicao_para_asterisco(TabNovo,[X,Y],1,TabNovo2),
	mudarTabuleiro_posicao_paraT_P(TabNovo2,[W,Z],1,TP,TabNovo3),!.

mudarTabuleiro(Tab,[X,Y],[W,Z],TP,TabNovo3):-  % comer para cima dir
	C2 is X-W,
	C2 == 2,
	Z>Y,

	X1 is X-1,
	Y1 is Y+1,
	mudarTabuleiro_posicao_para_asterisco(Tab,[X1,Y1],1,TabNovo),	
	mudarTabuleiro_posicao_para_asterisco(TabNovo,[X,Y],1,TabNovo2),
	mudarTabuleiro_posicao_paraT_P(TabNovo2,[W,Z],1,TP,TabNovo3),!.

mudarTabuleiro(Tab,[X,Y],[W,Z],TP,TabNovo3):-  % comer para baixo
	C1 is W-X,
	C1 == 2,
	Y<Z,
	
	X1 is X+1,
	Y1 is Y+1,
	mudarTabuleiro_posicao_para_asterisco(Tab,[X1,Y1],1,TabNovo),
	mudarTabuleiro_posicao_para_asterisco(TabNovo,[X,Y],1,TabNovo2),
	mudarTabuleiro_posicao_paraT_P(TabNovo2,[W,Z],1,TP,TabNovo3),!.

mudarTabuleiro(Tab,[X,Y],[W,Z],TP,TabNovo3):-  % comer para baixo esq
	C1 is W-X,
	C1 == 2,
	Y>Z,
	
	X1 is X+1,
	Y1 is Y-1,
	mudarTabuleiro_posicao_para_asterisco(Tab,[X1,Y1],1,TabNovo),
	mudarTabuleiro_posicao_para_asterisco(TabNovo,[X,Y],1,TabNovo2),
	mudarTabuleiro_posicao_paraT_P(TabNovo2,[W,Z],1,TP,TabNovo3),!.

mudarTabuleiro(Tab,Pj,Pm,TP,TabNovo2):-
	mudarTabuleiro_posicao_para_asterisco(Tab, Pj,1,TabNovo),
	mudarTabuleiro_posicao_paraT_P(TabNovo,Pm,1,TP,TabNovo2),!.




%mudarTabuleiro_posicao_jogada(Tabuleiro,posição jogada,nr da linha, novo tabuleiro)
% Quando encontro a linha que quero mando para a mudarTabuleiro_posicao_jogada2 que me vai modificar a coluna certa.
mudarTabuleiro_posicao_para_asterisco([],_,_,[]).
mudarTabuleiro_posicao_para_asterisco([Z|Zs],[X,Y],L,[NL|Zs]):-
	X==L,
	mudarTabuleiro_posicao_para_asterisco2(Z,Y,1,NL).

mudarTabuleiro_posicao_para_asterisco([Z|Zs],Pj,L,[Z|Tabs]):-
	L1 is L+1,
	mudarTabuleiro_posicao_para_asterisco(Zs,Pj,L1,Tabs).


%mudarTabuleiro_posicao_jogada2(linha que quero, coluna da peça jogada, nr da coluna, nova linha)
mudarTabuleiro_posicao_para_asterisco2([],_,_,[]).

mudarTabuleiro_posicao_para_asterisco2([_|Zs],Y,C,[*|Zs]):-
	Y==C, !.

mudarTabuleiro_posicao_para_asterisco2([Z|Zs],Y,C,[Z|NL]):-
	C1 is C+1,
	mudarTabuleiro_posicao_para_asterisco2(Zs,Y,C1,NL).




%mudarTabuleiro_posicao_mudada(Tabuleiro,posição jogada,nr da linha, Tipo de peça, novo tabuleiro)
% Quando encontro a linha que quero mando para a mudarTabuleiro_posicao_mudada2 que me vai modificar a coluna certa.
mudarTabuleiro_posicao_paraT_P([],_,_,_,[]).

mudarTabuleiro_posicao_paraT_P([Z|Zs],[X,Y],L,TP,[NL|Zs]):-
	X==L,
	L==8,
	TP==p,
	mudarTabuleiro_posicao_paraT_P2(Z,Y,1,1,NL).

mudarTabuleiro_posicao_paraT_P([Z|Zs],[X,Y],L,TP,[NL|Zs]):-
	X==L,
	L==1,
	TP==b,
	mudarTabuleiro_posicao_paraT_P2(Z,Y,1,2,NL).

mudarTabuleiro_posicao_paraT_P([Z|Zs],[X,Y],L,TP,[NL|Zs]):-
	X==L,
	mudarTabuleiro_posicao_paraT_P2(Z,Y,1,TP,NL).

mudarTabuleiro_posicao_paraT_P([Z|Zs],Pj,L,TP,[Z|Tabs]):-
	L1 is L+1,
	mudarTabuleiro_posicao_paraT_P(Zs,Pj,L1,TP,Tabs).


%mudarTabuleiro_posicao_mudada2(linha que quero, coluna da peça jogada, nr da coluna, tipo de peça,nova linha)
mudarTabuleiro_posicao_paraT_P2([],_,_,[]).

mudarTabuleiro_posicao_paraT_P2([_|Zs],Y,C,TP,[TP|Zs]):-
	Y==C, !.

mudarTabuleiro_posicao_paraT_P2([Z|Zs],Y,C,TP,[Z|NL]):-
	C1 is C+1,
	mudarTabuleiro_posicao_paraT_P2(Zs,Y,C1,TP,NL).





%%%%%%%%%%%%%%%%%%%%%%  Algoritmo minimax alfa-beta pruning %%%%%%%%%%%%%%%%%%%%%%%%%%



alphaBeta(Pos, _, _, 1, _, Val):-
	staticval(Pos, Val),!.

alphaBeta(Pos, _, _, _, _, Val):-
	moves(Pos, []),
	staticval(Pos,Val),!.

alphaBeta(Pos, Alpha, Beta, Depth, GoodPos, Val):-
	moves(Pos, PosList),!,
	Depth1 is Depth - 1,
	boundedBest(PosList, Alpha, Beta, Depth1, GoodPos, Val);
	staticval(Pos, Val).



boundedBest([Pos|PosList], Alpha, Beta,Depth, GoodPos, GoodVal):-
	alphaBeta(Pos, Alpha, Beta, Depth, _, Val),
	goodEnough(PosList, Alpha, Beta, Pos, Val, Depth, GoodPos, GoodVal).


goodEnough([], _, _, Pos, Val, _, Pos, Val):- !.
goodEnough(_, Alpha, Beta, Pos, Val, _, Pos, Val):-
	min_to_move(Pos), Val > Beta, !;
	max_to_move(Pos), Val < Alpha, !.

goodEnough(PosList, Alpha, Beta, Pos, Val, Depth, GoodPos, GoodVal):-
	newBounds(Alpha, Beta, Pos, Val, NewAlpha, NewBeta),
	boundedBest(PosList, NewAlpha, NewBeta,Depth, Pos1, Val1),
	betterof(Pos, Val, Pos1, Val1, GoodPos, GoodVal).


newBounds(Alpha, Beta, Pos, Val, Val, Beta):-
	min_to_move(Pos),
	Val > Alpha,!.

newBounds(Alpha, Beta, Pos, Val, Alpha, Val):-
	max_to_move(Pos),
	Val < Beta,!.

newBounds(Alpha, Beta, _, _, Alpha, Beta).


betterof(Pos, Val, _, Val1, Pos, Val):-
	min_to_move(Pos), Val > Val1, !;
	max_to_move(Pos), Val < Val1, !.

betterof(_, _, Pos1, Val1, Pos1, Val1).

min_to_move([1, _, _]).
max_to_move([2, _, _]).	



%staticval ([quem é a vez de jogar, o estado do jogo, tabuleiro], valor do tabuleiro)
staticval([1, win,_], 300).
staticval([2, win,_], -300).

staticval([_, play, Tab],Val):-
	tabuleiro_Pontos(T_Pontos),
	valor_Tab(Tab, T_Pontos, 0, Val),!.

%valor_Tab(Tabuleiro, Tabuleiro de pontod, Val, Val).
%calcula o valor do tabuleiro
valor_Tab([], _, P, P):-!.

valor_Tab([Z|Zs], [X|Xs], P, N):-
	valor_Tab2(Z, X, 0, Ps),  
	P2 is P + Ps,
	valor_Tab(Zs, Xs, P2, N).


%recebe uma linha e manda o valor da linha
valor_Tab2([], _, P, P):-!.

valor_Tab2([p|Zs], [X|Xs], P, N):-
	P2 is P - X, 
	valor_Tab2(Zs, Xs, P2, N),
	!.

valor_Tab2([b|Zs], [X|Xs], P, N):-
	P2 is P + X,
	valor_Tab2(Zs, Xs, P2, N),
	!.

valor_Tab2([1|Zs], [X|Xs], P, N):-
	P1 is P - X,
	P2 is P1 - X,
	valor_Tab2(Zs, Xs, P2, N),
	!.

valor_Tab2([2|Zs], [X|Xs], P, N):-
	P1 is P + X,
	P2 is P1 + X,
	valor_Tab2(Zs, Xs, P2, N),
	!.


valor_Tab2([_|Zs], [_|Xs], P, N):-
	valor_Tab2(Zs, Xs, P, N).


%constroi a lista de Pos ----------
moves([1, _, Tab], PosList):-
	construir_array_P(Tab, 1,Tab, X),  %recebe aqui a lista com as opçoes
	verifica(X,X2),
	criar_Lista_tabuleiros(Tab, X2, ListTab),
	criar_PosList(ListTab, 1, PosList).

moves([2, _, Tab], PosList):-
	construir_array_B(Tab, 1,Tab, X),  %recebe aqui a lista com as opçoes
	verifica(X,X2),
	criar_Lista_tabuleiros(Tab, X2, ListTab),
	criar_PosList(ListTab, 2, PosList).

%elimina as listas vezias
verifica([],[]).
verifica([[]|Zs], NZs):-
	verifica(Zs,NZs),!.
verifica([Z|Zs],[Z|NZs]):-
	verifica(Zs, NZs).


%vai pegar linha à linha do array e se nao for vazia manda para a função seguinte
criar_Lista_tabuleiros(_, [], []):-!.
criar_Lista_tabuleiros(Tab, [Z|ZS], L):-
	length(Z, R),
	R \== 0, 
	criar_Lista_tabuleiros2(Tab, Z, T),
	criar_Lista_tabuleiros(Tab, ZS, TS),
	append(T,TS, L),
	!.


criar_Lista_tabuleiros(Tab, [_|ZS], TS):-
	criar_Lista_tabuleiros(Tab, ZS, TS),!.



%recebe a linha da lista e vai buscar caso a caso
criar_Lista_tabuleiros2(_, [], []):-!.
criar_Lista_tabuleiros2(Tab, [Z|ZS], L):-
	criar_Lista_tabuleiros3(Tab, Z, T),
	criar_Lista_tabuleiros2(Tab, ZS, TS),
	append(T,TS, L),
	!.


%recebe ja as posições
criar_Lista_tabuleiros3(_, [_], []):-!.
criar_Lista_tabuleiros3(Tab, [Z, ZS | Z1], [T|TS]):-
	ver_tipo_peca(Tab, Z, 1,TP),
	mudarTabuleiro(Tab, Z, ZS, TP, T),
	criar_Lista_tabuleiros3(Tab, [Z|Z1], TS),
	!.


criar_PosList([], _, []):-!.
criar_PosList([Tab|Tabs], X, [T|TS]):-
	nextPlayer(X, X1),
	verifica_vencedor(Tab, X),
	T = [X1, win, Tab], 
	criar_PosList(Tabs, X, TS),
	!.

criar_PosList([Tab|Tabs], X, [T|TS]):-
	nextPlayer(X, X1),
	T = [X1, play, Tab],
	criar_PosList(Tabs, X, TS),
	!.


nextPlayer(1,2).
nextPlayer(2,1).