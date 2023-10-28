% Tanque 1
:- module(tanque1, [obter_controles/2]).
:- dynamic entradaAnterior/1.
:- dynamic comandoAnterior/1.

%% Explicação:
% Sensores:
% X: posição horizontal do tanque
% Y: posiçao vertical do tanque
% ANGLE: angulo de inclinacao do robo: 0 para virado para frente até PI*2 (~6.28)
% Sensores: esquerda (S1,S2), centro (S3), direita (S4,S5), ré (S6)
%   S1,S2,S3,S4,S5,S6: valores de 0 à 1, onde 0 indica sem obstáculo e 1 indica tocando o tanque
% SCORE: inteiro com a "vida" do tanque. Em zero, ele perdeu
% Controles:
% [FORWARD, REVERSE, LEFT, RIGHT, BOOM]
% FORWARD: 1 para ir pra frente e 0 para não ir
% REVERSE: 1 para ir pra tras e 0 para não ir
% LEFT: 1 para ir pra esquerda e 0 para não ir
% RIGHT: 1 para ir pra direita e 0 para não ir
% BOOM: 1 para tentar disparar (BOOM), pois ele só pode disparar uma bala a cada segundo
% obter_controles([X,Y,ANGLE,S1,S2,S3,S4,S5,S6], [FORWARD, REVERSE, LEFT, RIGHT, BOOM]) :-
%     FORWARD is 1,
%     REVERSE is 0,
%     LEFT is 1,
%     RIGHT is 0,
%     BOOM is 1.

%Nomeclatura
%Sentido Frente ou Ré
%Direção Direita ou Esquerda
%Trajeto Sentido e direção

%controles comums
movimentoValido([1, 0, 0, 0, 0]).
movimentoValido([1, 0, 0, 1, 0]).
movimentoValido([1, 0, 1, 0, 0]).
movimentoValido([0, 1, 0, 0, 0]).
movimentoValido([0, 1, 0, 1, 0]).
movimentoValido([0, 1, 1, 0, 0]).
movimentoValido([1, 0, 0, 0, 1]).
movimentoValido([1, 0, 0, 1, 1]).
movimentoValido([1, 0, 1, 0, 1]).
movimentoValido([0, 1, 0, 0, 1]).
movimentoValido([0, 1, 0, 1, 1]).
movimentoValido([0, 1, 1, 0, 1]).
movimentoValido([0, 0, 0, 0, 1]).
movimentoValido([0, 0, 0, 0, 0]).

%estados anterior
entradaAnterior([0, 0, 0, 0, 0, 0, 0, 0, 0, 100]).
comandoAnterior([0, 0, 0, 0, 0]).

%Atalizando estados
atualizar_estados(Entradas, Comandos) :- 
    retract(entradaAnterior(_)),
    retract(comandoAnterior(_)),
    asserta(entradaAnterior(Entradas)),
    asserta(comandoAnterior(Comandos)).

%Identificação
%Identificação de proximidade de sentido
%Retorno [Frentre, Re]
identificar_proximidade_sentido([_, _, _, _, _, 1, _, _, 1, _], [1 , 1]) :- !.
identificar_proximidade_sentido([_, _, _, _, _, F, _, _, T, _], [1 , 0]) :- T >= F, !.
identificar_proximidade_sentido(_, [0 , 1]) :- !.

%Identificação de proximidade de direção
%Retorno [Direita, Esquerda]
identificar_proximidade_direcao([_, _, _, 1, 1 ,_, 1, 1, _, _], [1, 1]) :- !.
identificar_proximidade_direcao([_, _, _, E1, E2 ,_, D1, D2, _, _], [1, 0]) :- (E1 + E2) / 2 <= (D1 + D2) / 2, !.
identificar_proximidade_direcao(_, [0, 1]).

%Identifica necessidade de atirar
Identifica_necessidade_atirar([_, _, _, _, _ ,T, _, _, _, _], 1) :- T < 1, !.
Identifica_necessidade_atirar(_, 0).

%%% Faça seu codigo a partir daqui, sendo necessario sempre ter o predicado:
%%%% obter_controles([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [FORWARD, REVERSE, LEFT, RIGHT, BOOM]) :- ...
troca(0, 1) :- !.
troca(1, 0).

obter_controles([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [FORWARD, REVERSE, LEFT, RIGHT, BOOM]) :-
    identificar_proximidade_sentido([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [FORWARD, REVERSE]),
    identificar_proximidade_direcao([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [LEFT, RIGHT]),
    Identifica_necessidade_atirar([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], BOOM),
    movimentoValido([FORWARD, REVERSE, LEFT, RIGHT, BOOM]),
    atualizar_estados([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [FORWARD, REVERSE, LEFT, RIGHT, BOOM]),
    !.

obter_controles(_, [FORWARD, REVERSE, LEFT, RIGHT, BOOM]) :- comandoAnterior([FORWARD, REVERSE, LEFT, RIGHT, BOOM]), !.

%Aleatoriedade
% [FORWARD, REVERSE, LEFT, RIGHT, BOOM]
obter_controles([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [FORWARD, REVERSE, LEFT, RIGHT, BOOM]) :-
    random_between(0,1,AA),
    troca(AA, BB),
    random_between(0,1,CC),
    FORWARD is AA,
    REVERSE is BB,
    LEFT is AA,
    RIGHT is BB,
    BOOM is CC,
    atualizar_estados([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [FORWARD, REVERSE, LEFT, RIGHT, BOOM]),
    !.

% Para evitar erros, o tanque para:
obter_controles(_, [0,0,0,0,0]).
