% Tanque 1
:- module(tanque1, [obter_controles/2]).
:- dynamic entradaAnterior/1.
:- dynamic comandoAnterior/1.
:- dynamic movimentoRepetido/1.

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
%Sentido FORWARD, REVERSE
%Direção  LEFT, RIGHT

%controles comums
%FORWARD, REVERSE, LEFT, RIGHT, BOOM
movimentoValido([FORWARD, REVERSE, LEFT, RIGHT, BOOM]) :- FORWARD =\= REVERSE, LEFT =\= RIGHT, !.

%comparação de lista
listas_iguais([], []).
listas_iguais([H1|T1], [H2|T2]) :-
    H1 =:= H2,
    listas_iguais(T1, T2).

%estados anterior
entradaAnterior([0, 0, 0, 0, 0, 0, 0, 0, 0, 100]).
comandoAnterior([1, 0, 1, 0, 0]).
movimentoRepetido(0).

%Atalizando estados
atualizar_estados_movimentoRepetido(Comandos) :- 
    comandoAnterior(Lc),
    movimentoRepetido(X),
    listas_iguais(Comandos, Lc),
    retract(movimentoRepetido(_)),
    Xi is X + 1,
    asserta(movimentoRepetido(Xi)),
    !
    .
atualizar_estados_movimentoRepetido( _) :- 
    retract(movimentoRepetido(_)),
    asserta(movimentoRepetido(0))
    .

atualizar_estados(Entradas, Comandos) :- 
    atualizar_estados_movimentoRepetido(Comandos),
    retract(entradaAnterior(_)),
    retract(comandoAnterior(_)),
    asserta(entradaAnterior(Entradas)),
    asserta(comandoAnterior(Comandos)).

%Verificando se algum tanque bateu nele e o sensor não captou
batida(SCORE) :- (entradaAnterior([_, _, _, _, _, _, _, _, _, ScoreAnterior]), SCORE =:= ScoreAnterior + 2).

%Escolhas
%Escolha de sentido
%Retorno [Frente, Ré]
escolher_sentido([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [1, 0]) :- 
    batida(SCORE), 
    S1 =\= 0, S2 =\= 0, S3 =\= 0, S4 =\= 0, S5 =\= 0, S6 =\= 0.
escolher_sentido([_, _, _, _, _, 1, _, _, 1, _], [F , R]) :- 
    comandoAnterior([F, R, _, _, _]).
escolher_sentido([_, _, _, _, _, F, _, _, T, _], [1 , 0]) :- 
    F =< (T - 0.3).
escolher_sentido(_, [0 , 1]).

%Escolhas de direção
%Retorno [Esquerda, Direita]
escolher_direcao([_, _, _, 1, 1, _, 1, 1, _, _], [E, D]) :- comandoAnterior([_, _, E, D, _]).
escolher_direcao([_, _, _, E1, E2, _, D1, D2, _, _], [1, 0]) :- ((E1 + E2) / 2) =< ((D1 + D2) / 2).
escolher_direcao(_, [0, 1]).

%Identifica necessidade de atirar
identifica_necessidade_atirar([_, _, _, _, _, T, _, _, _, _], 1) :- T < 0.6, comandoAnterior([_, _, _, _, 0]), !.
identifica_necessidade_atirar(_, 0).

%%% Faça seu codigo a partir daqui, sendo necessario sempre ter o predicado:
%%%% obter_controles([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [FORWARD, REVERSE, LEFT, RIGHT, BOOM]) :- ...
troca(0, 1).
troca(1, 0).

obter_controles([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [FORWARD, REVERSE, LEFT, RIGHT, BOOM]) :-
    escolher_sentido([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [FORWARD, REVERSE]),
    escolher_direcao([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [LEFT, RIGHT]),
    identifica_necessidade_atirar([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], BOOM),
    movimentoValido([FORWARD, REVERSE, LEFT, RIGHT, BOOM]),
    atualizar_estados([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [FORWARD, REVERSE, LEFT, RIGHT, BOOM]),
    movimentoRepetido(Qtd),
    Qtd =< 5,
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