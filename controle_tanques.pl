% Controle dos tanques
:- module(controle_tanques, [vez/3]).

% deve existir um arquivo com o nome "tanque0.pl" ou "tanque0.pro" com o predicado obter_controles/2
:- use_module(tanque0, [obter_controles/2 as obter_controles0]).

% deve existir um arquivo com o nome "tanque1.pl" ou "tanque1.pro" com o predicado obter_controles/2
:- use_module(tanque1, [obter_controles/2 as obter_controles1]).

%Exemplo de mais um
%:- use_module(tanque2, [obter_controles/2 as obter_controles2]).

% sempre deve-se iniciar pelo zero (0)
vez(0, SENSORES, CONTROLES) :- obter_controles0(SENSORES,CONTROLES).
vez(1, SENSORES, CONTROLES) :- obter_controles1(SENSORES,CONTROLES).
%Exemplo de mais um
%vez(2, SENSORES, CONTROLES) :- obter_controles2(ENSORES,CONTROLES).
