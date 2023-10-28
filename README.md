# Batalha Tanques com Prolog

Trabalho Prático da Disciplina de Inteligência Artificial.

Este trabalho foi desenvolvido como trabalho prático da disciplina de Inteligência Artificial - 2023.

A parte em HTML/JS/CSS foi desenvolvida sobre o trabalho do ano passado, [Corrida em Prolog](https://github.com/jeiks/corrida_em_prolog), que tinha se baseado no código do *Radu Mariescu-Istodor*, disponível em seu [github](https://github.com/gniziemazity/Self-driving-car). 

O objetivo desse trabalho é controlar um tanque utilizando código em Prolog ([SWI-Prolog](https://www.swi-prolog.org/)) para que ele seja o único sobrevivente.
Após isso, também será realizado uma disputa em sala de aula, com todos os trabalhos.
<p align="center"><img src="screenshot-all.png"></p>

Regras:
* Cada tanque pode atirar uma bala por segundo;
* Cada tanque começa com uma vida de 100 pontos;
* Se bater nas laterais do ambiente ou em outro tanque, perde 2 pontos de vida por segundo que estiver em contato/atrito;
* Se for atingido por uma bala, perde 10 pontos de vida.

Teclas:
* Setas do teclado: movimentação do tanque\
  Obs.: só pode movimentar para a esquerda ou direita se estiver andando pra frente ou para trás.
* Espaço ou ENTER: atira\
  Obs.: só pode atirar uma bala por segundo.
* Tecla "s": exibe e esconde o placar do jogo.

### Como configurar a execução do jogo

Há várias formas de utilizar esse código, escolhendo colocar um tanque movido pelo teclado, nenhum ou vários tanques movidos aleatoriamente pelo JavaScript e nenhum ou vários tanques movidos pelo Prolog.

Para isso, edite as seguintes linhas do arquivo  ``main.js``:
```js
const speed=1.5, arenaPadding=10, tankW=50, tankH=30,
      score=100, // vida de cada tanque
      dummyTanks=10, // quantidade de tanques aleatórios
      keysTank=true, // modifique para ter um tanque controlado pelo teclado
      // nome dos tanques controlados por Prolog (obs.: tem que adaptar o controle_tanques.pl ao mexer aqui)
      // a quantidade é referente a quantidade de nomes, na falta de criatividade, o nome pode repetir... rs
      // exemplos de dois:
      //prologTanks=["Ligerin"], // escolha aqui o nome de seu tanque controlado por prolog
      //prologTanks=["Ligerin", "ApagaFogo"], // se quiser colocar dois tanques prolog, faça assim
      prologTanks=[], //se não quiser nenhum tanque prolog, faça assim
      showSensors=false, //modifique para mostrar os sensores dos tanques PROLOG e KEYS
      showSensorsOfDummyTanks=false; //modifique para mostrar os sensores dos tanques DUMMY
```

As linhas já estão explicadas, mas sendo mais claro, as opções são:

* **Ativar o controle de um tanque pelo teclado (setas do teclado e ENTER ou Espaço para atirar)**\
  ```js
  keysTank=true, // modifique para ter um tanque controlado pelo teclado
  ```
* **Modificar a quantidade de tanques controlados pelo JS (movimentos quase-aleatórios)**
  ```js
  dummyTanks=10, // quantidade de tanques aleatórios
  ```
* **Não utilizar nenhum tanque controlado por Prolog**
  ```js
  prologTanks=[], //se não quiser nenhum tanque prolog, faça assim
  ```
* **Utilizar um tanque controlado por Prolog**
  * No arquivo ``main.js``
    ```js
    prologTanks=["Ligerin"], // escolha aqui o nome de seu tanque controlado por prolog
    //prologTanks=[], //se não quiser nenhum tanque prolog, faça assim
    ```
  * No arquivo ``controle_tanques.pl``
    ```prolog
    % deve existir um arquivo com o nome "tanque0.pl" ou "tanque0.pro" com o predicado obter_controles/2
    :- use_module(tanque0, [obter_controles/2 as obter_controles0]).
    % sempre deve-se iniciar pelo zero (0)
    vez(0, SENSORES, CONTROLES) :- obter_controles0(SENSORES,CONTROLES).
    ```
* **Utilizar dois tanques controlados por Prolog**
  * No arquivo ``main.js``
    ```js
    prologTanks=["Ligerin", "ApagaFogo"], // se quiser colocar dois tanques prolog, faça assim
    //prologTanks=["Ligerin"], // escolha aqui o nome de seu tanque controlado por prolog
    //prologTanks=[], //se não quiser nenhum tanque prolog, faça assim
    ```
  * No arquivo ``controle_tanques.pl``
    ```prolog
    % deve existir um arquivo com o nome "tanque0.pl" ou "tanque0.pro" com o predicado obter_controles/2
    :- use_module(tanque0, [obter_controles/2 as obter_controles0]).
    % deve existir um arquivo com o nome "tanque1.pl" ou "tanque1.pro" com o predicado obter_controles/2
    :- use_module(tanque1, [obter_controles/2 as obter_controles1]).
    % sempre deve-se iniciar pelo zero (0)
    vez(0, SENSORES, CONTROLES) :- obter_controles0(SENSORES,CONTROLES).
    vez(1, SENSORES, CONTROLES) :- obter_controles1(SENSORES,CONTROLES). 
    ```

Adicionais:
* mudar a velocidade e a vida dos tanques:
  ```js
   speed=1.5, // mude aqui a velocidade máxima dos tanques
   score=100, // mude aqui a vida dos tanques
  ```
* ativar a visualização dos sensores dos tanques:
  ```js
  ```

### Como executar o servidor pelo Prolog

No momento que você adicionar tanques movidos pelo Prolog, você deve iniciar o servidor para que eles funcionem.

Para isso,
1. instale o [SWI-Prolog](https://www.swi-prolog.org/). Em distribuições baseadas na Debian (como Ubuntu), basta executar:
   ```sh
   sudo apt install swi-prolog
   ```
2. faça o download do código:
   ```sh
   git clone https://github.com/jeiks/batalha_tanques
   ```
3. e execute o comando:
   ```sh
   cd batalha_tanques
   swipl -s servidor.pl (ou abra esse arquivo no swi-prolog)
   ```
   Ele já iniciará o servidor automaticamente e exibirá a seguinte mensagem:
   ```
   --========================================--

   % Started server at http://localhost:8080/


   --========================================--
   ```
4. abra o navegador e acesse [http://localhost:8080/](http://localhost:8080/) (eu testei tudo no Brave-Browser, compatível com Google Chrome).
   
A implentação fornecida aqui está fornecendo dois tanques que tem movimentos aleatórios.

O primeiro é implementado no arquivo ``tanque0.pl`` e o segundo é uma cópia dele, com somente o número modificado, em ``tanque1.pl``.

Ao adicionar mais tanques no ``main.js``, edite o arquivo ``controle_tanques.pl``, seguindo os exemplos já apresentados lá.

Para implementar a "inteligência" dos tanques, edite e siga as intruções do arquivo ``tanque0.pl``  ou ``tanque1.pl``.

### Como modificar o comportamento do tanque no Prolog

Após modificar o ``main.js`` e o ``controle_tanques.pl``, edite o arquivo ``tanque0.pl``:
```prolog
% Tanque 0 -- "tanque0" para seguir o nome do arquivo
:- module(tanque0, [obter_controles/2]).

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

%%% Faça seu codigo a partir daqui, sendo necessario sempre ter o predicado:
%%%% obter_controles([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [FORWARD, REVERSE, LEFT, RIGHT, BOOM]) :- ...

troca(0, 1).
troca(1, 0).
% [FORWARD, REVERSE, LEFT, RIGHT, BOOM]
obter_controles([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [FORWARD, REVERSE, LEFT, RIGHT, BOOM]) :-
    random_between(0,1,AA),
    troca(AA, BB),
    random_between(0,1,CC),
    FORWARD is AA,
    REVERSE is BB,
    LEFT is AA,
    RIGHT is BB,
    BOOM is CC.

% Para evitar erros, o tanque para:
obter_controles(_, [0,0,0,0,0]).
```

Assim, você deve apagar as regras acima e escrever as regras ``obter_controles``. Essa regra deve sempre receber os sensores e retornar 5 ações. Assim, ela deve sempre seguir esse padrão:
```prolog
obter_controles([X,Y,ANGLE,S1,S2,S3,S4,S5,S6,SCORE], [FORWARD, REVERSE, LEFT, RIGHT, BOOM]) :-
```
Claro que, além delas, você deve escrever todas as demais regras e fatos necessários para que o tanque se comporte bem no cenário apresentado.

Have fun =)
