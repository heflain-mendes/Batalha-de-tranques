//// não mexa nessas linhas:
var windowHeight=$(window).height(), windowWidth=$(window).width(), lastUpdateScore=Date.now(), prologTankIDs=0, timeForUpdatingProlog=200;
const canvas=document.getElementById("myCanvas");
canvas.width=800;//windowWidth;
canvas.height=600;//windowHeight;
const ctx = canvas.getContext("2d");
const dummyTunkNames = /*from chatGPT*/[ "Boladão", "Rabugento", "Trovão", "Bagunceiro", "Marrento", "Trambiqueiro", "Espertinho", "Sorriso", "Soneca", "Maluco", "Zé Bala", "Trapalhão", "Fofinho", "Dengoso", "Terremoto", "Estabanado", "Cuspidor de Fogo", "Doidivanas", "Trovador", "Curioso", "Esquentadinho", "Pestinha", "Trapaceiro", "Esperto", "Relâmpago", "Roncador", "Surpresa", "Malandrinho", "Borbulhante", "Folgado", "Trovão Azul", "Espião", "Explosivo", "Cabeça de Vento", "Malabarista", "Tristonho", "Saltitante", "Dorminhoco", "Felpudo", "Arrasador", "Espirra-Água", "Trapaceiro", "Esquentado", "Reluzente", "Fofoqueiro", "Torpedo", "Dente de Leão", "Terrível", "Sapeca", "Bate-Papo", "Barulhento", "Faísca", "Linguarudo", "Abobalhado", "Bagunceiro", "Furacão", "Tagarela", "Artilheiro", "Engraçadinho", "Furioso", "Bicudo", "Mágico", "Espanta-Mosquito", "Ziguezague", "Estiloso", "Brincalhão", "Trancado", "Bagunçado", "Sorridente", "Tornado", "Desastrado", "Malabarista", "Mala Sem Alça", "Borbulhante", "Dorminhoco", "Trovão Azul", "Risadinha", "Bagunceiro", "Barulhento", "Fofinho", "Sorriso Largo", "Reluzente", "Esperto", "Arrepiante", "Mexeriqueiro", "Estelar", "Roncador", "Zigzag", "Fanfarrão", "Bate-Papo", "Trapaceiro", "Estourado", "Espirra-Água", "Bagunceiro", "Bagunça", "Trovador", "Saltitante", "Cabeça de Vento", "Veloz" ];
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//// Essa área pode ser configurada por você, mas aconselho não trocar o speer, arenaPadding e tamanho dos tanques.
const speed=1.5, arenaPadding=10, tankW=50, tankH=30,
      score=100, // vida de cada tanque
      dummyTanks=10, // quantidade de tanques aleatórios
      keysTank=true, // modifique para ter um tanque controlado pelo teclado
      // nome dos tanques controlados por Prolog (obs.: tem que adaptar o servidor.pl ao mexer aqui)
      // a quantidade é referente a quantidade de nomes, na falta de criatividade, o nome pode repetir... rs
      // exemplos de dois:
      prologTanks=["Ligerin", "ApagaFogo"], // se quiser colocar dois tanques proloog, faça assim
      //prologTanks=["Ligerin"], // escolha aqui o nome de seu tanque controlado por prolog
      //prologTanks=[], //se não quiser nenhum tanque prolog, faça assim
      showSensors=false, //modifique para mostrar os sensores dos tanques PROLOG e KEYS
      showSensorsOfDummyTanks=false; //modifique para mostrar os sensores dos tanques DUMMY
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

//// Não mexa daqui para baixo:
const arena = new Arena(canvas.height, canvas.width, arenaPadding), _colors = new Colors(0);
var tanks = [];
if (keysTank) tanks.push(newTank("KEYS"));
for (let i=0;i<prologTanks.length;i++) tanks.push(newTank("PROLOG"));
for (let i=0;i<dummyTanks;i++) tanks.push(newTank("DUMMY"));

var allBOOMS = new Array(), lastBullet = new Array(tanks.length);
for (let i=0;i<lastBullet.length;i++) lastBullet[i] = 0;

animate();

// controls: ["DUMMY", "KEYS", "PROLOG"]
function newTank(controls="DUMMY") {
    let pos = getPosition(), name, id=-1;
    switch(controls) {
        case "PROLOG":
            name=prologTanks[prologTankIDs];
            id=prologTankIDs++;
            break;
        case "DUMMY":
            name=dummyTunkNames[Math.round(Math.random()*dummyTunkNames.length)%dummyTunkNames.length];
            break;
        case "KEYS":
            name="Humano";
            break;
    }
    return new Tank(pos.x,pos.y,tankH,tankW,
                    canvas.height,canvas.width,
                    arenaPadding,controls,speed,
                    _colors.getColor(), score,
                    (controls=="PROLOG")?id:-1,
                    name, timeForUpdatingProlog);
}

function getPosition() {
    let tankPadding = arenaPadding+Math.max(tankH, tankW);
    let x = parseInt(Math.random()*(canvas.width-tankPadding*2)+tankPadding),
        y = parseInt(Math.random()*(canvas.height-tankPadding*2)+tankPadding);
    return {x:x, y:y};
}

function getScores() {
    let ret = {scores:new Array(tanks.length), winner:undefined}, aux;
    aux = 0;
    for (let i=0;i<tanks.length;i++) {
        ret.scores[i]=tanks[i].score;
        if (tanks[i].score > 0)
            if (aux++ == 0) ret.winner = i;
            else ret.winner = undefined;
    }
    return ret;
}

function updateScoresDiv(scores) {
    let e = $('#id_score');
    e.empty();
    if (scores.winner != undefined) {
        $('#id_winner').text("Vencedor: Tank "+scores.winner+
                             " ("+tanks[scores.winner].name+","+tanks[scores.winner].controlType+")");
        $('#id_km').hide();
    }else{
        lastUpdateScore = Date.now();
        for (let i=0;i<scores.scores.length;i++){
            e.append('<label style="color:'+tanks[i].color+';">'+tanks[i].name+': '+scores.scores[i]+'</label>');
        }
    }
}

$('#kmdiv').toggle();
updateScoresDiv(getScores());
document.addEventListener("keydown",function(event) {
    switch(event.key){
        case "s":
            $('#kmdiv').toggle();
            break;
    }
});

function updateCanvas(){
    // Updating booms (removing deactivted ones)
    var newBooms = new Array();
    for (let i=0;i<allBOOMS.length;i++){    
        if (!allBOOMS[i].deactivated)
            newBooms.push(allBOOMS[i]);
    }
    allBOOMS = newBooms;

    // Updating tanks
    for(let i=0;i<tanks.length;i++){
        // Tanks list :
        let newTanks = new Array();
        for(let j=0;j<tanks.length;j++){
            if (i!=j) newTanks.push(tanks[j]);
        }
        // For each tank, it will check the other tanks and bombs
        boom = tanks[i].update(arena.borders,newTanks,allBOOMS);
        // If it goes more than 1000 milliseconds after the last bomb, it can fire again
        if (boom[0] && Math.abs(lastBullet[i]-Date.now())>1000) {
            let bomb = new Boom(boom[1],boom[2],boom[3],Math.max(tankH, tankW));
            lastBullet[i]=Date.now();
            allBOOMS.push(bomb);
        }
    }
    // bombs update:
    for (let i=0;i<allBOOMS.length;i++){
        allBOOMS[i].update(arena.position);
    }
}

function animate(){
    var scores = getScores();
    var runFinished = scores.winner != undefined;
    if ((Date.now() - lastUpdateScore) > 1000)
        updateScoresDiv(scores);
    
    updateCanvas()  

    ctx.save();
    arena.draw(ctx);
    
    for(let i=0;i<tanks.length;i++){
        if (tanks[i].controlType == "DUMMY")
            tanks[i].draw(ctx, showSensorsOfDummyTanks);
        else
            tanks[i].draw(ctx, showSensors);
    }
    for (let i=0;i<allBOOMS.length;i++){
        if (allBOOMS[i] != undefined)
            allBOOMS[i].draw(ctx);
    }

    ctx.restore();

    if (runFinished) {
        scores = getScores();
        updateScoresDiv(scores);
        updateScoresDiv = function(){};
        $('#kmdiv').show();
    }else{
        requestAnimationFrame(animate);
    }
}
