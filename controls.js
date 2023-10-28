class Controls{
    constructor(type, timeForUpdateProlog=100){
        this.forward=false;
        this.left=false;
        this.right=false;
        this.reverse=false;
        this.boom=false;
        this.timeForUpdateProlog=timeForUpdateProlog
        this.lastUpdate=Date.now();

        switch(type){
            case "KEYS":
                this.#addKeyboardListeners();
                break;
            case "DUMMY":
                this.forward=Math.random() > 0.5;
                this.reverse=!this.forward;
                this.left=Math.random() > 0.5;
                this.right=!this.left;
                break;
            case "PROLOG":
                // this.updateProlog();
                break;
        }
    }

    updateJSONKeys(controls) {
        //console.log(controls);
        this.forward = (controls.forward)?true:false;
        this.reverse = (controls.reverse)?true:false;
        this.left = (controls.left)?true:false;
        this.right = (controls.right)?true:false;
        this.boom = (controls.boom)?true:false;
        //console.log('forward: '+this.forward+' ('+controls.forward+') -- reverse: '+this.reverse+' ('+controls.reverse+') -- '+
        //            'left: '+this.left+' ('+controls.left+') -- right: '+this.right+' ('+controls.right+')'+this.boom+' ('+controls.boom+')')
    }

    updateProlog(sensors, x, y, angle, prologID, score) {
        // delay para evitar travamento:
        if (Date.now() - this.lastUpdate < this.timeForUpdateProlog) return;
        this.lastUpdate = Date.now();
        var s1=sensors[0], s2=sensors[1], s3=sensors[2], s4=sensors[3], s5=sensors[4], s6=sensors[5];
        if (x==undefined || y ==undefined || angle== undefined || s1==undefined ||
            s2==undefined || s3==undefined || s4==undefined || s5==undefined || s6==undefined) return;
        var URL = ("./action?"+
                   "id="+prologID+
                   "&s1="+s1+"&s2="+s2+"&s3="+s3+"&s4="+s4+"&s5="+s5+"&s6="+s6+
                   "&x="+x+"&y="+y+"&angle="+angle+"&score="+score);
        //console.log(URL);
        $.getJSON(
            URL,
            this.updateJSONKeys.bind(this)
        );
    }

    updateDUMMYKeys(sensors) {
        if ((Date.now() - this.lastUpdate) < 1000)
            return;
        else{
            this.lastUpdate = Date.now();
            const r = Math.random();
            if (r < .4) {
                this.forward = true;
                this.reverse = false;
                this.left = false;
                this.right = false;
            } else if (r < .5) {
                this.reverse = true;
                this.forward = false;
                this.left = false;
                this.right = false;
            } else if (r < .8) {
                this.left = true;
                this.right = false;
            }else{
                this.right = true;
                this.left = false;
            }
            // console.log('R:'+this.right+', L:'+this.left+', F:'+this.forward+', R:'+this.reverse);
        }
        if (sensors[2] != undefined){
            if (sensors[2] > .3)
                this.boom = true;
            if (sensors[2] > .8) {
                this.forward = false;
                this.reverse = true;
            }
        }
    }

    getBOOM() {
        let ret = this.boom;
        this.boom = false;
        return ret;
    }

    #addKeyboardListeners(){
        document.addEventListener("keydown",(event) => {
            switch(event.key){
                case "ArrowLeft":
                    this.left=true;
                    break;
                case "ArrowRight":
                    this.right=true;
                    break;
                case "ArrowUp":
                    this.forward=true;
                    break;
                case "ArrowDown":
                    this.reverse=true;
                    break;
                case "Enter":
                case " ":
                    this.boom=true;
                    break;
            }
        });
        document.addEventListener("keyup",(event) => {
            switch(event.key){
                case "ArrowLeft":
                    this.left=false;
                    break;
                case "ArrowRight":
                    this.right=false;
                    break;
                case "ArrowUp":
                    this.forward=false;
                    break;
                case "ArrowDown":
                    this.reverse=false;
                    break;
            }
        });
    }
}
