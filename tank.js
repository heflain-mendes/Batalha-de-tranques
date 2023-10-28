class Tank{
    constructor(x,y,width,height,
                arenaheight,arenawidth,padding,
                controlType,maxSpeed=3,color="lightBlue",
                score=100,prologID=-1,name="Humano",
                timeForUpdateProlog=100){
        this.x=x;
        this.y=y;
        this.width=width;
        this.height=height;
        this.name=name;
        this.color=color;

        this.prologID=prologID;
        this.arenaleft = padding;
        this.arenatop = padding;
        this.arenawidth = arenawidth - padding;
        this.arenaheight = arenaheight - padding;

        this.speed=0;
        this.acceleration=0.2;
        this.maxSpeed=maxSpeed;
        this.friction=0.05;
        this.angle=0;
        this.controlType=controlType;
        this.steps=0;
        this.smallSteps=0;
        this.damage=false;
        this.sensor=new Sensor(this);
        this.score=score;
        this.lastDamage = Date.now();

        this.controls=new Controls(controlType,timeForUpdateProlog);
        this.polygon=this.#createPolygon();

        [this.img, this.mask] = this.#getTankImg(color);
        [this.img2, this.mask2] = this.#getTankImg("black");
    }

    #getTankImg(color) {
        var img=new Image();
        if (this.controlType == "DUMMY")
            // Image adapted from: https://freesvg.org/1528027603
            img.src = "tank_dummy.png"
        else
            // Image adapted from: https://freesvg.org/1528027543
            img.src="tank.png"

        var mask=document.createElement("canvas");
        mask.width=this.width;
        mask.height=this.height;
        
        const maskCtx=mask.getContext("2d");
        img.onload=()=>{
            maskCtx.fillStyle=color;
            maskCtx.rect(0,0,this.width,this.height);
            maskCtx.fill();

            maskCtx.globalCompositeOperation="destination-atop";
            maskCtx.drawImage(img,0,0,this.width,this.height);
        }
        return [img, mask];
    }

    update(arenaBorders,tanks,booms){
        if (this.score > 0){
            switch(this.controlType) {
                case "PROLOG":
                    this.controls.updateProlog(this.getSensors(), this.x, this.y, this.angle, this.prologID, this.score);
                    break
                case "DUMMY":
                    this.controls.updateDUMMYKeys(this.getSensors());
                    break;
                case "KEYS":
                    //console.log(this.getSensors());
                    break;
            }
            this.#move();
        } else {
            this.img = this.img2;
            this.mask = this.mask2;
        }
        this.polygon=this.#createPolygon();
        if (this.#assessDamage(arenaBorders,tanks)) {
            this.damage = true;
            this.#collision();
        }else{
            this.damage = false;
        }
        if (this.#boomDamage(booms)) {
            this.score=(this.score-10>0)?this.score-10:0;
        }
        if(this.sensor){
            this.sensor.update(arenaBorders,tanks);
        }

        let boom = this.controls.getBOOM() && (this.score > 0);
        return [boom, this.x, this.y, this.angle];
    }

    getSensors() {
        if(this.sensor){
            const offsets=this.sensor.readings.map(
                s=>s==null?0:1-s.offset
            );
            return offsets;
        }else{
            return [0,0,0,0,0];
        }
    }

    #boomDamage(booms){
        for (let i=0;i<booms.length;i++) {
            if (polysIntersect(this.polygon, booms[i].getCircle())) {
                booms[i].deactivate();
                return true;
            }
        }
        return false;
    }

    #assessDamage(arenaBorders,tanks){
        for(let i=0;i<arenaBorders.length;i++){
            if(polysIntersect(this.polygon,arenaBorders[i])){
                return true;
            }
        }
        for(let i=0;i<tanks.length;i++){
            if(polysIntersect(this.polygon,tanks[i].polygon)){
                return true;
            }
        }
        if (this.x < 0 || this.y < 0 ||
            this.x > this.arenawidth || this.y > this.arenaheight) {
            this.score = 0;
        }
        return false;
    }

    #createPolygon(){
        const points=[];
        const rad=Math.hypot(this.width,this.height)/2;
        const alpha=Math.atan2(this.width,this.height);
        points.push({
            x:this.x-Math.sin(this.angle-alpha)*rad,
            y:this.y-Math.cos(this.angle-alpha)*rad
        });
        points.push({
            x:this.x-Math.sin(this.angle+alpha)*rad,
            y:this.y-Math.cos(this.angle+alpha)*rad
        });
        points.push({
            x:this.x-Math.sin(Math.PI+this.angle-alpha)*rad,
            y:this.y-Math.cos(Math.PI+this.angle-alpha)*rad
        });
        points.push({
            x:this.x-Math.sin(Math.PI+this.angle+alpha)*rad,
            y:this.y-Math.cos(Math.PI+this.angle+alpha)*rad
        });
        return points;
    }

    #move(){
        if(this.controls.forward){
            this.speed+=this.acceleration;
        }
        if(this.controls.reverse && !this.damage){
            this.speed-=this.acceleration;
        }

        if(this.speed>this.maxSpeed){
            this.speed=this.maxSpeed;
        }
        if(this.speed<-this.maxSpeed/2){
            this.speed=-this.maxSpeed/2;
        }

        if(this.speed>0){
            this.speed-=this.friction;
        }
        if(this.speed<0){
            this.speed+=this.friction;
        }
        if(Math.abs(this.speed)<this.friction){
            this.speed=0;
        }

        if(this.speed!=0){
            const flip=this.speed>0?1:-1;
            if(this.controls.left){
                this.#updateAngle(this.angle+0.03*flip);
            }
            if(this.controls.right){
                this.#updateAngle(this.angle-0.03*flip);
            }
        }
        
        this.x-=Math.sin(this.angle)*this.speed;
        this.y-=Math.cos(this.angle)*this.speed;
    }

    #updateAngle(newAngle) {
        newAngle = newAngle % (Math.PI*2);
        if (newAngle < 0) newAngle = Math.PI*2 + newAngle;
        this.angle = newAngle;
    }

    #collision() {
        if (this.score <= 0) return;
        var oldSpeed = this.speed, sensors, inc=1.5;
        const flip=this.speed>0?1:-1;
        if(this.controls.left){
            this.#updateAngle(this.angle+0.03*flip);
        }
        if(this.controls.right){
            this.#updateAngle(this.angle-0.03*flip);
        }
        sensors = this.getSensors();
        for (var i=0;i<sensors.length-1;i++)
            if (sensors[i] > 0.8) this.speed = -0.2;
        this.x-=Math.sin(this.angle)*this.speed*inc;
        this.y-=Math.cos(this.angle)*this.speed*inc;
        inc = 0.01;
        this.speed = (oldSpeed>0)?oldSpeed*inc:oldSpeed*inc;
        if ((Date.now()-this.lastDamage) > 1000) {
            this.score=(this.score-2>0)?this.score-2:0;
            this.lastDamage = Date.now();
        }
    }

    draw(ctx,drawSensor=false){
        ctx.save();
        ctx.translate(this.x,this.y);
        ctx.rotate(-this.angle);
        ctx.drawImage(this.mask,
            -this.width/2,
            -this.height/2,
            this.width,
            this.height);
        ctx.globalCompositeOperation="multiply";
        ctx.drawImage(this.img,
            -this.width/2,
            -this.height/2,
            this.width,
            this.height);
        if (this.score > 20)
            ctx.fillStyle = "black";
        else
            ctx.fillStyle = "red";
        ctx.font = "bold 10px Arial";
        ctx.fillText(''+this.score,-this.width/4,this.height/3);
        ctx.restore();

        if(this.sensor && drawSensor){
            this.sensor.draw(ctx);
        }
    }
}
