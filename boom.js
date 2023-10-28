class Boom{
    constructor(x, y, angle, tankSize, size=5, speed=5){
        this.x = x - Math.sin(angle)*(tankSize/2);
        this.y = y - Math.cos(angle)*(tankSize/2);
        this.angle = angle;
        this.size = size;
        this.speed = speed;
        this.deactivated = false;
        this.circle = this.#createCircle();
    }

    deactivate() {
        this.deactivated = true;
    }

    getCircle() {
        if (!this.deactivated) return this.circle;
        else return [];
    }

    #move(){
        this.x-=Math.sin(this.angle)*this.speed;
        this.y-=Math.cos(this.angle)*this.speed;
    }

    update(arena) {
        if (this.deactivated) return;
        this.#move();
        this.circle = this.#createCircle();
        //if (this.#assessDamage(arena))
        if (this.x < arena.left || this.x > arena.right || this.y < arena.top || this.y > arena.bottom) {
            this.deactivate()
        }
        
    }
    
    #createCircle() {
        const points=[];
        const rad=Math.hypot(this.size,this.size)/2;
        const alpha=Math.atan2(this.size,this.size);
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

    #assessDamage(arenaBorders){
        for(let i=0;i<arenaBorders.length;i++){
            if(polysIntersect(this.circle,arenaBorders[i])){
                return true;
            }
        }
        
        return false;
    }

    draw(ctx){
        if (this.deactivated) return;
        ctx.save();
        //ctx.translate(this.x,this.y);
        //ctx.rotate(-this.angle);
        ctx.beginPath();
        ctx.arc(this.x, this.y, this.size, 0, 2 * Math.PI);
        ctx.fillStyle = "black";
        ctx.fill();
        ctx.restore();
        //console.log(this.x, this.y);
    }
}
